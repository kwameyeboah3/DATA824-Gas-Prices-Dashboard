# DATA 824 - U.S. Gas Prices Explorer
# Official EIA historical gasoline prices; nominal USD/gallon including taxes.
required <- c('shiny','ggplot2','dplyr','plotly','scales','readxl')
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop('Install packages: install.packages(c(',paste(shQuote(missing),collapse=', '),'))')
library(shiny);library(ggplot2);library(dplyr);library(plotly);library(scales)

# EIA's multi-area historical gasoline workbook (NOT the single-series workbook).
source_url <- 'https://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls'
local_file <- 'pswrgvwall.xls'
parse_dates <- function(v) {
  s <- trimws(as.character(v)); s[s %in% c('','NA','N/A','-')] <- NA_character_
  out <- as.Date(rep(NA_character_,length(s)))
  n <- suppressWarnings(as.numeric(s))
  is_serial <- !is.na(n) & n>20000 & n<90000
  out[is_serial] <- as.Date(floor(n[is_serial]),origin='1899-12-30')
  for(fmt in c('%Y-%m-%d','%Y-%m-%d %H:%M:%S','%m/%d/%Y','%m/%d/%y','%m/%d/%Y %H:%M:%S','%d-%b-%Y','%b %d, %Y','%m-%d-%Y')) {
    idx <- which(is.na(out)&!is.na(s)); if(!length(idx)) break
    z <- suppressWarnings(as.Date(s[idx],format=fmt)); out[idx[!is.na(z)]] <- z[!is.na(z)]
  }
  out
}

# Match only the regular gasoline (all formulations) price series.
region_patterns <- c('United States'='U\\.S\\.|United States|\\bUS\\b',
 'East Coast'='East Coast|PADD ?1(?![ABC])',
 'Midwest'='Midwest|PADD ?2', 'Gulf Coast'='Gulf Coast|PADD ?3',
 'Rocky Mountain'='Rocky Mountain|PADD ?4',
 'West Coast'='West Coast(?! less)|PADD ?5',
 'New England'='New England|PADD ?1A',
 'Central Atlantic'='Central Atlantic|PADD ?1B',
 'Lower Atlantic'='Lower Atlantic|PADD ?1C',
 'West Coast excluding California'='West Coast less California|West Coast excluding California',
 'California'='California')
identify_region <- function(h) {
  # Prefer more specific areas to avoid e.g. California inside West Coast less California.
  order_names <- c('West Coast excluding California','New England','Central Atlantic','Lower Atlantic',
                   'East Coast','Midwest','Gulf Coast','Rocky Mountain','West Coast','California','United States')
  for(nm in order_names) if(grepl(region_patterns[[nm]],h,ignore.case=TRUE,perl=TRUE)) return(nm)
  NA_character_
}

read_sheet <- function(path,sheet) {
  a <- as.data.frame(readxl::read_excel(path,sheet=sheet,col_names=FALSE,
                                        col_types='text',.name_repair='unique'))
  if(ncol(a)<2 || nrow(a)<10) return(NULL)
  date_opts <- lapply(a,parse_dates)
  counts <- vapply(date_opts,function(x) sum(!is.na(x)&x>=as.Date('1990-01-01')&x<=Sys.Date()+30),integer(1))
  date_j <- which.max(counts)
  if(counts[date_j]<10) return(NULL)
  dt <- date_opts[[date_j]]
  first_data <- which(!is.na(dt)&dt>=as.Date('1990-01-01')&dt<=Sys.Date()+30)[1]
  hrows <- seq_len(min(first_data-1,15L))
  if(!length(hrows)) return(NULL)
  result <- list()
  for(j in setdiff(seq_len(ncol(a)),date_j)) {
    num <- suppressWarnings(as.numeric(a[[j]]))
    valid <- !is.na(dt)&!is.na(num)&num>0&num<15
    if(sum(valid)<10) next
    head <- paste(na.omit(as.character(a[[j]][hrows])),collapse=' | ')
    reg <- identify_region(head)
    # EIA's gasoline-only workbook sometimes uses abbreviated column headers.
    if(is.na(reg)) next
    # Skip explicit midgrade, premium, diesel, and all-grades series.
    if(grepl('midgrade|premium|diesel|all grades',head,ignore.case=TRUE)) next
    result[[length(result)+1L]] <- data.frame(date=dt[valid],region_name=reg,
                                               gasoline_regular=num[valid])
  }
  if(!length(result)) return(NULL)
  dplyr::bind_rows(result)
}

load_prices <- function() {
  if(!file.exists(local_file)) {
    tryCatch(download.file(source_url,local_file,mode='wb',quiet=TRUE),
      error=function(e) stop('Could not download the EIA regional workbook. Open ',source_url,
        ' in your browser and save it as ',local_file,' beside app.R. ',conditionMessage(e)))
  }
  sheets <- readxl::excel_sheets(local_file)
  x <- dplyr::bind_rows(lapply(sheets,function(s) read_sheet(local_file,s)))
  if(!nrow(x)) stop('No regional regular-gasoline series could be identified in ',local_file,
    '. Please send the output of readxl::excel_sheets("',local_file,'") and a screenshot of its first sheet.')
  x <- x %>% filter(!is.na(date),!is.na(gasoline_regular)) %>%
    group_by(date,region_name) %>% summarise(gasoline_regular=mean(gasoline_regular),.groups='drop') %>%
    arrange(date,region_name)
  x
}
prices <- load_prices()
regions <- sort(unique(prices$region_name))
selected <- if('United States' %in% regions) 'United States' else regions[1]
year_min <- as.integer(format(min(prices$date),'%Y'))
year_max <- as.integer(format(max(prices$date),'%Y'))

ui <- fluidPage(
  tags$head(tags$style(HTML('
    body{background:#f5f7fb;color:#243246;font-family:Arial,sans-serif}
    .well,.metric,.insight{background:#fff;border:1px solid #e1e6ee;border-radius:12px}
    .well{padding:22px}.metric{padding:18px;margin-bottom:14px;min-height:112px}
    .metric .value{font-size:25px;font-weight:700;color:#15517d}
    .insight{padding:14px 18px;margin:12px 0}
    h2,h3{color:#15517d}.subtitle{color:#536477;margin-bottom:22px}
    .tab-content{background:#fff;padding:12px;border-radius:0 0 10px 10px}
    .nav-tabs{margin-bottom:0}
  '))),
  h2('U.S. Gas Prices Explorer'),
  p(class='subtitle','Explore how gasoline prices change over time, differ across regions, and affect everyday driving costs.'),
  sidebarLayout(
    sidebarPanel(
      selectInput('region','Choose an area',choices=regions,selected=selected),
      sliderInput('years','Year range',min=year_min,max=year_max,
                  value=c(max(year_min,2015L),year_max),sep='',step=1),
      numericInput('tank','Vehicle tank size (gallons)',value=12,min=1,max=60,step=1),
      checkboxInput('national','Show U.S. average on the trend chart',value=TRUE),
      tags$hr(),p('All prices are weekly averages in U.S. dollars per gallon, including taxes.'),
      p('Historical prices are not adjusted for inflation.'),
      tags$a('Official EIA gasoline price data',href=source_url,target='_blank'),
      tags$br(),tags$a('EIA gasoline and diesel overview',href='https://www.eia.gov/petroleum/gasdiesel/',target='_blank')
    ),
    mainPanel(
      fluidRow(
        column(4,div(class='metric',p('Most recent price in selected range'),div(class='value',textOutput('latest')))),
        column(4,div(class='metric',p('Estimated cost to fill tank'),div(class='value',textOutput('fill')))),
        column(4,div(class='metric',p('Weekly observations'),div(class='value',textOutput('weeks'))))
      ),
      tabsetPanel(
        tabPanel('Price trends',br(),plotlyOutput('trend',height='410px'),
                 div(class='insight',textOutput('trend_insight'))),
        tabPanel('Compare regions',br(),plotlyOutput('compare',height='430px'),
                 div(class='insight',textOutput('region_insight')),
                 p('Bars compare average prices over the selected years. U.S. and subregion averages overlap geographically; they are not independent groups.')),
        tabPanel('Seasonal patterns',br(),radioButtons('season_mode','View monthly averages',
                    choices=c('Across selected years'='all','Compare individual years'='byyear'),
                    selected='all',inline=TRUE),
                 plotlyOutput('season',height='420px'),
                 div(class='insight',textOutput('season_insight'))),
        tabPanel('About',br(),h3('About this dashboard'),
                 p('This interactive application uses weekly regular gasoline retail prices published by the U.S. Energy Information Administration (EIA).'),
                 p('Select a region and time range to see historical price changes. Compare regions to understand geographic differences, or examine seasonal patterns by month and year.'),
                 p('The tank calculator multiplies the most recent observed average price within your selected period by the number of gallons entered. It is an illustration, not a station quote or forecast.'),
                 p('Regional price series begin in different years. Comparisons use the observations available for each region in the chosen period. Seasonal patterns are descriptive, not proof that the month causes price changes.'),
                 p('The data are nominal (not inflation adjusted). The selected regions are EIA reporting areas, not every state.'),
                 tags$a('EIA source and definitions',href='https://www.eia.gov/petroleum/gasdiesel/learnmore.php',target='_blank'))
      )
    )
  )
)
server <- function(input,output,session) {
  within_years <- reactive({prices %>% filter(as.integer(format(date,'%Y'))>=input$years[1],
                                            as.integer(format(date,'%Y'))<=input$years[2])})
  chosen <- reactive({within_years() %>% filter(region_name==input$region) %>% arrange(date)})
  last <- reactive({x<-chosen();if(nrow(x)) x[nrow(x),] else NULL})
  output$latest <- renderText({x<-last();if(is.null(x)) 'No data' else sprintf('$%.2f / gal',x$gasoline_regular)})
  output$fill <- renderText({x<-last();if(is.null(x)) 'No data' else sprintf('$%.2f',x$gasoline_regular*input$tank)})
  output$weeks <- renderText({format(nrow(chosen()),big.mark=',')})
  output$trend <- renderPlotly({
    x<-chosen();validate(need(nrow(x)>0,'No data in this time range.'))
    p<-ggplot(x,aes(date,gasoline_regular,color=region_name))+
      geom_line(linewidth=.85)+scale_color_manual(values=setNames('#156b9d',input$region))+
      labs(title='Gas prices over time',x='Year',y='Dollars per gallon',color='Area')+
      scale_y_continuous(labels=dollar_format())+theme_minimal(base_size=13)
    if(isTRUE(input$national)&&input$region!='United States'&&'United States'%in%regions){
      us<-within_years()%>%filter(region_name=='United States')
      if(nrow(us)) p<-p+geom_line(data=us,aes(date,gasoline_regular,color=region_name),linewidth=.75)+
        scale_color_manual(values=c(setNames('#156b9d',input$region),'United States'='#d28a3c'))
    }
    ggplotly(p,tooltip=c('x','y','colour'))
  })
  output$trend_insight <- renderText({
    x<-chosen();if(nrow(x)<2)return('Choose a period with at least two weeks of data.')
    sprintf('In %s, the price changed from $%.2f to $%.2f per gallon between %s and %s.',
      input$region,x$gasoline_regular[1],tail(x$gasoline_regular,1),format(x$date[1],'%b %Y'),format(tail(x$date,1),'%b %Y'))
  })
  regional <- reactive({within_years()%>%group_by(region_name)%>%
      summarise(avg=mean(gasoline_regular),weeks=n(),.groups='drop')%>%arrange(avg)})
  output$compare <- renderPlotly({
    x<-regional();validate(need(nrow(x)>1,'Only one region is available. Download the multi-area EIA workbook.'))
    p<-ggplot(x,aes(reorder(region_name,avg),avg,text=paste('Weeks:',weeks)))+
      geom_col(fill='#237eac',width=.75)+coord_flip()+
      labs(title='Average gasoline price by area',x=NULL,y='Average dollars per gallon')+
      scale_y_continuous(labels=dollar_format())+theme_minimal(base_size=12)
    ggplotly(p,tooltip=c('x','y','text'))
  })
  output$region_insight <- renderText({
    x<-regional();if(nrow(x)<2)return('Regional comparisons require at least two EIA series.')
    sprintf('Across the selected years, %s had the highest average ($%.2f/gal), while %s had the lowest ($%.2f/gal).',
      tail(x$region_name,1),tail(x$avg,1),x$region_name[1],x$avg[1])
  })
  monthly <- reactive({chosen()%>%mutate(month_num=as.integer(format(date,'%m')),
                     month=factor(month.abb[month_num],levels=month.abb),year=format(date,'%Y'))})
  output$season <- renderPlotly({
    x<-monthly();validate(need(nrow(x)>0,'No data for selected period.'))
    if(input$season_mode=='byyear'){
      y<-x%>%group_by(year,month)%>%summarise(avg=mean(gasoline_regular),.groups='drop')
      p<-ggplot(y,aes(month,avg,group=year,color=year))+geom_line(linewidth=.8)+geom_point(size=1.4)+
        labs(title='Monthly prices by year',x='Month',y='Average dollars per gallon',color='Year')
    }else{
      y<-x%>%group_by(month)%>%summarise(avg=mean(gasoline_regular),.groups='drop')
      p<-ggplot(y,aes(month,avg))+geom_col(fill='#df8d39',width=.72)+
        labs(title='Typical monthly prices in selected years',x='Month',y='Average dollars per gallon')
    }
    ggplotly(p+scale_y_continuous(labels=dollar_format())+theme_minimal(base_size=12))
  })
  output$season_insight <- renderText({
    x<-monthly()%>%group_by(month)%>%summarise(avg=mean(gasoline_regular),.groups='drop')
    if(nrow(x)<2)return('Select a longer date range to compare months.')
    sprintf('Across the selected years, %s had the highest average ($%.2f/gal) and %s the lowest ($%.2f/gal).',
      as.character(x$month[which.max(x$avg)]),max(x$avg),as.character(x$month[which.min(x$avg)]),min(x$avg))
  })
}
shinyApp(ui,server)
