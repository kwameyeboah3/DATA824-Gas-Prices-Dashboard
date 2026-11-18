# DATA 824 — U.S. Gas Prices Explorer

An interactive R Shiny dashboard based on the **official U.S. Energy Information Administration (EIA)** weekly regular gasoline price data.

## Start
1. In RStudio, open `app.R`.
2. Install packages once:
   `install.packages(c("shiny","ggplot2","dplyr","plotly","scales","readxl"))`
3. Click **Run App**.
4. The app downloads `pswrgvwall.xls` from EIA on its first run. Keep the downloaded workbook next to `app.R`.

If the automatic download fails, manually download the **historical motor gasoline prices spreadsheet** at:
https://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls
Save it as `pswrgvwall.xls` in the project folder.

**Important:** The previous `eia_weekly_regular_gas.xls` is a *single U.S. series* and cannot support regional comparisons. This upgraded app deliberately uses a different workbook.

## Features
- National and regional price trends; optional U.S. comparison line
- Regional bar chart and plain-language takeaway
- Monthly seasonal averages or individual-year patterns
- Tank-filling calculator and explanatory About tab

## Methodology and caveats
Prices are weekly observations in nominal U.S. dollars per gallon, including taxes. Regional series start at different dates, so averages for very long ranges can cover unequal numbers of weeks. The seasonal analysis is descriptive, not causal. U.S. and subregion averages overlap geographically. Tank costs are estimates based on a regional weekly average, not station quotes.

## Data citation
U.S. Energy Information Administration. *Gasoline and Diesel Fuel Update: Historical motor gasoline prices*. https://www.eia.gov/petroleum/gasdiesel/

## Suggested project files for GitHub
Upload `app.R`, `README.md`, and optionally the public-domain EIA workbook after verifying its license/size. Do not upload private files. The application requires internet only if the workbook is not already present.
