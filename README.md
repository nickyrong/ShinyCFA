**Authors:** <br/>
Nick Rong ([nickyrong](https://github.com/nickyrong)) <br/>
Nate Smith ([WraySmith](https://github.com/WraySmith)) <br/><br/>
Last Update 12/19/2019 <br/>
Last Change: Added Frequency Analysis Tab<br/>

### Overview
This app allows the user to enter a Water Survey of Canada (WSC) station ID to access the Hydat database:
[Hydat Database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)
<br/><br/>
The data can be summarized in tabular or graphical formats and flood frequency analysis (FFA) can be performed using various distributions.

Functionality in this ShinyApp is provided by [tidyhydat package](https://github.com/ropensci/tidyhydat). 
<br/>
<img src="https://github.com/ropensci/tidyhydat/raw/master/man/figures/tidyhydat.png" width="10%" />
<br/>



### Usage:
Currently the app allows the user to:

- Load the Hydat database and download available streamflow and water level data
- View the locations of Water Survey of Canada (WSC) hydrometric stations (non-searchable map)  
- Select a hydrometric station and summarize streamflow data in a table format (daily, monthly, yearly) and download the selected format  
- View a hydrometric station historical daily hydrograph in an interactive graph  
- Run a Flood Frequency Analysis on the annual daily or instantaneous maximum streamflow series using various distributions  

Currently the app needs to be downloaded and run on local computer using Shiny through RStudio with the Hydat database downloaded through tidyhydat.
<br/>

To run the app you need to do the following:

1) Install [R](https://cran.r-project.org/)
2) Install [RStudio](https://rstudio.com/products/rstudio/download/) (just the free version)
3) Install the required R Packages (see dependencies.R file for the list)
4) Install the HYDAT Database using the tidyhydat function download_hydat()
5) Open the server.R and ui.R scripts in RStudio and click the Run App button

Steps 3 and 4 can alternatively be completed by running the dependencies.R script
<br/>

### Future updates

- Additional data summaries and graphs (likely added through using the [fasstr package](https://github.com/bcgov/fasstr) 
- Add statistical tests to assess best fit extreme frequency distributions  

### Feedback & Suggestions

Please contact [Nick](https://github.com/nickyrong) or [Nate](https://github.com/WraySmith).
<br/>
