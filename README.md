
<br/> **Authors:** <br/>
Nick Rong (Junior Scientist at *Knight Piesold Consulting*) <br/>
Nate Smith (Senior Engineer at *Knight Piesold Consulting*) <br/><br/>
Last Update 12/19/2019 <br/>
Last Change: Added Frequency Analysis Tab and Functionality <br/>

### Overview
Personal pet project!! of Nick Rong (nickyrong) and Nate Smith (WraySmith) to recreate the Consolidated Frequency Analysis (CFA) using Shiny R. Absolutely no warranty!!

This app allows the user to enter a station ID to access the Hydat database:
[Hydat Database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)
<br/>

Functionality in this ShinyApp is provided by [tidyhydat package](https://github.com/ropensci/tidyhydat).
<br/>


### Usage:
Currently the app allows the user to:

- Load the Hydat database and download the streamflow and water level data available
- View the locations of Water Survey of Canada (WSC) hydrometric stations (non-searchable map)  
- Select a hydrometric station and summarize streamflow data in a table format (daily, monthly, yearly) and download the selected format  
- View a hydrometric station historical daily hydrograph in an interactive graph  
- Run a Flood Frequency Analysis on the annual daily maximum streamflow series using various distributions  

Currently the app needs to be downloaded run on local computer using Shiny through RStudio with the Hydat database downloaded using tidyhydat
<br/>

### Future updates

- Additional data summaries and graphs (likely added through using the [fasstr package](https://github.com/bcgov/fasstr)
- Add statistical tests to assess best fit extreme frequency distributions  

### Feedback & Suggestions

Please contact [Nick](https://github.com/nickyrong) or [Nate](https://github.com/WraySmith).
<br/>
