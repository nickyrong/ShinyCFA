# ShinyCFA
Personal pet project!! of Nick Rong (nickyrong) and Nate Smith (WraySmith)

Recreating the Consolidated Frequency Analysis using Shiny R. Absolutely no warranty!!

Currently the app allows the user to:
- Load the Hydat database  
- View the locations of Water Survey of Canada (WSC) hydrometric stations (non-searchable map)  
- Select a hydrometric station and summarize streamflow data in a table format (daily, monthly, yearly)  
- View a hydrometric station historical daily hydrograph in an interactive graph  
- Download the streamflow and water level data available for a selected station from the Hydat database  

The app uses the tidyhydat package (https://github.com/ropensci/tidyhydat) to pull data from the Hydat database.

Currently the app needs to be downloaded run on local computer using Shiny through RStudio with the Hydat database downloaded using tidyhydat{}. However, it will shortly be hosted on a server allowing the app to be run directly without the need to download the code or database (unless desired).  

Future plans for the app include: 
- Allowing the user to download selected summary tables  
- Additional data summaries and graphs (likely added through using the fasstr package https://github.com/bcgov/fasstr)  
- Add statistical tests to assess best fit extreme frequency distributions  
- Frequency analysis using the CFA methodology
