rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)
library(plotly)
library(renv)
library(shinycssloaders) # withSpinner() indicate calculation/rendering in progress
library(shinythemes)
library(shinyalert) # for pop-up message in the data removal tab



# Define UI
shinyUI(fluidPage(
  
  # Change font color of error message to red
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
      }
    "))
  ),
  
  # SideBar UI configuration
  pageWithSidebar(
    
    headerPanel('Water Survey of Canada Hydrometric Data Tool'),
    
    sidebarPanel(
      width = 3,
      "Enter a valid WSC Station ID",
      br(), br(),
      # https://shiny.rstudio.com/articles/selectize.html
      selectizeInput("stn_id_input", label = h3("WSC Station ID"), selected = "08GA010",
                     choices = NULL,
                     multiple= FALSE,
                     options = list(maxOptions = 5)),
      downloadButton("downloadData", "Download Data"),
      br(),
      h5(textOutput("stn_input_name"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Read Me"
        ),
        
        tabPanel("Stations Map",
                 br(),
                 h3("Database is still loading if map is not visible"),
                 br(),
                 shinycssloaders::withSpinner( 
                   leafletOutput("MapPlot", height = 800)
                 ),#End of busy indicator
                 "Zoom into map to see station locations",
                 br(),
                 "Click on a location to see Station ID, Station Name, and Status: active/discontinued"
        ),
        
        
        tabPanel("Explore Data",
                
                 #Sub-tabs
                 tabsetPanel(
                    
                   #---- Sub tab: Time Series
                   tabPanel("Time Series",
                            br(),
                            h3("Daily Average Discharge Data"),
                            shinycssloaders::withSpinner(
                              dygraphOutput(outputId = "tsgraph", height = "600px")
                            ),
                            br(),
                            "This is an interactive graph: drag to zoom in and double click to zoom out"
                   ), # End of Time Series sub-tab
                   
                   #---- Sub tab: Data Summary Table
                   tabPanel("Summary Table"
                   ), # End of Data Summary Table sub-tab
                   
                   #---- Sub tab: Data Summary Table
                   tabPanel("Hydrograph"
                   ) # End of Hydrograph sub-tab
                   
                 ) # End of all sub-tabs
        ), # End of Explore Data tab
        
        
        
        tabPanel("Frequency Analysis"
        )
        
      ) # End of tabsetPanel
      
    ) # End of main panel
    
  ) # End of pageWithSidebar
  
)) # End of shinyUI & fluidPage
