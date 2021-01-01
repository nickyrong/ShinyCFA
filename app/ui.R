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
      h5(htmlOutput("stn_input_name"))
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
                   tabPanel("Summary Table",
                            
                            br(),
                            "Select Daily, Monthly, or Yearly Resolution",
                            selectInput("sum_period", "", c("Daily" = "Daily", "Monthly" = "Monthly", "Yearly" = "Yearly"),
                                        selected = "Daily"), p(""),
                            downloadButton("downloadSummary", "Download Summary Data"),
                            br(), br(),
                            DT::dataTableOutput("table")
                   ), # End of Data Summary Table sub-tab
                   
                   #---- Sub tab: Data Summary Table
                   tabPanel("Hydrograph",
                            br(),
                            fluidRow(
                              column(6,
                                     "Select Start of Water Year",
                                     selectInput("wy_start_hydrograph", "", 
                                                  c("January" = 1, "February" = 2, "March" = 3,
                                                    "April" = 4, "May" = 5, "June" = 6,
                                                    "July" = 7, "August" = 8, "September" = 9,
                                                    "October" = 10, "November" = 11, "December" = 12),
                                                 selected = "January"), p(""),
                              ),
                              
                              column(6,
                                     "Y-axis Limit",
                                     sliderInput('hydrograph_ylim', '% of Max Value',
                                                 min = 1, max = 100, value = 100)
                                     )
                              ),# End of fluidRow


                            br(), 
                            plotOutput("hydrograph"),
                            br()
                   ), # End of Hydrograph sub-tab
                   
                   #---- Sub tab: Trend Test
                   tabPanel("Trends",
                            br(),
                            fluidRow(
                              column(6,
                                     "Select Start of Water Year",
                                     selectInput("wy_start_trend", "", 
                                                 c("January" = 1, "February" = 2, "March" = 3,
                                                   "April" = 4, "May" = 5, "June" = 6,
                                                   "July" = 7, "August" = 8, "September" = 9,
                                                   "October" = 10, "November" = 11, "December" = 12),
                                                 selected = "January"), p(""),
                              ),
                              
                              column(6 # empty place holder
                              )
                            ),# End of fluidRow
                            
                            
                            br(), 
                            shinycssloaders::withSpinner( 
                              plotOutput("trend")
                            ),#End of busy indicator
                            br()
                   ) # End of Trend sub-tab
                   
                 ) # End of all sub-tabs
        ), # End of Explore Data tab
        
        
        
        tabPanel("Frequency Analysis",
                 
                 #Sub-tabs
                 tabsetPanel(
                   
                   #---- Sub tab: Qdaily
                   tabPanel("Daily Q"
                   ), # End of Q daily Frequency sub-tab
                   
                   #---- Sub tab: Frequency Distribution
                   tabPanel("Instantaneous Q"
                   ) # End of Q Inst sub-tab
                   

                 ) # End of all sub-tabs
        )
        
      ) # End of tabsetPanel
      
    ) # End of main panel
    
  ) # End of pageWithSidebar
  
)) # End of shinyUI & fluidPage
