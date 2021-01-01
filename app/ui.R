rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)
library(plotly)
library(renv)
library(shinycssloaders) # withSpinner() indicate calculation/rendering in progress
library(shinythemes)
library(shinyalert) # for pop-up message in the data removal tab

options(shiny.sanitize.errors = TRUE)

# Define UI
shinyUI(fluidPage(
  
  theme = shinytheme("yeti"),
  
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
    
    headerPanel('Hydrometric Data Tool for WSC HYDAT'),
    
    sidebarPanel(
      width = 3,
      h5("Placeholder for HYDAT Version"),
      br(), 
      # https://shiny.rstudio.com/articles/selectize.html
      selectizeInput("stn_id_input", label = h3("WSC Station ID"), selected = "08GA010",
                     choices = NULL,
                     multiple= FALSE,
                     options = list(maxOptions = 5)),
      br(),
      selectInput("wy_start", h3("Start of Water Year"), 
                  c("January" = 1, "February" = 2, "March" = 3,
                    "April" = 4, "May" = 5, "June" = 6,
                    "July" = 7, "August" = 8, "September" = 9,
                    "October" = 10, "November" = 11, "December" = 12),
                  selected = "January"),
      br(),
      h3("Station Info"),
      h6(htmlOutput("stn_input_info"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Read Me"
        ),
        
        tabPanel("Stations Map",
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
                  
                   #---- Sub tab: Data Summary Table
                   tabPanel("Summary Table",
                            br(),
                            fluidRow(
                              column(3,
                                     selectInput("sum_period", 
                                                 h4("Select Daily, Monthly, or Yearly Resolution"),
                                                 c("Daily" = "Daily", 
                                                   "Monthly" = "Monthly", 
                                                   "Yearly" = "Yearly"),
                                                 selected = "Daily"
                                     )
                              ),
                              
                              column(3, 
                                     downloadButton("downloadSummary", "Download Summary Data")
                              ),
                              column(3 #empty placeholder
                                     
                              ),
                              column(3 #empty placeholder
                                     
                              )
                            ),# End of fluidRow
                            
                            # Vertical alignment for the two
                            tags$style(type='text/css', "#sum_period {margin-top: 25px;}"),
                            tags$style(type='text/css', "#downloadSummary {margin-top: 60px;}"),
                            
                            br(),
                            shinycssloaders::withSpinner(
                              DT::dataTableOutput("table")
                            )
                            
                   ), # End of Data Summary Table sub-tab
                   
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
                   tabPanel("Hydrograph",
                            br(),
                            fluidRow(
                              column(6,
                                     br(),
                                     sliderInput('hydrograph_ylim', h4('Y-axis Limit (% of Max Value)'),
                                                 min = 1, max = 100, value = 100)
                              ),
                              
                              column(6 # empty placeholder
                                     
                              )
                              ),# End of fluidRow


                            br(), 
                            plotOutput("hydrograph"),
                            br()
                   ), # End of Hydrograph sub-tab
                   
                   #---- Sub tab: Trend Test
                   tabPanel("Trends",
                            
                            br(), 
                            shinycssloaders::withSpinner( 
                              plotOutput("trends")
                            ),#End of busy indicator
                            br()
                   ) # End of Trend sub-tab
                   
                 ) # End of all sub-tabs
        ), # End of Explore Data tab
        
        
        
        tabPanel("Flood Frequency Analysis",
                 
                 #Sub-tabs
                 tabsetPanel(
                   
                   #---- Sub tab: Qdaily
                   tabPanel("Daily Q",
                            br(),
                            
                            fluidRow(
                              column(4,
                                     selectizeInput("remove_year_Qdaily", "Select Years to Remove", 
                                                    selected = NULL,
                                                    choices = NULL,
                                                    multiple= TRUE
                                     )
                              ),
                              column(4,
                                     sliderInput('selector_days', "Complete Year Threshold",
                                         min = 0, max = 365, value = 355)
                              ),
                              column(4, 
                                     selectizeInput("months_Qdaily", "Select Month of Year", 
                                                    selected = base::month.abb,
                                                    choices = base::month.abb,
                                                    multiple= TRUE
                                     )
                              )
                            ), # End of FluidRow
                            
                            htmlOutput("complete_years_Qdaily"),
                            
                            br(),
                            plotlyOutput("daily_ams_fig"),
                            
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
