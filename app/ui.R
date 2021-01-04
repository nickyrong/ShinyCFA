rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)
library(plotly)
library(renv)
library(shinythemes) 
library(waiter) #loading screen/spinner


options(shiny.sanitize.errors = TRUE)
linebreaks <- function(n){HTML(strrep(br(), n))}


  
# Define UI
shinyUI(fluidPage(
  
  use_waiter(),
  waiter_show_on_load(html = spin_fading_circles(), color = "black"), # place at the top before content
  
  
  theme = shinytheme("yeti"),
  tags$head(HTML("<title>Hydrometric Data Tool for WSC HYDAT</title>")),

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
    
    headerPanel(title=div("Hydrometric Data Tool for WSC HYDAT", 
                          img(src='', 
                              style = "float:right;"
                          )
    )
  ),

    
    sidebarPanel(
      width = 3,
      h6(htmlOutput('HYDAT_version')),

      # https://shiny.rstudio.com/articles/selectize.html
      selectizeInput("stn_id_input", label = h3("WSC Station ID"),
                     choices = NULL,
                     selected = "Loading...",
                     multiple= FALSE,
                     options = list(maxOptions = 5)),
      
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
        tabPanel("Read Me",
                 htmlOutput("README") #it is technically a markdown render but HTML works
        ),
        
        tabPanel("Stations Map",
                 br(),
                 leafletOutput("MapPlot", height = 800),
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
                                                 h4("Select Daily, Monthly, or Yearly Summary"),
                                                 c("Daily" = "Daily", 
                                                   "Monthly" = "Monthly", 
                                                   "Yearly" = "Yearly"),
                                                 selected = "Daily"
                                     )
                              ),
                              
                              column(3, 
                                     downloadButton("downloadSummary", "Download Summary")
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
                            DT::dataTableOutput("table")
                            
                   ), # End of Data Summary Table sub-tab
                   
                   #---- Sub tab: Time Series
                   tabPanel("Time Series",
                            br(),
                            "This is an interactive graph: drag to zoom in and double click to zoom out",
                            br(),
                            h3("Daily Average Discharge Data"),
                            dygraphOutput(outputId = "tsgraph", height = "600px"),
                            br()
                            
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
                   ) # End of Hydrograph sub-tab
                   
                   #---- Sub tab: Trend Test
                   # tabPanel("Trends",
                   #          "Trend analysis provided by the [FlowScreen Package](https://cran.r-project.org/web/packages/FlowScreen/index.html)",
                   #          br(), 
                   #          shinycssloaders::withSpinner( 
                   #            plotOutput("trends")
                   #          ),#End of busy indicator
                   #          br()
                   # ) # End of Trend sub-tab
                   
                 ) # End of all sub-tabs
        ), # End of Explore Data tab
        
        
        
        tabPanel("Flood Frequency Analysis",
                 
                 #Sub-tabs
                 tabsetPanel(
                   
                   #---- Sub tab: Qdaily
                   tabPanel("Daily Q",
                            br(),
                            h4('Frequency analysis is currently only performed using calendar year'),
                            h2('1. Settings'),
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
                                     selectizeInput("months_Qdaily", "Select Months to Include", 
                                                    selected = base::month.abb,
                                                    choices = base::month.abb,
                                                    multiple= TRUE
                                     )
                              )
                            ), # End of FluidRow
                            
                            htmlOutput("complete_years_Qdaily"),
          
                            br(),
                            
                            h2('2. Empirical Frequency'),
                            h5("Descriptive Statistics (pastecs::stat.desc):"),
                            verbatimTextOutput("daily_simplestats", placeholder = FALSE),
                            br(),
                            DT::dataTableOutput("daily_ams_table"),
                            br(),
                            plotlyOutput("daily_ams_fig"),
                            br(),
                            
                            h2('3. Analytical Frequency Distribution'),
                            fluidRow(
                              column(6,
                                     selectizeInput('daily_selector_dist', 'Select Distributions',
                                                    choices = NULL, multiple = TRUE)),
                              column(6,
                                     selectizeInput('daily_selector_Tr', 'Select Return Periods (Years)',
                                                    choices = c(2, 5, 10, 20, 25, 30, 40, 50, 
                                                                75, 100, 200, 250, 300,
                                                                400, 500, 1000, 2500, 10000), 
                                                    multiple = TRUE,
                                                    options = list(maxItems = 10), selected = 2))
                              ),
                            h5("Sampled Linear Moments & Ratios (lmom::samlmu):"),
                            verbatimTextOutput("daily_lmoments", placeholder = FALSE),
                            DT::dataTableOutput("daily_dist_table"),
                            br(),
                            plotlyOutput("daily_dist_fig"),
                            
                            linebreaks(30)

                   ), # End of Q daily Frequency sub-tab
                   
                   #---- Sub tab: Frequency Distribution
                   tabPanel("Instantaneous Q",
                            br(),
                            h4('Frequency analysis is currently only performed using calendar year'),
                            h2('1. Settings'),
                            fluidRow(
                              column(4,
                                     selectizeInput("remove_year_Qinst", "Select Years to Remove", 
                                                    selected = NULL,
                                                    choices = NULL,
                                                    multiple= TRUE
                                     )
                              ),
                              column(4 #placeholder
                              ),
                              column(4 #placeholder
                              )
                            ), # End of FluidRow
                            htmlOutput("selected_years_Qinst"),
                            br(),
                            
                            h2('2. Empirical Frequency'),
                            h5("Descriptive Statistics (pastecs::stat.desc):"),
                            verbatimTextOutput("inst_simplestats", placeholder = FALSE),
                            br(),
                            DT::dataTableOutput("inst_ams_table"),
                            br(),
                            plotlyOutput("inst_ams_fig"),
                            br(),
                            
                            h2('3. Analytical Frequency Distribution'),
                            fluidRow(
                              column(6,
                                     selectizeInput('inst_selector_dist', 'Select Distributions',
                                                    choices = NULL, multiple = TRUE)),
                              column(6,
                                     selectizeInput('inst_selector_Tr', 'Select Return Periods (Years)',
                                                    choices = c(2, 5, 10, 20, 25, 30, 40, 50, 
                                                                75, 100, 200, 250, 300,
                                                                400, 500, 1000, 2500, 10000), 
                                                    multiple = TRUE,
                                                    options = list(maxItems = 10), selected = 2))
                            ),
                            h5("Sampled Linear Moments & Ratios (lmom::samlmu):"),
                            verbatimTextOutput("inst_lmoments", placeholder = FALSE),
                            DT::dataTableOutput("inst_dist_table"),
                            br(),
                            plotlyOutput("inst_dist_fig"),
                            
                            linebreaks(30)

                   ) # End of Q Inst sub-tab
                   

                 ) # End of all sub-tabs
        )
        
      ) # End of tabsetPanel
      
    ) # End of main panel
    
  ) # End of pageWithSidebar
  
)) # End of shinyUI & fluidPage
