rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)
library(plotly)

# Lists
Dist_Options <- c("Exponential", "Gamma", "GEV", "Gen. Logistic", "Gen. Normal", "Gen. Pareto",
                  "Gumbel", "Kappa", "Normal", "Pearson III", "Wakeby", "Weibull", "Log-Normal", "LP3")
Tr_Options <- c(1.01, 2, 5, 10, 20, 25, 30, 40, 50, 75, 100, 200, 250, 300,
                400, 500, 1000, 2500, 10000)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    pageWithSidebar(

        headerPanel('Water Survey of Canada Hydrometric Stations'),

        sidebarPanel(
            width = 3,
            "Enter a valid WSC Station ID",
            br(), br(),
            textInput("stn.id", label = h3("WSC Station ID"), value = "08GA010"),
            downloadButton("downloadData", "Download Data"),
            br(),
            h3(textOutput("status"))
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Read Me", htmlOutput("ReadMe_HTML")
                ),

                tabPanel("Stations Map",
                         br(),
                         h3("Database is still loading if map is not visible"),
                         br(),
                         leafletOutput("MapPlot"),
                         "Zoom into map to see station locations",
                         br(),
                         "Click on a location to see Station ID, Station Name, and Status: active/discontinued"
                ),


                tabPanel("Data Table",
                         br(),
                         textOutput("name"),
                         br(),
                         "Select Daily, Monthly, or Yearly Resolution",
                         selectInput("Reso", "", c("Daily" = "Daily", "Monthly" = "Monthly", "Yearly" = "Yearly"),
                                     selected = "Daily"), p(""),
                         downloadButton("downloadSummary", "Download Summary Data"),
                         br(), br(),
                         DT::dataTableOutput("table")
                ),


                tabPanel("Time Series",
                         br(),
                         dygraphOutput(outputId = "graph", height = "600px"),
                         br(),
                         "This is an interactive graph: drag to zoom in and double click to zoom out"
                ),

                tabPanel("Frequency Analysis",
                    br(),
                    selectizeInput('selector_dist', 'Select Distributions',
                                   choices = Dist_Options, multiple = TRUE),
                    selectizeInput('selector_Tr', 'Select Return Periods (Years)',
                                   choices = Tr_Options, multiple = TRUE,
                                   options = list(maxItems = 10), selected = 200),
                    DT::dataTableOutput("ffa.table"),
                    br(), br(),
                    plotlyOutput("ffa.figure"),
                    br(), br(),
                    plotlyOutput("max.figure")

                )

            ) # End of tabsetPanel

        ) # End of main panel

    ) # End of pageWithSidebar

)) # End of shinyUI & fluidPage
