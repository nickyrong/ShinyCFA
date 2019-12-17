rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    pageWithSidebar(
        
        headerPanel('Water Survey of Canada Hydrometric Stations'),
        
        sidebarPanel(
            width = 3,
            "Enter a valid WSC Station ID",
            br(), br(),
            textInput("stn.id", label = h3("WSC Station ID"), value = "08GA010"),
            downloadButton("downloadData", "Download Station Data"),
            br(),
            h3(textOutput("status"))
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Read Me",
                         br(),
                         "This app allows the user to enter a Water Survey of Canada Hydrometric Station ID to view streamflow data.",
                         br(), br(),
                         "Still in development phase and !!!NOT SUITABLE FOR ASSIGNMENTS!!!",
                         br(), br(),
                         "Next phase is to include frequency analysis functionality. Contact Nate/Nick if you have any suggestions or bugs to report",
                         br(), br(),
                         "The Map tab plots the locations of the hydrometric stations",
                         br(), br(),
                         "The Data Table tab shows tabular data for the selected station in either daily, monthly, or yearly formats",
                         br(), br(),
                         "The Time Series tab plots a graph of the data and colour codes values based on data flags"
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
                         DT::dataTableOutput("table")
                ),

                
                tabPanel("Time Series",
                         br(),
                         dygraphOutput(outputId = "graph", height = "600px"),
                         br(),
                         "This is an interactive graph: drag to zoom in and double click to zoom out"
                ),
                
                tabPanel("Frequency Analysis",
                    h3("This page is under construction")
                )
                
            ) # End of tabsetPanel
            
        ) # End of main panel
        
    ) # End of pageWithSidebar
    
)) # End of shinyUI & fluidPage
