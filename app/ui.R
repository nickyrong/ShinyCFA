rm(list=ls())
library(shiny)
library(leaflet)
library(dygraphs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    pageWithSidebar(
        headerPanel('Water Survey of Canada Hydrometric Stations in BC'),
        sidebarPanel(
            width = 3,
            print("Enter a valid WSC Station ID"),
            br(), br(),
            textInput("stn.id", label = h3("WSC Station ID"), value = "08GA010"),
            downloadButton("downloadData", "Download Station Data"),
            br(),
            h3(textOutput("status"))
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Instructions",
                         br(),
                         print("This app allows the user to enter a Water Survey of Canada Hydrometric Station ID to view streamflow data."),
                         br(), br(),
                         print("Still in development phase and !!!NOT SUITABLE FOR ASSIGNMENTS!!!"),
                         br(), br(),
                         print("Next phase is to include frequency analysis functionality. Contact Nate/Nick if you have any suggestions or bugs to report"),
                         br(), br(),
                         print("The Map tab plots the locations of the hydrometric stations"),
                         br(), br(),
                         print("The Data Table tab shows tabular data for the selected station in either daily, monthly, or yearly formats"),
                         br(), br(),
                         print("The Time Series tab plots a graph of the data and colour codes values based on data flags")
                         ),

                tabPanel("Stations Map",
                         br(),
                         print(h3("Database is still loading if map is not visible")),
                         br(),
                         leafletOutput("MapPlot"),
                         print("Zoom into map to see station locations"),
                         br(),
                         print("Click on a location to see Station ID, Station Name, and Status: active/discontinued")
                         ),


                tabPanel("Data Table",
                         br(),
                         textOutput("name"),
                         br(),
                         print("Select Daily, Monthly, or Yearly Resolution"),
                         selectInput("Reso", "", c("Daily" = "Daily", "Monthly" = "Monthly", "Yearly" = "Yearly"),
                                     selected = "Daily"), p(""),
                         DT::dataTableOutput("table")
                         ),

#                tabPanel("Time Series",
#                         br(),
#                         plotOutput(outputId = "ts", height = "600px")
#                         ),
                
                tabPanel("Time Series",
                         br(),
                         dygraphOutput(outputId = "graph", height = "600px"),
                         br(),
                         print("This is an interactive graph: drag to zoom in and double click to zoom out")
                )
            )
        )
    )
))
