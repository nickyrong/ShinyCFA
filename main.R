library(shiny)

# Define UI ----
ui <- fluidPage(
  
  pageWithSidebar(
    
    headerPanel('HYDAT Data Access Portal'),
    
    sidebarPanel(
      textInput("stn.id", label = h3("WSC Station ID"), value = "08GA010"),
    ),
    
    mainPanel(
      verbatimTextOutput("stn.id"),
      verbatimTextOutput("stn.name")
    )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  library(tidyhydat)
  library(FlowScreen)
  
  source("~/Documents/GitHub/ShinyCFA/read.wsc.flows.R")
  
  # Return Station ID
  output$stn.id <- renderPrint({ input$stn.id })
  
  # Return Station Name
  output$stn.name <- renderPrint({ 
    tidyhydat::hy_stations(input$stn.id)$STATION_NAME 
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)