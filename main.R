rm(list=ls())
library(shiny)

# Define UI ----
ui <- fluidPage(
  
  pageWithSidebar(
    
    headerPanel('HYDAT Data Access Portal'),
    
    sidebarPanel(
      
      textInput("stn.id", label = h3("WSC Station ID"), value = "08GA010"),
      # Select date range to be plotted

      sliderInput("date", label = h3("Subset Range"),
                  min = 1900, max = as.numeric(format(Sys.Date(), "%Y")), 
                  value = c(1900, as.numeric(format(Sys.Date(), "%Y"))),
                  sep = "")
    ),
    
    mainPanel(

      tabsetPanel(
                  
                  tabPanel("Data Table", DT::dataTableOutput("table")),
        
                  tabPanel("Screening Summary", 
                           print("This function might take quite some time (30s) to plot"),
                           br(),
                           plotOutput(outputId = "screening", height = "600px")),
        
                  tabPanel("Time Series", 
                           plotOutput(outputId = "ts", height = "600px")),
                  
                  tabPanel("Annual Hyrograph", 
                           plotOutput(outputId = "regime", height = "600px")),
                  
                  tabPanel("Flow Duration", 
                           plotOutput(outputId = "fdc", height = "600px")),
                  
                  tabPanel("Frequency Analysis", "This panel is intentionally left blank")
      )
    )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  library(tidyhydat)
  library(FlowScreen)
  
  source("~/Documents/GitHub/ShinyCFA/read.wsc.flows.R")
  
  
  # Date Range Warning
  selected_window <- reactive({
    req(input$date)
    req(input$stn.id)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    validate(need(!is.na(input$stn.id), "Error: Start date should be earlier than end date."))
  })
  
  # Return Station ID
  output$stn.id <- renderPrint({ input$stn.id })
  
  # Return Station Name
  output$stn.name <- renderPrint({ 
    tidyhydat::hy_stations(input$stn.id)$STATION_NAME 
    })
  
  # Time Series
  output$ts <- renderPlot({
    
    TS <- FlowScreen::create.ts(read.wsc.flows(input$stn.id)) %>%
            subset(
              Date > base::as.Date(paste0(input$date[1], "-01-01")) &
                Date < base::as.Date(paste0(input$date[2], "-12-31"))
              )

    SYMs <- c("", "E", "A", "B", "D", "R")
    SYMnames <- c("No Code", "Estimate", "Partial Day", "Ice Conditions", "Dry", "Revised")
    SYMcol <- c("grey", "#E41A1C", "#4DAF4A", "#377EB8", "#FF7F00", "#984EA3")
    codes <- as.factor(TS$Code)
    codes <- match(codes, SYMs)
    graphics::par(mar=c(4,4,0,0.5))
    mYlims <- c(0, 1.2*max(TS$Flow))
    graphics::plot(TS$Date, TS$Flow, typ = "l", col = "grey",
                   xlab="Date", ylab="Flow (m^3/s)", ylim=mYlims)
    graphics::points(TS$Date, TS$Flow,
               pch=19, col=SYMcol[codes], cex=0.5)
    graphics::legend(TS$Date[1], 1.15*max(TS$Flow), SYMnames, pch=19, pt.cex=0.9, cex=0.9, col=SYMcol,
                     bty="n", xjust=0, x.intersp=0.5, yjust=0.5, ncol=3)
  })
  
  # Screening summary (don't put this as home page, take forever to load)
  output$screening <- renderPlot({
    
    read.wsc.flows(toupper(input$stn.id)) %>%
      subset(
        Date > base::as.Date(paste0(input$date[1], "-01-01")) &
          Date < base::as.Date(paste0(input$date[2], "-12-31"))
      ) %>% create.ts() %>% metrics.all() %>% screen.summary()

    
  })
  
  # Hydrograph
  output$regime <- renderPlot({
    
    FlowScreen::create.ts(read.wsc.flows(toupper(input$stn.id))) %>%
      subset(
        Date > base::as.Date(paste0(input$date[1], "-01-01")) &
          Date < base::as.Date(paste0(input$date[2], "-12-31"))
      ) %>%
      FlowScreen::regime(text = NULL)
    

  })
  
  # FDC
  output$fdc <- renderPlot({
    
    flows.subset <- read.wsc.flows(toupper(input$stn.id)) %>%
      subset(
        Date > base::as.Date(paste0(input$date[1], "-01-01")) &
          Date < base::as.Date(paste0(input$date[2], "-12-31"))
      )
    
    subset(flows.subset, !is.na(flows.subset$Flow))$Flow %>% FDC()
    
  })
  
  # Data table
  output$table <- DT::renderDataTable({
    DT::datatable({
      read.wsc.flows(toupper(input$stn.id)) %>%
        subset(
          Date > base::as.Date(paste0(input$date[1], "-01-01")) &
            Date < base::as.Date(paste0(input$date[2], "-12-31"))
        ) %>% FlowScreen::create.ts()
    })

  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)