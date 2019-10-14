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
      verbatimTextOutput("stn.name"),
      plotOutput(outputId = "ts", height = "300px")
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
  
  output$ts <- renderPlot({
    
    TS = FlowScreen::create.ts(read.wsc.flows(input$stn.id))

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
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)