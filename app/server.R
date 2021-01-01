rm(list = ls())
library(shiny)
library(tidyhydat)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(xts)
library(dygraphs)
library(tidyverse)
library(lmom)
library(plotly)
library(rlang)
library(renv)
library(shinyalert) # for pop-up message in the data removal tab
library(shinybusy) # busy indicator for rendering plots/tables
library(FlowScreen)


source("./functions.R")
source("./FlowScreen_hyear_internal_fixed.R")

# ------------ HYDAT database loading ------------

# Determine Hydat database location
if(file.exists("./database/Hydat.sqlite3")) {
  Hydat_Location <- "./database/Hydat.sqlite3"
} else {
  Hydat_Location <- tidyhydat::hy_default_db()
}

# ------------ Shiny Server In/Output ------------


# Define server logic
shinyServer(function(input, output, session) {
  
  # -1- SideBar UI --------------------------------------------
  
  # Station ID Selection by User
  updateSelectInput(session, 'stn_id_input',
                    choices = tidyhydat::allstations$STATION_NUMBER,
                    selected = "08GA010"
  )
  
  output$stn_input_name <- renderText({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    # Return Station Name & Info
    map_data %>% filter(STATION_NUMBER == input$stn_id_input) %>%
      select(text) %>% as.character()
    
    
  })
  
  # Central data processing: read Q daily
  Qdaily <- reactive({
    
    read_dailyflow(input$stn_id_input)
    
  })
  
  # -2- ReadMe Tab --------------------------------------------
  
  # -3- Map Tab -----------------------------------------------
  output$MapPlot <- renderLeaflet({
    map_data %>%
      
      # Some locations are wrong (impossible values)
      drop_na(LONGITUDE, LATITUDE) %>%
      filter(between(LONGITUDE, -142, -51), # East to west boundary of Canada
             between(LATITUDE, 41, 84) # South to north boundary of Canada
      ) %>%
      
      leaflet() %>%
      addTiles(urlTemplate = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      # default zoom to BC
      setView(lng = -122.7497, lat = 53.9171, zoom = 5) %>%
      
      addMarkers(~LONGITUDE, ~LATITUDE, popup = ~text, clusterOptions = markerClusterOptions())
  })
  

  # -4- Time Series Tab ---------------------------------------

  ### Plot an interactive graph
  output$tsgraph <- renderDygraph({
    
    # Spread the flow data by the flag
    TS <- Qdaily() %>%
      select(Date, Flow, Symbol = SYM)
    codes <- as.factor(TS$Symbol)
    codes <- match(codes, SYMs)
    TS$Symbol <- SYMnames[codes]
    plot_data <- spread(TS, Symbol, Flow)
    number_flags <- length(unique(TS$Symbol))
    flag_names <- colnames(plot_data)[-1]
    flag_names <- replace(flag_names, flag_names == "<NA>", "No Code")
    
    # Convert to an xts format required by the dygraphs package
    xts_Date <- as.POSIXct(plot_data$Date, format = "%Y-%m-%d")
    xts_Format <- as.xts(order.by = xts_Date, x = plot_data[,2])
    
    # Combine flag columns, :(number_flags - 1)
    if(!number_flags == 1) {
      for (i in 1:(number_flags - 1)) {
        index <- 2 + i
        temp <- as.xts(order.by = xts_Date, x = plot_data[,index])
        xts_Format <- cbind(xts_Format, temp[,1])
        rm(temp)
      }
    }
    colnames(xts_Format) <- flag_names
    
    y_axis <- "Discharge (m<sup>3</sup>/s)"
    dy_plots <- dygraph(xts_Format, ylab = y_axis) %>%
      dyLegend(width = 600) %>%
      dyCrosshair(direction = "vertical") %>%
      dyRangeSelector()
    
    # Plot the series if it exists
    #c("No Code", "Estimate", "Partial Day", "Ice Conditions", "Dry", "Revised", "RealTime")
    if(1 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("No Code", drawPoints = TRUE, color = "blue", fillGraph = TRUE)
    }
    
    if(2 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("Estimate", drawPoints = TRUE, color = "#E41A1C", fillGraph = TRUE)
    }
    
    if(3 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("Partial Day", drawPoints = TRUE, color = "#4DAF4A", fillGraph = TRUE)
    }
    
    if(4 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("Ice Conditions", drawPoints = TRUE, color = "#377EB8", fillGraph = TRUE)
    }
    
    if(5 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("Dry", drawPoints = TRUE, color = "#FF7F00")
    }
    
    if(6 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("Revised", drawPoints = TRUE, color = "#984EA3")
    }
    
    if(7 %in% match(flag_names, SYMnames)) {
      dy_plots <- dy_plots %>% dySeries("RealTime", drawPoints = TRUE, color = "#FFA500")
    }
    
    # call to plot
    dy_plots
    
  }) # End of interactive graph
  
  
  

  # -5- Data Summary Tab ---------------------------------------
  
  # the summrized data are needed in both table rendering and download button
  summarized <- reactive({
    
    data <- Dataset_Summarize(station_number = input$stn_id_input,
                              summary_period = input$sum_period)
    
    data # return data
    
  })
  
  output$table <- DT::renderDataTable({
    
     summarized()%>% 
      
      DT::datatable(
      # Data table formatting options
      extensions = c('Buttons', 'FixedColumns', 'Scroller'),
      options = list(
        
        # Options for extension "Buttons"
        dom = 'Bfrtip',
        
        buttons = list(I('colvis')),
        
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        
        # Options for extension "FixedColumns"
        scrollX = TRUE,
        fixedColumns = TRUE,
        
        # Options for extension "Scroller"
        deferRender = TRUE,
        scrollY = 600,
        scroller = TRUE
      )
    )
    
  })# end of summary table render block
  
  # Summarized Data Download button
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste0(input$stn_id_input,"_", input$sum_period, ".csv")
    },
    content = function(file) {
      write.csv(summarized(), file, row.names = FALSE)
    }
  ) # End of csv file download
  
  
  # -6- Hydrograph Tab ---------------------------------------
  
  output$hydrograph <- renderPlot({

    Qdaily() %>%
      # FlowScreen package uses blank to represent "NA"
      mutate(SYM = replace_na(SYM,"")) %>% 
      # Definition of WY changes day of year calculation
      create.ts(hyrstart = as.numeric(input$wy_start_hydrograph)) %>%
      
      # Let user define y-axis limit (by % of max value)
      # lims must be defined as c(min, max)
      regime(y.lims = 
               c(0, as.numeric(input$hydrograph_ylim)/100 * max(Qdaily()$Flow, na.rm = TRUE)))
      
    
  })
  
  # -7- Trend Tests Tab ---------------------------------------
  
  output$trend <- renderPlot({
    
    Qdaily_ts <- Qdaily() %>%
      # FlowScreen package uses blank to represent "NA"
      mutate(SYM = replace_na(SYM,"")) %>% 
      # Definition of WY changes day of year calculation
      FlowScreen::create.ts(hyrstart = as.numeric(input$wy_start_trend))
    
    
    Qdaily_res <- Qdaily_ts %>% FlowScreen::metrics.all()
    opar <- par(no.readonly = TRUE)
    layout(matrix(c(1:12), 4, 3, byrow=TRUE))
    stninfo <- station.info(Qdaily_ts, Plot=TRUE)
    screen.frames(Qdaily_res, type="h", text=NULL, multi=TRUE)
    
    

  })
  
  
  
}) # End of ShinyServer(){}