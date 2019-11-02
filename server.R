rm(list=ls())
library(shiny)
library(tidyhydat)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)
library(xts)
library(dygraphs)
library(tidyr)

# Determine Hydat database location
if(file.exists ("./Database/Hydat.sqlite3")){
    Hydat_Location <- ".\\Database/Hydat.sqlite3"
} else {
    Hydat_Location <- tidyhydat::hy_default_db()
    }

# Generate the data for the map by calling coordinates, labels, and date ranges from the HYDAT database
range.df <-  tidyhydat::hy_stn_data_range(hydat_path = Hydat_Location) %>% 
    
    # Grab Flow record range (Q)
    filter(DATA_TYPE == "Q") %>%
    select(STATION_NUMBER, Qfrom = Year_from, Qto = Year_to, Qn = RECORD_LENGTH) %>%
    
    # Grab Stage record range (H) and append to the df
    left_join(
        tidyhydat::hy_stn_data_range(hydat_path = Hydat_Location) %>%
            filter(DATA_TYPE == "H") %>%
            select(STATION_NUMBER, Hfrom = Year_from, Hto = Year_to, Hn = RECORD_LENGTH),
        by = "STATION_NUMBER"
    ) # end of left_join()

map_data <- tidyhydat::allstations %>%
    left_join(range.df, by = "STATION_NUMBER") %>%
    mutate(text = paste(sep = "<br/>", paste("<b>", STATION_NUMBER, "</b>"), 
                        STATION_NAME, 
                        HYD_STATUS,
                        paste0("Flow Record: from ", Qfrom, " to ", Qto, " (", Qn, " Yrs)"),
                        paste0("Stage Record: from ", Hfrom, " to ", Hto, " (", Hn, " Yrs)")
    ))

# A function to wrangle the tidyhydat table into FlowScreen compatible format
read.wsc.flows <- function(station_number) {
    
    # read Qdaily from HYDAT
    Q_Daily = tidyhydat::hy_daily_flows(station_number = station_number, hydat_path = Hydat_Location)
    
    # put the Qdaily into format readable by FlowScreen Package
    wsc_input_df <- Q_Daily %>% 
        # Flow parameter: 1 = Flow, 2 = level
        mutate(ID = station_number, PARAM = 1, Agency = "WSC") %>% 
        select(ID, PARAM, Date, Flow = Value, SYM = Symbol, Agency)
    
    return(wsc_input_df)
} # EOF read.wsc.flows()

# Create flag names and plotting colours for data symbols
SYMs <- c("", "E", "A", "B", "D", "R")
SYMnames <- c("No Code", "Estimate", "Partial Day", "Ice Conditions", "Dry", "Revised")
SYMcol <- c("grey", "#E41A1C", "#4DAF4A", "#377EB8", "#FF7F00", "#984EA3")

# Define server logic
shinyServer(function(input, output) {
    
    # Status if station is invalid
    output$status <- renderText({
        if (!(toupper(input$stn.id) %in% tidyhydat::allstations$STATION_NUMBER)){
            "Station Invalid"
            } else {""}
    })

    # Reactive statement to allow functions to run only if station is valid
    id.check <- reactive({
        # Make sure station is valid & output CAPITALIZED ID (always go through ID CHECK)
        req(toupper(input$stn.id) %in% tidyhydat::allstations$STATION_NUMBER)

        toupper(input$stn.id)
    }) # EOF id.check()

    # Station Name
    output$name <- renderText({
        printname <- tidyhydat::allstations %>% filter(STATION_NUMBER == id.check()) %>%
            select(STATION_NAME)
        printname[[1]]
    })
    
    # Station Dataset
    Dataset <- reactive({
        Q_Daily = tidyhydat::hy_daily_flows(station_number = id.check(), hydat_path = Hydat_Location)
        Q_Daily
    })

    # Map
    output$MapPlot <- renderLeaflet({
        map_data %>% 
            leaflet() %>%
            addTiles() %>%
            addMarkers(~LONGITUDE, ~LATITUDE, popup = ~text, clusterOptions = markerClusterOptions())
    })

    # Data table
    output$table <- DT::renderDataTable({

            DT::datatable({
            TS = read.wsc.flows(station_number = id.check()) %>%
                mutate(Year = year(Date) , Month = month(Date), Day = day(Date)) %>%
                select(Date, Year, Month, Day, Flow, Symbol = SYM)

            codes <- as.factor(TS$Symbol)
            codes <- match(codes, SYMs)
            codes <- SYMnames[codes]

            TS <- mutate(TS, Flag = codes) %>% select(-Symbol)

            if(input$Reso == "Daily"){
                TS$Flow <- round(TS$Flow, digits = 0)
                TS
            }

            else if(input$Reso == "Monthly"){
                TS <- TS %>% group_by(Year, Month) %>%
                    summarise(Average_Flow = mean(Flow, na.rm = TRUE), Count = sum(is.na(Flow)==FALSE))
                TS$Average_Flow <- round(TS$Average_Flow, digits = 0)
                TS
            }
            else if(input$Reso == "Yearly"){
                TS <- TS %>% group_by(Year) %>%
                    summarise(Average_Flow = mean(Flow, na.rm = TRUE), Max_Daily = max(Flow, na.rm = TRUE),
                              Min_Daily = min(Flow, na.rm = TRUE), Count = sum(is.na(Flow)==FALSE))
                TS <- round(select(TS, -Year), digits = 0)
                TS
            }
        })
    })

    # Time Series Plot
    output$ts <- renderPlot({

        TS = read.wsc.flows(station_number = id.check()) %>%
            select(Date, Flow, Symbol = SYM)

        codes <- as.factor(TS$Symbol)
        codes <- match(codes, SYMs)

        graphics::par(mar=c(4,4,0,0.5))
        mYlims <- c(0, 1.2*max(TS$Flow, na.rm = TRUE))
        graphics::plot(TS$Date, TS$Flow, typ = "l", col = "grey",
                       xlab="Date", ylab="Flow (m^3/s)", ylim=mYlims)
        graphics::points(TS$Date, TS$Flow,
                         pch=19, col=SYMcol[codes], cex=0.5)
        graphics::legend(TS$Date[1], 1.15*max(TS$Flow, na.rm = TRUE), SYMnames, pch=19, pt.cex=0.9, cex=0.9, col=SYMcol,
                         bty="n", xjust=0, x.intersp=0.5, yjust=0.5, ncol=3)

    })
    
    ### Plot an interactive graph
#    output$test <- renderDygraph({
#    
#        # Spread the flow data by the flag
#        TS = read.wsc.flows(station_number = id.check()) %>%
#            select(Date, Flow, Symbol = SYM)
#        
#        plot_data <- spread(TS, Symbol, Flow)
#        number_flags <- length(unique(TS$Symbol))
#        flag_names <- colnames(plot_data)[-1]
#        
#        # Convert to an xts format required by the dygraphs package
#        xts_Date <- as.POSIXct(plot_data$Date, format = "%Y-%m-%d")
#        xts_Format <- as.xts(order.by = xts_Date, x = plot_data[,2])
#        
#        # Combine flag columns, :(number_flags - 1)
#        if(!number_flags == 1) {
#            for (i in 1:1) {
#                index <- 2 + i
#                xts_Format <- cbind(xts_Format, plot_data[,index])
#            }
#        }
#        colnames(xts_Format) <- paste0("Flag_", flag_names)
#        Title <- "Hydrograph Plot"
#        y_axis <- "Flow (m3/s)"
#        dygraph(xts_Format, main = Title, ylab = y_axis) %>% dyRangeSelector()
#    })
    
    # Download Data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(id.check(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Dataset(), file, row.names = FALSE)
        }
    )

})
