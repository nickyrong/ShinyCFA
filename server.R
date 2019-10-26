rm(list=ls())
library(shiny)
library(tidyhydat)
library(leaflet)
library(dplyr)
library(lubridate)
library(readr)

# Generate the data for the map by calling coordinates and labels from the HYDAT database
#station_info <- tidyhydat::allstations %>%
#    filter(PROV_TERR_STATE_LOC == "BC") %>%
#    select(number = STATION_NUMBER, lat = LATITUDE, lng = LONGITUDE, name = STATION_NAME, status = HYD_STATUS) %>%
#    mutate(text = paste(sep = "<br/>", paste("<b>", number, "</b>"), name, status))

# A function to wrange the tidyhydat table into FlowScreen compatible format
read.wsc.flows <- function (station_number) {
    
    # read Qdaily from HYDAT
    dailyflow_tibble = tidyhydat::hy_daily_flows(station_number = station_number)
    
    # put the Qdaily into format readable by FlowScreen{}
    wsc_input_df = data.frame(
        ID = rep(station_number, length.out = dim(dailyflow_tibble)[1]),
        # Flow parameter: 1 = Flow, 2 = level
        PARAM = rep(1, length.out = dim(dailyflow_tibble)[1]),
        Date = dailyflow_tibble$Date,
        Flow = dailyflow_tibble$Value,
        SYM = dailyflow_tibble$Symbol,
        Agency = rep("WSC", length.out = dim(dailyflow_tibble)[1])
    )
    
    return(wsc_input_df)
} # EOF read.wsc.flows()

#### EOF
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

    # Map
    output$MapPlot <- renderLeaflet({

        grab_range <- function() {
            
            range.df <-  hy_stn_data_range() %>% 
                
                # Grab Flow record range (Q)
                filter(DATA_TYPE == "Q") %>%
                select(STATION_NUMBER, Qfrom = Year_from, Qto = Year_to, Qn = RECORD_LENGTH) %>%
                
                # Grab Stage record range (H) and append to the df
                left_join(
                    hy_stn_data_range() %>%
                        filter(DATA_TYPE == "H") %>%
                        select(STATION_NUMBER, Hfrom = Year_from, Hto = Year_to, Hn = RECORD_LENGTH),
                    by = "STATION_NUMBER"
                ) # end of left_join()
            # end of range.df piping
            
        return(range.df)
        } #end of grab_range()
        
        tidyhydat::allstations %>%
            left_join(grab_range(), by = "STATION_NUMBER") %>%
            mutate(text = paste(sep = "<br/>", paste("<b>", STATION_NUMBER, "</b>"), 
                                STATION_NAME, 
                                HYD_STATUS,
                                paste0("Flow Record: from ", Qfrom, " to ", Qto, " (", Qn, " Yrs)"),
                                paste0("Stage Record: from ", Hfrom, " to ", Hto, " (", Hn, " Yrs)")
                                )
                   ) %>%
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
                    summarise(Average_Flow = mean(Flow, na.rm = TRUE), Count = sum(is.na(Flow)==FALSE))
                TS$Average_Flow <- round(TS$Average_Flow, digits = 0)
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

})
