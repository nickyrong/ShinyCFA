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


# ------------ HYDAT database loading ------------

# Determine Hydat database location
if(file.exists("./database/Hydat.sqlite3")) {
    Hydat_Location <- "./database/Hydat.sqlite3"
} else {
    Hydat_Location <- tidyhydat::hy_default_db()
}


# ------------ Station Map Plotting ------------

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



# ------------ HYDAT data processing ------------
# A function to wrangle the tidyhydat table into FlowScreen compatible format for potential future use
read.wsc.flows <- function(station_number, type = "Qdaily") {


    if(type == "Qdaily") {
        
        # read Qdaily from HYDAT
        Q_Daily = tidyhydat::hy_daily_flows(station_number = station_number, hydat_path = Hydat_Location)
        
        # put the Qdaily into format readable by FlowScreen Package for potential future use
        wsc_input_df <- Q_Daily %>%
                        filter(Parameter == 'Flow') %>%

            # Flow parameter: 1 = Flow, 2 = level
            mutate(ID = station_number, PARAM = 1, Agency = "WSC") %>%
            select(ID, PARAM, Date, Flow = Value, SYM = Symbol, Agency)

        
    } else if (type == "Qinst") {
        
        # read Qinst from HYDAT
        Q_Inst = tidyhydat::hy_annual_instant_peaks(station_number = station_number, hydat_path = Hydat_Location)
        
        # put the QiNST into format readable by FlowScreen Package for potential future use
        wsc_input_df <- Q_Inst %>%
                        filter(Parameter == 'Flow',
                               PEAK_CODE == 'MAX') %>%
            
            # Flow parameter: 1 = Flow, 2 = level
            mutate(ID = station_number, PARAM = 1, Agency = "WSC") %>%
            select(ID, PARAM, Date, Flow = Value, SYM = Symbol, Agency)
    }
    
    return(wsc_input_df)
} # EOF read.wsc.flows()

# Create flag names and plotting colours for data symbols
SYMs <- c("", "E", "A", "B", "D", "R")
SYMnames <- c("No Code", "Estimate", "Partial Day", "Ice Conditions", "Dry", "Revised")
SYMcol <- c("grey", "#E41A1C", "#4DAF4A", "#377EB8", "#FF7F00", "#984EA3")

# Distributions List for Flood Frequency Analyis (FFA)
Dist_Options <- c("Exponential", "Gamma", "GEV", "Gen. Logistic", "Gen. Normal", "Gen. Pareto",
                  "Gumbel", "Kappa", "Normal", "Pearson III", "Wakeby", "Weibull", "Log-Normal", "LP3")
Dist_Key <- c("Qp.exp", "Qp.gam", "Qp.gev", "Qp.glo", "Qp.gno", "Qp.gpa",
              "Qp.gum", "Qp.kap", "Qp.nor", "Qp.pe3", "Qp.wak", "Qp.wei", "Qp.ln3", "Qp.LP3")

# ---------------- FFA FUNCTION ------------------
lmom_Q <- function(Qp, empirical.Tr = NA, evaluation = FALSE) {


    # Custom output (let's keep the custom of having largest on top)
    if(evaluation == TRUE) (
        ReturnPeriods <- empirical.Tr %>% sort(decreasing = TRUE)
    ) else if(evaluation == "Plot") (
        ReturnPeriods  <- c(seq(1.01, 1.99, by = 0.01), seq(2, 9.9, by = 0.1), 10:1000) %>% sort(decreasing = TRUE)
    ) else (
        ReturnPeriods <- empirical.Tr %>% sort(decreasing = TRUE)
    )

    Pnonexc = 1 - (1/ReturnPeriods)

    # samlmu() gets the sample L-moments, pelxxx() estimates the distribution's parameters from L-moments
    # Quaxxx generates quantile given probability and distribution parameters
    # xxx = "exp" "gam" "gev" "glo" "gno" "gpa" "gum" "kap" "ln3" "nor" "pe3" "wak" "wei"

    lmoms <- samlmu(Qp, nmom = 5)
    log.lmoms <- samlmu(log10(Qp),nmom = 5)

    error_value <- as.numeric(rep(NA, length(Pnonexc)))
    # using tryCatch to allow the app to continue running if a particular distibution can't be fit to the data
    extremes <- tibble(ReturnPeriods = ReturnPeriods,
                       Pnonexc = Pnonexc,
                       Qp.exp = tryCatch(error = function(err) {return(error_value)}, quaexp(f = Pnonexc, para = pelexp(lmoms))), # exponential
                       Qp.gam = tryCatch(error = function(err) {return(error_value)}, quagam(f = Pnonexc, para = pelgam(lmoms))), # gamma
                       Qp.gev = tryCatch(error = function(err) {return(error_value)}, quagev(f = Pnonexc, para = pelgev(lmoms))), # generalized extreme-value
                       Qp.glo = tryCatch(error = function(err) {return(error_value)}, quaglo(f = Pnonexc, para = pelglo(lmoms))), # generalized logistic
                       Qp.gno = tryCatch(error = function(err) {return(error_value)}, quagno(f = Pnonexc, para = pelgno(lmoms))), # generalized normal
                       Qp.gpa = tryCatch(error = function(err) {return(error_value)}, quagpa(f = Pnonexc, para = pelgpa(lmoms))), # generalized pareto
                       Qp.gum = tryCatch(error = function(err) {return(error_value)}, quagum(f = Pnonexc, para = pelgum(lmoms))), # gumbel (extreme value type I)
                       Qp.kap = tryCatch(error = function(err) {return(error_value)}, quakap(f = Pnonexc, para = pelkap(lmoms))), # kappa
                       Qp.nor = tryCatch(error = function(err) {return(error_value)}, quanor(f = Pnonexc, para = pelnor(lmoms))), # normal
                       Qp.pe3 = tryCatch(error = function(err) {return(error_value)}, quape3(f = Pnonexc, para = pelpe3(lmoms))), # pearson type III
                       Qp.wak = tryCatch(error = function(err) {return(error_value)}, quawak(f = Pnonexc, para = pelwak(lmoms))), # wakeby
                       Qp.wei = tryCatch(error = function(err) {return(error_value)}, quawei(f = Pnonexc, para = pelwei(lmoms))), # weibull

                       # Logged distribution from the package
                       Qp.ln3 = tryCatch(error = function(err) {return(error_value)}, qualn3(f = Pnonexc, para = pelln3(lmoms))), # lognormal

                       # Manually created log distribution
                       Qp.LP3 = tryCatch(error = function(err) {return(error_value)}, 10^quape3(f = Pnonexc, para = pelpe3(log.lmoms))) # log pearson type III
    )

    if (evaluation == TRUE) {
        extremes <- extremes %>% mutate(Qp.obs = Qp) # observed Qp
    }

    return(extremes)
} # End of Flood Frequency Function


# ------------ Shiny Server In/Output ------------


# Define server logic
shinyServer(function(input, output) {


    # Status if station is invalid
    output$status <- renderText({
        if (!(toupper(input$stn.id) %in% tidyhydat::allstations$STATION_NUMBER)) {
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

    # Station Dataset Summarized
    # Uses user input from the resolution dropdown
    Dataset_Summarize <- reactive({
        TS <- read.wsc.flows(station_number = id.check(), type = input$Qtype) %>%
            mutate(Year = year(Date) , Month = month(Date), Day = day(Date)) %>%
            select(Date, Year, Month, Day, Flow, Symbol = SYM)

        codes <- as.factor(TS$Symbol)
        codes <- match(codes, SYMs)
        codes <- SYMnames[codes]

        TS <- mutate(TS, Flag = codes) %>% select(-Symbol)

        if(input$Reso == "Daily"){
            TS$Flow <- round(TS$Flow, digits = 3)
        }

        else if(input$Reso == "Monthly"){
            TS <- TS %>% group_by(Year, Month) %>%
                summarise(Average_Flow = mean(Flow, na.rm = TRUE), Count = sum(is.na(Flow)==FALSE))
            TS$Average_Flow <- round(TS$Average_Flow, digits = 2)
        }

        else if(input$Reso == "Yearly"){
            TS <- TS %>% group_by(Year) %>%
                summarise(Average_Flow = mean(Flow, na.rm = TRUE), Max_Daily = max(Flow, na.rm = TRUE),
                          Min_Daily = min(Flow, na.rm = TRUE), Count = sum(is.na(Flow) == FALSE)) %>%
                round(digits = 2)
        }
        TS
    })
    
    # Download Data button
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(id.check(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Dataset(), file, row.names = FALSE)
        }
    ) # End of csv file download
    
    # ----------- REAMDE tab----------- -----------
    # Directly using includeHTML in ui.R will break Shiny (stop execution of everything follows)
    
    output$ReadMe_HTML <- renderUI({
        includeHTML("./Intro_tab.html")
        
    })

    # -------- Station Map Plotting tab -----------
    output$MapPlot <- renderLeaflet({
        map_data %>%
            leaflet() %>%
            addTiles() %>%
            addMarkers(~LONGITUDE, ~LATITUDE, popup = ~text, clusterOptions = markerClusterOptions())
    })


    # ------------ Data Table tab-------------------
    output$table <- DT::renderDataTable({

        Dataset_Summarize() %>% DT::datatable(
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

    })

    # Download Data Table
    output$downloadSummary <- downloadHandler(
        filename = function() {
            paste(id.check(), "_", as.character(input$Reso),".csv", sep = "")
        },
        content = function(file) {
            write.csv(Dataset_Summarize(), file, row.names = FALSE)
        }
    )

    # ------------ Time Series Graph (interactive) ------------
    ### Plot an interactive graph
    output$graph <- renderDygraph({

        # Spread the flow data by the flag
        TS <- read.wsc.flows(station_number = id.check(), type = "Qdaily") %>%
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
            dyRoller(rollPeriod = 1) %>%
            dyCrosshair(direction = "vertical") %>%
            dyRangeSelector()

        # Plot the series if it exists
        #c("No Code", "Estimate", "Partial Day", "Ice Conditions", "Dry", "Revised")
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

        # call to plot
        dy_plots

    }) # End of interactive graph


    # ------------ Frequency Analysis tab --------

    # Reactive statement to generate complete years for the FFA
    FFA_Years <- reactive({
        
        req(input$Qtype)
        
        # Only drop years with Qdaiy, force Qtype in read.wsc.flows()
        complete.years <- read.wsc.flows(station_number = id.check(), type = "Qdaily") %>%
            mutate(Year = year(Date)) %>%
            drop_na(Flow) %>%
            group_by(Year) %>%
            count(Year) %>%
            filter(n >= input$selector_days) %>% # discard incomplete years based on day selection
            pull(Year)

        complete.years
    })

    # Output years for year removal selector in the ui
    output$year_list <- renderUI({
        selectizeInput('selector_years', 'Remove Years',
                       choices = FFA_Years(), multiple = TRUE)
    })

    # Subset FFA based on selection
    FFA <- reactive({
        
        req(input$Qtype)
        
        complete.years <- FFA_Years()
        
        if(input$Qtype == "Qdaily") {
            
            empirical.ffa <- read.wsc.flows(station_number = id.check(), type = "Qdaily") %>%
                filter(year(Date) %in% complete.years) %>% #only include complete years
                filter(month(Date) >= input$selector_months[1] & month(Date) <= input$selector_months[2]) %>% #subset based on month selection
                drop_na(Flow) %>%
                group_by(Year = year(Date)) %>%
                filter(Flow == max(Flow)) %>%
                rename(AMS = Flow) %>%
                ungroup() %>%
                filter(!Year %in% input$selector_years) %>% #remove years the user has selected for removal
                mutate(Rank = base::rank(-AMS, ties.method = "random"),
                       Tr = ((length(Rank)+1) / Rank)
                ) %>%
                arrange(Rank)
            
        } else if(input$Qtype == "Qinst") {
            
            empirical.ffa <- read.wsc.flows(station_number = id.check(), type = "Qinst") %>%
                filter(year(Date) %in% complete.years) %>%
                # Cannot subset by months
                drop_na(Flow) %>%
                group_by(Year = year(Date)) %>%
                filter(Flow == max(Flow)) %>%
                rename(AMS = Flow) %>%
                ungroup() %>%
                filter(!Year %in% input$selector_years) %>% #remove years the user has selected for removal
                mutate(Rank = base::rank(-AMS, ties.method = "random"),
                       Tr = ((length(Rank)+1) / Rank)
                ) %>%
                arrange(Rank)
        }


        empirical.ffa
    })

    # Output number of complete years for FFA
    output$FFA_complete_years <- renderText({
        if(nrow(FFA()) > 1) (
            paste("Complete Years:", nrow(FFA()))
        ) else (
            paste("Complete Years:", nrow(FFA()), "(Insufficient Years)")
        )
    })

    # Reactive statement to allow FFA to run only if number of years is > 1
    FFA.Allow <- reactive({
        req(nrow(FFA()) > 1)
        FFA()
    })

    # AMS DataTable
    output$AMS.table <- DT::renderDataTable({
        empirical.ffa <- FFA.Allow()

        empirical.ffa <- empirical.ffa %>%
            mutate(Return_Period = round(Tr, 2)) %>%
            select(ID, Year, Date, AMS, Rank, Return_Period, SYM)

        empirical.ffa %>% DT::datatable(
            # Options for data table formatting
            extensions = c('Buttons', 'FixedColumns'),
            options = list(

                columnDefs = list(list(className = "dt-center", targets = "_all")),

                # Options for extension "FixedColumns"
                scrollX = TRUE,
                
                # Rows
                lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
                pageLength = 10
            )
        )
    })

    # FFA DataTable
    output$ffa.table <- DT::renderDataTable({

        empirical.ffa <- FFA.Allow()
        desired_columns <- Dist_Key[match(input$selector_dist, Dist_Options)]

        if (length(input$selector_Tr) < 1) (
            ffa_results <- lmom_Q(Qp = empirical.ffa$AMS) %>%
                mutate_at(vars(-Pnonexc), round(., 2)) %>%
                mutate_at(vars(Pnonexc), round(., 2)) %>%
                select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
                rename("Return Periods" = ReturnPeriods, "Probability Non-Exc" = Pnonexc)

        ) else (
            ffa_results <- lmom_Q(Qp = empirical.ffa$AMS, empirical.Tr = as.integer(input$selector_Tr)) %>%
                mutate_at(vars(-Pnonexc), list(~round(., 2))) %>%
                mutate_at(vars(Pnonexc), list(~round(., 2))) %>%
                select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
                rename("Return Periods" = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
        )

        ffa_results %>% DT::datatable(
            # Options for data table formatting
            extensions = c('Buttons', 'FixedColumns'),
            options = list(

                # Options for extension "Buttons"
                dom = 'Bfrtip',

                buttons = list(I('colvis')),

                columnDefs = list(list(className = "dt-center", targets = "_all")),

                # Options for extension "FixedColumns"
                scrollX = TRUE,

                # Options for extension "Scroller"
                deferRender = TRUE,
                scroller = TRUE

            )
        )
    })

    # FFA Plot
    output$ffa.figure <- renderPlotly({

        empirical.ffa <- FFA.Allow()
        desired_columns <- Dist_Key[match(input$selector_dist, Dist_Options)]

        ffa_results <- lmom_Q(Qp = empirical.ffa$AMS, evaluation = "Plot") %>%
            select(-Pnonexc) %>%
            select(ReturnPeriods, !!desired_columns)

        ffa_results <- gather(ffa_results, "Distribution", "Q", -1)

        if (length(desired_columns) > 0) (
                
            ffa_plot <- ggplot(ffa_results, aes(x = ReturnPeriods, y = Q, color = Distribution)) +
                geom_line() + theme_bw() +
                geom_point(data = empirical.ffa, aes(x = Tr, y = AMS, colour = "Observed")) +
                scale_x_log10(name = "Annual Return Periods") +
                ggtitle("Qdaily Flood Frequency Analysis") + 
                
                {if(input$Qtype == "Qdaily") scale_y_continuous(name = 'Q Daily (m3/s)', limits=c(0, NA))} + 
                {if(input$Qtype == "Qdaily") ggtitle("Qdaily Flood Frequency Analysis")} +
                
                {if(input$Qtype == "Qinst") scale_y_continuous(name = 'Q Inst (m3/s)', limits=c(0, NA))} + 
                {if(input$Qtype == "Qinst") ggtitle("Q Instantaneous Flood Frequency Analysis")}

        )

        if (length(desired_columns) > 0) (ggplotly(ffa_plot, height = 800, width = 1280))
    })


    # AMS Plot
    output$max.figure <- renderPlotly({

        empirical.ffa <- FFA.Allow()
        max_plot <- ggplot(empirical.ffa, aes(x = Year, y = AMS)) +
                        geom_point() + theme_bw() +
                        scale_y_continuous(name = "Q (m3/s)", limits=c(0, NA)) +
                        scale_x_continuous(name = "Year", limits=c(NA, NA)) +
                        {if(input$Qtype == "Qdaily") ggtitle("Annual Daily Maximum Series")} +
                        {if(input$Qtype == "Qinst") ggtitle("Annual Instantaneous Maximum Series")}

        ggplotly(max_plot)
    })

})
