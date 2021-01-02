rm(list = ls())
library(shiny) #important
library(tidyhydat) # for downloading/opening HYDAT database
library(leaflet) # for station map
library(dplyr) # data manipulation
library(lubridate) # datetime support
library(xts) # required for time series plotting
library(dygraphs) # interactive timeseries plot
library(tidyr) # data manipulation
library(lmom) # frequency distribution fitting
library(plotly) # interactive plots
library(rlang) # distribution error handling
library(renv) # package version control
library(shinybusy) # busy indicator for rendering plots/tables
library(FlowScreen) # hydrograph and trend test functionality
library(pastecs) # quick descriptive stats stat.desc()
library(httr) #http_status() to check HYDAT version

# -------------- Custom Scripts ------------------

source("./functions.R")
source("./FlowScreen_funs_fixed.R")

# ------------ Shiny Server In/Output ------------


# Define server logic
shinyServer(function(input, output, session) {
  
  # -1- SideBar UI --------------------------------------------
  
  # HYDAT version query
  output$HYDAT_version <- renderText({
    
    db_ver_date <- tidyhydat::hy_version(hydat_path = Hydat_Location) %>% 
      "[["(1,2) %>% base::as.Date() #*
    
    validate(
      need(
        http_status(GET('https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/'))$reason == "OK",
        paste0("HYDAT Databse Ver.: ", db_ver_date, "\n",
               "(Ver verification failed: cannot connect to WSC)")
        
      )
    )
    
    HYDAT_page <- readLines('https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/')
    sqlite3_line <- HYDAT_page[grep(pattern = "Hydat_sqlite3", x = HYDAT_page)]
    
    most_updated_ver <- gsub('^.*Hydat_sqlite3_\\s*|\\s*.zip.*$', '', sqlite3_line) %>% 
      base::as.Date(format = "%Y%m%d")
    
    if (db_ver_date < most_updated_ver) {
      
      paste0("HYDAT Databse Ver.: ", db_ver_date, "\n",
             "<p style='color:red'> (Outdated - Contact App Maintainers) </p>")
      
    } else if(db_ver_date == most_updated_ver){
      
      paste0("HYDAT Databse Ver.: ", db_ver_date, "\n",
             "<p style='color:green'> (Up-to-date) </p>")
      
    } else{
      
      paste0("HYDAT Databse Ver.: ", db_ver_date, "\n",
             "<p style='color:orange'> (Version Status Unknown) </p>")
    }
    
  })
  
  # Station ID Selection by User
  updateSelectInput(session, 'stn_id_input',
                    choices = tidyhydat::allstations$STATION_NUMBER,
                    selected = "08MH147"
  )
  
  output$stn_input_info <- renderText({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    # Return Station Name & Info
    map_data %>% filter(STATION_NUMBER == input$stn_id_input) %>%
      select(text) %>% as.character()
    
    
  })
  
  # Central data processing: read Q daily
  Qdaily <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    read_dailyflow(input$stn_id_input)
    
  })
  
  # Central data processing: read Q inst
  Qinst <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    read_Qinst(input$stn_id_input)
    
  })
  
  # -2- ReadMe Tab --------------------------------------------
  # using HTML will mess up CSS style/theme format for some reasons, use Markdown
  
  output$README <- renderUI({
    
    # When run locally, README is in parent folder (for github/gitlab)
    if(file.exists("../README.md")) {
        includeMarkdown("../README.md")
    } else {
        # when deployed, README is copied to the same deployment folder
        includeMarkdown("./README.md")
    }

  })

  
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
  
  # -4- Data Summary Tab ---------------------------------------
  
  # the summarized data are needed in both table rendering and download button
  summarized <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    data <- Dataset_Summarize(station_number = input$stn_id_input,
                              summary_reso = input$sum_period,
                              wy_month = as.numeric(input$wy_start))
    
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
  

  # -5- Time Series Tab ---------------------------------------

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
  
  
  # -6- Hydrograph Tab ---------------------------------------
  
  output$hydrograph <- renderPlot({

    Qdaily() %>%
      # FlowScreen package uses blank to represent "NA"
      mutate(SYM = replace_na(SYM,"")) %>% 
      # Definition of WY changes day of year calculation
      create.ts(hyrstart = as.numeric(input$wy_start)) %>%
      
      # Let user define y-axis limit (by % of max value)
      # lims must be defined as c(min, max)
      regime(y.lims = 
               c(0, as.numeric(input$hydrograph_ylim)/100 * max(Qdaily()$Flow, na.rm = TRUE)))
      
    
  })
  
  # -7- Trend Tests Tab ---------------------------------------
  
  # output$trends <- renderPlot({
  #   
  #   Qdaily_ts <- Qdaily() %>%
  #     # FlowScreen package uses blank to represent "NA"
  #     mutate(SYM = replace_na(SYM,"")) %>% 
  #     # Definition of WY changes day of year calculation
  #     FlowScreen::create.ts(hyrstart = as.numeric(input$wy_start))
  #   
  #   
  #   Qdaily_res <- Qdaily_ts %>% FlowScreen::metrics.all()
  #   opar <- par(no.readonly = TRUE)
  #   layout(matrix(c(1:12), 4, 3, byrow=TRUE))
  #   stninfo <- station.info(Qdaily_ts, Plot=TRUE)
  #   screen.frames(Qdaily_res, type="h", text=NULL, multi=TRUE)
  #   
  #   
  # 
  # })
  
  
  # -8- Qdaily Frequency Analysis Tab -------------------------

  
  # Only keep complete years base on threshold selector
  Qdaily_years <- reactive({

    complete_years <- Qdaily() %>%
      mutate(Year = lubridate::year(Date)) %>%
      drop_na(Flow) %>%
      group_by(Year) %>%
      count(Year) %>%
      filter(n >= input$selector_days) %>% # discard incomplete years based on day selection
      pull(Year)
    
    complete_years
  })

  # Need to observe years available for removal after input$selector_days
  observe({
    # Update Years Available for Removal
    updateSelectInput(session, 'remove_year_Qdaily',
                      choices = Qdaily_years()
    )
  })

  # Output complete years (after threshold & manual removal)
  complete_years_Qdaily <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    validate(
      need((Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]) > 2,
           "!!! Insufficient Amount of Complete Year !!!"))
    
    Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]
    
  })
  
  # Render a text sentence for the # of complete years
  output$complete_years_Qdaily <- renderText({

    paste0("There are <b>", length(complete_years_Qdaily()), 
           "</b> complete years after removal.")

  })
  
  # reactive function to prevent an empty selected months
  selected_months_num <- reactive({
    
    validate(
      need(length(input$months_Qdaily)>0, "Select at least 1 month")
      )
    
    #Convert month abbreviations to numerical values
    match(input$months_Qdaily, base::month.abb)
    
  })
  
  # Empirical frequency analysis - df needed for plot and table
  empirical_daily <- reactive({
    
    validate(
      need((Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]) > 2,
           "!!! Insufficient Amount of Complete Year !!!"))
    
    validate(
      need(length(input$months_Qdaily)>0, "Select at least 1 month")
    )
    
    empirical_ffa(flow_df = Qdaily(), 
                  complete_years = complete_years_Qdaily(), 
                  selected_months = selected_months_num()) %>%
     
      rename(`Peak Discharge (cms)` = AMS)
    

  })
  
  # output simple descriptive statistics about the data
  output$daily_simplestats <- renderPrint({
    
    empirical_daily()$`Peak Discharge (cms)` %>% 
      stat.desc(basic = TRUE, desc = TRUE, norm = TRUE) %>% 
      round(digits = 3)
    
  })
  
  # Empirical frequency figure
  output$daily_ams_fig <- renderPlotly({


    daily_ams_plot <- ggplot(empirical_daily(), aes(x = Year, y = `Peak Discharge (cms)`, 
                                                    `Return Period` = `Return Period`)) +
      geom_point() + theme_bw() +
      scale_y_continuous(name = "Annual Max Peak Discharge (m<sup>3</sup>/s)", limits=c(0, NA)) +
      scale_x_continuous(name = "Year", limits=c(NA, NA)) +
      ggtitle(paste0(input$stn_id_input, " Daily Flow"))
    
    ggplotly(daily_ams_plot)
  })
  
  
  # Empirical frequency table
  output$daily_ams_table <- DT::renderDataTable({
    
    formated_df <- empirical_daily() %>%
      select(ID, Year, Date, `Peak Discharge (cms)`, Rank, `Return Period`, SYM) 
    
    formated_df %>% DT::datatable(
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
  
  # Analytical Frequency Distribution
  
  # Update available distribution list in the UI
  updateSelectInput(session, 'daily_selector_dist',
                    choices = Dist_Options,
                    selected = "GEV"
  )
  
  
  # Analytical frequency distribution - df needed for plot and table
  analytical_daily <- reactive({
    
    validate(
      need((Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]) > 2,
           "!!! Insufficient Amount of Complete Year !!!"))
    
    validate(
      need(length(input$months_Qdaily)>0, "Select at least 1 month")
    )
    
    desired_columns <- Dist_Options[match(input$daily_selector_dist, Dist_Options)]
    
    if (length(input$daily_selector_Tr) < 1) (
      ffa_results <- lmom_Q(Qp = empirical_daily()$`Peak Discharge (cms)`) %>%
        mutate_at(vars(-Pnonexc), round(., 2)) %>%
        mutate_at(vars(Pnonexc), round(., 6)) %>%
        select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
        rename("Return Periods" = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
      
    ) else (
      ffa_results <- lmom_Q(Qp = empirical_daily()$`Peak Discharge (cms)`, 
                            empirical.Tr = as.integer(input$daily_selector_Tr)) %>%
        mutate_at(vars(-Pnonexc), list(~round(., 2))) %>%
        mutate_at(vars(Pnonexc), list(~round(., 6))) %>%
        select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
        rename(`Return Period` = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
    )
    
    ffa_results 
  })
  
  output$daily_lmoments <- renderPrint({
    
    empirical_daily()$`Peak Discharge (cms)` %>% samlmu(nmom=5)
    
  })
  
  
  # Analytical frequency distribution table
  output$daily_dist_table <- DT::renderDataTable({
  
    
    analytical_daily() %>% DT::datatable(
      # Options for data table formatting
      extensions = c('Buttons', 'FixedColumns'),
      options = list(
        
        # Options for extension "Buttons"
        dom = 'Bfrtip',
        
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        
        # Options for extension "FixedColumns"
        scrollX = TRUE,
        
        # Options for extension "Scroller"
        deferRender = TRUE,
        scroller = TRUE
      )
    )
  })
  
  
  # Analytical frequency distribution figure
  output$daily_dist_fig <- renderPlotly({

    desired_columns <- Dist_Options[match(input$daily_selector_dist, Dist_Options)]
    
    ffa_results <- lmom_Q(Qp = empirical_daily()$`Peak Discharge (cms)`, evaluation = "Plot") %>%
      select(-Pnonexc) %>%
      select(ReturnPeriods, !!desired_columns) %>%
      rename(`Return Period` = ReturnPeriods)
    
    ffa_results <- gather(ffa_results, "Distribution", "Q", -1)
    
    ffa_reduced_variate <- -log(-log(1-1/ffa_results$`Return Period`))
    empirical_reduced_variate <- -log(-log(1-1/empirical_daily()$`Return Period`))
    
    if (length(desired_columns) > 0) (
      
      ffa_plot <- ggplot(ffa_results) + theme_bw() +
        
        # Need return period to be in the pop-up, but ggplot will protest unknown aesthetics, suppress
        suppressWarnings(geom_line(
                            aes(x = ffa_reduced_variate, y = Q, 
                                color = Distribution, Tr = `Return Period`))) +
        
        suppressWarnings(geom_point(data = empirical_daily(), 
                            aes(x = empirical_reduced_variate, y = `Peak Discharge (cms)`, 
                                colour = "Observed", Tr = `Return Period`))) +
        
        scale_x_continuous(name = "Return Periods (Year)", 
                           breaks =-log(-log(1-1/c(2,5,10,20,50,100,200,500,1000))), 
                           labels = c(2,5,10,20,50,100,200,500,1000)) +
        scale_y_continuous(name = "Daily Peak Discharge (m<sup>3</sup>/s)", limits=c(0, NA)) + 
        
        ggtitle("L-moments Fitted Frequency Distributions") 
      
    )
    
    if (length(desired_columns) > 0) (ggplotly(ffa_plot, height = 800, width = 1000))
  })
  

  
  # -9- Qinst Frequency Analysis Tab -------------------------
  
  # Years that not selected for removal
  Qinst_years <- reactive({
    
    Qinst_y <- Qinst() %>%
      mutate(Year = lubridate::year(Date)) %>%
      pull(Year)
    
    Qinst_y
  })
  
  # Need to observe years available for removal after input$selector_days
  observe({
    # Update Years Available for Removal
    updateSelectInput(session, 'remove_year_Qinst',
                      choices = Qinst_years()
    )
  })
  
  # Output selected years (after manual removal)
  selected_years_Qinst <- reactive({
    
    validate(
      need(input$stn_id_input, "Invalid Station ID"))
    
    validate(
      need(Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)] > 2,
           "!!! Insufficient Amount of Complete Year !!!"))
    
    Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)]
    
  })
  
  # Render a text sentence for the # of complete years
  output$selected_years_Qinst <- renderText({
    
    paste0("There are <b>", length(selected_years_Qinst()), 
           "</b> years in sample data after removal.")
    
  })
  
  
  # Empirical frequency analysis - df needed for plot and table
  empirical_inst <- reactive({
    
    validate(
      need(Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)] > 2,
           "!!! Insufficient Amount of Sample Data !!!"))
    

    empirical_ffa(flow_df = Qinst(), 
                  complete_years = selected_years_Qinst(), 
                  selected_months = seq(1:12)) %>%
      
      rename(`Inst. Peak Discharge (cms)` = AMS)
    
    
  })
  
  # output simple descriptive statistics about the data
  output$inst_simplestats <- renderPrint({
    
    empirical_inst()$`Inst. Peak Discharge (cms)` %>% 
      stat.desc(basic = TRUE, desc = TRUE, norm = TRUE) %>% 
      round(digits = 3)
    
  })
  
  # Empirical frequency figure
  output$inst_ams_fig <- renderPlotly({
    
    
    inst_ams_plot <- ggplot(empirical_inst(), aes(x = Year, y = `Inst. Peak Discharge (cms)`, 
                                                  `Return Period` = `Return Period`)) +
      geom_point() + theme_bw() +
      scale_y_continuous(name = "Annual Max Peak Discharge (m<sup>3</sup>/s)", limits=c(0, NA)) +
      scale_x_continuous(name = "Year", limits=c(NA, NA)) +
      ggtitle(paste0(input$stn_id_input, " Instantaneous Peakflow"))
    
    ggplotly(inst_ams_plot)
  })
  
  
  # Empirical frequency table
  output$inst_ams_table <- DT::renderDataTable({
    
    formated_df <- empirical_inst() %>%
      select(ID, Year, Date, `Inst. Peak Discharge (cms)`, Rank, `Return Period`, SYM) 
    
    formated_df %>% DT::datatable(
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
  
  # Analytical Frequency Distribution
  
  # Update available distribution list in the UI
  updateSelectInput(session, 'inst_selector_dist',
                    choices = Dist_Options,
                    selected = "GEV"
  )
  
  
  # Analytical frequency distribution - df needed for plot and table
  analytical_inst <- reactive({
    
    validate(
      need(Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)] > 2,
           "!!! Insufficient Amount of Sample Data !!!"))
    

    desired_columns <- Dist_Options[match(input$inst_selector_dist, Dist_Options)]
    
    if (length(input$inst_selector_Tr) < 1) (
      ffa_results <- lmom_Q(Qp = empirical_inst()$`Inst. Peak Discharge (cms)`) %>%
        mutate_at(vars(-Pnonexc), round(., 2)) %>%
        mutate_at(vars(Pnonexc), round(., 6)) %>%
        select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
        rename("Return Periods" = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
      
    ) else (
      ffa_results <- lmom_Q(Qp = empirical_inst()$`Inst. Peak Discharge (cms)`, 
                            empirical.Tr = as.integer(input$inst_selector_Tr)) %>%
        mutate_at(vars(-Pnonexc), list(~round(., 2))) %>%
        mutate_at(vars(Pnonexc), list(~round(., 6))) %>%
        select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
        rename(`Return Period` = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
    )
    
    ffa_results 
  })
  
  output$inst_lmoments <- renderPrint({
    
    empirical_inst()$`Inst. Peak Discharge (cms)` %>% samlmu(nmom=5)
    
  })
  
  
  # Analytical frequency distribution table
  output$inst_dist_table <- DT::renderDataTable({
    
    
    analytical_inst() %>% DT::datatable(
      # Options for data table formatting
      extensions = c('Buttons', 'FixedColumns'),
      options = list(
        
        # Options for extension "Buttons"
        dom = 'Bfrtip',
        
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        
        # Options for extension "FixedColumns"
        scrollX = TRUE,
        
        # Options for extension "Scroller"
        deferRender = TRUE,
        scroller = TRUE
      )
    )
  })
  
  
  # Analytical frequency distribution figure
  output$inst_dist_fig <- renderPlotly({
    
    desired_columns <- Dist_Options[match(input$inst_selector_dist, Dist_Options)]
    
    ffa_results <- lmom_Q(Qp = empirical_inst()$`Inst. Peak Discharge (cms)`, evaluation = "Plot") %>%
      select(-Pnonexc) %>%
      select(ReturnPeriods, !!desired_columns) %>%
      rename(`Return Period` = ReturnPeriods)
    
    ffa_results <- gather(ffa_results, "Distribution", "Q", -1)
    
    ffa_reduced_variate <- -log(-log(1-1/ffa_results$`Return Period`))
    empirical_reduced_variate <- -log(-log(1-1/empirical_inst()$`Return Period`))
    
    if (length(desired_columns) > 0) (
      
      ffa_plot <- ggplot(ffa_results) + theme_bw() +
        
        # Need return period to be in the pop-up, but ggplot will protest unknown aesthetics, suppress
        suppressWarnings(geom_line(
          aes(x = ffa_reduced_variate, y = Q, 
              color = Distribution, Tr = `Return Period`))) +
        
        suppressWarnings(geom_point(data = empirical_inst(), 
                                    aes(x = empirical_reduced_variate, y = `Inst. Peak Discharge (cms)`, 
                                        colour = "Observed", Tr = `Return Period`))) +
        
        scale_x_continuous(name = "Return Periods (Year)", breaks =-log(-log(1-1/c(2,5,10,20,50,100,200,500,1000))), 
                           labels = c(2,5,10,20,50,100,200,500,1000)) +
        scale_y_continuous(name = "Instantaneous Peak Discharge (m<sup>3</sup>/s)", limits=c(0, NA)) + 
        
        ggtitle("L-moments Fitted Frequency Distributions") 
      
    )
    
    if (length(desired_columns) > 0) (ggplotly(ffa_plot, height = 800, width = 1000))
  })
  
  
}) # End of ShinyServer(){}