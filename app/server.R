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
library(pastecs) # quick descriptive stats stat.desc()
library(httr) # require to check HYDAT newest version
library(sf)


# -------------- Custom Scripts ------------------

source("./functions.R")
source("./roi_functions.R")   # automated regionalization: ROI (GEO-PHY) + BC zones


# ------------ Shiny Server In/Output ------------


# Define server logic
shinyServer(function(input, output, session) {

  
  # Here set up any elements that require spinner
  spin_tsgraph <- Waiter$new("tsgraph", html = spin_fading_circles(), color = "black")
  spin_table <- Waiter$new("table", html = spin_fading_circles(), color = "black")
  
  
  # -1- SideBar UI --------------------------------------------
  
  # HYDAT version query
  output$HYDAT_version <- renderText({

    db_ver_date <- tryCatch(
      tidyhydat::hy_version(hydat_path = Hydat_Location) %>%
        "[["(1,2) %>% base::as.Date(), #*
      error = function(e) {
        message("HYDAT version read failed: ", conditionMessage(e))
        NULL
      }
    )
    validate(need(!is.null(db_ver_date), "HYDAT version could not be read."))

    # The newest-release check is only informative: if WSC can't be reached, still
    # show the installed version rather than an error.
    HYDAT_page <- wsc_collab_page()

    if (inherits(HYDAT_page, "error")) {

      paste0("HYDAT Database Ver.: ", db_ver_date, "\n",
             "<p style='color:orange'> (Ver verification failed: cannot connect to WSC) </p>")

    } else {

      sqlite3_line <- HYDAT_page[grep(pattern = "Hydat_sqlite3", x = HYDAT_page)]

      most_updated_ver <- gsub('^.*Hydat_sqlite3_\\s*|\\s*.zip.*$', '', sqlite3_line) %>%
        base::as.Date(format = "%Y%m%d")

      if (length(most_updated_ver) != 1 || is.na(most_updated_ver)) {

        paste0("HYDAT Database Ver.: ", db_ver_date, "\n",
               "<p style='color:orange'> (Version Status Unknown) </p>")

      } else if (db_ver_date < most_updated_ver) {

        paste0("HYDAT Database Ver.: ", db_ver_date, "\n",
               "<p style='color:red'> (Outdated - Contact App Maintainers) </p>")

      } else if (db_ver_date == most_updated_ver) {

        paste0("HYDAT Database Ver.: ", db_ver_date, "\n",
               "<p style='color:green'> (Up-to-date) </p>")

      } else {

        paste0("HYDAT Database Ver.: ", db_ver_date, "\n",
               "<p style='color:orange'> (Version Status Unknown) </p>")
      }
    }

  })
  
  # Station ID Selection by User (server-side selectize for performance)
  updateSelectizeInput(session, 'stn_id_input',
                       choices = map_data$STATION_NUMBER,  # from cached RDS (see functions.R)
                       selected = character(0),            # no default: map opens on the overview
                       server = TRUE)
  
  output$stn_input_info <- renderText({
    
    validate(
      need(input$stn_id_input, "Select a WSC station ID (top-left) to begin."))
    
    # Return Station Name & Info
    map_data %>% filter(STATION_NUMBER == input$stn_id_input) %>%
      select(text) %>% as.character()
    
    
  })
  
  # Central data processing: read Q daily, catch no-data errors
  Qdaily <- reactive({
    req(input$stn_id_input)
    daily <- tryCatch(
      read_dailyflow(input$stn_id_input),
      error = function(e) NULL
    )
    validate(
      need(!is.null(daily) && nrow(daily) > 0,
           "No daily flow data for this station.")
    )
    daily
  })

  # Central data processing: read Q inst, catch no-data errors
  Qinst <- reactive({
    req(input$stn_id_input)
    inst <- tryCatch(
      read_Qinst(input$stn_id_input),
      error = function(e) NULL
    )
    validate(
      need(!is.null(inst) && nrow(inst) > 0,
           "No instantaneous flow data for this station.")
    )
    inst
  })

  # ---- Realtime / provisional unit-value data (WSC collaboration site) -------
  # Kept in its OWN store so it never feeds the FFA or the regional analysis.
  rt_store <- reactiveVal(NULL)   # list(daily = <tibble>, meta = <list>) or NULL

  # "Plot Current Year" overlay toggle. Defined here (alongside rt_store) so it can
  # be reset whenever the realtime data goes away — the button is only shown while
  # realtime data is loaded, so a stuck-on overlay would be unclearable otherwise.
  show_cur_year <- reactiveVal(FALSE)
  reset_current_year <- function() {
    show_cur_year(FALSE)
    updateActionButton(session, "hydro_current_year", label = "Plot Current Year")
  }

  # Clear whenever the station changes (the data is station-specific).
  observeEvent(input$stn_id_input, { rt_store(NULL); reset_current_year() }, ignoreInit = TRUE)

  # Convenience accessor: the realtime DAILY tibble or NULL.
  realtime_daily <- reactive({ s <- rt_store(); if (is.null(s)) NULL else s$daily })

  # Loaded-state flag drives the conditional Retrieve/Remove buttons in the UI.
  output$realtime_loaded <- reactive({ !is.null(rt_store()) })
  outputOptions(output, "realtime_loaded", suspendWhenHidden = FALSE)

  observeEvent(input$retrieve_realtime, {
    req(input$stn_id_input)
    daily_hy   <- tryCatch(Qdaily(), error = function(e) NULL)
    hydat_last <- if (!is.null(daily_hy) && nrow(daily_hy))
                    max(as.Date(daily_hy$Date), na.rm = TRUE) else as.Date("1800-01-01")

    prog <- shiny::Progress$new(session)
    on.exit(prog$close())
    prog$set(message = "Contacting WSC collaboration site…", value = 0)

    res <- tryCatch(
      get_realtime_daily(input$stn_id_input, hydat_last, param = "Discharge", progress = prog),
      error = function(e) {
        showNotification(paste("Realtime retrieval failed:", conditionMessage(e)),
                         type = "error", duration = 8); NULL
      }
    )
    if (is.null(res)) {
      showNotification(paste0("No provisional unit-value data available for ",
                              input$stn_id_input, " after its HYDAT record."),
                       type = "warning", duration = 8)
      rt_store(NULL)
    } else {
      rt_store(res)
      showNotification(sprintf("Loaded %d provisional day(s): %s to %s (%s).",
                               res$meta$n_days, res$meta$date_range[1], res$meta$date_range[2],
                               paste(res$meta$sources, collapse = " + ")),
                       type = "message", duration = 8)
    }
  })

  observeEvent(input$remove_realtime, {
    if (is.null(rt_store())) {
      showNotification("No realtime unit data loaded.", type = "message")
    } else {
      rt_store(NULL)
      reset_current_year()
      showNotification("Removed realtime unit data.", type = "message")
    }
  })

  output$realtime_status <- renderUI({
    s <- rt_store()
    if (is.null(s)) return(tags$span(style = "color:#888;", "No realtime unit data loaded."))
    m <- s$meta
    gen <- sprintf("corrected %s / raw %s",
                   ifelse(is.na(m$corr_gen), "–", m$corr_gen),
                   ifelse(is.na(m$raw_gen),  "–", m$raw_gen))
    if (!is.null(m$ws_gen) && !is.na(m$ws_gen))
      gen <- paste0(gen, " / web service ", m$ws_gen)
    tagList(
      tags$span(style = "color:#2b8a3e; font-weight:bold;",
                sprintf("Loaded: %d day(s), %s → %s", m$n_days,
                        format(m$date_range[1]), format(m$date_range[2]))),
      tags$br(),
      tags$span(style = "color:#666; font-size:90%;",
                sprintf("Source(s): %s · generated: %s",
                        paste(m$sources, collapse = " + "), gen)),
      if (isFALSE(m$archive_ok)) tagList(
        tags$br(),
        tags$span(style = "color:#b35900; font-size:90%;",
                  paste("The WSC collaboration site could not be reached, so only the realtime",
                        "web service (~18 months) was used. There may be a gap after the HYDAT record.")))
    )
  })

  # -3- Map Tab -----------------------------------------------

  # Load hydrologic zones (ensure WGS84)
  hydro_zones_sf <- tryCatch({
    hz <- sf::read_sf("./regional_analysis/HYDZ_HYDROLOGICZONE_SP.geojson")
    if (is.na(sf::st_crs(hz))) {
      hz <- sf::st_set_crs(hz, 4326)
    } else if (sf::st_crs(hz)$epsg != 4326) {
      hz <- sf::st_transform(hz, 4326)
    }
    hz
  }, error = function(e) NULL)

  # Base station pool: geographic bounds applied once
  map_data_geo <- map_data %>%
    tidyr::drop_na(LONGITUDE, LATITUDE) %>%
    dplyr::filter(dplyr::between(LONGITUDE, -142, -51),
                  dplyr::between(LATITUDE,   41,   84))

  # Reactive: apply user-specified filters to the station pool
  filtered_map_data <- reactive({
    data <- map_data_geo

    # Filter 0: station name contains search text (case-insensitive)
    if (!is.null(input$map_filter_name) && nchar(trimws(input$map_filter_name)) > 0) {
      data <- dplyr::filter(data, grepl(trimws(input$map_filter_name), STATION_NAME, ignore.case = TRUE))
    }

    # Filter 1: station record starts on or before this year
    if (!is.na(input$map_filter_year_from)) {
      data <- dplyr::filter(data, !is.na(Qfrom), Qfrom <= input$map_filter_year_from)
    }

    # Filter 2: station record extends to or beyond this year
    if (!is.na(input$map_filter_year_to)) {
      data <- dplyr::filter(data, !is.na(Qto), Qto >= input$map_filter_year_to)
    }

    # Filter 3: minimum years of daily flow record
    if (!is.na(input$map_filter_min_record)) {
      data <- dplyr::filter(data, !is.na(Qn), Qn >= input$map_filter_min_record)
    }

    # Filter 4a: minimum drainage area
    if (!is.na(input$map_filter_da_min)) {
      data <- dplyr::filter(data, !is.na(DRAINAGE_AREA_GROSS),
                            DRAINAGE_AREA_GROSS >= input$map_filter_da_min)
    }

    # Filter 4b: maximum drainage area
    if (!is.na(input$map_filter_da_max)) {
      data <- dplyr::filter(data, !is.na(DRAINAGE_AREA_GROSS),
                            DRAINAGE_AREA_GROSS <= input$map_filter_da_max)
    }

    # Filter 5: distance to a user-specified lat/lon
    if (!is.na(input$map_filter_lat) &&
        !is.na(input$map_filter_lon) &&
        !is.na(input$map_filter_dist_km) &&
        input$map_filter_dist_km > 0) {

      user_pt <- sf::st_sfc(
        sf::st_point(c(input$map_filter_lon, input$map_filter_lat)),
        crs = 4326
      )
      stns_sf  <- sf::st_as_sf(data, coords = c("LONGITUDE", "LATITUDE"),
                                crs = 4326, remove = FALSE)
      dist_m   <- as.numeric(sf::st_distance(stns_sf, user_pt))
      data     <- data[dist_m / 1000 <= input$map_filter_dist_km, ]
    }

    data
  })

  # Station count label
  output$map_filter_count <- renderText({
    n     <- nrow(filtered_map_data())
    total <- nrow(map_data_geo)
    paste0("Showing ", n, " of ", total, " stations")
  })

  # Reset button: clear all filter inputs
  observeEvent(input$map_filter_reset, {
    updateTextInput(session, "map_filter_name", value = "")
    updateNumericInput(session, "map_filter_year_from",  value = NA)
    updateNumericInput(session, "map_filter_year_to",    value = NA)
    updateNumericInput(session, "map_filter_min_record", value = NA)
    updateNumericInput(session, "map_filter_da_min",     value = NA)
    updateNumericInput(session, "map_filter_da_max",     value = NA)
    updateNumericInput(session, "map_filter_lat",        value = NA)
    updateNumericInput(session, "map_filter_lon",        value = NA)
    updateNumericInput(session, "map_filter_dist_km",    value = NA)
  })

  # Render full map (re-renders when filters change)
  # Current station's catchment (kept so it can be recoloured on base-map change)
  # and whether the active base map is one of the satellite layers.
  basin_store       <- reactiveVal(NULL)
  base_is_satellite <- reactiveVal(FALSE)

  output$MapPlot <- renderLeaflet({

    # Filtered station data with color categories
    d <- filtered_map_data() %>%
      dplyr::mutate(
        color_category = dplyr::case_when(
          Qn < 10              ~ "red",
          Qn >= 10 & Qn < 20  ~ "orange",
          Qn >= 20 & Qn < 40  ~ "yellow",
          Qn >= 40 & Qn < 60  ~ "green",
          Qn >= 60             ~ "blue",
          TRUE                 ~ "gray"
        ),
        marker_text = as.character(Qto)
      )

    # Open centered on the currently selected station (if it has coordinates),
    # otherwise the province-wide overview. isolate() so selecting a station does
    # not force a full map re-render; live re-centering is done via leafletProxy.
    # NOTE: use shiny::isolate explicitly — library(renv) masks shiny's isolate()
    # with renv::isolate(), which would copy the whole library out of the cache.
    sel_row <- map_data %>%
      dplyr::filter(STATION_NUMBER == shiny::isolate(input$stn_id_input)) %>%
      dplyr::slice(1)
    has_sel <- nrow(sel_row) == 1 && !is.na(sel_row$LONGITUDE) && !is.na(sel_row$LATITUDE)

    # Selectable base maps, shared with the Regionalization mini-map
    # (see add_base_layers() in functions.R).
    base <- leaflet::leaflet() %>% add_base_layers()
    base <- if (has_sel) {
      base %>% leaflet::setView(lng = sel_row$LONGITUDE, lat = sel_row$LATITUDE, zoom = 10)
    } else {
      base %>% leaflet::setView(lng = -122.7497, lat = 53.9171, zoom = 5)
    }

    # BC hydrologic zones are no longer drawn here — they now live on the
    # Regionalization sub-tab's mini-map, where they drive zone selection.

    # Selected station's catchment boundary + flow arrow (under the markers).
    # No-ops when the basin GeoPackages aren't present beside Hydat.sqlite3.
    if (has_sel) {
      base <- add_basin_layers(base, tryCatch(read_basin(sel_row$STATION_NUMBER),
                                              error = function(e) NULL),
                               satellite = shiny::isolate(base_is_satellite()))
    }

    # Station markers
    icons <- leaflet::awesomeIcons(
      icon        = "stats",
      iconColor   = "white",
      library     = "fa",
      markerColor = d$color_category,
      text        = d$marker_text
    )

    map_out <- base %>%
      leaflet::addAwesomeMarkers(
        data = d,
        ~LONGITUDE, ~LATITUDE,
        icon = icons,
        popup = ~paste0(
          text,
          '<br><button type="button" ',
          'onclick="Shiny.setInputValue(\'map_click_station\', \'', STATION_NUMBER, '\', {priority: \'event\'})">',
          'Select Station</button>'
        ),
        clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 60)
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        colors   = c("red", "orange", "yellow", "green", "blue", "gray"),
        labels   = c("< 10 yrs", "10\u201320 yrs", "20\u201340 yrs",
                     "40\u201360 yrs", "60+ yrs", "No flow record"),
        title    = paste("Daily Record Length",
                         "<small>Marker text = last year of record</small>",
                         sep = "<br/>"),
        opacity  = 1
      ) %>%
      addMeasure(
        position          = "topright",
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "meters",
        primaryAreaUnit   = "hectares",
        secondaryAreaUnit = "sqmeters",
        activeColor       = "#3D535D",
        completedColor    = "#7D4479"
      ) %>%
      # Base-map switcher (top-right).
      add_base_switcher()

    # Ring highlight around the selected station so it stands out among clusters
    if (has_sel) {
      map_out <- map_out %>%
        leaflet::addCircleMarkers(
          lng = sel_row$LONGITUDE, lat = sel_row$LATITUDE,
          radius = 14, color = "#d40000", weight = 3, fill = FALSE, opacity = 1,
          group = "selected_stn",
          label = paste("Selected:", sel_row$STATION_NUMBER),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "top")
        )
    }

    map_out
  })

  # Live: fly the map to the selected station and (re)draw its highlight ring.
  # Runs when the station changes while the map already exists; the initial view
  # is handled inside renderLeaflet above.
  observeEvent(input$stn_id_input, {
    req(input$stn_id_input)
    b <- tryCatch(read_basin(input$stn_id_input), error = function(e) NULL)
    basin_store(b)
    stn <- map_data %>%
      dplyr::filter(STATION_NUMBER == input$stn_id_input) %>%
      dplyr::slice(1)
    if (nrow(stn) == 1 && !is.na(stn$LONGITUDE) && !is.na(stn$LATITUDE)) {
      prox <- leaflet::leafletProxy("MapPlot") %>%
        leaflet::clearGroup("selected_stn") %>%
        leaflet::clearGroup("catchment") %>%
        leaflet::flyTo(lng = stn$LONGITUDE, lat = stn$LATITUDE, zoom = 11) %>%
        leaflet::addCircleMarkers(
          lng = stn$LONGITUDE, lat = stn$LATITUDE,
          radius = 14, color = "#d40000", weight = 3, fill = FALSE, opacity = 1,
          group = "selected_stn",
          label = paste("Selected:", stn$STATION_NUMBER),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "top")
        )
      add_basin_layers(prox, b, satellite = base_is_satellite())  # drawn when gpkg present
    }
  }, ignoreInit = TRUE)

  # Recolour the catchment when the user switches base maps: yellow over the two
  # satellite layers, blue otherwise. `input$MapPlot_groups` lists the visible
  # groups (fires on base changes AND our own catchment add) — only redraw when
  # the satellite state actually flips, to avoid a redraw loop.
  observeEvent(input$MapPlot_groups, {
    sat <- any(c("Esri Imagery", "Sentinel-2 Satellite") %in% input$MapPlot_groups)
    if (identical(sat, base_is_satellite())) return()
    base_is_satellite(sat)
    b <- basin_store()
    if (!is.null(b)) {
      leaflet::leafletProxy("MapPlot") %>%
        leaflet::clearGroup("catchment") %>%
        add_basin_layers(b, satellite = sat)
    }
  }, ignoreInit = TRUE)

  # ---- Watershed polygon export (sidebar) -----------------------------------
  # Availability piggybacks on basin_store(), which the map observer above has
  # already filled for the current station, so nothing extra is read until the
  # user actually clicks a download button.
  output$basin_available <- reactive({ !is.null(basin_store()) })
  outputOptions(output, "basin_available", suspendWhenHidden = FALSE)

  output$basin_export_info <- renderText({
    id <- input$stn_id_input
    if (is.null(id) || !nzchar(id)) return("")
    b <- basin_store()
    if (is.null(b))
      return("<span style='color:#a00'>No WSC basin polygon available for this station.</span>")
    paste0("Catchment boundary",
           if (!is.na(b$area_km2)) paste0(" (", format(b$area_km2, big.mark = ","), " km<sup>2</sup>)") else "",
           " from the WSC National Hydrometric Network Basin Polygons.")
  })

  # The selected station's HYDAT row, for the export attribute tables.
  basin_export_stn <- reactive({
    id <- input$stn_id_input
    if (is.null(id) || !nzchar(id)) return(NULL)
    stn <- map_data %>% dplyr::filter(STATION_NUMBER == id) %>% dplyr::slice(1)
    if (nrow(stn) == 1) stn else NULL
  })

  # Full-resolution polygon with a tidy attribute table. Deliberately NOT the
  # simplified geometry drawn on the map.
  basin_export <- reactive({
    id <- input$stn_id_input
    if (is.null(id) || !nzchar(id)) return(NULL)
    raw <- tryCatch(read_basin_raw(id), error = function(e) NULL)
    if (is.null(raw)) return(NULL)
    stn <- basin_export_stn()
    basin_export_sf(
      raw, id,
      station_name = if (!is.null(stn)) stn$STATION_NAME        else NA_character_,
      hydat_da_km2 = if (!is.null(stn)) stn$DRAINAGE_AREA_GROSS else NA_real_)
  })

  # Gauge location shipped alongside the boundary (NULL if it can't be located).
  basin_station_export <- reactive({
    id <- input$stn_id_input
    if (is.null(id) || !nzchar(id)) return(NULL)
    stn <- basin_export_stn()
    tryCatch(basin_station_export_sf(
      id,
      station_name = if (!is.null(stn)) stn$STATION_NAME else NA_character_,
      lat          = if (!is.null(stn)) stn$LATITUDE     else NA_real_,
      lon          = if (!is.null(stn)) stn$LONGITUDE    else NA_real_),
      error = function(e) NULL)
  })

  output$download_basin_shp <- downloadHandler(
    filename    = function() paste0(input$stn_id_input, "_watershed_shp.zip"),
    contentType = "application/zip",
    content     = function(file) {
      b <- basin_export()
      validate(need(!is.null(b), "No basin polygon available for this station."))
      write_basin_shp_zip(b, basin_station_export(), input$stn_id_input, file)
    }
  )

  output$download_basin_kml <- downloadHandler(
    filename    = function() paste0(input$stn_id_input, "_watershed.kml"),
    contentType = "application/vnd.google-earth.kml+xml",
    content     = function(file) {
      b <- basin_export()
      validate(need(!is.null(b), "No basin polygon available for this station."))
      write_basin_kml(b, basin_station_export(), input$stn_id_input, file)
    }
  )

  # Update station selector when "Select Station" button clicked in popup.
  # Must use updateSelectizeInput(server = TRUE) — the ID box is a server-side
  # selectize, so updateSelectInput() can't select a value that isn't already in
  # the client's loaded option set (it silently no-ops -> "Invalid Station ID").
  observeEvent(input$map_click_station, {
    req(input$map_click_station)
    updateSelectizeInput(session, "stn_id_input",
                         choices = map_data$STATION_NUMBER,
                         selected = input$map_click_station,
                         server = TRUE)
  })
  
  # -4- Data Summary Tab ---------------------------------------
  
  # the summarized data are needed in both table rendering and download button
  summarized <- reactive({

    validate(
      need(input$stn_id_input, "Select a WSC station ID (top-left) to begin."))

    base <- Dataset_Summarize(station_number = input$stn_id_input,
                              summary_reso = input$sum_period,
                              wy_month = as.numeric(input$wy_start))
    base$Source <- "HYDAT"

    # Append provisional realtime unit data (complete days only) when retrieved.
    rt <- realtime_daily()
    if (!is.null(rt)) {
      rt_sum <- Dataset_Summarize_realtime(rt, input$sum_period, as.numeric(input$wy_start))
      if (!is.null(rt_sum)) {
        # HYDAT monthly/yearly summaries have no Flag column; add one so binding aligns.
        if (!"Flag" %in% names(base)) base$Flag <- NA_character_
        base <- dplyr::bind_rows(base, rt_sum)
      }
    }

    base # return data

  })
  
  output$table <- DT::renderDataTable({
    
    spin_table$show() #show spinner
    
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
    
    spin_tsgraph$show() # show spinner
    
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

    # --- Provisional realtime unit data (daily average), split by grade flag ---
    rt_series_names <- character(0)
    rt <- realtime_daily()
    if (!is.null(rt) && nrow(rt)) {
      rt_wide <- rt %>%
        dplyr::mutate(Series = paste0("Realtime: ", Flag)) %>%
        dplyr::select(Date, Flow, Series) %>%
        tidyr::pivot_wider(names_from = Series, values_from = Flow)
      rt_dates <- as.POSIXct(rt_wide$Date, format = "%Y-%m-%d")
      rt_xts   <- xts::xts(as.matrix(rt_wide[, -1, drop = FALSE]), order.by = rt_dates)
      rt_series_names <- colnames(rt_xts)
      xts_Format <- cbind(xts_Format, rt_xts)
      # cbind.xts mangles names ("No Code" -> "No.Code"); restore the intended ones
      colnames(xts_Format) <- c(flag_names, rt_series_names)
    }

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

    # Style each provisional realtime grade series with its own flag colour
    for (nm in rt_series_names) {
      flag <- sub("^Realtime: ", "", nm)
      col  <- unname(UV_GRADE_COLORS[flag]); if (is.na(col)) col <- "#FF7F00"
      dy_plots <- dy_plots %>%
        dygraphs::dySeries(nm, drawPoints = TRUE, pointSize = 2, strokeWidth = 1.2, color = col)
    }

    # call to plot
    dy_plots
    
  }) # End of interactive graph
  
  
  # -6- Hydrograph Tab (average / regime hydrograph, interactive) ------------

  # Populate the period-split choices with the water years available for the
  # current station (a split at year Y starts a new period at Y, so the first
  # year is not offered). Preserve any still-valid selections.
  observeEvent(list(input$stn_id_input, input$wy_start), {
    daily <- tryCatch(Qdaily(), error = function(e) NULL)
    req(!is.null(daily) && nrow(daily) > 0)
    yrs <- sort(unique(hydro_yr(daily$Date, as.numeric(input$wy_start))))
    choices <- if (length(yrs) > 1) yrs[-1] else numeric(0)
    keep <- shiny::isolate(input$hydro_splits)
    keep <- keep[keep %in% as.character(choices)]
    updateSelectizeInput(session, "hydro_splits", choices = choices, selected = keep)

    # Trim bounds: any available water year (empty = no trimming). Preserve a
    # still-valid selection so switching band/water-year doesn't reset it.
    keep_min <- shiny::isolate(input$hydro_year_min)
    keep_min <- keep_min[keep_min %in% as.character(yrs)]
    keep_max <- shiny::isolate(input$hydro_year_max)
    keep_max <- keep_max[keep_max %in% as.character(yrs)]
    updateSelectizeInput(session, "hydro_year_min", choices = yrs, selected = keep_min)
    updateSelectizeInput(session, "hydro_year_max", choices = yrs, selected = keep_max)
  })

  # Per-period, per-day-of-water-year statistics for the plot
  hydro_data <- reactive({
    req(input$stn_id_input)
    band   <- if (is.null(input$hydro_band)) c(10, 90) else input$hydro_band
    wy_m   <- as.numeric(input$wy_start)
    daily  <- Qdaily()

    # Trim to the chosen water-year window (drop sparse early/late years).
    # Empty selectize -> character(0) -> treat as no bound.
    scalar_year <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (length(x) == 1L && is.finite(x)) x else NA_real_
    }
    ymin <- scalar_year(input$hydro_year_min)
    ymax <- scalar_year(input$hydro_year_max)
    if (!is.na(ymin) || !is.na(ymax)) {
      wy   <- hydro_yr(daily$Date, wy_m)
      keep <- rep(TRUE, length(wy))
      if (!is.na(ymin)) keep <- keep & wy >= ymin
      if (!is.na(ymax)) keep <- keep & wy <= ymax
      daily <- daily[keep, , drop = FALSE]
    }
    validate(need(nrow(daily) > 0, "No daily flow data in the selected water-year range."))

    splits <- suppressWarnings(as.numeric(input$hydro_splits))
    hs <- hydrograph_stats(
      flow_df  = daily,
      wy_month = wy_m,
      splits   = splits[is.finite(splits)],
      probs    = c(band[1], band[2]) / 100
    )
    validate(need(!is.null(hs) && nrow(hs$stats) > 0, "No daily flow data to plot."))
    hs
  })

  # ---- "Plot Current Year" overlay (most recent 365 days) -------------------
  observeEvent(input$hydro_current_year, {
    new_state <- !show_cur_year()
    show_cur_year(new_state)
    updateActionButton(session, "hydro_current_year",
                       label = if (new_state) "Hide Current Year" else "Plot Current Year")
    if (new_state && is.null(rt_store())) {
      showNotification(paste("Retrieve realtime unit data (Explore Data tab) so the current",
                             "year shows recent flows beyond the HYDAT record."),
                       type = "warning", duration = 7)
    }
  })

  # Daily flow for the most recent 365 days: provisional realtime data, plus any
  # HYDAT day that falls in the window (the two never overlap by date). The
  # window is anchored to the LAST available day (not today) so there is no empty
  # tail, which would otherwise draw a straight line across the missing days.
  current_year_daily <- reactive({
    parts <- list()
    hy <- tryCatch(Qdaily(), error = function(e) NULL)
    if (!is.null(hy) && nrow(hy)) {
      parts[[length(parts) + 1L]] <- hy %>%
        dplyr::transmute(Date = as.Date(Date), Flow) %>% dplyr::filter(!is.na(Flow))
    }
    rt <- realtime_daily()
    if (!is.null(rt) && nrow(rt)) {
      parts[[length(parts) + 1L]] <- rt %>%
        dplyr::transmute(Date = as.Date(Date), Flow) %>% dplyr::filter(!is.na(Flow))
    }
    cy <- dplyr::bind_rows(parts)
    if (!nrow(cy)) return(NULL)
    cy   <- dplyr::distinct(cy, Date, .keep_all = TRUE)
    last <- max(cy$Date); from <- last - 365
    cy %>% dplyr::filter(Date >= from, Date <= last) %>% dplyr::arrange(Date)
  })

  output$hydrograph <- plotly::renderPlotly({
    hs    <- hydro_data()
    stats <- hs$stats; mad <- hs$mad; lv <- hs$levels
    pal   <- hydro_period_palette[((seq_along(lv) - 1L) %% length(hydro_period_palette)) + 1L]
    names(pal) <- lv
    # X-axis spans the FULL water year (from wy_start), not just the days that
    # happen to have data. Deriving it from the data range truncates the axis for
    # seasonal / gappy stations (e.g. a January water year missing December, which
    # also clips the current-year overlay's January flows).
    wy_m  <- as.integer(input$wy_start)
    if (length(wy_m) != 1L || is.na(wy_m)) wy_m <- 1L
    x_lo  <- lubridate::make_date(2000L, wy_m, 1L)
    x_rng <- c(x_lo, x_lo %m+% lubridate::years(1L) - 1L)

    band_lab <- sprintf("Shaded band: %s–%s percentile",
                        ordinal_pct(hs$probs[1] * 100), ordinal_pct(hs$probs[2] * 100))

    p <- plotly::plot_ly()
    for (L in lv) {
      col <- pal[[L]]
      d   <- dplyr::filter(stats, Period == L)

      # Shaded percentile band (upper first, then lower filled to the upper)
      if (isTRUE(input$hydro_show_band)) {
        p <- p %>%
          plotly::add_trace(data = d, x = ~PlotDate, y = ~Upper, type = "scatter",
                            mode = "lines", line = list(width = 0),
                            legendgroup = L, showlegend = FALSE, hoverinfo = "skip",
                            name = paste0(L, " band")) %>%
          plotly::add_trace(data = d, x = ~PlotDate, y = ~Lower, type = "scatter",
                            mode = "lines", fill = "tonexty",
                            fillcolor = hex_to_rgba(col, 0.18), line = list(width = 0),
                            legendgroup = L, showlegend = FALSE, hoverinfo = "skip",
                            name = paste0(L, " band"))
      }

      # Mean daily hydrograph line
      nyr     <- mad$n_years[mad$Period == L]
      leg_lab <- sprintf("%s (%d complete year%s)", L, nyr, if (nyr == 1L) "" else "s")
      p <- p %>%
        plotly::add_trace(data = d, x = ~PlotDate, y = ~Mean, type = "scatter",
                          mode = "lines", line = list(color = col, width = 2.2),
                          legendgroup = L, name = leg_lab,
                          hovertemplate = paste0("<b>", L, "</b><br>%{x|%b %d}",
                                                 "<br>Mean: %{y:.1f} m³/s<extra></extra>"))

      # MAD (mean annual discharge) dashed line
      if (isTRUE(input$hydro_show_mad)) {
        madv <- mad$MAD[mad$Period == L]
        p <- p %>%
          plotly::add_trace(x = x_rng, y = c(madv, madv), type = "scatter",
                            mode = "lines", line = list(color = col, width = 1.4, dash = "dash"),
                            legendgroup = L, showlegend = FALSE, hoverinfo = "skip")
      }
    }

    # Legend key for the shading: one neutral swatch naming the user-chosen
    # percentiles (the per-period bands themselves stay out of the legend so
    # each period keeps a single toggle).
    if (isTRUE(input$hydro_show_band)) {
      # Drawn just outside the fixed x range so only the legend swatch shows
      # (an all-NA trace loses its name and fill colour in plotly_build).
      off_x <- rep(x_rng[1] - 30, 2)
      p <- p %>%
        plotly::add_trace(x = off_x, y = c(0, 0), type = "scatter", mode = "lines",
                          fill = "toself", fillcolor = "rgba(110,110,110,0.30)",
                          line = list(width = 0), name = band_lab,
                          hoverinfo = "skip", showlegend = TRUE)
    }

    # Optional current/previous-year overlay (most recent 365 days). Split by
    # WATER YEAR (not calendar year) so the wrap-around matches the water-year
    # x-axis: the latest water year in black, the one before it in grey. Using
    # the calendar year would wrongly split the current water year in two when
    # the water year does not start in January.
    if (isTRUE(show_cur_year())) {
      cur <- current_year_daily()
      if (!is.null(cur) && nrow(cur) > 0) {
        wy_m <- as.numeric(input$wy_start)
        cur <- cur %>%
          dplyr::mutate(Yr = hydro_yr(Date, wy_m),
                        PlotDate = plot_doy_date(Date, wy_m))
        y_cur <- max(cur$Yr); y_prev <- y_cur - 1L
        add_year_line <- function(p, yr, lab, col) {
          d <- cur %>% dplyr::filter(Yr == yr) %>% dplyr::arrange(PlotDate)
          if (!nrow(d)) return(p)
          plotly::add_trace(p, data = d, x = ~PlotDate, y = ~Flow, type = "scatter",
            mode = "lines", line = list(color = col, width = 2.4),
            name = lab, legendgroup = lab,
            hovertemplate = paste0("<b>", lab, "</b><br>%{x|%b %d}",
                                   "<br>%{y:.1f} m³/s<extra></extra>"))
        }
        # previous water year first (grey, underneath), then current (black, on top)
        p <- add_year_line(p, y_prev, sprintf("Previous Water Year (%d)", y_prev), "#8C8C8C")
        p <- add_year_line(p, y_cur,  sprintf("Current Water Year (%d)",  y_cur),  "#111111")
      }
    }

    # MAD value labels. With <= 2 periods, place each label on its own line
    # (alternate above/below so near-equal MADs don't overlap). With > 2
    # periods the on-line labels collide, so consolidate them into a single
    # colour-coded box anchored 15% above the highest MAD line, which keeps it
    # clear of the legend block in the top-left corner.
    annos <- list()
    if (isTRUE(input$hydro_show_mad)) {
      if (length(lv) <= 2L) {
        annos <- lapply(seq_along(lv), function(i) {
          L <- lv[i]; madv <- mad$MAD[mad$Period == L]
          above <- (i %% 2L) == 1L
          list(x = x_rng[1] + 5, y = madv, xref = "x", yref = "y",
               text = sprintf("MAD (%s) = %.1f m³/s", L, madv),
               showarrow = FALSE, xanchor = "left",
               yanchor = if (above) "bottom" else "top",
               font = list(color = pal[[L]], size = 11),
               bgcolor = "rgba(255,255,255,0.55)")
        })
      } else {
        rows <- vapply(lv, function(L) {
          madv <- mad$MAD[mad$Period == L]
          sprintf('<span style="color:%s">%s = %.1f</span>', pal[[L]], L, madv)
        }, character(1))
        box_text <- paste0("<b>MAD (m³/s)</b><br>", paste(rows, collapse = "<br>"))
        annos <- list(list(
          x = 0.02, y = max(mad$MAD) * 1.15, xref = "paper", yref = "y",
          text = box_text, showarrow = FALSE, align = "left",
          xanchor = "left", yanchor = "bottom", font = list(size = 11, color = "#000000"),
          bgcolor = "rgba(255,255,255,0.6)",
          bordercolor = "rgba(0,0,0,0.15)", borderwidth = 1))
      }
    }

    ttl <- paste0(input$stn_id_input, " average hydrograph",
                  if (length(lv) > 1) paste0(" — ", length(lv), " periods") else "")

    ink       <- "#000000"
    axis_ttl  <- list(size = 13, color = ink)
    axis_tick <- list(size = 12, color = ink)

    p %>% plotly::layout(
      title = list(text = ttl, x = 0.03, font = list(size = 15, color = ink)),
      font  = list(color = ink),
      xaxis = list(title = list(text = "Month", font = axis_ttl),
                   tickfont = axis_tick, type = "date", tickformat = "%b", dtick = "M1",
                   range = c(format(x_rng[1]), format(x_rng[2]))),
      yaxis = list(title = list(text = "<b>Mean Daily Discharge (m³/s)</b>", font = axis_ttl),
                   tickfont = axis_tick, rangemode = "tozero", zeroline = FALSE),
      hovermode = "x unified",
      annotations = annos,
      legend = list(x = 0.02, y = 0.98, xanchor = "left", yanchor = "top",
                    font = list(size = 12, color = ink),
                    bgcolor = "rgba(255,255,255,0.6)",
                    bordercolor = "rgba(0,0,0,0.15)", borderwidth = 1)
    )
  })
  
  
  # -8- Qdaily Frequency Analysis Tab -------------------------

  
  # Only keep complete years base on threshold selector
  Qdaily_years <- reactive({

    complete_years <- Qdaily() %>%
      mutate(Year = hydro_yr(Date, as.numeric(input$wy_start))) %>%
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
      need(input$stn_id_input, "Select a WSC station ID (top-left) to begin."))
    
    validate(
      need(length(Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]) > 2,
           "[Insufficient Amount of Complete Year]"))
    
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
      need(length(Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]) > 2,
           "[Insufficient Amount of Complete Year]"))
    
    validate(
      need(length(input$months_Qdaily)>0, "Select at least 1 month")
    )
    
    empirical_ffa(flow_df = Qdaily(), 
                  complete_years = complete_years_Qdaily(), 
                  selected_months = selected_months_num(),
                  wy_month = as.numeric(input$wy_start)) %>%
     
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
      {if(as.numeric(input$wy_start)==1) scale_x_continuous(name = "Year", limits=c(NA, NA))} +
      {if(as.numeric(input$wy_start)!=1) scale_x_continuous(name = "Water Year", limits=c(NA, NA))} +
      ggtitle(paste0(input$stn_id_input, " Daily Flow"))
    
    ggplotly(daily_ams_plot)
  })
  
  
  # Empirical frequency table
  output$daily_ams_table <- DT::renderDataTable({
    
    formated_df <- empirical_daily() %>%
      select(ID, Year, Date, `Peak Discharge (cms)`, Rank, `Return Period`, SYM) 
    
    if (as.numeric(input$wy_start) != 1) {
      formated_df <- formated_df %>% rename(`WaterYear` = Year)
    }
    
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
      need(length(Qdaily_years()[!(Qdaily_years() %in% input$remove_year_Qdaily)]) > 2,
           "[Insufficient Amount of Complete Year]"))
    
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
      need(input$stn_id_input, "Select a WSC station ID (top-left) to begin."))
    
    validate(
      need(length(Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)]) > 2,
           "[Insufficient Amount of Complete Year]"))
    
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
      need(length(Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)]) > 2,
           "[Insufficient Amount of Sample Data]"))
    

    empirical_ffa(flow_df = Qinst(), 
                  complete_years = selected_years_Qinst(), 
                  selected_months = seq(1:12),
                  #do not use water year in Qinst!!
                  wy_month = 1) %>%
      
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
      need(length(Qinst_years()[!(Qinst_years() %in% input$remove_year_Qinst)]) > 2,
           "[Insufficient Amount of Sample Data]"))
    

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
  
# -4- Regional Analysis Tab ---------------------------------------

# Subtab 1: Regionalization - Stream Gauge Selection
  
  # Pre-load BC Hydrologic Zones catalog (WSC IDs per zone)
  bc_zone_catalog <- tryCatch({
    read.csv("./regional_analysis/WSC_Station_ID_in_BC_HydrologicZones.csv",
             stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) NULL)

  # Stations committed by the automated (Option A) location tool; used as a
  # fallback in regional_stations() when the server-side selectize does not echo
  # its programmatic selection back to input$bc_zone_station_ids.
  roi_applied_ids <- reactiveVal(character(0))
  
  # Reactively filter by selected zone(s); empty selection returns all
  bc_zone_stations <- reactive({
    req(!is.null(bc_zone_catalog))
    zones <- input$bc_zone_selector
    if (is.null(zones) || length(zones) == 0) return(bc_zone_catalog)
    dplyr::filter(bc_zone_catalog, as.character(Zone) %in% as.character(zones))
  })

  # Initialize zone station picker with ALL station IDs (no selection by default)
  all_station_ids <- map_data$STATION_NUMBER  # from cached RDS (see functions.R)
  updateSelectizeInput(session, "bc_zone_station_ids",
                       choices = all_station_ids, selected = NULL, server = TRUE)

  # When zones are added, add their WSC IDs to the current selection
  observeEvent(input$bc_zone_selector, {
    zones <- input$bc_zone_selector
    if (!is.null(zones) && length(zones) > 0 && !is.null(bc_zone_catalog)) {
      allowed_ids <- dplyr::filter(bc_zone_catalog, as.character(Zone) %in% as.character(zones))$WSC_ID %>%
        unique() %>% sort()
  # Select exactly the IDs belonging to the currently selected zones
  updateSelectizeInput(session, "bc_zone_station_ids",
           choices = all_station_ids, selected = allowed_ids, server = TRUE)
    } else {
  # No zones selected: keep choices as all, clear selection
  updateSelectizeInput(session, "bc_zone_station_ids",
           choices = all_station_ids, selected = character(0), server = TRUE)
    }
  })

  # Resolve the working station set: the manual station selection if any,
  # otherwise the stations implied by the selected BC zones. Used by the counter,
  # the Envelope Curve, and the Qinst/Qdaily Ratio tab so they always agree.
  # (When zones populate the station box via updateSelectizeInput(server=TRUE),
  # the client shows the chips but input$bc_zone_station_ids may not sync back,
  # so we fall back to the zone-derived list.)
  regional_stations <- reactive({
    stations <- input$bc_zone_station_ids
    if (is.null(stations) || length(stations) == 0) {
      zones <- input$bc_zone_selector
      if (!is.null(zones) && length(zones) > 0 && !is.null(bc_zone_catalog)) {
        stations <- bc_zone_catalog %>%
          dplyr::filter(as.character(Zone) %in% as.character(zones)) %>%
          dplyr::pull(WSC_ID) %>% unique() %>% sort()
      } else if (length(roi_applied_ids()) > 0) {
        # Automated Option A selection (see roi_apply observer below)
        stations <- roi_applied_ids()
      } else {
        stations <- character(0)
      }
    }
    stations
  })

  # Live count of stations currently selected for regional analysis
  output$regional_selection_count <- renderText({
    n <- length(regional_stations())
    if (n == 0) {
      "No stations selected yet — pick a BC zone above, or type WSC IDs into the box."
    } else {
      paste0(n, " station(s) currently selected for regional analysis.")
    }
  })

  # Clear-all button: reset both the zone picker and the station list
  observeEvent(input$clear_regional, {
    roi_applied_ids(character(0))
    updateSelectizeInput(session, "bc_zone_selector", selected = character(0))
    updateSelectizeInput(session, "bc_zone_station_ids",
                         choices = all_station_ids, selected = character(0), server = TRUE)
  })

  # ---- Option A: automated selection by location (ROI / BC zones) ----------
  # Donor pool for the outside-BC ROI (built once; NULL if descriptor rds absent)
  roi_pool <- tryCatch(build_roi_donor_pool(map_data, roi_load_descriptors()),
                       error = function(e) NULL)
  # National boundary to keep target selection within Canada
  canada_sf <- tryCatch(roi_load_canada(), error = function(e) NULL)
  # Coordinates of the BCSI catalog stations (for distance-based BC pooling)
  bc_catalog_coords <- tryCatch({
    if (is.null(bc_zone_catalog)) NULL else
      bc_zone_catalog %>%
        dplyr::distinct(WSC_ID) %>%
        dplyr::inner_join(map_data %>%
                            dplyr::select(WSC_ID = STATION_NUMBER, LONGITUDE, LATITUDE),
                          by = "WSC_ID")
  }, error = function(e) NULL)

  # Confirmed target location (set by map click or the "Find region" button)
  roi_pt <- reactiveVal(NULL)

  # Base mini-map: same selectable base maps as the Stations Map, plus the BC
  # hydrologic zones, centred on western Canada.
  output$roi_map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet() %>%
      add_base_layers() %>%
      add_base_switcher() %>%
      leaflet::setView(lng = -118, lat = 55, zoom = 4)
    if (!is.null(hydro_zones_sf)) {
      m <- m %>% leaflet::addPolygons(
        data = hydro_zones_sf, group = "zones_all",
        stroke = TRUE, weight = 1, color = "#1f78b4",
        fill = TRUE, fillOpacity = 0.05, fillColor = "#1f78b4",
        label = ~paste0("Zone ", HYDROLOGICZONE_NO, " — ", HYDROLOGICZONE_NAME))

      # Zone number labels (placed at an interior point of each zone)
      zpts <- tryCatch({
        hz_proj  <- sf::st_transform(hydro_zones_sf, 3978)
        pts_ll   <- sf::st_transform(
          sf::st_as_sf(sf::st_point_on_surface(sf::st_geometry(hz_proj))), 4326)
        cbind(as.data.frame(sf::st_coordinates(pts_ll)),
              label = as.character(hydro_zones_sf$HYDROLOGICZONE_NO))
      }, error = function(e) NULL)
      if (!is.null(zpts)) {
        m <- m %>% leaflet::addLabelOnlyMarkers(
          data = zpts, lng = ~X, lat = ~Y, label = ~label,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE, direction = "center", textOnly = TRUE,
            style = list("color" = "#1f78b4", "font-weight" = "bold",
                         "font-size" = "11px", "background" = "rgba(255,255,255,0.6)",
                         "border-radius" = "4px", "padding" = "1px 3px")))
      }
    }
    m
  })

  # Map click drops the target pin and mirrors coords into the numeric inputs
  observeEvent(input$roi_map_click, {
    cl <- input$roi_map_click
    roi_pt(list(lon = cl$lng, lat = cl$lat))
    updateNumericInput(session, "roi_lat", value = round(cl$lat, 4))
    updateNumericInput(session, "roi_lon", value = round(cl$lng, 4))
  })

  # "Find region" button reads the typed coordinates
  observeEvent(input$roi_locate, {
    lat <- suppressWarnings(as.numeric(input$roi_lat))
    lon <- suppressWarnings(as.numeric(input$roi_lon))
    if (is.na(lat) || is.na(lon)) return()
    roi_pt(list(lon = lon, lat = lat))
    leaflet::leafletProxy("roi_map") %>%
      leaflet::flyTo(lng = lon, lat = lat, zoom = 6)
  })

  # Core live preview: BC zones or ROI donors for the current point + parameters.
  # Depends on every parameter input so the map updates live.
  roi_preview <- reactive({
    pt <- roi_pt()
    if (is.null(pt)) return(NULL)
    bc_dist <- if (is.null(input$roi_bc_dist)) 0 else input$roi_bc_dist
    sp <- bc_station_pool(pt$lon, pt$lat, hydro_zones_sf, bc_zone_catalog,
                          bc_catalog_coords, dist_km = bc_dist)
    if (isTRUE(sp$in_bc)) {
      selstn <- if (!is.null(bc_catalog_coords))
        dplyr::filter(bc_catalog_coords, WSC_ID %in% sp$station_ids) else NULL
      list(mode = "BC", lon = pt$lon, lat = pt$lat,
           enclosing = sp$enclosing, station_ids = sp$station_ids,
           bc_dist = bc_dist, bc_stations = selstn,
           n_enclosing = length(sp$enclosing_ids), n_nearby = length(sp$nearby_ids),
           nearby_zones = sp$nearby_zones,
           zones = if (!is.na(sp$enclosing)) sp$enclosing else integer(0),
           donors = NULL)
    } else if (identical(point_in_canada(pt$lon, pt$lat, canada_sf), FALSE)) {
      # Outside BC and outside Canada -> not a valid target
      list(mode = "OUTSIDE", lon = pt$lon, lat = pt$lat, zones = integer(0),
           station_ids = character(0), donors = NULL)
    } else {
      if (is.null(roi_pool)) {
        return(list(mode = "ROI", lon = pt$lon, lat = pt$lat, zones = integer(0),
                    station_ids = character(0), donors = NULL, no_data = TRUE))
      }
      descs <- input$roi_descriptors
      if (is.null(descs) || !length(descs)) descs <- ROI_DEFAULT_DESCRIPTORS
      donors <- build_roi_pool(
        target = list(lon = pt$lon, lat = pt$lat, da_km2 = input$roi_target_da),
        pool = roi_pool, descriptors = descs,
        m1 = if (is.null(input$roi_m1)) 100 else input$roi_m1,
        m2 = if (is.null(input$roi_m2)) 25 else input$roi_m2,
        min_years = if (is.null(input$roi_min_years)) 20 else input$roi_min_years)
      list(mode = "ROI", lon = pt$lon, lat = pt$lat, zones = integer(0),
           station_ids = if (is.null(donors)) character(0) else donors$STATION_NUMBER,
           donors = donors)
    }
  })

  # Mode string for the conditionalPanels ("BC" / "ROI" / "")
  output$roi_mode <- renderText({
    pv <- roi_preview(); if (is.null(pv)) "" else pv$mode
  })
  outputOptions(output, "roi_mode", suspendWhenHidden = FALSE)

  output$roi_mode_label <- renderText({
    pv <- roi_preview()
    if (is.null(pv)) "Pick a location to begin."
    else if (pv$mode == "BC") "In BC → hydrologic zones"
    else if (pv$mode == "OUTSIDE") "⚠ Outside Canada — pick a Canadian location"
    else "Outside BC → ROI donor group"
  })

  # Live map update: target pin + selected zones (BC) or donor gauges (ROI)
  observe({
    pv <- roi_preview()
    prox <- leaflet::leafletProxy("roi_map")
    prox %>% leaflet::clearGroup("roi_target") %>%
             leaflet::clearGroup("roi_zones")  %>%
             leaflet::clearGroup("roi_donors")
    if (is.null(pv)) return()
    prox %>% leaflet::addMarkers(lng = pv$lon, lat = pv$lat, group = "roi_target",
                                 label = "Target site")
    if (pv$mode == "BC") {
      # Highlight the enclosing zone
      if (!is.na(pv$enclosing) && !is.null(hydro_zones_sf)) {
        sel <- hydro_zones_sf[as.integer(hydro_zones_sf$HYDROLOGICZONE_NO) == pv$enclosing, ]
        prox %>% leaflet::addPolygons(
          data = sel, group = "roi_zones",
          stroke = TRUE, weight = 2, color = "#e31a1c",
          fill = TRUE, fillOpacity = 0.18, fillColor = "#fb9a99",
          label = ~paste0("Enclosing zone ", HYDROLOGICZONE_NO, " — ", HYDROLOGICZONE_NAME))
      }
      # Distance buffer used to pull in neighbouring BCSI stations
      if (!is.null(pv$bc_dist) && pv$bc_dist > 0) {
        prox %>% leaflet::addCircles(
          lng = pv$lon, lat = pv$lat, radius = pv$bc_dist * 1000, group = "roi_zones",
          stroke = TRUE, weight = 1, color = "#e31a1c", dashArray = "4",
          fill = FALSE, label = sprintf("%g km", pv$bc_dist))
      }
      # Selected BCSI stations (enclosing zone + nearby neighbours)
      if (!is.null(pv$bc_stations) && nrow(pv$bc_stations)) {
        prox %>% leaflet::addCircleMarkers(
          data = pv$bc_stations, lng = ~LONGITUDE, lat = ~LATITUDE, group = "roi_zones",
          radius = 4, weight = 1, color = "#b2182b", fillColor = "#ef8a62",
          fillOpacity = 0.85, label = ~WSC_ID)
      }
    } else if (pv$mode == "ROI" && !is.null(pv$donors) && nrow(pv$donors)) {
      d <- pv$donors
      prox %>% leaflet::addCircleMarkers(
        data = d, lng = ~LONGITUDE, lat = ~LATITUDE, group = "roi_donors",
        radius = 5, weight = 1, color = "#b2182b", fillColor = "#ef8a62",
        fillOpacity = 0.85,
        label = ~paste0(STATION_NUMBER, " (rank ", rank, ", ",
                        round(GEO_dist_km), " km)"))
    }
  })

  # Live textual summary of the current automated selection
  output$roi_result_summary <- renderText({
    pv <- roi_preview()
    if (is.null(pv)) return("No location selected yet.")
    if (identical(pv$mode, "OUTSIDE"))
      return("That point is outside Canada. Regionalization is only available for Canadian locations — pick a point within Canada.")
    if (isTRUE(pv$no_data))
      return("ROI descriptor data not found on this server (hysets_descriptors.rds).")
    n <- length(pv$station_ids)
    if (pv$mode == "BC") {
      encl <- if (is.na(pv$enclosing)) "—" else pv$enclosing
      base_txt <- sprintf("BC: enclosing zone %s (%d BCSI station%s)",
                          encl, pv$n_enclosing, if (pv$n_enclosing == 1) "" else "s")
      if (!is.null(pv$bc_dist) && pv$bc_dist > 0) {
        zt <- if (length(pv$nearby_zones))
          paste0(" from zone(s) ", paste(pv$nearby_zones, collapse = ", ")) else ""
        base_txt <- sprintf("%s + %d nearby BCSI station%s within %g km%s",
                            base_txt, pv$n_nearby, if (pv$n_nearby == 1) "" else "s",
                            pv$bc_dist, zt)
      }
      sprintf("%s → %d station(s) total.", base_txt, n)
    } else {
      if (n == 0) return("ROI: no donors match the current filters.")
      d <- pv$donors
      sprintf("ROI: %d donor(s), GEO distance %.0f–%.0f km.", n,
              min(d$GEO_dist_km), max(d$GEO_dist_km))
    }
  })

  # Commit the automated selection into the shared station list
  observeEvent(input$roi_apply, {
    pv <- roi_preview()
    if (is.null(pv) || !length(pv$station_ids)) return()
    ids <- sort(unique(pv$station_ids))
    roi_applied_ids(ids)
    updateSelectizeInput(session, "bc_zone_selector", selected = character(0))
    updateSelectizeInput(session, "bc_zone_station_ids",
                         choices = all_station_ids, selected = ids, server = TRUE)
  })

  # ---- ROI flood-quantile estimate (GLS-style regression) ------------------
  # Reads HYDAT for the current donor group, so gated behind its own button.
  roi_estimate <- eventReactive(input$roi_estimate_btn, {
    pv <- isolate(roi_preview())
    if (is.null(pv) || pv$mode != "ROI" || is.null(pv$donors) || !nrow(pv$donors))
      return(list(error = "Pick an outside-BC location and let the ROI donors compute first."))
    preds <- if (identical(input$roi_est_predictors, "da_map")) c("log_da", "map_mm") else "log_da"
    rp <- suppressWarnings(as.numeric(input$roi_est_rp)); rp <- rp[is.finite(rp)]
    if (!length(rp)) rp <- c(2, 10, 100)
    withProgress(message = "Estimating regional flood quantiles", value = 0, {
      estimate_flood_quantile(
        target = list(lon = pv$lon, lat = pv$lat, da_km2 = input$roi_target_da),
        donors = pv$donors, pool = roi_pool,
        return_periods = rp, reg_predictors = preds,
        series = input$roi_est_series, dist = input$roi_est_dist,
        progress = function(v, m) setProgress(value = v, detail = m))
    })
  })

  output$roi_estimate_table <- renderTable({
    res <- roi_estimate()
    if (!is.null(res$error)) return(NULL)
    da <- res$da_km2
    res$estimate %>%
      dplyr::transmute(
        `Return period (yr)` = ReturnPeriod,
        `Flood estimate (m³/s)` = round(Q_est, 1),
        `90% prediction interval (m³/s)` = ifelse(is.na(Q_est), "—",
              paste0(round(lower, 1), " – ", round(upper, 1))),
        `Unit discharge (L/s/km²)` = round(Q_est / da * 1000, 1),
        `Donors used` = n_used,
        `R²` = round(r2, 3))
  }, striped = TRUE, hover = TRUE, width = "100%", na = "—")

  output$roi_estimate_note <- renderUI({
    res <- roi_estimate()
    if (!is.null(res$error)) return(helpText(style = "color:#b2182b;", res$error))
    preds <- paste(vapply(res$predictors, function(p) ROI_DESCRIPTOR_LABELS[[p]], character(1)),
                   collapse = " + ")
    helpText(HTML(sprintf(
      "Method: weighted log-log regression of donor at-site <b>%s</b> (%s distribution) flood ",
      if (res$series == "daily") "annual-max daily" else "annual-max instantaneous", res$dist)),
      HTML(sprintf("quantiles on <b>%s</b>, weighted by record length &times; ROI similarity. ", preds)),
      "The 90% prediction interval reflects regional scatter and is wide by nature — treat these ",
      "as screening-level estimates for an ungauged site, not a substitute for an at-site analysis.")
  })


  # End the app loading spinner----
  waiter_hide()

  # Subtab 2: Envelope Curve - single plot: ggplot base + reactive line -> ggplotly
  # Heavy computation (reads HYDAT for every selected station) is gated behind the
  # "Draw Figure" button so the tab does not recompute on every input change.
  scaling_data <- eventReactive(input$envelope_draw_btn, {
    req(input$envelope_return_period)

    # Station set from the shared regional_stations() resolver (manual IDs, else
    # the IDs implied by the selected BC zones)
    stations <- regional_stations()
    validate(need(length(stations) > 1, "Select at least two stations for the envelope curve."))

    # Resolve return period value
    rp_input <- input$envelope_return_period
    rp <- if (rp_input == "MAD") "MAD" else suppressWarnings(as.numeric(rp_input))
    if (!identical(rp, "MAD") && (is.na(rp) || rp <= 0)) {
      validate(need(FALSE, "Invalid return period."))
    }

    # Compute FFA for desired return period; try instantaneous first then fallback to daily
    return_periods <- if (identical(rp, "MAD")) 2 else rp  # placeholder proxy if MAD chosen
    ffa <- withProgress(message = "Computing regional FFA for selected stations", value = NULL, {
      f <- tryCatch(
        regional_ffa(station_ids = stations, return_periods = return_periods, series = "inst", dist = "GEV"),
        error = function(e) NULL)
      if (is.null(f) || nrow(f) == 0) {
        f <- tryCatch(
          regional_ffa(station_ids = stations, return_periods = return_periods, series = "daily", dist = "GEV"),
          error = function(e) NULL)
      }
      f
    })
    validate(need(!is.null(ffa) && nrow(ffa) > 0, "No FFA results for selected stations. Try different stations or zones."))

    # Build optional zone mapping for aesthetics
    zones_map <- tryCatch({
      if (!is.null(bc_zone_catalog)) {
        bc_zone_catalog %>% dplyr::select(Station = WSC_ID, Zone = Zone) %>% dplyr::distinct()
      } else NULL
    }, error = function(e) NULL)

    # Compute scatter data and domains once
    # Attach record length N from FFA (constant per station) before computing scaling data
    ffa_for_plot <- ffa %>% dplyr::group_by(Station) %>% dplyr::summarise(Q = dplyr::first(Q), N = dplyr::first(N), .groups = 'drop')
    cd <- compute_scaling_data(
      ffa_df = ffa_for_plot,
      station_zones = zones_map,
      drainage_lookup = NULL,
      x_limits = NULL,
      y_limits = NULL,
      extend_decades = 1
    )
    list(cd = cd, rp = rp, series = "inst")
  })
  # Auto-fit envelope: power-law curve above all points minimising squared
  # log-distance, via DDS global search + Levenberg-Marquardt polish. Writes the
  # fitted a/b back into the numeric inputs so the plot redraws the line.
  observeEvent(input$envelope_autofit_btn, {
    if (input$envelope_draw_btn == 0) {
      showNotification("Click 'Draw Figure' first, then auto-fit the envelope.",
                       type = "warning")
      return()
    }
    sd  <- tryCatch(scaling_data(), error = function(e) NULL)
    dat <- if (!is.null(sd)) sd$cd$dat else NULL
    if (is.null(dat) || nrow(dat) < 3) {
      showNotification("Need at least 3 plotted points to fit an envelope.", type = "error")
      return()
    }

    fit <- withProgress(message = "Fitting envelope (DDS + Levenberg-Marquardt)", value = NULL, {
      tryCatch(fit_envelope_curve(dat$DrainageArea_km2, dat$UnitQ_Lps_km2),
               error = function(e) NULL)
    })
    if (is.null(fit) || !is.finite(fit$a) || !is.finite(fit$b)) {
      showNotification("Envelope fit did not converge; adjust a/b manually.", type = "error")
      return()
    }

    updateNumericInput(session, "envelope_a", value = signif(fit$a, 4))
    updateNumericInput(session, "envelope_b", value = signif(fit$b, 4))
    updateCheckboxInput(session, "envelope_draw_line", value = TRUE)
    showNotification(
      HTML(sprintf("Envelope fitted: UnitQ = %s &middot; DA<sup>%.3f</sup> &nbsp; (%d of %d points above the curve).",
                   format(signif(fit$a, 4), big.mark = ","), fit$b, fit$n_above, nrow(dat))),
      type = "message", duration = 8)
  })

  # Single combined plot: base ggplot + optional envelope line -> plotly
  output$envelope_plot <- plotly::renderPlotly({
    sd <- scaling_data()
    cd <- sd$cd
    dat <- cd$dat
    xb <- cd$xb; yb <- cd$yb; xr <- cd$xr_use; yr <- cd$yr_use

    # Ensure legend shows "Non-BC stations" instead of NA for Zone
    if ("Zone" %in% names(dat)) {
      dat$Zone <- ifelse(is.na(dat$Zone), "Station Not Assigned to Any Hydrologic Zone", as.character(dat$Zone))
      dat$Zone <- factor(dat$Zone)
    }

    suppressPackageStartupMessages({ library(ggplot2); library(scales) })
    # Compute minor gridline positions at 2..9 within each decade of the current domain
    logx_lo <- floor(log10(xr[1])); logx_hi <- ceiling(log10(xr[2]))
    logy_lo <- floor(log10(yr[1])); logy_hi <- ceiling(log10(yr[2]))
    x_minor <- unlist(lapply(logx_lo:logx_hi, function(k) 10^k * (2:9)))
    x_minor <- x_minor[x_minor >= xr[1] & x_minor <= xr[2]]
    y_minor <- unlist(lapply(logy_lo:logy_hi, function(k) 10^k * (2:9)))
    y_minor <- y_minor[y_minor >= yr[1] & y_minor <= yr[2]]

    # Colour points by record length so data reliability is visible at a glance
    # (works for both BC-zone and ROI/non-BC selections, unlike zone colouring).
    dat$RecordClass <- cut(dat$N, breaks = c(-Inf, 20, 35, 50, 75, Inf),
                           labels = c("< 20", "20–34", "35–49",
                                      "50–74", "≥ 75"),
                           right = FALSE)
    dat$tip <- if ("Zone" %in% names(dat))
      paste0("Station: ", dat$Station, "<br>Zone: ", dat$Zone,
             "<br>Record length: ", dat$N, " yrs")
    else
      paste0("Station: ", dat$Station, "<br>Record length: ", dat$N, " yrs")

    p <- ggplot2::ggplot(dat, ggplot2::aes(x = DrainageArea_km2, y = UnitQ_Lps_km2)) +
      # Simulated minor ticks as faint gridlines drawn behind points
      ggplot2::geom_vline(xintercept = x_minor, color = "#D9D9D9", linewidth = 0.25, alpha = 0.8, show.legend = FALSE) +
      ggplot2::geom_hline(yintercept = y_minor, color = "#D9D9D9", linewidth = 0.25, alpha = 0.8, show.legend = FALSE) +
      suppressWarnings(ggplot2::geom_point(ggplot2::aes(colour = RecordClass, text = tip),
                                           size = 2.8, alpha = 0.9)) +
      { if (isTRUE(input$envelope_show_ids)) ggplot2::geom_text(ggplot2::aes(label = Station), size = 3, vjust = -0.8, check_overlap = TRUE, show.legend = FALSE) } +
      ggplot2::scale_x_log10(breaks = xb, labels = scales::label_number(big.mark = ",", accuracy = 1)) +
      ggplot2::scale_y_log10(breaks = yb, labels = scales::label_number(big.mark = ",", accuracy = 1)) +
      ggplot2::scale_colour_viridis_d(name = "Record length\n(years)", drop = FALSE,
                                      direction = -1, end = 0.92) +
      ggplot2::labs(title = NULL, x = "Drainage Area (km²)",
            y = paste0(sd$rp, "-year Daily Unit Flow (L/s/km²)")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::coord_cartesian(xlim = xr, ylim = yr)

    # Reactive envelope line
    a <- suppressWarnings(as.numeric(input$envelope_a))
    b <- suppressWarnings(as.numeric(input$envelope_b))
    draw <- isTRUE(input$envelope_draw_line)
    if (draw && is.finite(a) && is.finite(b)) {
      x_seq <- 10^seq(log10(xr[1]), log10(xr[2]), length.out = 200)
      y_seq <- a * (x_seq ^ b)
      p <- p + ggplot2::geom_line(data = data.frame(DrainageArea_km2 = x_seq, UnitQ_Lps_km2 = y_seq),
                                  ggplot2::aes(x = DrainageArea_km2, y = UnitQ_Lps_km2),
                                  inherit.aes = FALSE, color = 'black', linewidth = 1)

  # Add the equation as an annotation in the top-left of the panel
  eq_label <- sprintf("Envelope: UnitQ (L/s/km²) = %s · DA^%0.2f",
          format(signif(a, 4), big.mark = ","), b)
  # Place at bottom-left: ~3% from left and ~3% above bottom (in log space)
  logx <- log10(xr); logy <- log10(yr)
  x_pos <- 10^(logx[1] + 0.20 * (logx[2] - logx[1]))
  y_pos <- 10^(logy[1] + 0.03 * (logy[2] - logy[1]))
  p <- p + ggplot2::annotate("text", x = x_pos, y = y_pos, label = eq_label,
             hjust = 0, vjust = 0, size = 3.5)
    }

    plotly::ggplotly(p, tooltip = c("text")) %>%
      plotly::layout(
        hovermode = "closest"
      )
  })

  # Subtab 3: Qinst/Qdaily Ratio - pooled regional peaking-ratio density -------

  # Expensive step: read HYDAT records and compute per-station-year ratios.
  # Gated behind the "Run Ratio Analysis" button so it does not re-run on every
  # keystroke; downstream filtering (max ratio, min days, smoothing) is live.
  # Station set comes from the shared regional_stations() resolver.
  ratio_raw <- eventReactive(input$run_ratio, {
    stations <- regional_stations()
    validate(need(length(stations) >= 1,
                  "Select at least one station (or a BC zone) in the Regionalization sub-tab, then click Run Ratio Analysis."))

    n <- length(stations)
    withProgress(message = "Reading HYDAT flow records", value = 0, {
      compute_ratio_data(
        stations,
        progress = function(i, stn) {
          incProgress(1 / n, detail = paste0("(", i, "/", n, ") ", stn))
        }
      )
    })
  })

  # Live-filtered data used by the plot, summary, and table
  ratio_data <- reactive({
    dat <- ratio_raw()
    validate(need(!is.null(dat) && nrow(dat) > 0,
                  "None of the selected stations have both instantaneous and daily peak-flow records."))
    dat <- dat %>%
      dplyr::filter(!is.na(DrainageArea_km2), DrainageArea_km2 > 0,
                    is.finite(Ratio), Ratio > 0)
    if (!is.na(input$ratio_min_days)) {
      dat <- dplyr::filter(dat, n_days >= input$ratio_min_days)
    }
    if (!is.na(input$ratio_max)) {
      dat <- dplyr::filter(dat, Ratio <= input$ratio_max)
    }
    validate(need(nrow(dat) >= 5,
                  "Fewer than 5 valid station-years after filtering. Broaden the station selection, lower the minimum daily obs., or raise the max-ratio cap."))
    dat
  })

  # Colours shared by the percentile points and their fitted lines
  ratio_pal <- c(Mean = "#1f78b4", P75 = "#ff7f00", P95 = "#e31a1c", P99 = "#6a3d9a")

  # One row per station per percentile (Mean / P75 / P95 of that station's
  # ratio sample), keeping stations with at least `ratio_min_years` years.
  station_pct <- reactive({
    min_yrs <- if (is.na(input$ratio_min_years)) 1 else max(1, input$ratio_min_years)
    sp <- compute_station_percentiles(ratio_data(), min_years = min_yrs)
    validate(need(nrow(sp) > 0,
                  paste0("No stations have at least ", min_yrs,
                         " qualifying years. Lower 'Min years per station' or the ",
                         "completeness threshold, or add more stations.")))
    sp
  })

  # Regional trend line fitted to the per-station percentile points, one per
  # selected percentile. Linear: Value = b0 + b1*log10(DA); Exponential:
  # Value = a*exp(b*log10(DA)) fit on log(Value). Returns a named list.
  ratio_fits <- reactive({
    type <- input$ratio_fit
    sel  <- intersect(c("Mean", "P75", "P95", "P99"), input$ratio_pcts)
    if (is.null(type) || type == "none" || !length(sel)) return(list())

    sp <- station_pct()
    fits <- list()
    for (pc in sel) {
      d <- dplyr::filter(sp, Percentile == pc)
      if (nrow(d) < 3 || dplyr::n_distinct(d$DrainageArea_km2) < 3) next
      x <- log10(d$DrainageArea_km2); y <- d$Value
      xseq <- seq(min(x), max(x), length.out = 200)

      if (type == "exp") {
        m  <- stats::lm(log(y) ~ x); co <- stats::coef(m)
        a  <- exp(co[[1]]); b <- co[[2]]
        yseq <- a * exp(b * xseq); yhat <- a * exp(b * x)
        eq   <- sprintf("%s = %.3f × exp(%+.4f × log₁₀ DA)", pc, a, b)
      } else {
        m  <- stats::lm(y ~ x); co <- stats::coef(m)
        yseq <- co[[1]] + co[[2]] * xseq; yhat <- co[[1]] + co[[2]] * x
        eq   <- sprintf("%s = %.3f %+.4f × log₁₀ DA", pc, co[[1]], co[[2]])
      }

      ss_tot <- sum((y - mean(y))^2)
      r2 <- if (ss_tot > 0) 1 - sum((y - yhat)^2) / ss_tot else NA_real_

      # Ratios are physically >= 1, so truncate any part of the fitted line that
      # would dip below 1.0 rather than drawing it.
      line_df <- data.frame(DrainageArea_km2 = 10^xseq, Value = yseq)
      line_df <- line_df[is.finite(line_df$Value) & line_df$Value >= 1, , drop = FALSE]
      fits[[pc]] <- list(line   = line_df,
                         colour = unname(ratio_pal[[pc]]), eq = eq, r2 = r2)
    }
    fits
  })

  # Summary line: pooled counts, stations used for percentiles, and fit equations
  output$ratio_summary <- renderUI({
    dat <- ratio_data()
    sp  <- station_pct()
    n_obs     <- nrow(dat)
    n_stn_all <- dplyr::n_distinct(dat$Station)
    n_stn_pct <- dplyr::n_distinct(sp$Station)

    fits <- tryCatch(ratio_fits(), error = function(e) list())
    fit_label <- c(linear = "Linear", exp = "Exponential")[input$ratio_fit]
    fit_txt <- if (length(fits)) {
      paste0("<br><b>", fit_label, " fits:</b><br>",
             paste0(vapply(names(fits), function(pc) {
               paste0("&nbsp;&nbsp;<span style='color:", fits[[pc]]$colour, "'>&#9632;</span> ",
                      fits[[pc]]$eq, "&nbsp;&nbsp;(R² = ", round(fits[[pc]]$r2, 3), ")")
             }, character(1)), collapse = "<br>"))
    } else ""

    HTML(paste0(
      "<b>", n_stn_all, "</b> stations, <b>", n_obs,
      "</b> station-years pooled (complete years ≥ ", input$ratio_min_days,
      " daily obs.); <b>", n_stn_pct, "</b> stations with ≥ ", input$ratio_min_years,
      " years used for percentiles.", fit_txt
    ))
  })

  # 2-D scatter: per-station percentile ratios vs drainage area (log x),
  # with a selectable regional trend line per percentile
  output$ratio_2d <- plotly::renderPlotly({
    sp  <- station_pct()
    sel <- intersect(c("Mean", "P75", "P95", "P99"), input$ratio_pcts)
    validate(need(length(sel) >= 1,
                  "Select at least one percentile line (Mean / P75 / P95 / P99)."))
    spf  <- dplyr::filter(sp, Percentile %in% sel)
    fits <- ratio_fits()

    suppressPackageStartupMessages({ library(ggplot2); library(scales) })

    p <- ggplot2::ggplot()

    # Optional faint cloud of every station-year ratio for context
    if (isTRUE(input$ratio_show_raw)) {
      p <- p + suppressWarnings(ggplot2::geom_point(
        data = ratio_data(),
        ggplot2::aes(x = DrainageArea_km2, y = Ratio,
                     text = paste0("Station: ", Station, "<br>Year: ", Year,
                                   "<br>Ratio: ", round(Ratio, 3))),
        colour = "grey75", size = 1, alpha = 0.35))
    }

    # Per-station percentile points, coloured by percentile
    p <- p + suppressWarnings(ggplot2::geom_point(
      data = spf,
      ggplot2::aes(x = DrainageArea_km2, y = Value, colour = Percentile,
                   text = paste0("Station: ", Station,
                                 "<br>", Percentile, " ratio: ", round(Value, 3),
                                 "<br>Drainage area: ",
                                 formatC(round(DrainageArea_km2), big.mark = ","), " km²",
                                 "<br>Years: ", n_years)),
      size = 2.4, alpha = 0.9)) +
      ggplot2::scale_colour_manual(values = ratio_pal, drop = FALSE, name = NULL) +
      ggplot2::scale_x_log10(labels = scales::label_number(big.mark = ",", accuracy = 1)) +
      ggplot2::labs(x = "Drainage Area (km², log scale)", y = "Qinst / Qdaily ratio") +
      ggplot2::theme_bw()

    # Fitted regional line per selected percentile
    for (pc in names(fits)) {
      p <- p + ggplot2::geom_line(
        data = fits[[pc]]$line,
        ggplot2::aes(x = DrainageArea_km2, y = Value),
        colour = fits[[pc]]$colour, linewidth = 1)
    }

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(hovermode = "closest")
  })

  # Per-station percentile table
  output$ratio_table <- DT::renderDataTable({
    station_pct() %>%
      tidyr::pivot_wider(names_from = Percentile, values_from = Value) %>%
      dplyr::transmute(
        Station,
        `DA (km2)` = round(DrainageArea_km2, 1),
        Years      = n_years,
        Mean     = round(Mean, 3),
        P75        = round(P75, 3),
        P95        = round(P95, 3),
        P99        = round(P99, 3)
      ) %>%
      dplyr::arrange(`DA (km2)`) %>%
      DT::datatable(
        extensions = c("Buttons", "Scroller"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          scrollX = TRUE, deferRender = TRUE, scrollY = 400, scroller = TRUE
        )
      )
  })

  # Downloads: per-station percentiles, and the underlying station-year ratios
  output$download_ratio_pct <- downloadHandler(
    filename = function() "qinst_qdaily_station_percentiles.csv",
    content = function(file) write.csv(
      tidyr::pivot_wider(station_pct(), names_from = Percentile, values_from = Value),
      file, row.names = FALSE)
  )
  output$download_ratio <- downloadHandler(
    filename = function() "qinst_qdaily_station_years.csv",
    content = function(file) write.csv(ratio_data(), file, row.names = FALSE)
  )


}) # End of ShinyServer()
