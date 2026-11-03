
# ------------ HYDAT database loading ------------

# Determine Hydat database location
# ./database        - database beside app.R (self-contained copy)
# ../database       - repo root when running from the app/ working directory
# ../../database    - shared store one level above the Shiny Server app folder
if(file.exists("./database/Hydat.sqlite3")) {
  Hydat_Location <- "./database/Hydat.sqlite3"
} else if (file.exists("../database/Hydat.sqlite3")) {
    Hydat_Location <- "../database/Hydat.sqlite3"
} else if (file.exists("../../database/Hydat.sqlite3")) {
    Hydat_Location <- "../../database/Hydat.sqlite3"
} else {
  Hydat_Location <- tidyhydat::hy_default_db()
}

# Variables needed globally
# Create flag names and plotting colours for data symbols
SYMs <- c("", "E", "A", "B", "D", "R", "RealTime")
SYMnames <- c("No Code", "Estimate", "Partial Day", "Ice Conditions", "Dry", "Revised", "RealTime")
SYMcol <- c("grey", "#E41A1C", "#4DAF4A", "#377EB8", "#FF7F00", "#984EA3", "#FFA500")

# Distributions List for Flood Frequency Analyis (FFA)
Dist_Options <- c("Exponential", "Gamma", "GEV", "Gen. Logistic", "Gen. Normal", "Gen. Pareto",
                  "Gumbel", "Kappa", "Normal", "Pearson III", "Wakeby", "Weibull", "Log-Normal", "LP3")

# Build the station metadata table (coordinates, labels, drainage area, record
# ranges) used by the map, the station-ID selectors, and the info panels.
# The hy_stn_data_range() + hy_stations() HYDAT queries are the slowest part of
# app startup, so the result is cached to an RDS (see below).
build_map_data <- function() {
  range.df <- tidyhydat::hy_stn_data_range(hydat_path = Hydat_Location) %>%

    # Grab Flow record range (Q)
    filter(DATA_TYPE == "Q") %>%
    select(STATION_NUMBER, Qfrom = Year_from, Qto = Year_to, Qn = RECORD_LENGTH) %>%

    # Grab Stage record range (H) and append to the df
    left_join(
      tidyhydat::hy_stn_data_range(hydat_path = Hydat_Location) %>%
        filter(DATA_TYPE == "H") %>%
        select(STATION_NUMBER, Hfrom = Year_from, Hto = Year_to, Hn = RECORD_LENGTH),
      by = "STATION_NUMBER"
    ) %>%

    # Grab drainage area & RHBN status
    left_join(
      tidyhydat::hy_stations(hydat_path = Hydat_Location) %>%
        select(STATION_NUMBER, DRAINAGE_AREA_GROSS, REAL_TIME, RHBN),
      by = "STATION_NUMBER"
    ) %>%

    # Round DA number to 2 digits
    mutate_at(vars(DRAINAGE_AREA_GROSS), list(~round(., 2)))

  tidyhydat::allstations %>%
    left_join(range.df, by = "STATION_NUMBER") %>%
    mutate(text = paste(sep = "<br/>", paste("<b>", STATION_NUMBER, "</b>"),
                        STATION_NAME,
                        HYD_STATUS,
                        paste0("Drainage Area: ", DRAINAGE_AREA_GROSS, " km<sup>2</sup>"),
                        paste0("Benchmark Station: ", RHBN),
                        paste0("Flow Record: from ", Qfrom, " to ", Qto, " (", Qn, " Yrs)"),
                        paste0("Stage Record: from ", Hfrom, " to ", Hto, " (", Hn, " Yrs)")
    ))
}

# Load map_data from an on-disk RDS cache when it is at least as new as the HYDAT
# database; otherwise (first launch, or after a HYDAT update) rebuild from HYDAT
# and refresh the cache. This makes the station list / map load near-instantly
# on subsequent launches. The cache lives in ./data and is git-ignored.
.map_data_cache <- "./data/map_data.rds"
map_data <- local({
  hydat_mtime <- tryCatch(file.info(Hydat_Location)$mtime, error = function(e) NA)
  fresh <- file.exists(.map_data_cache) && !is.na(hydat_mtime) &&
    file.info(.map_data_cache)$mtime >= hydat_mtime
  if (fresh) {
    cached <- tryCatch(readRDS(.map_data_cache), error = function(e) NULL)
    if (!is.null(cached) && nrow(cached) > 0) return(cached)
  }
  md <- build_map_data()
  tryCatch({
    dir.create(dirname(.map_data_cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(md, .map_data_cache)
  }, error = function(e) NULL)
  md
})



# ------------ HYDAT data processing ------------
# Daily Flow Time Series
read_dailyflow <- function(station_number) {
  
  # read Qdaily from HYDAT historical data
  hy_daily <-  tidyhydat::hy_daily_flows(station_number = station_number, 
                                         hydat_path = Hydat_Location) %>%
                  filter(Parameter == 'Flow')
  
  
  Q_daily <- hy_daily %>%
    # put the Qdaily into format readable by FlowScreen Package
    # Flow parameter: 1 = Flow, 2 = level
    mutate(ID = station_number, PARAM = 1, Agency = "WSC") %>%
    select(ID, PARAM, Date, Flow = Value, SYM = Symbol, Agency)
    
    
  return(Q_daily)
} # EOF read_dailyflow

# Instantaneous Time Series
read_Qinst <- function(station_number) {
  
  # read Qinst from HYDAT
  Q_Inst = tidyhydat::hy_annual_instant_peaks(station_number = station_number, 
                                              hydat_path = Hydat_Location)
  
  
  # put the QiNST into format readable by FlowScreen Package if required
  Qinst <- Q_Inst %>%
    filter(Parameter == 'Flow',
           PEAK_CODE == 'MAX') %>%
    
    # Flow parameter: 1 = Flow, 2 = level
    mutate(ID = station_number, PARAM = 1, Agency = "WSC") %>%
    select(ID, PARAM, Date, Flow = Value, SYM = Symbol, Agency)
  
  
  return(Qinst)
} # EOF read_Qinst


# function to calculate water year base on date
hydro_yr <- function(., hyrstart) {
  
  # Convert possible character vector into date
  d1 = base::as.Date(.)
  
  df <- tibble(Year = lubridate::year(d1),
               WaterYear = Year, #initial set-up
               Month = lubridate::month(d1)
                   )

  ## define hydrologic year based on start month
  if (hyrstart > 6.5) {
    
    MonthsUp <- c(hyrstart:12)
    df$WaterYear <- base::ifelse(df$Month %in% MonthsUp, df$Year+1, df$Year)

  } else if (hyrstart == 1) {
    
    df <- df
    
  } else if (hyrstart < 6.5) {
    
    MonthsDown <- c(1:(hyrstart-1))
    df$WaterYear <- base::ifelse(df$Month %in% MonthsDown, df$Year-1, df$Year)
    
  }
  
  # Return the water year
  return(df$WaterYear)
}

# Station Dataset Summarized
# Uses user input from the resolution dropdown
Dataset_Summarize <- function(station_number, summary_reso, wy_month) {
  
  TS <- read_dailyflow(station_number = station_number) %>%
    mutate(Year = year(Date) , Month = month(Date), Day = day(Date)) %>%
    select(Date, Year, Month, Day, Flow, Symbol = SYM) %>% 
    rename(Flow_cms = Flow)
  
  codes <- as.factor(TS$Symbol)
  codes <- match(codes, SYMs)
  codes <- SYMnames[codes]
  
  # Calculate Water Year
  TS <- TS %>% 
        # calculate water year
        mutate(WaterYear = hydro_yr(Date, hyrstart = wy_month),
               Flag = codes) %>% 
    
        # re-arrange column order to have WaterYear beside Year
        dplyr::relocate(WaterYear, .after = Year) %>%
        # Remove Symbol (using Flag instead)
        select(-Symbol) 
  
  if(summary_reso == "Daily"){
    TS$Flow_cms <- round(TS$Flow_cms, digits = 3)
  }
  
  else if(summary_reso == "Monthly"){
    TS <- TS %>% 
            group_by(Year, Month) %>%
            summarise(Average_Flow_cms = mean(Flow_cms, na.rm = TRUE), 
                      Count = sum(is.na(Flow_cms)==FALSE),
                      WaterYear = max(WaterYear, na.rm = TRUE),
                      .groups = "drop") %>% 
            dplyr::relocate(WaterYear, .after = Year)
    
    TS$Average_Flow_cms <- round(TS$Average_Flow_cms, digits = 3)
  }
  
  # Make sure to group by water year
  else if(summary_reso == "Yearly"){
    TS <- TS %>% 
      group_by(WaterYear) %>%
      summarise(Average_Flow_cms = mean(Flow_cms, na.rm = TRUE), 
                Max_Daily_cms = max(Flow_cms, na.rm = TRUE),
                Min_Daily_cms = min(Flow_cms, na.rm = TRUE), 
                Count = sum(is.na(Flow_cms) == FALSE),
                .groups = "drop") %>%
                round(digits = 3)
  }

  return(TS)
} # EOF for flow period summary



# ---------------- FFA function ------------------
lmom_Q <- function(Qp, empirical.Tr = NA, evaluation = FALSE) {
  
  
  # Custom output (let's keep the custom of having largest on top)
  if(evaluation == TRUE) (
    ReturnPeriods <- empirical.Tr %>% sort(decreasing = FALSE)
  ) else if(evaluation == "Plot") (
    ReturnPeriods  <- c(seq(1.01, 1.99, by = 0.01), seq(2, 9.9, by = 0.1), 10:1000) %>% sort(decreasing = FALSE)
  ) else (
    ReturnPeriods <- empirical.Tr %>% sort(decreasing = FALSE)
  )
  
  Pnonexc = 1 - (1/ReturnPeriods)
  
  # samlmu() gets the sample L-moments, pelxxx() estimates the distribution's parameters from L-moments
  # Quaxxx generates quantile given probability and distribution parameters
  # xxx = "exp" "gam" "gev" "glo" "gno" "gpa" "gum" "kap" "ln3" "nor" "pe3" "wak" "wei"
  
  lmoms <- samlmu(Qp, nmom = 5)
  log.lmoms <- samlmu(log10(Qp),nmom = 5)
  
  error_value <- as.numeric(rep(NA, length(Pnonexc)))
  # using tryCatch to allow the app to continue running 
  # if a particular distribution can't be fit to the data
  extremes <- tibble(ReturnPeriods = ReturnPeriods,
                     Pnonexc = Pnonexc,
                     Exponential = tryCatch(error = function(err) {return(error_value)}, 
                                            quaexp(f = Pnonexc, para = pelexp(lmoms))), # exponential
                     
                     Gamma = tryCatch(error = function(err) {return(error_value)}, 
                                      quagam(f = Pnonexc, para = pelgam(lmoms))), # gamma
                     
                     GEV = tryCatch(error = function(err) {return(error_value)}, 
                                    quagev(f = Pnonexc, para = pelgev(lmoms))), # generalized extreme-value
                     
                     `Gen. Logistic` = tryCatch(error = function(err) {return(error_value)}, 
                                                quaglo(f = Pnonexc, para = pelglo(lmoms))), # generalized logistic
                     
                     `Gen. Normal` = tryCatch(error = function(err) {return(error_value)}, 
                                              quagno(f = Pnonexc, para = pelgno(lmoms))), # generalized normal
                     
                     `Gen. Pareto` = tryCatch(error = function(err) {return(error_value)}, 
                                              quagpa(f = Pnonexc, para = pelgpa(lmoms))), # generalized pareto
                     
                     Gumbel = tryCatch(error = function(err) {return(error_value)}, 
                                       quagum(f = Pnonexc, para = pelgum(lmoms))), # gumbel (extreme value type I)
                     
                     Kappa = tryCatch(error = function(err) {return(error_value)}, 
                                      quakap(f = Pnonexc, para = pelkap(lmoms))), # kappa
                     
                     Normal = tryCatch(error = function(err) {return(error_value)}, 
                                       quanor(f = Pnonexc, para = pelnor(lmoms))), # normal
                     
                     `Pearson III` = tryCatch(error = function(err) {return(error_value)}, 
                                              quape3(f = Pnonexc, para = pelpe3(lmoms))), # pearson type III
                     
                     Wakeby = tryCatch(error = function(err) {return(error_value)}, 
                                       quawak(f = Pnonexc, para = pelwak(lmoms))), # wakeby
                     
                     Weibull = tryCatch(error = function(err) {return(error_value)}, 
                                        quawei(f = Pnonexc, para = pelwei(lmoms))), # weibull
                     
                     # Logged distribution from the package
                     `Log-Normal` = tryCatch(error = function(err) {return(error_value)}, qualn3(f = Pnonexc, para = pelln3(lmoms))), # lognormal
                     
                     # Manually created log distribution
                     LP3 = tryCatch(error = function(err) {return(error_value)}, 10^quape3(f = Pnonexc, para = pelpe3(log.lmoms))) # log pearson type III
  )
  
  if (evaluation == TRUE) {
    extremes <- extremes %>% mutate(Qp.obs = Qp) # observed Qp
  }
  
  return(extremes)
} # End of Flood Frequency Function


# calculate empirical frequency
empirical_ffa <- function(flow_df, complete_years, selected_months, wy_month = NULL){
  
  flow_df %>% 
    
    # only years in the complete years (and after removal)
    # only months selected
    filter(lubridate::year(Date) %in% complete_years,
           lubridate::month(Date) %in% selected_months) %>%
    drop_na(Flow) %>%
    group_by(Year = hydro_yr(Date, hyrstart = wy_month)) %>%
    # Keep only ONE row per year: the first occurrence of the max flow (handles ties)
    slice_max(Flow, n = 1, with_ties = FALSE) %>%
    rename(AMS = Flow) %>%
    ungroup() %>%
    mutate(Rank = base::rank(-AMS, ties.method = "random"),
           `Return Period` = round(((length(Rank)+1) / Rank), digits = 3)
    ) %>%
    arrange(Rank) %>%
    mutate(AMS = round(AMS, digits = 3))
}
  
 
# ---------------- Regional FFA helper ------------------
# Compute L-moment frequency estimates for multiple stations at desired return periods
# Inputs:
# - station_ids: character vector of WSC station IDs
# - return_periods: numeric vector of Tr to evaluate (e.g., c(2,5,10,25,50,100,200))
# - series: 'daily' (default, AMS from daily flows by water year) or 'inst' (annual instantaneous peaks)
# - dist: one distribution name matching a column produced by lmom_Q (e.g., 'GEV', 'LP3', 'Log-Normal', etc.)
# - wy_month: start month of water year for 'daily' series (default 1 = January)
# - months: integer vector 1-12 months to include (default all months)
# - complete_days_threshold: optional integer; if set, only include water years with at least this many non-NA daily values
# Output: tibble with columns Station, Series, N, ReturnPeriod, Distribution, Q
regional_ffa <- function(
  station_ids,
  return_periods = c(2, 5, 10, 25, 50, 100, 200,1000),
  series = c("daily", "inst"),
  dist = "GEV",
  wy_month = 1,
  months = 1:12,
  complete_days_threshold = NULL
) {
  series <- match.arg(series)
  # Validate distribution label; fall back to GEV if unknown
  valid_dists <- c("GEV", "Gumbel", "LP3")
  if (!dist %in% valid_dists) dist <- "GEV"

  all_results <- lapply(station_ids, function(stn) {
    Qp <- NULL
    if (series == "daily") {
      df <- tryCatch(read_dailyflow(stn), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) return(NULL)
      df <- df %>%
        dplyr::filter(lubridate::month(Date) %in% months)
      # Optionally enforce completeness by water year
      if (!is.null(complete_days_threshold)) {
        wy_counts <- df %>%
          dplyr::mutate(WY = hydro_yr(Date, hyrstart = wy_month)) %>%
          dplyr::summarise(n = sum(!is.na(Flow)), .by = WY)
        keep_wy <- wy_counts$WY[wy_counts$n >= complete_days_threshold]
        df <- df %>% dplyr::mutate(WY = hydro_yr(Date, hyrstart = wy_month)) %>%
          dplyr::filter(WY %in% keep_wy)
      }
      # Annual maxima by water year
      ams <- df %>%
        dplyr::mutate(WY = hydro_yr(Date, hyrstart = wy_month)) %>%
        dplyr::filter(!is.na(Flow)) %>%
        dplyr::summarise(AMS = max(Flow), .by = WY)
      Qp <- ams$AMS
    } else if (series == "inst") {
      df <- tryCatch(read_Qinst(stn), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) return(NULL)
      # Annual instantaneous peaks are already yearly maxima; ensure one per year
      ams <- df %>%
        dplyr::filter(!is.na(Flow)) %>%
        dplyr::summarise(AMS = max(Flow), .by = lubridate::year(Date))
      Qp <- ams$AMS
    }

    # Need at least 3-5 years for stable L-moments; keep minimal guard
    if (is.null(Qp) || length(Qp[is.finite(Qp)]) < 3) return(NULL)

    res <- tryCatch(lmom_Q(Qp = Qp, empirical.Tr = as.integer(return_periods)), error = function(e) NULL)
    if (is.null(res) || !nrow(res)) return(NULL)
    # Pull the chosen distribution column and tidy
    if (!dist %in% names(res)) return(NULL)
    tibble::tibble(
      Station = stn,
      Series = series,
      N = length(Qp),
      ReturnPeriod = res$ReturnPeriods,
      Distribution = dist,
      Q = res[[dist]]
    )
  })

  out <- dplyr::bind_rows(all_results)
  if (is.null(out) || !nrow(out)) {
    return(tibble::tibble(Station = character(0), Series = character(0), N = integer(0),
                          ReturnPeriod = numeric(0), Distribution = character(0), Q = numeric(0)))
  }
  out
}


# Helper: compute scatter data and axis domains for regional scaling
# Returns a list(dat, xr_use, yr_use, xb, yb) where dat has columns:
#   Station, DrainageArea_km2, UnitQ_Lps_km2, Zone (optional)
compute_scaling_data <- function(
  ffa_df,
  station_zones = NULL,
  drainage_lookup = NULL,
  x_limits = NULL,
  y_limits = NULL,
  extend_decades = 1
) {
  stopifnot(is.data.frame(ffa_df))
  if (!all(c("Station","Q") %in% names(ffa_df))) {
    stop("ffa_df must contain Station and Q columns")
  }

  # Drainage area lookup
  lk <- NULL
  if (!is.null(drainage_lookup)) {
    lk <- drainage_lookup %>% dplyr::select(Station, DrainageArea_km2)
  } else if (exists("map_data", inherits = TRUE)) {
    lk <- tryCatch(map_data %>% dplyr::select(Station = STATION_NUMBER, DrainageArea_km2 = DRAINAGE_AREA_GROSS), error = function(e) NULL)
  }
  if (is.null(lk)) {
    lk <- tryCatch(
      tidyhydat::hy_stations(hydat_path = Hydat_Location) %>% dplyr::select(Station = STATION_NUMBER, DrainageArea_km2 = DRAINAGE_AREA_GROSS),
      error = function(e) NULL
    )
  }
  if (is.null(lk) || !nrow(lk)) stop("Unable to determine drainage areas for stations.")

  dat <- ffa_df %>% dplyr::left_join(lk, by = "Station") %>%
    dplyr::filter(!is.na(DrainageArea_km2), DrainageArea_km2 > 0, !is.na(Q), Q > 0) %>%
    dplyr::mutate(UnitQ_Lps_km2 = Q * 1000 / DrainageArea_km2)
  if (!nrow(dat)) stop("No valid rows after merging drainage areas.")

  if (!is.null(station_zones)) {
    if (is.data.frame(station_zones)) {
      dat <- dat %>% dplyr::left_join(station_zones %>% dplyr::select(Station, Zone), by = "Station")
    } else if (is.vector(station_zones) && !is.null(names(station_zones))) {
      dat <- dat %>% dplyr::left_join(tibble::tibble(Station = names(station_zones), Zone = unname(station_zones)), by = "Station")
    }
    if ("Zone" %in% names(dat)) dat <- dat %>% dplyr::mutate(Zone = as.factor(Zone))
  }

  # Compute decade-extended domains and breaks
  xr_raw <- if (is.null(x_limits)) range(dat$DrainageArea_km2, na.rm = TRUE) else x_limits
  yr_raw <- if (is.null(y_limits)) range(dat$UnitQ_Lps_km2, na.rm = TRUE) else y_limits
  xr_raw[1] <- max(xr_raw[1], min(dat$DrainageArea_km2[dat$DrainageArea_km2 > 0], na.rm = TRUE))
  yr_raw[1] <- max(yr_raw[1], min(dat$UnitQ_Lps_km2[dat$UnitQ_Lps_km2 > 0], na.rm = TRUE))
  x_lo_dec <- floor(log10(xr_raw[1])); x_hi_dec <- ceiling(log10(xr_raw[2]))
  y_lo_dec <- floor(log10(yr_raw[1])); y_hi_dec <- ceiling(log10(yr_raw[2]))
  xr_use <- if (is.null(x_limits)) c(10^(x_lo_dec - extend_decades), 10^(x_hi_dec + extend_decades)) else xr_raw
  yr_use <- if (is.null(y_limits)) c(10^(y_lo_dec - extend_decades), 10^(y_hi_dec + extend_decades)) else yr_raw
  xb <- 10^seq(floor(log10(xr_use[1])), ceiling(log10(xr_use[2])))
  yb <- 10^seq(floor(log10(yr_use[1])), ceiling(log10(yr_use[2])))

  list(dat = dat, xr_use = xr_use, yr_use = yr_use, xb = xb, yb = yb)
}


# ---------------- Qinst / Qdaily ratio helpers ------------------
# Pool the annual instantaneous-to-daily peaking ratio across many stations so
# the regional behaviour can be examined even though individual stations have
# short instantaneous records.
#
# For every station and calendar year that has BOTH an annual instantaneous peak
# and daily-flow data, the ratio is
#     Ratio = annual-maximum instantaneous peak / annual-maximum daily mean flow
# WSC determines annual instantaneous maxima on a calendar-year basis, so daily
# maxima are also taken by calendar year (no water-year shift). Because the
# annual instantaneous maximum is, by definition, >= the annual daily maximum,
# Ratio is >= 1.
#
# Inputs:
# - station_ids: character vector of WSC station IDs to pool
# - drainage_lookup: optional data frame with Station, DrainageArea_km2
# - progress: optional function(i, station) called before each station (for a
#   Shiny progress bar)
# Output: tibble with columns Station, Year, Qinst, Qdaily, Ratio,
#   DrainageArea_km2, n_days (non-NA daily observations in that year)
compute_ratio_data <- function(station_ids, drainage_lookup = NULL, progress = NULL) {

  # Drainage area lookup (reuse the app-wide map_data if available)
  lk <- NULL
  if (!is.null(drainage_lookup)) {
    lk <- drainage_lookup %>% dplyr::select(Station, DrainageArea_km2)
  } else if (exists("map_data", inherits = TRUE)) {
    lk <- tryCatch(map_data %>%
                     dplyr::select(Station = STATION_NUMBER,
                                   DrainageArea_km2 = DRAINAGE_AREA_GROSS),
                   error = function(e) NULL)
  }
  if (is.null(lk)) {
    lk <- tryCatch(
      tidyhydat::hy_stations(hydat_path = Hydat_Location) %>%
        dplyr::select(Station = STATION_NUMBER, DrainageArea_km2 = DRAINAGE_AREA_GROSS),
      error = function(e) NULL)
  }

  station_ids <- unique(as.character(station_ids))

  results <- lapply(seq_along(station_ids), function(i) {
    stn <- station_ids[i]
    if (is.function(progress)) progress(i, stn)

    # Instantaneous annual peaks (already annual maxima; guard against dups)
    inst <- tryCatch(read_Qinst(stn), error = function(e) NULL)
    if (is.null(inst) || !nrow(inst)) return(NULL)
    inst_annual <- inst %>%
      dplyr::filter(!is.na(Flow), Flow > 0) %>%
      dplyr::mutate(Year = lubridate::year(Date)) %>%
      dplyr::summarise(Qinst = max(Flow), .by = Year)
    if (!nrow(inst_annual)) return(NULL)

    # Daily-flow annual maxima (calendar year), with a completeness count
    daily <- tryCatch(read_dailyflow(stn), error = function(e) NULL)
    if (is.null(daily) || !nrow(daily)) return(NULL)
    daily_annual <- daily %>%
      dplyr::filter(!is.na(Flow), Flow > 0) %>%
      dplyr::mutate(Year = lubridate::year(Date)) %>%
      dplyr::summarise(Qdaily = max(Flow), n_days = dplyr::n(), .by = Year)
    if (!nrow(daily_annual)) return(NULL)

    merged <- dplyr::inner_join(inst_annual, daily_annual, by = "Year") %>%
      dplyr::filter(Qdaily > 0) %>%
      dplyr::mutate(Station = stn, Ratio = Qinst / Qdaily)

    if (!nrow(merged)) return(NULL)
    merged
  })

  out <- dplyr::bind_rows(results)
  if (is.null(out) || !nrow(out)) {
    return(tibble::tibble(Station = character(0), Year = integer(0),
                          Qinst = numeric(0), Qdaily = numeric(0),
                          Ratio = numeric(0), DrainageArea_km2 = numeric(0),
                          n_days = integer(0)))
  }

  if (!is.null(lk)) out <- out %>% dplyr::left_join(lk, by = "Station")

  out %>%
    dplyr::select(Station, Year, Qinst, Qdaily, Ratio, DrainageArea_km2, n_days) %>%
    dplyr::arrange(Station, Year)
}


# Per-station ratio percentiles, used for the regional percentile-line fits.
# Collapses the pooled station-year ratios to one row per station per percentile,
# keeping only stations with at least `min_years` qualifying years so the upper
# percentiles are not driven by a single event. Returns a long tibble:
#   Station, DrainageArea_km2, n_years, Percentile (Mean/P75/P95/P99), Value
compute_station_percentiles <- function(ratio_df, min_years = 5) {
  lvls  <- c("Mean", "P75", "P95", "P99")
  empty <- tibble::tibble(Station = character(0), DrainageArea_km2 = numeric(0),
                          n_years = integer(0),
                          Percentile = factor(character(0), levels = lvls),
                          Value = numeric(0))
  if (is.null(ratio_df) || !nrow(ratio_df)) return(empty)

  wide <- ratio_df %>%
    dplyr::filter(!is.na(DrainageArea_km2), DrainageArea_km2 > 0, is.finite(Ratio)) %>%
    dplyr::group_by(Station) %>%
    dplyr::filter(dplyr::n() >= min_years) %>%
    dplyr::summarise(
      DrainageArea_km2 = dplyr::first(DrainageArea_km2),
      n_years = dplyr::n(),
      Mean   = mean(Ratio, na.rm = TRUE),
      P75    = stats::quantile(Ratio, 0.75, na.rm = TRUE, names = FALSE),
      P95    = stats::quantile(Ratio, 0.95, na.rm = TRUE, names = FALSE),
      P99    = stats::quantile(Ratio, 0.99, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  if (!nrow(wide)) return(empty)

  wide %>%
    tidyr::pivot_longer(c(Mean, P75, P95, P99),
                        names_to = "Percentile", values_to = "Value") %>%
    dplyr::mutate(Percentile = factor(Percentile, levels = lvls))
}


# ---------------- Envelope-curve auto-fit (DDS + Levenberg-Marquardt) --------
# Dynamically Dimensioned Search (Tolson & Shoemaker 2007): a dependency-free
# global minimiser of `fn` over the box [lower, upper] on a fixed evaluation
# budget, with a single perturbation-size tuning parameter r.
dds_min <- function(fn, lower, upper, start = NULL, max_eval = 2000L,
                    r = 0.2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  d <- length(lower)
  rng <- upper - lower

  reflect <- function(x, lo, hi) {
    # Reflect a perturbed value back into [lo, hi]; fold once, then clamp.
    if (x < lo) x <- lo + (lo - x)
    if (x > hi) x <- hi - (x - hi)
    min(max(x, lo), hi)
  }

  x_best <- if (is.null(start)) lower + stats::runif(d) * rng else start
  f_best <- fn(x_best)

  for (i in 2:max_eval) {
    P   <- 1 - log(i) / log(max_eval)          # selection probability, decays
    sel <- stats::runif(d) < P
    if (!any(sel)) sel[sample.int(d, 1)] <- TRUE

    x_new <- x_best
    for (j in which(sel)) {
      x_new[j] <- reflect(x_best[j] + r * rng[j] * stats::rnorm(1), lower[j], upper[j])
    }
    f_new <- fn(x_new)
    if (f_new <= f_best) { f_best <- f_new; x_best <- x_new }
  }

  list(par = x_best, value = f_best)
}

# Auto-fit a regional envelope curve UnitQ = a * DA^b that sits ON TOP of every
# point (envelope constraint) while minimising the squared log-distance to the
# points. Fitting is done in log10 space (Y = A + b*X, A = log10 a), with an
# asymmetric penalty that heavily punishes points left above the curve.
#
# Two-stage optimiser, mirroring the rating-curve app: DDS for a robust global
# search, then a Levenberg-Marquardt polish (minpack.lm) on the residual vector
# c(gap, sqrt(penalty)*max(0, -gap)). Returns list(a, b, n_above, max_gap_log,
# mean_gap_log) or NULL when there are too few points.
fit_envelope_curve <- function(DA, UnitQ, max_eval = 3000L, penalty = 1e4, seed = 1L) {
  ok <- is.finite(DA) & is.finite(UnitQ) & DA > 0 & UnitQ > 0
  DA <- DA[ok]; UnitQ <- UnitQ[ok]
  if (length(DA) < 3) return(NULL)

  X <- log10(DA); Y <- log10(UnitQ)

  # OLS line, shifted up so it clears every point -> a feasible starting envelope
  ols <- stats::lm(Y ~ X)
  A0  <- unname(stats::coef(ols)[1])
  b0  <- unname(stats::coef(ols)[2])
  if (!is.finite(A0)) A0 <- mean(Y)
  if (!is.finite(b0)) b0 <- 0
  A_feas <- A0 + max(Y - (A0 + b0 * X))

  # Objective: mean squared gap above the points + heavy penalty for any point
  # sitting above the curve (gap < 0). Minimised when the curve rests as low as
  # possible on the uppermost points.
  obj <- function(p) {
    gap <- (p[1] + p[2] * X) - Y
    mean(gap^2) + penalty * mean(pmax(0, -gap)^2)
  }

  lower <- c(A_feas - 4, b0 - 2.5)
  upper <- c(A_feas + 3, b0 + 2.5)
  start <- c(A_feas, b0)

  # --- Stage 1: DDS global search ---
  par <- dds_min(obj, lower, upper, start, max_eval = max_eval, r = 0.2, seed = seed)$par

  # --- Stage 2: Levenberg-Marquardt polish ---
  if (requireNamespace("minpack.lm", quietly = TRUE)) {
    residual_fn <- function(p) {
      gap <- (p[1] + p[2] * X) - Y
      c(gap, sqrt(penalty) * pmax(0, -gap))
    }
    pol <- tryCatch(
      minpack.lm::nls.lm(par = par, lower = lower, upper = upper, fn = residual_fn,
                         control = minpack.lm::nls.lm.control(maxiter = 200,
                                                              ftol = 1e-12, ptol = 1e-12)),
      error = function(e) NULL)
    if (!is.null(pol) && all(is.finite(pol$par)) && obj(pol$par) <= obj(par)) {
      par <- pol$par
    }
  }

  A <- par[1]; b <- par[2]

  # Guarantee a true envelope: lift the intercept by the largest residual where a
  # point still sits above the fitted line, so the curve clears every point while
  # keeping the optimised slope. The lift is minimal (0 if already feasible).
  lift <- max(0, max(Y - (A + b * X)))
  A <- A + lift

  gap <- (A + b * X) - Y
  list(a = 10^A, b = b,
       n_above     = sum(gap < -1e-6),   # points above the envelope (0 after lift)
       max_gap_log = max(gap),
       mean_gap_log = mean(gap))
}


# ---------------- Hydrograph (regime) helpers ------------------
# Qualitative palette for hydrograph periods (green, purple, blue, orange, ...)
hydro_period_palette <- c("#4DAF4A", "#984EA3", "#377EB8", "#FF7F00",
                          "#E41A1C", "#A65628", "#F781BF", "#666666")

# hex colour -> "rgba(r,g,b,a)" string (for translucent plotly fills)
hex_to_rgba <- function(hex, alpha = 1) {
  v <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3], format(alpha, trim = TRUE))
}

# Per-period, per-day-of-water-year statistics for the average-hydrograph plot.
# - flow_df : data frame with Date, Flow (daily mean discharge)
# - wy_month: water-year start month (1-12); the Month axis begins here
# - splits  : water years at which to START a new period (empty = one period)
# - probs   : c(lower, upper) in [0, 1] for the shaded percentile band
# Returns list(stats, mad, levels, probs):
#   stats : Period, PlotDate, Mean, Lower, Upper, N
#   mad   : Period, MAD (mean of all daily flows in the period), n_years
#   levels: ordered period labels
#   probs : the sorted c(lower, upper) actually used for the band
# Every record is mapped onto one common water-year window (leap year 2000 rolling
# into 2001) via PlotDate so all years overlay on a single Month axis.
hydrograph_stats <- function(flow_df, wy_month = 1, splits = numeric(0),
                             probs = c(0.10, 0.90)) {
  if (is.null(flow_df) || !nrow(flow_df)) return(NULL)
  wy_month <- as.integer(wy_month)
  probs <- sort(probs)

  df <- flow_df %>%
    dplyr::filter(!is.na(Flow)) %>%
    dplyr::mutate(
      WY = hydro_yr(Date, hyrstart = wy_month),
      m  = lubridate::month(Date),
      d  = lubridate::day(Date),
      yr = ifelse(m >= wy_month, 2000L, 2001L),
      # Feb 29 exists only in leap years: keep it in the 2000 (leap) bucket, fold
      # it onto Feb 28 in the 2001 (non-leap) bucket to avoid an invalid date.
      d  = ifelse(m == 2L & d == 29L & yr == 2001L, 28L, d),
      PlotDate = lubridate::make_date(yr, m, d)
    ) %>%
    dplyr::filter(!is.na(PlotDate))
  if (!nrow(df)) return(NULL)

  # Build periods from split years (a split at year Y starts a new period at Y)
  wy_min <- min(df$WY); wy_max <- max(df$WY)
  sp <- sort(unique(splits[is.finite(splits) & splits > wy_min & splits <= wy_max]))
  bounds <- c(wy_min, sp, wy_max + 1)
  labels <- vapply(seq_len(length(bounds) - 1L), function(i) {
    a <- bounds[i]; b <- bounds[i + 1L] - 1L
    if (a == b) as.character(a) else paste0(a, "–", b)
  }, character(1))
  df$Period <- factor(labels[findInterval(df$WY, bounds)], levels = labels)

  stats <- df %>%
    dplyr::group_by(Period, PlotDate) %>%
    dplyr::summarise(
      Mean  = mean(Flow),
      Lower = stats::quantile(Flow, probs[1], names = FALSE, type = 7),
      Upper = stats::quantile(Flow, probs[2], names = FALSE, type = 7),
      N     = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Period, PlotDate)

  mad <- df %>%
    dplyr::group_by(Period) %>%
    dplyr::summarise(MAD = mean(Flow), n_years = dplyr::n_distinct(WY), .groups = "drop")

  list(stats = stats, mad = mad, levels = labels, probs = probs)
}

# 10 -> "10th", 21 -> "21st"; used to label the percentile band in legends.
ordinal_pct <- function(x) {
  n <- round(as.numeric(x))
  sfx <- ifelse(n %% 100L %in% 11:13, "th",
                c("th", "st", "nd", "rd", rep("th", 6))[(n %% 10L) + 1L])
  paste0(n, sfx)
}

# Map a set of dates onto one common water-year window (day-of-water-year) for
# overlaying a single year on the regime hydrograph's Month axis. Mirrors the
# PlotDate logic inside hydrograph_stats().
plot_doy_date <- function(dates, wy_month = 1) {
  wy_month <- as.integer(wy_month)
  dates <- as.Date(dates)
  m <- lubridate::month(dates); d <- lubridate::day(dates)
  yr <- ifelse(m >= wy_month, 2000L, 2001L)
  d  <- ifelse(m == 2L & d == 29L & yr == 2001L, 28L, d)
  lubridate::make_date(yr, m, d)
}


# ============================================================================
#  Realtime / provisional Unit Value Data  (EC collaboration site)
#  Source: https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/UnitValueData/
#  Layout: {Discharge,Stage}/{corrected,raw}/<region2>/
#          <Param>.Working@<STATION>.<YYYYMMDD>_<type>.csv.xz   (xz-compressed)
#  These are PROVISIONAL sub-daily ("unit value") data that bridge the gap
#  between the published HYDAT record and the present. They are NOT used in the
#  flood frequency analysis or the regional analysis.
# ============================================================================

UV_BASE_URL <- "https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/UnitValueData"

# Landing page of the WSC collaboration site as text lines, or the error condition
# if it can't be reached. Some hosts can't reach this site and requests hang until
# they time out, which blocks every session on the R process, so the wait is short
# and the result (success or failure) is reused for an hour.
.wsc_page_cache <- new.env(parent = emptyenv())
wsc_collab_page <- function(timeout_s = 8, ttl_hours = 1) {
  if (!is.null(.wsc_page_cache$at) &&
      difftime(Sys.time(), .wsc_page_cache$at, units = "hours") < ttl_hours) {
    return(.wsc_page_cache$page)
  }
  page <- tryCatch({
    res <- httr::GET("https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/",
                     httr::timeout(timeout_s))
    httr::stop_for_status(res)
    strsplit(httr::content(res, as = "text", encoding = "UTF-8"), "\n", fixed = TRUE)[[1]]
  }, error = function(e) {
    message("WSC collaboration site unreachable: ", conditionMessage(e))
    e
  })
  .wsc_page_cache$at <- Sys.time()
  .wsc_page_cache$page <- page
  page
}

# Grading Codes from UnitValueData_Disclaimer.pdf
UV_GRADE_CODES <- c("-2" = "Unusable", "-1" = "Unspecified", "0" = "Undefined",
                    "10" = "Ice", "20" = "Estimated", "30" = "Partial Day",
                    "40" = "Dry", "50" = "Revised")

# A distinct colour per grade flag for the daily time-series plot.
UV_GRADE_COLORS <- c(
  "Unspecified" = "#1F78B4",  # normal record (blue)
  "Ice"         = "#56B4E9",  # light blue
  "Estimated"   = "#E41A1C",  # red
  "Partial Day" = "#4DAF4A",  # green
  "Dry"         = "#FF7F00",  # orange
  "Revised"     = "#984EA3",  # purple
  "Undefined"   = "#A6761D",  # brown
  "Unusable"    = "#333333"   # near-black
)

# Grade code (integer) -> flag name; anything unknown/NA defaults to "Unspecified".
uv_grade_name <- function(code) {
  code <- suppressWarnings(as.integer(code))
  nm <- UV_GRADE_CODES[as.character(code)]
  nm[is.na(nm)] <- "Unspecified"
  unname(nm)
}

# Read a remote text listing/URL, always closing the connection.
uv_read_url <- function(u) {
  con <- url(u)
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  readLines(con, warn = FALSE)
}

# Resolve the exact remote file URL for a station's unit-value file (the date
# stamp in the name varies per station, so it must be read from the directory
# listing). Returns NULL if the station has no file of that type.
uv_file_url <- function(station_id, type = c("corrected", "raw"), param = "Discharge") {
  type   <- match.arg(type)
  region <- substr(station_id, 1, 2)
  dir_url <- sprintf("%s/%s/%s/%s/", UV_BASE_URL, param, type, region)
  listing <- tryCatch(uv_read_url(dir_url), error = function(e) character(0))
  if (!length(listing)) return(NULL)
  pat  <- sprintf("%s\\.Working@%s\\.[0-9]{8}_%s\\.csv\\.xz", param, station_id, type)
  hits <- unique(regmatches(listing, regexpr(pat, listing)))
  hits <- hits[nzchar(hits)]
  if (!length(hits)) return(NULL)
  paste0(dir_url, hits[1])
}

# Download & parse a station's provisional unit-value CSV (.xz). Returns a tibble
# (DateTime in station LOCAL standard time, Value, Grade) or NULL if unavailable.
# Header metadata (generation date, location) is attached as attributes.
fetch_unit_series <- function(station_id, type = c("corrected", "raw"),
                              param = "Discharge") {
  type <- match.arg(type)
  file_url <- uv_file_url(station_id, type, param)
  if (is.null(file_url)) return(NULL)

  tmp <- tempfile(fileext = ".csv.xz")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({ utils::download.file(file_url, tmp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!isTRUE(ok) || !file.exists(tmp) || file.info(tmp)$size == 0) return(NULL)

  # header metadata (first ~14 comment lines)
  hdr <- tryCatch(readLines(xzfile(tmp), n = 14), error = function(e) character(0))
  gen <- {
    h <- hdr[grepl("generated at", hdr)]
    if (length(h)) {
      g <- strsplit(h[1], "generated at ", fixed = TRUE)[[1]]
      # the timestamp that follows is a fixed "YYYY-MM-DD HH:MM:SS" (19 chars)
      if (length(g) >= 2) trimws(substr(g[2], 1, 19)) else NA_character_
    } else NA_character_
  }
  loc <- {
    h <- hdr[grepl("# Location:", hdr)]
    if (length(h)) trimws(sub("# Location:", "", h[1], fixed = TRUE)) else NA_character_
  }
  # station UTC offset, e.g. "# UTC offset: (UTC-08:00)" -> -8 (local standard time)
  off <- {
    h <- hdr[grepl("UTC offset:", hdr)]
    if (length(h)) {
      m <- regmatches(h[1], regexpr("UTC[+-][0-9]{2}", h[1]))
      if (length(m)) suppressWarnings(as.integer(sub("UTC", "", m))) else NA_integer_
    } else NA_integer_
  }

  # data — readr auto-decompresses .xz; "#" comment lines are skipped, so the
  # first non-comment line ("ISO 8601 UTC,...") supplies the header. col_types
  # is applied positionally; columns are renamed below.
  df <- tryCatch(
    readr::read_csv(tmp, comment = "#", col_types = "ccdcic", progress = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) return(NULL)
  names(df) <- c("utc", "local", "Value", "Approval", "Grade", "Qualifiers")[seq_len(ncol(df))]

  out <- df %>%
    dplyr::transmute(
      DateTime = lubridate::ymd_hms(local, quiet = TRUE),
      Value    = suppressWarnings(as.numeric(Value)),
      Grade    = suppressWarnings(as.integer(Grade))
    ) %>%
    dplyr::filter(!is.na(DateTime))
  if (!nrow(out)) return(NULL)
  attr(out, "generated")  <- gen
  attr(out, "location")   <- loc
  attr(out, "utc_offset") <- off
  out
}

# Station local-standard-time UTC offset (hours). Prefer the offset parsed from a
# collaboration-site header; otherwise estimate from longitude (WSC reports daily
# values in local standard time, so a fixed offset — no DST — is appropriate).
station_lst_offset <- function(station_id, header_offset = NULL) {
  if (!is.null(header_offset) && length(header_offset) == 1 && is.finite(header_offset))
    return(as.numeric(header_offset))
  lon <- tryCatch(
    tidyhydat::hy_stations(station_number = station_id,
                           hydat_path = Hydat_Location)$LONGITUDE[1],
    error = function(e) NA_real_)
  if (length(lon) != 1 || is.na(lon)) return(0)
  round(lon / 15)
}

# Fetch recent realtime discharge from the ECCC web service (realtime_ws, ~18 mo
# window) and return it in the same shape as fetch_unit_series(): DateTime in
# station local standard time, Value, Grade. The web service does not populate
# grades, so Grade is NA (-> flagged "Unspecified"). Returns NULL if unavailable.
fetch_realtime_ws <- function(station_id, start_date, end_date, offset_hours = 0) {
  ws <- tryCatch(
    tidyhydat::realtime_ws(station_number = station_id, parameters = 47L,
                           start_date = start_date, end_date = end_date),
    error = function(e) NULL
  )
  if (is.null(ws) || !nrow(ws)) return(NULL)
  ws <- dplyr::filter(ws, Parameter == 47L, !is.na(Value))
  if (!nrow(ws)) return(NULL)
  if (length(offset_hours) != 1 || !is.finite(offset_hours)) offset_hours <- 0

  d_utc <- ws$Date
  attr(d_utc, "tzone") <- "UTC"                       # assert UTC per realtime_ws docs
  out <- tibble::tibble(
    DateTime = d_utc + offset_hours * 3600,            # UTC -> local standard clock
    Value    = suppressWarnings(as.numeric(ws$Value)),
    Grade    = NA_integer_
  ) %>% dplyr::filter(!is.na(DateTime))
  if (!nrow(out)) return(NULL)
  out
}

# Aggregate unit values to DAILY summaries (local calendar day). A day is
# "complete" when it has enough readings spanning ~24 h with no large gaps.
unit_to_daily <- function(unit_df, min_span_hours = 23, min_n = 20, max_gap_hours = 3) {
  if (is.null(unit_df) || !nrow(unit_df)) return(NULL)
  unit_df %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::arrange(DateTime) %>%
    dplyr::mutate(Date = as.Date(DateTime)) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(
      Flow   = mean(Value),
      Min    = min(Value),
      Max    = max(Value),
      N      = dplyr::n(),
      span_h = as.numeric(difftime(max(DateTime), min(DateTime), units = "hours")),
      gap_h  = { dd <- diff(as.numeric(DateTime)) / 3600; if (length(dd)) max(dd) else Inf },
      GradeCode = {
        g <- Grade[!is.na(Grade)]
        if (length(g)) as.integer(names(sort(table(g), decreasing = TRUE))[1]) else NA_integer_
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Complete = N >= min_n & span_h >= min_span_hours & gap_h <= max_gap_hours,
      Flag     = uv_grade_name(GradeCode)
    )
}

# Build the realtime DAILY series that EXTENDS beyond HYDAT's last day.
# Priority (per project spec): HYDAT > corrected > raw > realtime web service.
# Only complete days after `hydat_last_date` are kept; corrected takes precedence,
# raw fills the remaining gap, and the ECCC realtime web service (realtime_ws,
# ~18-month window) fills the recent tail the static archive can't reach yet.
# Returns list(daily, meta) or NULL when nothing is available. `progress` is an
# optional shiny::Progress object.
get_realtime_daily <- function(station_id, hydat_last_date, param = "Discharge",
                               progress = NULL, use_ws = TRUE) {
  step <- function(msg, v) if (!is.null(progress)) progress$set(message = msg, value = v)

  # Skip the archive when the site is unreachable; each listing would otherwise
  # stall for the full 60 s connection timeout before the web service is tried.
  archive_ok <- !inherits(wsc_collab_page(), "error")
  corr_u <- NULL
  raw_u  <- NULL
  if (archive_ok) {
    step("Fetching corrected unit data…", 0.10)
    corr_u <- tryCatch(fetch_unit_series(station_id, "corrected", param), error = function(e) NULL)
    step("Fetching raw unit data…", 0.35)
    raw_u  <- tryCatch(fetch_unit_series(station_id, "raw", param), error = function(e) NULL)
  }

  step("Aggregating to daily…", 0.55)
  corr_d <- unit_to_daily(corr_u)
  raw_d  <- unit_to_daily(raw_u)

  last <- suppressWarnings(as.Date(hydat_last_date))
  if (length(last) != 1 || is.na(last)) last <- as.Date("1800-01-01")

  # keep only complete days strictly after `cutoff`, tagged with a source label
  keep_after <- function(d, src, cutoff) {
    if (is.null(d)) return(NULL)
    d <- dplyr::filter(d, Complete, Date > cutoff)
    if (!nrow(d)) return(NULL)
    dplyr::mutate(d, Source = src)
  }

  # Layers 2 & 3: corrected (priority), then raw filling the remaining dates.
  corr_d <- keep_after(corr_d, "Corrected", last)
  raw_d  <- keep_after(raw_d,  "Raw", last)
  merged <- corr_d
  if (!is.null(raw_d)) {
    add    <- if (is.null(corr_d)) raw_d else dplyr::filter(raw_d, !(Date %in% corr_d$Date))
    merged <- dplyr::bind_rows(merged, add)
  }

  # Layer 4: ECCC realtime web service fills the recent tail (from the day after
  # the last archive day, up to today) that the static archive can't reach yet.
  ws_gen <- NA_character_
  if (isTRUE(use_ws) && identical(param, "Discharge")) {
    archive_last <- if (!is.null(merged) && nrow(merged)) max(merged$Date) else last
    ws_start <- max(archive_last, Sys.Date() - 545)   # web-service window ~18 months
    if (ws_start <= Sys.Date()) {
      step("Fetching realtime web service…", 0.80)
      offset_hours <- station_lst_offset(
        station_id,
        header_offset = if (!is.null(corr_u)) attr(corr_u, "utc_offset")
                        else if (!is.null(raw_u)) attr(raw_u, "utc_offset") else NULL)
      ws_u <- tryCatch(
        fetch_realtime_ws(station_id, format(ws_start), format(Sys.Date() + 1), offset_hours),
        error = function(e) NULL)
      ws_d <- keep_after(unit_to_daily(ws_u), "Realtime (WS)", archive_last)
      if (!is.null(ws_d) && nrow(ws_d)) {
        merged <- dplyr::bind_rows(merged, ws_d)
        ws_gen <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      }
    }
  }

  if (is.null(merged) || !nrow(merged)) return(NULL)
  merged <- merged %>%
    dplyr::arrange(Date) %>%
    dplyr::select(Date, Flow, Min, Max, N, Flag, Source)

  meta <- list(
    station    = station_id,
    hydat_last = last,
    date_range = range(merged$Date),
    n_days     = nrow(merged),
    sources    = unique(merged$Source),     # chronological first-seen order
    archive_ok = archive_ok,
    corr_gen   = if (!is.null(corr_u)) attr(corr_u, "generated") else NA_character_,
    raw_gen    = if (!is.null(raw_u))  attr(raw_u,  "generated") else NA_character_,
    ws_gen     = ws_gen,
    location   = if (!is.null(corr_u)) attr(corr_u, "location")
                 else if (!is.null(raw_u)) attr(raw_u, "location") else NA_character_
  )
  step("Done", 1)
  list(daily = merged, meta = meta)
}

# Summarize the realtime DAILY series to Daily/Monthly/Yearly, mirroring
# Dataset_Summarize()'s column layout, plus Flag (PDF grade) and Source, so it
# can be bound onto the HYDAT summary table.
Dataset_Summarize_realtime <- function(daily_df, summary_reso, wy_month) {
  if (is.null(daily_df) || !nrow(daily_df)) return(NULL)
  dominant <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
  }

  TS <- daily_df %>%
    dplyr::mutate(
      Year = lubridate::year(Date), Month = lubridate::month(Date),
      Day  = lubridate::day(Date),
      WaterYear = hydro_yr(Date, hyrstart = wy_month)
    ) %>%
    dplyr::rename(Flow_cms = Flow)

  if (summary_reso == "Daily") {
    TS %>% dplyr::transmute(Date, Year, WaterYear, Month, Day,
                            Flow_cms = round(Flow_cms, 3), Flag, Source)
  } else if (summary_reso == "Monthly") {
    TS %>%
      dplyr::group_by(Year, Month) %>%
      dplyr::summarise(Average_Flow_cms = round(mean(Flow_cms), 3),
                       Count = dplyr::n(), WaterYear = max(WaterYear),
                       Flag = dominant(Flag), Source = dominant(Source),
                       .groups = "drop") %>%
      dplyr::relocate(WaterYear, .after = Year)
  } else { # Yearly
    TS %>%
      dplyr::group_by(WaterYear) %>%
      dplyr::summarise(Average_Flow_cms = round(mean(Flow_cms), 3),
                       Max_Daily_cms = round(max(Max), 3),
                       Min_Daily_cms = round(min(Min), 3),
                       Count = dplyr::n(),
                       Flag = dominant(Flag), Source = dominant(Source),
                       .groups = "drop")
  }
}


# ============================================================================
#  Leaflet base maps
#  Shared by the Stations Map and the Regionalization mini-map so the two offer
#  the same choices. All four are keyless: CARTO's basemaps (CartoDB.Positron
#  and friends) now require an API key and paint an "API KEY REQUIRED"
#  watermark over every tile without one, so they are not used here.
# ============================================================================

# Group names, in the order added. The first is the default shown on load.
BASE_GROUPS <- c("Esri Topo", "OpenTopoMap", "Esri Imagery", "Sentinel-2 Satellite")

# Add the four selectable base maps to a leaflet map. Sentinel-2 cloudless is a
# cloud-free ESA/Copernicus mosaic served by EOX; NRCan has no free satellite
# tile service (its imagery is download-only via EODMS).
add_base_layers <- function(map) {
  map %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Esri Topo") %>%
    leaflet::addProviderTiles("OpenTopoMap",       group = "OpenTopoMap") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://tiles.maps.eox.at/wmts/1.0.0/",
                           "s2cloudless-2021_3857/default/g/{z}/{y}/{x}.jpg"),
      attribution = paste("Sentinel-2 cloudless 2021 by EOX IT Services GmbH",
                          "(Contains modified Copernicus Sentinel data 2021)"),
      group       = "Sentinel-2 Satellite",
      options     = leaflet::tileOptions(maxNativeZoom = 15, maxZoom = 18))
}

# The base-map switcher control (top-right), matching add_base_layers().
add_base_switcher <- function(map) {
  map %>% leaflet::addLayersControl(
    baseGroups = BASE_GROUPS,
    options    = leaflet::layersControlOptions(collapsed = TRUE),
    position   = "topright")
}


# ============================================================================
#  Catchment (drainage-basin) boundary
#  Source: WSC National Hydrometric Network Basin Polygons (collaboration site),
#  GeoPackage per Major Drainage Area: <db dir>/BasinPolygons/MDA_ADP_<NN>.gpkg
#  (NN = first two chars of the station number). Each gpkg has layers
#  DrainageBasin_BassinDeDrainage (polygon, key StationNum), PourPoint_PointExutoire
#  (outlet), Station. Files are ~2.8 GB total so they live beside Hydat.sqlite3,
#  not in git; the feature silently no-ops when the files are absent.
# ============================================================================

# Directory holding the per-MDA basin GeoPackages (next to the HYDAT database).
basin_dir <- function() file.path(dirname(Hydat_Location), "BasinPolygons")

# Read one station's catchment boundary at full resolution, in the GeoPackage's
# native CRS, as a plain sf object (NULL when unavailable). Used for the GIS
# exports, which must not be simplified; a single-feature SQL query, so it stays
# fast even on the 1 GB drainage areas.
read_basin_raw <- function(station_id) {
  if (is.null(station_id) || !nzchar(station_id)) return(NULL)
  gpkg <- file.path(basin_dir(), sprintf("MDA_ADP_%s.gpkg", substr(station_id, 1, 2)))
  if (!file.exists(gpkg)) return(NULL)

  basin <- tryCatch(sf::st_read(
    gpkg, quiet = TRUE,
    query = sprintf('SELECT * FROM "DrainageBasin_BassinDeDrainage" WHERE StationNum = %s',
                    shQuote(station_id))), error = function(e) NULL)
  if (is.null(basin) || !nrow(basin)) return(NULL)
  basin
}

# Map-ready version of the above: simplified (120 m) and reprojected to WGS84.
# Returns a list (basin, area_km2, name) or NULL if unavailable.
read_basin <- function(station_id) {
  basin <- read_basin_raw(station_id)
  if (is.null(basin)) return(NULL)

  basin_m  <- sf::st_transform(basin, 3857)
  basin_ll <- tryCatch(
    sf::st_transform(sf::st_simplify(basin_m, dTolerance = 120, preserveTopology = TRUE), 4326),
    error = function(e) sf::st_transform(basin_m, 4326))

  list(
    basin    = basin_ll,
    area_km2 = if ("Area_km2" %in% names(basin)) round(basin$Area_km2[1], 1) else NA_real_,
    name     = if ("NameNom"  %in% names(basin)) basin$NameNom[1] else NA_character_
  )
}

# The station's own location, from the GeoPackage's "Station" layer, as a WGS84
# point sf (NULL when unavailable). Kept separate from the basin so the exports
# can carry the gauge location alongside the boundary. The sibling
# "PourPoint_PointExutoire" layer holds the outlet snapped to the flow network,
# which sits tens of metres off the gauge -- the gauge is what the app shows, so
# that is what gets exported.
read_basin_station_pt <- function(station_id) {
  if (is.null(station_id) || !nzchar(station_id)) return(NULL)
  gpkg <- file.path(basin_dir(), sprintf("MDA_ADP_%s.gpkg", substr(station_id, 1, 2)))
  if (!file.exists(gpkg)) return(NULL)
  pt <- tryCatch(sf::st_read(
    gpkg, quiet = TRUE,
    query = sprintf('SELECT * FROM "Station" WHERE StationNum = %s',
                    shQuote(station_id))), error = function(e) NULL)
  if (is.null(pt) || !nrow(pt)) return(NULL)
  sf::st_transform(pt, 4326)
}

# Add a station's catchment boundary polygon to a leaflet map or proxy (group
# "catchment"). `b` is the list from read_basin() (NULL -> map unchanged).
# `satellite = TRUE` switches to a yellow outline + light-yellow fill so the
# boundary stays visible over the satellite base maps (blue otherwise).
add_basin_layers <- function(map, b, satellite = FALSE) {
  if (is.null(b) || is.null(b$basin)) return(map)
  lab  <- if (!is.na(b$area_km2)) sprintf("Catchment ≈ %s km²", format(b$area_km2, big.mark = ",")) else "Catchment"
  col  <- if (isTRUE(satellite)) "#FFD400" else "#0033cc"
  fill <- if (isTRUE(satellite)) "#FFF6A8" else "#3366ff"
  fop  <- if (isTRUE(satellite)) 0.18 else 0.10
  map %>% leaflet::addPolygons(
    data = b$basin, group = "catchment",
    stroke = TRUE, weight = 2.5, color = col, opacity = 0.95,
    fill = TRUE, fillColor = fill, fillOpacity = fop, label = lab)
}


# ---- Catchment polygon export (GIS / Google Earth) -------------------------
# The exports use read_basin_raw() (full resolution), NOT the 120 m-simplified
# geometry drawn on the map — an exported boundary is a deliverable and should
# carry the source resolution.

# Tidy, self-describing attribute table for the exported polygon. Field names
# are kept <= 10 characters so the shapefile writer does not have to truncate
# (and so the .shp and .kml carry identical names).
basin_export_sf <- function(basin, station_id, station_name = NA_character_,
                            hydat_da_km2 = NA_real_) {
  poly_area <- if ("Area_km2" %in% names(basin)) round(as.numeric(basin$Area_km2[1]), 2) else NA_real_
  sf::st_sf(
    Station  = station_id,
    StnName  = if (is.na(station_name)) NA_character_ else as.character(station_name),
    PolyAreaKm = poly_area,
    HydatAreKm = if (is.na(hydat_da_km2)) NA_real_ else round(as.numeric(hydat_da_km2), 2),
    Source   = "WSC National Hydrometric Network Basin Polygons (ECCC)",
    Exported = format(Sys.Date(), "%Y-%m-%d"),
    geometry = sf::st_geometry(basin)
  )
}

# Matching attribute table for the station location point. Prefers the basin
# GeoPackage's own Station layer so the point is consistent with the polygon
# dataset, and falls back to the HYDAT coordinates. Returns NULL when neither
# source has a location.
basin_station_export_sf <- function(station_id, station_name = NA_character_,
                                    lat = NA_real_, lon = NA_real_) {
  pt  <- tryCatch(read_basin_station_pt(station_id), error = function(e) NULL)
  src <- "WSC National Hydrometric Network Basin Polygons (ECCC)"
  if (is.null(pt)) {
    if (is.na(lat) || is.na(lon)) return(NULL)
    geom <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    src  <- "HYDAT station coordinates (ECCC)"
  } else {
    geom <- sf::st_geometry(pt)
  }
  xy <- sf::st_coordinates(geom)
  sf::st_sf(
    Station  = station_id,
    StnName  = if (is.na(station_name)) NA_character_ else as.character(station_name),
    Lat      = round(xy[1, 2], 5),
    Lon      = round(xy[1, 1], 5),
    Source   = src,
    Exported = format(Sys.Date(), "%Y-%m-%d"),
    geometry = geom
  )
}

# Write the catchment as a zipped ESRI Shapefile (.shp/.shx/.dbf/.prj/.cpg) at
# `zipfile`. A shapefile holds one geometry type, so the station point ships as a
# second shapefile (<id>_station) in the same zip. Polygon geometry is kept in
# the source CRS so no reprojection error is baked into the GIS deliverable, and
# the point is reprojected to match it so both layers line up. Errors (missing
# `zip` binary) propagate.
write_basin_shp_zip <- function(basin_sf, station_sf, station_id, zipfile) {
  tmp <- file.path(tempdir(), paste0("basin_shp_", station_id, "_",
                                     format(Sys.time(), "%H%M%OS3")))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # ENCODING=UTF-8 writes the .cpg sidecar so accented station names (e.g.
  # "Riviere du Loup") survive the trip into ArcGIS/QGIS.
  sf::st_write(basin_sf, file.path(tmp, paste0(station_id, "_watershed.shp")),
               driver = "ESRI Shapefile", quiet = TRUE,
               layer_options = "ENCODING=UTF-8")
  if (!is.null(station_sf)) {
    sf::st_write(sf::st_transform(station_sf, sf::st_crs(basin_sf)),
                 file.path(tmp, paste0(station_id, "_station.shp")),
                 driver = "ESRI Shapefile", quiet = TRUE,
                 layer_options = "ENCODING=UTF-8")
  }

  parts <- list.files(tmp, full.names = TRUE)
  if (!length(parts)) stop("Shapefile export produced no files.")
  if (!nzchar(Sys.which("zip")))
    stop("No `zip` program found on this machine, so the shapefile cannot be packaged. ",
         "Use the KML export instead.")
  # -j drops the temp directory from the archive paths; -X drops extra attrs.
  owd <- setwd(tmp); on.exit(setwd(owd), add = TRUE, after = FALSE)
  st <- utils::zip(zipfile = zipfile, files = basename(parts), flags = "-j9Xq")
  if (!identical(as.integer(st), 0L)) stop("Packaging the shapefile into a zip failed.")
  invisible(zipfile)
}

# Write the catchment (and the station point) as a KML for Google Earth. KML is
# WGS84-only, so geometry is reprojected. Its driver cannot append layers, but a
# single layer may mix geometry types -- each feature becomes its own Placemark --
# so boundary and gauge ship as two placemarks in one file. GDAL's KML driver
# maps fields literally named Name/Description onto the placemark label and
# balloon, so no layer options are needed.
write_basin_kml <- function(basin_sf, station_sf, station_id, kmlfile) {
  b       <- sf::st_transform(basin_sf, 4326)
  stn_lbl <- if (!is.na(b$StnName[1])) paste0(" - ", b$StnName[1]) else ""

  feats <- list(sf::st_geometry(b)[[1]])
  meta  <- data.frame(
    Name = paste0(station_id, stn_lbl),
    Description = paste0(
      "WSC station ", station_id,
      if (!is.na(b$PolyAreaKm[1])) paste0("<br/>Polygon area: ", b$PolyAreaKm[1], " km2") else "",
      if (!is.na(b$HydatAreKm[1])) paste0("<br/>HYDAT gross drainage area: ", b$HydatAreKm[1], " km2") else "",
      "<br/>Source: ", b$Source[1], "<br/>Exported: ", b$Exported[1]),
    Feature = "Watershed boundary",
    stringsAsFactors = FALSE)

  if (!is.null(station_sf)) {
    p <- sf::st_transform(station_sf, 4326)
    feats <- c(feats, list(sf::st_geometry(p)[[1]]))
    meta  <- rbind(meta, data.frame(
      Name = paste0(station_id, " (WSC station)"),
      Description = paste0(
        "WSC station ", station_id,
        if (!is.na(p$StnName[1])) paste0("<br/>", p$StnName[1]) else "",
        "<br/>Location: ", p$Lat[1], ", ", p$Lon[1], " (WGS84)",
        "<br/>Source: ", p$Source[1], "<br/>Exported: ", p$Exported[1]),
      Feature = "WSC station location",
      stringsAsFactors = FALSE))
  }

  x <- sf::st_sf(meta, geometry = sf::st_sfc(feats, crs = 4326))

  # GDAL requires the .kml extension to pick the driver, so write to a temp file
  # with the right name and copy onto the (extension-less) download target.
  tmp <- file.path(tempdir(), paste0(station_id, "_watershed.kml"))
  on.exit(unlink(tmp), add = TRUE)
  unlink(tmp)
  sf::st_write(x, tmp, driver = "KML", quiet = TRUE)
  if (!file.copy(tmp, kmlfile, overwrite = TRUE)) stop("Writing the KML file failed.")
  invisible(kmlfile)
}

