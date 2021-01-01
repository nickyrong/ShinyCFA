
# ------------ HYDAT database loading ------------

# Determine Hydat database location
if(file.exists("./database/Hydat.sqlite3")) {
  Hydat_Location <- "./database/Hydat.sqlite3"
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

Dist_Key <- Dist_Options

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
  ) %>%
  
  # Grab drainage area & RHBN status
  left_join(
    tidyhydat::hy_stations(hydat_path = Hydat_Location) %>%
      select(STATION_NUMBER, DRAINAGE_AREA_GROSS, REAL_TIME, RHBN),
    by = "STATION_NUMBER"
  ) %>%
  
  # Round DA number to 2 digits
  mutate_at(vars(DRAINAGE_AREA_GROSS), list(~round(., 2)))

map_data <- tidyhydat::allstations %>%
  left_join(range.df, by = "STATION_NUMBER") %>%
  mutate(text = paste(sep = "<br/>", paste("<b>", STATION_NUMBER, "</b>"),
                      STATION_NAME,
                      HYD_STATUS,
                      paste0("Drainage Area: ", DRAINAGE_AREA_GROSS, " km<sup>2</sup>"),
                      paste0("Benchmark Station: ", RHBN),
                      paste0("Flow Record: from ", Qfrom, " to ", Qto, " (", Qn, " Yrs)"),
                      paste0("Stage Record: from ", Hfrom, " to ", Hto, " (", Hn, " Yrs)")
  ))



# ------------ HYDAT data processing ------------
# Daily Flow Time Series
read_dailyflow <- function(station_number) {
  
  # read Qdaily from HYDAT historical data
  hy_daily <-  tidyhydat::hy_daily_flows(station_number = station_number, 
                                         hydat_path = Hydat_Location) %>%
                  filter(Parameter == 'Flow')
  
  # read Qdaily from real time data
  # unfortunately WSC Datamart only provides most recent 30-days data
  # up to 18-month of real time data can be downloaded from WSC website
  # https://wateroffice.ec.gc.ca/search/real_time_e.html
  
  # Second thought...maybe real time data is not worth it...
  
  #rt_daily <- realtime_dd(station_number) %>% 
  #                realtime_daily_mean() %>% 
  #                filter(Parameter == "Flow") %>%
  #                select(-PROV_TERR_STATE_LOC) %>%
  #                mutate(Symbol = "RealTime")
  
  #Q_daily <- bind_rows(hy_daily, rt_daily) %>%
  
  Q_daily <- hy_daily %>%
    # put the Qdaily into format readable by FlowScreen Package for potential future use
    # Flow parameter: 1 = Flow, 2 = level
    mutate(ID = station_number, PARAM = 1, Agency = "WSC") %>%
    select(ID, PARAM, Date, Flow = Value, SYM = Symbol, Agency)
    
    
  return(Q_daily)
} # EOF read_dailyflow

# function to calculate water year base on date
hydro_yr <- function(., hyrstart) {
  
  # Convert possible character vector into date
  d1 = base::as.Date(.)
  
  df <- tibble(Year = lubridate::year(d1),
               Wyear = Year, #initial set-up
               Month = lubridate::month(d1)
                   )

  ## define hydrologic year based on start month
  if (hyrstart > 6.5) {
    
    MonthsUp <- c(hyrstart:12)
    df$Wyear <- base::ifelse(df$Month %in% MonthsUp, df$Year+1, df$Year)

  } else if (hyrstart == 1) {
    
    df <- df
    
  } else if (hyrstart < 6.5) {
    
    MonthsDown <- c(1:(hyrstart-1))
    df$Wyear <- base::ifelse(df$Month %in% MonthsDown, df$Year-1, df$Year)
    
  }
  
  # Return the water year
  return(df$Wyear)
}

# Station Dataset Summarized
# Uses user input from the resolution dropdown
Dataset_Summarize <- function(station_number, summary_period, wy_month) {
  
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
        mutate(WYear = hydro_yr(Date, hyrstart = wy_month),
               Flag = codes) %>% 
    
        # re-arrange column order to have WYear beside Year
        dplyr::relocate(WYear, .after = Year) %>%
        # Remove Symbol (using Flag instead)
        select(-Symbol) 
  
  if(summary_period == "Daily"){
    TS$Flow_cms <- round(TS$Flow_cms, digits = 3)
  }
  
  else if(summary_period == "Monthly"){
    TS <- TS %>% 
            group_by(Year, Month) %>%
            summarise(Average_Flow_cms = mean(Flow_cms, na.rm = TRUE), 
                      Count = sum(is.na(Flow_cms)==FALSE),
                      .groups = "drop")
    
    TS$Average_Flow_cms <- round(TS$Average_Flow_cms, digits = 3)
  }
  
  # Make sure to group by water year
  else if(summary_period == "Yearly"){
    TS <- TS %>% 
      group_by(WYear) %>%
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
  # using tryCatch to allow the app to continue running if a particular distibution can't be fit to the data
  extremes <- tibble(ReturnPeriods = ReturnPeriods,
                     Pnonexc = Pnonexc,
                     Exponential = tryCatch(error = function(err) {return(error_value)}, quaexp(f = Pnonexc, para = pelexp(lmoms))), # exponential
                     Gamma = tryCatch(error = function(err) {return(error_value)}, quagam(f = Pnonexc, para = pelgam(lmoms))), # gamma
                     GEV = tryCatch(error = function(err) {return(error_value)}, quagev(f = Pnonexc, para = pelgev(lmoms))), # generalized extreme-value
                     `Gen. Logistic` = tryCatch(error = function(err) {return(error_value)}, quaglo(f = Pnonexc, para = pelglo(lmoms))), # generalized logistic
                     `Gen. Normal` = tryCatch(error = function(err) {return(error_value)}, quagno(f = Pnonexc, para = pelgno(lmoms))), # generalized normal
                     `Gen. Pareto` = tryCatch(error = function(err) {return(error_value)}, quagpa(f = Pnonexc, para = pelgpa(lmoms))), # generalized pareto
                     Gumbel = tryCatch(error = function(err) {return(error_value)}, quagum(f = Pnonexc, para = pelgum(lmoms))), # gumbel (extreme value type I)
                     Kappa = tryCatch(error = function(err) {return(error_value)}, quakap(f = Pnonexc, para = pelkap(lmoms))), # kappa
                     Normal = tryCatch(error = function(err) {return(error_value)}, quanor(f = Pnonexc, para = pelnor(lmoms))), # normal
                     `Pearson III` = tryCatch(error = function(err) {return(error_value)}, quape3(f = Pnonexc, para = pelpe3(lmoms))), # pearson type III
                     Wakeby = tryCatch(error = function(err) {return(error_value)}, quawak(f = Pnonexc, para = pelwak(lmoms))), # wakeby
                     Weibull = tryCatch(error = function(err) {return(error_value)}, quawei(f = Pnonexc, para = pelwei(lmoms))), # weibull
                     
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



empirical_ffa <- function(flow_df, complete_years, selected_months){
  
  flow_df %>% 
    
    # only years in the complete years (and after removal)
    # only months selected
    filter(lubridate::year(Date) %in% complete_years,
           lubridate::month(Date) %in% selected_months) %>%
    drop_na(Flow) %>%
    group_by(Year = lubridate::year(Date)) %>%
    filter(Flow == max(Flow)) %>%
    rename(AMS = Flow) %>%
    ungroup() %>%
    mutate(Rank = base::rank(-AMS, ties.method = "random"),
           Tr = ((length(Rank)+1) / Rank)
    ) %>%
    arrange(Rank)
}
  
  