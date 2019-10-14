# A script to bridge the tidyhat and flowscreen
# the "read.flows" function of FlowScreen{} is intended for CSV files only

library(tidyhydat)

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
}

#### EOF
