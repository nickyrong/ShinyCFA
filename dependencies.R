# This script installs the packages required

## Install and load packages
package_list <- c("shiny", "tidyhydat", "leaflet", "dplyr", "lubridate", "readr",
                  "xts", "dygraphs", "tidyverse", "lmom", "plotly")

missing_list <- package_list %in% rownames(installed.packages())

for (i in package_list) (
        if (missing_list[i] == FALSE) (
                install.packages(package_list[i])
        )
)

## Install HYDAT Database
library(tidyhydat)
download_hydat()