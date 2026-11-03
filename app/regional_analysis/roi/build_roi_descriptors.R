# build_roi_descriptors.R
# -----------------------------------------------------------------------------
# Dev-time precompute (run once; NOT executed at app startup).
#
# Produces `hysets_descriptors.rds`: per-WSC-station catchment descriptors used
# by the Region-of-Influence (ROI/GEO-PHY) donor selection outside BC.
#
# Source: HYSETS (Arsenault et al. 2020, Scientific Data 7:243), watershed
# properties table, hosted on OSF project rpc3w. Only the small static
# properties file is needed here (NOT the multi-GB meteorological NetCDFs).
#   Download: https://osf.io/download/us795/  (HYSETS_watershed_properties.txt)
#
# Descriptor set retained (Durocher et al. 2018 Table 1):
#   da_km2  drainage area          Drainage_Area_km2   (properties file)
#   elev_m  mean basin elevation   Elevation_m         (properties file)
#   slope_deg mean basin slope     Slope_deg           (properties file)
#   gravelius basin compactness    Gravelius           (properties file)
#   waterbody % waterbody area     Land_Use_Water_frac (properties file)
#   wetland   % wetland area       Land_Use_Wetland_frac (properties file)
#   permeability log permeability  Permeability_logk_m2 (properties file)
#   porosity  subsoil porosity     Porosity_frac       (properties file)
#   map_mm  mean annual precip     NRCAN_pr_month       (monthly meteo NetCDF,
#             mean monthly precip x 12; ERA5 fallback where NRCAN is missing)
#
# NOTE: "stream density" (Durocher Table 1) is NOT distributed in any HYSETS
# file. It would require a separate stream-network dataset (e.g. HydroRIVERS)
# intersected with the HYSETS basin polygons; left as a future enhancement.
#
# Run from the app/ directory:  Rscript regional_analysis/roi/build_roi_descriptors.R
# -----------------------------------------------------------------------------

suppressMessages({library(dplyr); library(readr)})

roi_dir  <- "regional_analysis/roi"
raw_txt  <- file.path(roi_dir, "HYSETS_watershed_properties.txt")
meteo_nc <- file.path(roi_dir, "HYSETS_2023_update_monthly_meteorological_data.nc")
out_rds  <- file.path(roi_dir, "hysets_descriptors.rds")
osf_url       <- "https://osf.io/download/us795/"   # watershed properties (3.4 MB)
osf_meteo_url <- "https://osf.io/download/sc4ge/"   # monthly meteo NetCDF (481 MB)

# --- 1. Obtain the raw properties file (download once if absent) -------------
if (!file.exists(raw_txt)) {
  message("Downloading HYSETS watershed properties from OSF ...")
  utils::download.file(osf_url, raw_txt, mode = "wb", quiet = FALSE)
}
stopifnot(file.exists(raw_txt))

h <- readr::read_csv(raw_txt, show_col_types = FALSE, na = c("NaN", "NA", ""))

# --- 1b. Mean annual precipitation (MAP) from the monthly meteo NetCDF -------
# Downloads the 481 MB monthly file once if absent. MAP (mm/yr) per watershed =
# long-term mean monthly precip x 12, from NRCAN (Canadian gridded obs), with
# ERA5 filling any watershed NRCAN does not cover.
map_tbl <- NULL
if (requireNamespace("ncdf4", quietly = TRUE)) {
  if (!file.exists(meteo_nc)) {
    message("Downloading HYSETS monthly meteorological NetCDF (481 MB) from OSF ...")
    utils::download.file(osf_meteo_url, meteo_nc, mode = "wb", quiet = FALSE)
  }
  nc  <- ncdf4::nc_open(meteo_nc)
  wid <- ncdf4::ncvar_get(nc, "watershedID")
  map_from <- function(var) {
    m <- ncdf4::ncvar_get(nc, var)                 # [time, watershed]
    colMeans(m, na.rm = TRUE) * 12                 # mm/yr
  }
  nrcan <- suppressWarnings(map_from("NRCAN_pr_month"))
  era5  <- suppressWarnings(map_from("ERA5_pr_month"))
  ncdf4::nc_close(nc)
  map_mm <- ifelse(is.finite(nrcan), nrcan, era5)
  map_tbl <- tibble::tibble(Watershed_ID = as.integer(wid),
                            map_mm = round(as.numeric(map_mm), 1))
} else {
  message("ncdf4 not installed; skipping MAP (install ncdf4 to add mean annual precip).")
}
if (!is.null(map_tbl)) h <- dplyr::left_join(h, map_tbl, by = "Watershed_ID")

# --- 2. Keep Canadian (HYDAT / WSC) stations, rename to tidy descriptors -----
desc <- h %>%
  dplyr::filter(Source == "HYDAT") %>%
  dplyr::transmute(
    STATION_NUMBER = Official_ID,
    da_km2         = Drainage_Area_km2,
    elev_m         = Elevation_m,
    slope_deg      = Slope_deg,
    gravelius      = Gravelius,
    waterbody      = Land_Use_Water_frac,
    wetland        = Land_Use_Wetland_frac,
    permeability   = Permeability_logk_m2,
    porosity       = Porosity_frac,
    map_mm         = if ("map_mm" %in% names(h)) map_mm else NA_real_,
    hysets_lat     = Hydrometric_station_latitude,
    hysets_lon     = Hydrometric_station_longitude
  ) %>%
  dplyr::filter(!is.na(STATION_NUMBER), !is.na(da_km2), da_km2 > 0) %>%
  dplyr::distinct(STATION_NUMBER, .keep_all = TRUE) %>%
  dplyr::arrange(STATION_NUMBER)

# --- 3. Report coverage & save ----------------------------------------------
saveRDS(desc, out_rds)

descriptor_cols <- c("da_km2","elev_m","slope_deg","gravelius",
                     "waterbody","wetland","permeability","porosity","map_mm")
cat("Saved:", out_rds, "\n")
cat("WSC stations with descriptors:", nrow(desc), "\n")
cat("Per-descriptor completeness:\n")
for (v in descriptor_cols)
  cat(sprintf("  %-13s complete: %d / %d\n", v, sum(!is.na(desc[[v]])), nrow(desc)))
