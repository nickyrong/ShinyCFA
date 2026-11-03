# roi_functions.R
# =============================================================================
# Automated regionalization for the "Regionalization" sub-tab.
#
# Given a target location (lat/lon, optionally with catchment descriptors), pick
# a pooling group of WSC stations two ways:
#
#   * Inside BC  -> the BC hydrologic zone that encloses the point, plus every
#                   zone within a user distance (uses the BC Streamflow Inventory
#                   zone polygons). Stations come from the zone catalog CSV.
#
#   * Outside BC -> a Region-of-Influence (ROI) donor group, GEO-PHY method of
#                   Durocher, Burn & Zadeh (2018): narrow to the m1 geographically
#                   nearest gauges, then rank that subset by Mahalanobis distance
#                   on standardized catchment descriptors (HYSETS) and keep m2.
#
# Descriptor source: hysets_descriptors.rds (see regional_analysis/roi/).
# =============================================================================

# --- Descriptor configuration ------------------------------------------------
# Physiographic descriptors available for PHY (Mahalanobis) distance. `log_da`
# is derived from da_km2 at runtime. Default active set balances coverage
# (~2,365 of 2,375 stations) against physical meaning; `gravelius` is optional
# (adds compactness but ~20% of stations lack it).
ROI_DESCRIPTOR_LABELS <- c(
  log_da    = "Drainage area (log10)",
  map_mm    = "Mean annual precip",
  elev_m    = "Mean elevation",
  slope_deg = "Mean slope",
  waterbody = "Waterbody fraction",
  wetland   = "Wetland fraction",
  gravelius = "Compactness (Gravelius)",
  permeability = "Permeability (log k)",
  porosity  = "Subsoil porosity"
)
ROI_DEFAULT_DESCRIPTORS <- c("log_da", "map_mm", "elev_m", "slope_deg", "waterbody")

# Raw HYSETS column that backs each descriptor (log_da is derived from da_km2).
.roi_desc_source <- c(log_da = "da_km2", map_mm = "map_mm", elev_m = "elev_m",
                      slope_deg = "slope_deg", waterbody = "waterbody",
                      wetland = "wetland", gravelius = "gravelius",
                      permeability = "permeability", porosity = "porosity")

# Equal-distance projection for Canada (Statistics Canada Lambert). Used for GEO
# distance so nearest-neighbour ranking is in metres, not lat/lon degrees.
ROI_GEO_CRS <- 3347
# BC Albers, for point-to-zone distances in the BC branch.
BC_ALBERS_CRS <- 3005

# --- Load the committed descriptor table -------------------------------------
roi_load_descriptors <- function() {
  cands <- c("./regional_analysis/roi/hysets_descriptors.rds",
             "regional_analysis/roi/hysets_descriptors.rds",
             "app/regional_analysis/roi/hysets_descriptors.rds")
  hit <- cands[file.exists(cands)]
  if (!length(hit)) return(NULL)
  tryCatch(readRDS(hit[1]), error = function(e) NULL)
}

# Add derived descriptor columns (log_da) to a descriptor data frame.
.roi_add_derived <- function(df) {
  df$log_da <- ifelse(!is.na(df$da_km2) & df$da_km2 > 0, log10(df$da_km2), NA_real_)
  df
}

# --- Build the outside-BC donor pool (call once at startup) ------------------
# Joins HYSETS descriptors to the app's station metadata (coords, province,
# record length), keeps non-BC stations with a descriptor row, and pre-projects
# coordinates to ROI_GEO_CRS so per-query GEO distance is a cheap vector op.
build_roi_donor_pool <- function(map_data, descriptors = roi_load_descriptors()) {
  if (is.null(descriptors) || is.null(map_data)) return(NULL)

  meta <- map_data %>%
    dplyr::transmute(
      STATION_NUMBER,
      STATION_NAME,
      PROV = PROV_TERR_STATE_LOC,
      LATITUDE, LONGITUDE,
      record_years = suppressWarnings(as.numeric(Qn))
    )

  pool <- descriptors %>%
    dplyr::inner_join(meta, by = "STATION_NUMBER") %>%
    dplyr::filter(PROV != "BC", !is.na(LATITUDE), !is.na(LONGITUDE)) %>%
    .roi_add_derived()

  if (!nrow(pool)) return(NULL)

  # Pre-project coordinates (km) in the equal-distance CRS.
  xy <- sf::st_coordinates(
    sf::st_transform(
      sf::st_as_sf(pool, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE),
      ROI_GEO_CRS))
  pool$X_km <- xy[, 1] / 1000
  pool$Y_km <- xy[, 2] / 1000
  pool
}

# --- Core: GEO-PHY ROI pooling ----------------------------------------------
# target: list/data.frame with `lon`, `lat`, and optionally raw descriptor
#         values (da_km2, elev_m, slope_deg, waterbody, ...). Missing descriptors
#         are filled from the m1 nearest neighbours (locally-informed estimate).
# pool:   output of build_roi_donor_pool().
# Returns a tibble of donors: STATION_NUMBER, STATION_NAME, LAT/LON,
#         GEO_dist_km, PHY_dist, rank (1 = most similar), + record_years.
build_roi_pool <- function(target, pool,
                           descriptors = ROI_DEFAULT_DESCRIPTORS,
                           m1 = 100, m2 = 25, min_years = 20,
                           exclude_ids = character(0)) {
  if (is.null(pool) || !nrow(pool)) return(NULL)
  descriptors <- intersect(descriptors, names(ROI_DESCRIPTOR_LABELS))
  if (!length(descriptors)) descriptors <- ROI_DEFAULT_DESCRIPTORS

  # 1. Candidate filter: record length, complete active descriptors, exclusions.
  cand <- pool
  if (!is.na(min_years) && min_years > 0)
    cand <- cand[!is.na(cand$record_years) & cand$record_years >= min_years, , drop = FALSE]
  if (length(exclude_ids))
    cand <- cand[!cand$STATION_NUMBER %in% exclude_ids, , drop = FALSE]
  complete_rows <- Reduce(`&`, lapply(descriptors, function(d) !is.na(cand[[d]])))
  cand <- cand[complete_rows, , drop = FALSE]
  if (nrow(cand) < 2) return(NULL)

  # 2. GEO distance from target to every candidate (km), keep nearest m1.
  tpt <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(target$lon, target$lat)), crs = 4326), ROI_GEO_CRS)
  txy <- sf::st_coordinates(tpt) / 1000
  cand$GEO_dist_km <- sqrt((cand$X_km - txy[1])^2 + (cand$Y_km - txy[2])^2)
  cand <- cand[order(cand$GEO_dist_km), , drop = FALSE]
  m1 <- max(2L, min(as.integer(m1), nrow(cand)))
  geo <- cand[seq_len(m1), , drop = FALSE]

  # 3. Standardize descriptors over the GEO subset (reference population).
  X <- as.matrix(geo[, descriptors, drop = FALSE])
  ctr <- colMeans(X, na.rm = TRUE)
  scl <- apply(X, 2, stats::sd, na.rm = TRUE)
  scl[!is.finite(scl) | scl == 0] <- 1
  Xs <- scale(X, center = ctr, scale = scl)

  # 4. Target descriptor vector; fill any missing from the GEO-subset mean.
  tvec <- vapply(descriptors, function(d) {
    src <- .roi_desc_source[[d]]
    v <- if (d == "log_da") {
      if (!is.null(target$da_km2) && !is.na(target$da_km2) && target$da_km2 > 0)
        log10(as.numeric(target$da_km2)) else NA_real_
    } else if (!is.null(target[[src]])) as.numeric(target[[src]]) else NA_real_
    v
  }, numeric(1))
  filled <- is.na(tvec)
  if (any(filled)) tvec[filled] <- ctr[filled]           # local mean fallback
  tvec_s <- (tvec - ctr) / scl

  # 5. PHY (Mahalanobis) distance from target to each GEO neighbour, keep m2.
  S <- tryCatch(stats::cov(Xs, use = "complete.obs"), error = function(e) NULL)
  d2 <- tryCatch(
    stats::mahalanobis(Xs, center = tvec_s, cov = S),
    error = function(e) rowSums(sweep(Xs, 2, tvec_s)^2))  # fallback: Euclidean
  geo$PHY_dist <- sqrt(pmax(d2, 0))
  geo <- geo[order(geo$PHY_dist), , drop = FALSE]
  m2 <- max(1L, min(as.integer(m2), nrow(geo)))
  out <- geo[seq_len(m2), , drop = FALSE]
  out$rank <- seq_len(nrow(out))

  tibble::as_tibble(out[, c("STATION_NUMBER", "STATION_NAME", "LATITUDE", "LONGITUDE",
                            "record_years", "GEO_dist_km", "PHY_dist", "rank",
                            "da_km2", "elev_m", "slope_deg")])
}

# --- BC branch: zone lookup by location --------------------------------------
# Returns list(in_bc, enclosing, zones) where `zones` is an integer vector of
# HYDROLOGICZONE_NO within `dist_km` of the point (0 km = enclosing zone only).
bc_zone_lookup <- function(lon, lat, zones_sf, dist_km = 0, in_bc_tol_km = 10) {
  out <- list(in_bc = FALSE, enclosing = NA_integer_, zones = integer(0))
  if (is.null(zones_sf) || is.null(lon) || is.null(lat) ||
      is.na(lon) || is.na(lat)) return(out)

  zn <- suppressWarnings(as.integer(zones_sf$HYDROLOGICZONE_NO))
  pt <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), BC_ALBERS_CRS)
  zp <- sf::st_transform(sf::st_geometry(zones_sf), BC_ALBERS_CRS)

  d_km <- as.numeric(sf::st_distance(pt, zp)) / 1000   # 0 if point inside a zone
  if (!length(d_km) || all(!is.finite(d_km))) return(out)

  inside <- which(d_km <= .Machine$double.eps^0.5)
  if (length(inside)) out$enclosing <- zn[inside[1]]
  out$in_bc <- (min(d_km, na.rm = TRUE) <= in_bc_tol_km)
  if (!out$in_bc) return(out)

  keep <- which(d_km <= max(dist_km, 0) + 1e-6)
  if (!length(keep)) keep <- which.min(d_km)           # always the nearest zone
  out$zones <- sort(unique(zn[keep]))
  out
}

# Station IDs belonging to a set of BC hydrologic zones, via the zone catalog.
bc_zone_station_ids <- function(zone_nos, zone_catalog) {
  if (is.null(zone_catalog) || !length(zone_nos)) return(character(0))
  zone_catalog %>%
    dplyr::filter(as.character(Zone) %in% as.character(zone_nos)) %>%
    dplyr::pull(WSC_ID) %>% unique() %>% sort()
}

# --- BC station pooling group by location ------------------------------------
# For a target point in BC, the pooling group is:
#   * every BC Streamflow Inventory (BCSI) catalog station in the ENCLOSING
#     hydrologic zone (the base region), PLUS
#   * BCSI catalog stations from OTHER zones whose gauge lies within `dist_km`
#     of the point (nearby neighbours, not whole neighbouring zones).
# coords: data.frame(WSC_ID, LONGITUDE, LATITUDE) for the catalog stations.
bc_station_pool <- function(lon, lat, zones_sf, catalog, coords, dist_km = 0,
                            in_bc_tol_km = 10) {
  out <- list(in_bc = FALSE, enclosing = NA_integer_, station_ids = character(0),
              enclosing_ids = character(0), nearby_ids = character(0),
              nearby_zones = integer(0))
  lk <- bc_zone_lookup(lon, lat, zones_sf, dist_km = 0, in_bc_tol_km = in_bc_tol_km)
  out$in_bc <- lk$in_bc
  out$enclosing <- lk$enclosing
  if (!isTRUE(lk$in_bc)) return(out)

  enc_ids <- if (!is.na(lk$enclosing)) bc_zone_station_ids(lk$enclosing, catalog) else character(0)
  out$enclosing_ids <- enc_ids

  nearby_ids <- character(0)
  if (!is.null(coords) && nrow(coords) && !is.null(dist_km) && dist_km > 0) {
    cc <- coords[!is.na(coords$LONGITUDE) & !is.na(coords$LATITUDE), , drop = FALSE]
    if (nrow(cc)) {
      pts <- sf::st_as_sf(cc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
      tpt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
      d_km <- as.numeric(sf::st_distance(pts, tpt)) / 1000
      nearby_ids <- setdiff(cc$WSC_ID[d_km <= dist_km], enc_ids)
    }
  }
  out$nearby_ids <- nearby_ids
  out$station_ids <- sort(unique(c(enc_ids, nearby_ids)))
  if (length(nearby_ids) && !is.null(catalog)) {
    out$nearby_zones <- catalog %>%
      dplyr::filter(WSC_ID %in% nearby_ids) %>%
      dplyr::pull(Zone) %>% unique() %>% sort()
  }
  out
}

# --- Canada boundary gate ----------------------------------------------------
# canada_boundary.rds is a simplified national outline (sfc, EPSG:4326) built
# from Natural Earth 1:50m admin-0 (ne_50m_admin_0_countries, "Canada"),
# st_simplify(dTolerance = 0.01). Used to reject target points outside Canada.
roi_load_canada <- function() {
  cands <- c("./regional_analysis/roi/canada_boundary.rds",
             "regional_analysis/roi/canada_boundary.rds",
             "app/regional_analysis/roi/canada_boundary.rds")
  hit <- cands[file.exists(cands)]
  if (!length(hit)) return(NULL)
  tryCatch(readRDS(hit[1]), error = function(e) NULL)
}

# TRUE if the point is within (or within tol_km of) Canada; NA if the boundary
# is unavailable (callers treat NA as "allow" so the app degrades gracefully).
point_in_canada <- function(lon, lat, canada, tol_km = 5) {
  if (is.null(canada)) return(NA)
  if (is.null(lon) || is.null(lat) || is.na(lon) || is.na(lat)) return(FALSE)
  p <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  inside <- tryCatch(length(sf::st_within(p, canada)[[1]]) > 0, error = function(e) FALSE)
  if (isTRUE(inside)) return(TRUE)
  d <- tryCatch(as.numeric(sf::st_distance(p, canada)), error = function(e) Inf)
  isTRUE(d < tol_km * 1000)
}

# --- Regional flood-quantile estimate at an ungauged site (ROI / GLS) --------
# Durocher et al. (2018) style direct regression of flood quantiles on catchment
# descriptors over the ROI donor group. For each return period, a weighted
# log-log regression log10(Q_T) ~ log10(DA) [+ optional descriptors] is fitted
# across the donors and evaluated at the target, giving a quantile estimate and
# a predictive interval.
#
# Weights approximate GLS: w_i = record_years_i * bisquare(PHY_dist_i). Record
# length is the inverse-variance proxy for the at-site quantile's sampling error;
# the bisquare PHY kernel is the region-of-influence similarity weight (Burn 1990).
#
# Args:
#   target      list(lon, lat, da_km2, ...) - needs da_km2 (the key predictor).
#   donors      build_roi_pool() result (STATION_NUMBER, PHY_dist, record_years).
#   pool        build_roi_donor_pool() result (for donor descriptor lookups).
#   return_periods numeric Tr vector.
#   reg_predictors character subset of ROI descriptor names used as regressors
#                  (default log_da only; extra predictors dropped if donors are
#                  too few to support them). Target values for non-DA predictors
#                  are taken from `target` if supplied, else the donor-pool mean.
#   series/dist/complete_days_threshold  passed to regional_ffa().
# Returns list(estimate = tibble(ReturnPeriod, Q_est, lower, upper, r2, n_used),
#              donors = per-donor at-site quantiles used, predictors, series,
#              dist, level) or list(error = "...").
estimate_flood_quantile <- function(target, donors, pool,
                                     return_periods = c(2, 5, 10, 20, 50, 100, 200),
                                     reg_predictors = "log_da",
                                     series = "daily", dist = "GEV",
                                     complete_days_threshold = NULL,
                                     level = 0.90, progress = NULL) {
  if (is.null(target$da_km2) || is.na(target$da_km2) || target$da_km2 <= 0)
    return(list(error = "Enter the target drainage area (km²) to estimate flood quantiles."))
  if (is.null(donors) || !nrow(donors) || nrow(donors) < 5)
    return(list(error = "Need at least 5 ROI donors for a regional estimate."))

  reg_predictors <- intersect(reg_predictors, names(ROI_DESCRIPTOR_LABELS))
  if (!length(reg_predictors)) reg_predictors <- "log_da"
  reg_predictors <- union("log_da", reg_predictors)   # DA is always a regressor

  # 1. At-site T-year quantiles for each donor (reads HYDAT).
  ids <- donors$STATION_NUMBER
  if (is.function(progress)) progress(0.1, "Reading donor flood records")
  ffa <- tryCatch(
    regional_ffa(ids, return_periods = return_periods, series = series,
                 dist = dist, complete_days_threshold = complete_days_threshold),
    error = function(e) NULL)
  if (is.null(ffa) || !nrow(ffa))
    return(list(error = "Could not compute at-site quantiles for the donor gauges."))

  # 2. Assemble donor descriptors + ROI weights.
  desc_cols <- unique(unlist(lapply(reg_predictors, function(p) .roi_desc_source[[p]])))
  pool_desc <- pool[, c("STATION_NUMBER", intersect(desc_cols, names(pool))), drop = FALSE]
  meta <- donors %>%
    dplyr::select(STATION_NUMBER, PHY_dist, record_years) %>%
    dplyr::left_join(pool_desc, by = "STATION_NUMBER")
  meta$log_da <- log10(meta$da_km2)

  # bisquare PHY kernel * record length
  dref <- max(meta$PHY_dist, na.rm = TRUE) * 1.0001
  kern <- ifelse(meta$PHY_dist < dref, (1 - (meta$PHY_dist / dref)^2)^2, 0)
  meta$w <- pmax(meta$record_years, 1) * pmax(kern, 1e-3)

  # Guard predictor count against donor count (keep ~>= 6 donors per predictor).
  if (length(reg_predictors) > 1 && nrow(meta) < 6 * length(reg_predictors))
    reg_predictors <- "log_da"

  # 3. Target predictor vector (DA from user; others from target or donor mean).
  tvals <- sapply(reg_predictors, function(p) {
    if (p == "log_da") return(log10(target$da_km2))
    src <- .roi_desc_source[[p]]
    v <- if (!is.null(target[[src]]) && is.finite(target[[src]])) as.numeric(target[[src]])
         else mean(meta[[src]], na.rm = TRUE)
    v
  })
  newx <- as.data.frame(as.list(tvals)); names(newx) <- reg_predictors

  # 4. Per-return-period weighted log-log regression + predictive interval.
  tcrit_p <- 1 - (1 - level) / 2
  est <- lapply(sort(unique(return_periods)), function(Tr) {
    dat <- ffa %>% dplyr::filter(ReturnPeriod == Tr, is.finite(Q), Q > 0) %>%
      dplyr::inner_join(meta, by = c("Station" = "STATION_NUMBER"))
    dat <- dat[stats::complete.cases(dat[, c("Q", reg_predictors, "w")]), , drop = FALSE]
    if (nrow(dat) < length(reg_predictors) + 2)
      return(tibble::tibble(ReturnPeriod = Tr, Q_est = NA_real_, lower = NA_real_,
                            upper = NA_real_, r2 = NA_real_, n_used = nrow(dat)))
    dat$logQ <- log10(dat$Q)
    fit <- tryCatch(stats::lm(stats::reformulate(reg_predictors, "logQ"),
                              data = dat, weights = dat$w), error = function(e) NULL)
    if (is.null(fit))
      return(tibble::tibble(ReturnPeriod = Tr, Q_est = NA_real_, lower = NA_real_,
                            upper = NA_real_, r2 = NA_real_, n_used = nrow(dat)))
    pr    <- stats::predict(fit, newdata = newx, se.fit = TRUE)
    sigma <- summary(fit)$sigma
    sepred <- sqrt(pr$se.fit^2 + sigma^2)                 # prediction (not mean) SE
    tcrit  <- stats::qt(tcrit_p, df = max(fit$df.residual, 1))
    tibble::tibble(ReturnPeriod = Tr,
                   Q_est = 10^pr$fit,
                   lower = 10^(pr$fit - tcrit * sepred),
                   upper = 10^(pr$fit + tcrit * sepred),
                   r2 = summary(fit)$r.squared,
                   n_used = nrow(dat))
  })
  if (is.function(progress)) progress(1, "Done")

  list(estimate = dplyr::bind_rows(est),
       donors = ffa, predictors = reg_predictors, series = series,
       dist = dist, level = level, da_km2 = target$da_km2)
}
