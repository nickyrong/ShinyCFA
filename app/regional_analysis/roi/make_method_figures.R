# make_method_figures.R
# -----------------------------------------------------------------------------
# Dev-time: generate the explanatory figures shown in the Regional Analysis
# "Usage" help tab. These are ORIGINAL illustrations of the GEO-PHY ROI method
# (Durocher, Burn & Zadeh 2018) built from this app's real ROI engine and data
# for one worked example - not reproductions of the paper's copyrighted figures.
#
# Outputs (committed): app/www/roi_method_geo_phy.png, app/www/roi_method_regression.png
# Run from the app/ directory:  Rscript regional_analysis/roi/make_method_figures.R
# -----------------------------------------------------------------------------

suppressMessages({library(sf); library(dplyr); library(tibble); library(lmom)
                  library(tidyr); library(lubridate)})
source("functions.R"); source("roi_functions.R")

col_pool <- "#CFD8DC"; col_m1 <- "#5B9BD5"; col_m2 <- "#E15759"; col_tgt <- "#111111"

pool <- build_roi_donor_pool(readRDS("./data/map_data.rds"), roi_load_descriptors())

# Worked example: an ungauged site near Calgary, ~1000 km2.
target <- list(lon = -114.0, lat = 51.0, da_km2 = 1000)
m1 <- 100L; m2 <- 25L; min_years <- 20L
preds <- c("log_da", "map_mm", "elev_m", "slope_deg", "waterbody")

# Reproduce the GEO stage (m1 nearest) and the final m2 donor set.
cand <- pool[!is.na(pool$record_years) & pool$record_years >= min_years, ]
cc   <- Reduce(`&`, lapply(preds, function(d) !is.na(cand[[d]])))
cand <- cand[cc, ]
tpt  <- sf::st_coordinates(sf::st_transform(
          sf::st_sfc(sf::st_point(c(target$lon, target$lat)), crs = 4326), ROI_GEO_CRS)) / 1000
cand$GEO <- sqrt((cand$X_km - tpt[1])^2 + (cand$Y_km - tpt[2])^2)
geo   <- cand[order(cand$GEO), ][seq_len(m1), ]
donors <- build_roi_pool(target, pool, descriptors = preds, m1 = m1, m2 = m2, min_years = min_years)
is_m2 <- geo$STATION_NUMBER %in% donors$STATION_NUMBER

# ============================================================================
# FIGURE 1 - two-stage donor selection (GEO space + descriptor space)
# ============================================================================
png("www/roi_method_geo_phy.png", width = 1000, height = 460, res = 110)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1), xpd = NA)

## Panel A: geographic space
xl <- range(geo$LONGITUDE); yl <- range(geo$LATITUDE)
xpad <- diff(xl) * 0.35; ypad <- diff(yl) * 0.35
xlim <- xl + c(-xpad, xpad); ylim <- yl + c(-ypad, ypad)
inwin <- with(pool, LONGITUDE >= xlim[1] & LONGITUDE <= xlim[2] &
                    LATITUDE  >= ylim[1] & LATITUDE  <= ylim[2])
plot(NA, xlim = xlim, ylim = ylim, xlab = "Longitude (deg)", ylab = "Latitude (deg)",
     main = "", cex.axis = 0.85)
title(main = "Step 1  |  GEO: keep the m1 nearest gauges", cex.main = 1.0, font.main = 2)
points(pool$LONGITUDE[inwin], pool$LATITUDE[inwin], pch = 16, cex = 0.5, col = col_pool)
points(geo$LONGITUDE[!is_m2], geo$LATITUDE[!is_m2], pch = 16, cex = 0.8, col = col_m1)
points(geo$LONGITUDE[is_m2],  geo$LATITUDE[is_m2],  pch = 16, cex = 1.0, col = col_m2)
points(target$lon, target$lat, pch = 8, cex = 1.8, lwd = 2, col = col_tgt)
legend("topright", bty = "n", cex = 0.72, pt.cex = c(0.7, 0.9, 1.1, 1.4),
       pch = c(16, 16, 16, 8), col = c(col_pool, col_m1, col_m2, col_tgt),
       legend = c("all non-BC gauges", expression(m[1]*" nearest"),
                  expression(m[2]*" final donors"), "target site"))

## Panel B: catchment-descriptor space (log area vs mean annual precip)
gx <- log10(geo$da_km2); gy <- geo$map_mm
tx <- log10(target$da_km2); ty <- mean(geo$map_mm[is_m2], na.rm = TRUE)
plot(NA, xlim = range(gx, tx, finite = TRUE), ylim = range(gy, ty, finite = TRUE),
     xlab = expression("log"[10]*" drainage area  (km"^2*")"),
     ylab = "Mean annual precipitation (mm)", main = "", cex.axis = 0.85)
title(main = "Step 2  |  PHY: keep the m2 most similar", cex.main = 1.0, font.main = 2)
points(gx[!is_m2], gy[!is_m2], pch = 16, cex = 0.8, col = col_m1)
points(gx[is_m2],  gy[is_m2],  pch = 16, cex = 1.0, col = col_m2)
points(tx, ty, pch = 8, cex = 1.8, lwd = 2, col = col_tgt)
legend("topright", bty = "n", cex = 0.72, pt.cex = c(0.9, 1.1, 1.4),
       pch = c(16, 16, 8), col = c(col_m1, col_m2, col_tgt),
       legend = c(expression(m[1]*" subset"), expression(m[2]*" by Mahalanobis dist."),
                  "target site"))
par(op); dev.off()

# ============================================================================
# FIGURE 2 - regional flood regression at the target
# ============================================================================
Tr <- 100
res <- estimate_flood_quantile(target, donors, pool, return_periods = Tr,
                               reg_predictors = "log_da", series = "daily", dist = "GEV")
dd <- res$donors %>% filter(ReturnPeriod == Tr, is.finite(Q), Q > 0) %>%
  left_join(pool %>% select(Station = STATION_NUMBER, da_km2, record_years), by = "Station")
est <- res$estimate

png("www/roi_method_regression.png", width = 760, height = 500, res = 110)
par(mar = c(4.4, 4.6, 3, 1), xpd = FALSE)
# y-range must include the (wide) prediction interval, not just the donor points
yr <- range(c(dd$Q, est$lower, est$upper), na.rm = TRUE) * c(0.9, 1.1)
plot(dd$da_km2, dd$Q, log = "xy", pch = 16, ylim = yr,
     cex = 0.6 + 1.6 * (dd$record_years / max(dd$record_years)),
     col = adjustcolor(col_m2, 0.7),
     xlab = expression("Drainage area  (km"^2*")"),
     ylab = expression("Donor 100-year flood  Q"[100]*"  (m"^3*"/s)"),
     main = "", cex.axis = 0.85)
title(main = "Weighted regression of donor floods on drainage area", cex.main = 1.0)
# weighted fit line (log-log)
w <- pmax(dd$record_years, 1)
fit <- lm(log10(Q) ~ log10(da_km2), data = dd, weights = w)
xs <- 10^seq(log10(min(dd$da_km2)), log10(max(dd$da_km2)), length.out = 50)
lines(xs, 10^predict(fit, data.frame(da_km2 = xs)), col = "#333333", lwd = 2)
# target estimate + prediction interval
abline(v = target$da_km2, lty = 3, col = "#888888")
arrows(target$da_km2, est$lower, target$da_km2, est$upper, code = 3, angle = 90,
       length = 0.05, col = col_tgt, lwd = 1.6)
points(target$da_km2, est$Q_est, pch = 8, cex = 1.9, lwd = 2, col = col_tgt)
legend("topleft", bty = "n", cex = 0.75,
       pch = c(16, NA, 8), lty = c(NA, 1, NA), lwd = c(NA, 2, 2),
       col = c(adjustcolor(col_m2, 0.7), "#333333", col_tgt),
       pt.cex = c(1.2, NA, 1.6),
       legend = c("donor at-site Q100 (size = record length)",
                  "record-length-weighted fit",
                  "target estimate + 90% pred. interval"))
dev.off()

cat("Wrote www/roi_method_geo_phy.png and www/roi_method_regression.png\n")
cat(sprintf("Example: target DA=%d km2, %d donors; Q100 = %.0f m3/s (90%% PI %.0f-%.0f)\n",
            target$da_km2, nrow(donors), est$Q_est, est$lower, est$upper))
