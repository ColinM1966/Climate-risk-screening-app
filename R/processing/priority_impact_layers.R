# ============================================================
# PRIORITY IMPACT-LAYER DERIVATIONS
# ============================================================
# Starter functions intended to plug into existing terra workflows.
# They do not assume file paths, SSPs or periods.
# ============================================================

library(terra)

# ------------------------------------------------------------
# MONTHS < 100 mm AND MAXIMUM CONSECUTIVE DRY MONTHS
# ------------------------------------------------------------

max_cyclic_true_run <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  x <- as.logical(x)
  x[is.na(x)] <- FALSE
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (all(x)) return(n)
  y <- c(x, x)
  r <- rle(y)
  longest <- max(r$lengths[r$values], 0)
  min(longest, n)
}

calc_drymonth100 <- function(monthly_precip) {
  stopifnot(inherits(monthly_precip, "SpatRaster"))
  if (nlyr(monthly_precip) != 12) stop("Expected 12 monthly precipitation layers.")
  out <- app(monthly_precip, function(x) {
    if (all(is.na(x))) return(NA_real_)
    sum(x < 100, na.rm = TRUE)
  })
  names(out) <- "DRYMONTH100"
  out
}

calc_condrymonth100 <- function(monthly_precip) {
  stopifnot(inherits(monthly_precip, "SpatRaster"))
  if (nlyr(monthly_precip) != 12) stop("Expected 12 monthly precipitation layers.")
  out <- app(monthly_precip, function(x) max_cyclic_true_run(x < 100))
  names(out) <- "CONDRYMONTH100"
  out
}

# ------------------------------------------------------------
# CLIMATIC WATER DEFICIT
# ------------------------------------------------------------
# This is a CLIMATIC moisture-deficit metric, not soil-water deficit.
# Units are mm when P and PET are in mm/month.

calc_climatic_water_deficit <- function(monthly_precip, monthly_pet) {
  compareGeom(monthly_precip, monthly_pet, stopOnError = TRUE)
  if (nlyr(monthly_precip) != nlyr(monthly_pet)) stop("P and PET need the same number of layers.")
  deficit <- ifel(monthly_pet > monthly_precip, monthly_pet - monthly_precip, 0)
  out <- app(deficit, sum, na.rm = TRUE)
  names(out) <- "CWD_DEFICIT"
  out
}

# ------------------------------------------------------------
# VAPOUR PRESSURE DEFICIT (kPa)
# ------------------------------------------------------------
# With mean temperature and mean RH, actual vapour pressure is estimated
# as es(Tmean) * RHmean/100. This is suitable as a broad climate-exposure
# layer; retain this method in metadata because RHmean is an approximation.

saturation_vapour_pressure_kpa <- function(temp_c) {
  0.6108 * exp((17.27 * temp_c) / (temp_c + 237.3))
}

calc_vpd <- function(temp_c, rh_percent) {
  compareGeom(temp_c, rh_percent, stopOnError = TRUE)
  es <- saturation_vapour_pressure_kpa(temp_c)
  ea <- es * (rh_percent / 100)
  out <- pmax(es - ea, 0)
  names(out) <- paste0("VPD_", seq_len(nlyr(out)))
  out
}

calc_vpd_mean <- function(temp_c, rh_percent) {
  vpd <- calc_vpd(temp_c, rh_percent)
  out <- app(vpd, mean, na.rm = TRUE)
  names(out) <- "VPD_MEAN"
  out
}

calc_vpd_p90 <- function(temp_c, rh_percent) {
  vpd <- calc_vpd(temp_c, rh_percent)
  out <- app(vpd, function(x) {
    if (all(is.na(x))) return(NA_real_)
    as.numeric(quantile(x, 0.90, na.rm = TRUE, names = FALSE))
  })
  names(out) <- "VPD_P90"
  out
}

# ------------------------------------------------------------
# HEAVY-MANUAL-WORK CAPACITY FROM WBGT
# ------------------------------------------------------------
# Empirical broad-scale relationship used in climate/workability studies:
# LC = 100 - 25 * max(0, WBGT - 25)^(2/3), clamped to 0-100.
# Treat as potential work capacity for an acclimatised worker, not realised
# economic productivity.

work_capacity_heavy <- function(wbgt_c) {
  pmax(0, pmin(100, 100 - 25 * pmax(0, wbgt_c - 25)^(2/3)))
}

calc_workability_loss_heavy <- function(daily_wbgt) {
  stopifnot(inherits(daily_wbgt, "SpatRaster"))
  capacity <- work_capacity_heavy(daily_wbgt)
  mean_capacity <- app(capacity, mean, na.rm = TRUE)
  out <- 100 - mean_capacity
  names(out) <- "WORKABILITY_LOSS_HEAVY"
  out
}

calc_workability_lt75_days <- function(daily_wbgt) {
  stopifnot(inherits(daily_wbgt, "SpatRaster"))
  capacity <- work_capacity_heavy(daily_wbgt)
  out <- app(capacity, function(x) {
    if (all(is.na(x))) return(NA_real_)
    sum(x < 75, na.rm = TRUE)
  })
  names(out) <- "WORKABILITY_LT75_DAYS"
  out
}
