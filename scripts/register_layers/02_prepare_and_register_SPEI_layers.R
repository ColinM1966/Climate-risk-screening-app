# ============================================================
# 02_prepare_and_register_SPEI_layers.R
# Sabah Climate Risk Explorer
#
# PURPOSE
# -------
# Read completed SPEI products directly from:
#
#   D:/CHELSA_CMIP6_SPEI
#
# Extract the app-relevant single-band rasters and write them
# DIRECTLY into:
#
#   C:/Users/User/Documents/Climate-risk-screening-app/rasters/
#
# Then:
#   - validate the outputs
#   - register the absolute SPEI drought-frequency rasters in
#     config/raster_catalogue.csv
#   - add/update variable_metadata.csv
#   - add SPEI-3 and SPEI-6 to the existing drought/fire theme
#   - back up configuration files before editing them
#   - retain change and model-agreement rasters in the app raster
#     folders for later Forestry use, but do NOT register them yet
#
# APP METRIC
# ----------
# The primary SPEI screening metric is:
#
#   percentage of months with SPEI <= -1
#
# SPEI-3 = shorter-term / seasonal drought
# SPEI-6 = more persistent drought
#
# IMPORTANT
# ---------
# The future-minus-baseline change rasters use the SPEI workflow's
# scientifically preferred method:
#
#   model future - model historical
#   THEN ensemble median of model changes
#
# They are therefore copied from the final SPEI screening product,
# not recalculated as:
#
#   ensemble-median future - ensemble-median historical
#
# Run this script from anywhere; paths below are absolute.
#
# Suggested save location:
#   scripts/register_layers/02_prepare_and_register_SPEI_layers.R
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------------------
# 0. SETTINGS
# ------------------------------------------------------------

SOURCE_ROOT <- "D:/CHELSA_CMIP6_SPEI"

APP_ROOT <- "C:/Users/User/Documents/Climate-risk-screening-app"

APP_RASTER_ROOT <- file.path(APP_ROOT, "rasters")
CONFIG_DIR <- file.path(APP_ROOT, "config")
AUDIT_DIR <- file.path(APP_ROOT, "outputs", "audit")
BACKUP_DIR <- file.path(CONFIG_DIR, "backups")

OVERWRITE_APP_RASTERS <- TRUE
UPDATE_CONFIG <- TRUE

# Write the pre-calculated change and model-agreement layers into
# the appropriate future scenario/period folders for later use.
# These are not added to the normal raster catalogue in this script.
WRITE_SUPPORTING_CHANGE_AGREEMENT <- TRUE

NODATA_VALUE <- -9999

SPEI_SCALES <- c(3L, 6L)

FUTURE_SCENARIOS <- c(
  "ssp126",
  "ssp245",
  "ssp370",
  "ssp585"
)

PERIOD_TABLE <- tibble::tribble(
  ~period,      ~ensemble_prefix, ~screening_prefix,
  "2011-2040",  "near",           "2011_2040",
  "2041-2070",  "mid",            "2041_2070",
  "2071-2100",  "late",           "2071_2100"
)

BASELINE_PERIOD <- "1981-2010"

# ------------------------------------------------------------
# 1. BASIC PATH CHECKS
# ------------------------------------------------------------

if (!dir.exists(SOURCE_ROOT)) {
  stop(
    "SPEI source root not found: ",
    SOURCE_ROOT,
    call. = FALSE
  )
}

if (!dir.exists(APP_ROOT)) {
  stop(
    "Climate Risk Explorer project root not found: ",
    APP_ROOT,
    call. = FALSE
  )
}

if (!file.exists(file.path(APP_ROOT, "app.R"))) {
  stop(
    "app.R not found under APP_ROOT. Check APP_ROOT before continuing.",
    call. = FALSE
  )
}

required_config_files <- c(
  "raster_catalogue.csv",
  "variable_metadata.csv",
  "theme_variables.csv",
  "pathway_themes.csv"
)

missing_config <- required_config_files[
  !file.exists(file.path(CONFIG_DIR, required_config_files))
]

if (length(missing_config) > 0) {
  stop(
    "Missing app configuration file(s): ",
    paste(missing_config, collapse = ", "),
    call. = FALSE
  )
}

dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(BACKUP_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(APP_RASTER_ROOT, "baseline"),
           recursive = TRUE, showWarnings = FALSE)

for (ssp in FUTURE_SCENARIOS) {
  for (period in PERIOD_TABLE$period) {
    dir.create(
      file.path(APP_RASTER_ROOT, "future", ssp, period),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
}

normalise_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

# ------------------------------------------------------------
# 2. SOURCE FILE HELPERS
# ------------------------------------------------------------

historical_source_file <- function(scale) {
  file.path(
    SOURCE_ROOT,
    "spei_ensemble_30yr",
    paste0("SPEI", scale),
    paste0(
      "SPEI",
      scale,
      "_historical_1981_2010_ensemble_median.tif"
    )
  )
}

future_ensemble_source_file <- function(scale, ssp) {
  file.path(
    SOURCE_ROOT,
    "spei_ensemble_30yr",
    paste0("SPEI", scale),
    ssp,
    paste0(
      "SPEI",
      scale,
      "_",
      ssp,
      "_30yr_ensemble_median.tif"
    )
  )
}

screening_source_file <- function(scale, ssp) {
  file.path(
    SOURCE_ROOT,
    "spei_screening_Sabah",
    paste0("SPEI", scale),
    paste0(
      "SPEI",
      scale,
      "_",
      ssp,
      "_SCREENING_30yr_Sabah_land.tif"
    )
  )
}

# ------------------------------------------------------------
# 3. APP OUTPUT HELPERS
# ------------------------------------------------------------

baseline_relative_path <- function(scale) {
  file.path(
    "rasters",
    "baseline",
    paste0(
      "SPEI",
      scale,
      "_droughtfreq_historical_1981-2010.tif"
    )
  )
}

future_absolute_relative_path <- function(scale, ssp, period) {
  file.path(
    "rasters",
    "future",
    ssp,
    period,
    paste0(
      "SPEI",
      scale,
      "_droughtfreq_",
      ssp,
      "_",
      period,
      ".tif"
    )
  )
}

future_change_relative_path <- function(scale, ssp, period) {
  file.path(
    "rasters",
    "future",
    ssp,
    period,
    paste0(
      "SPEI",
      scale,
      "_droughtfreq_change_",
      ssp,
      "_",
      period,
      "_vs_1981-2010.tif"
    )
  )
}

future_agreement_relative_path <- function(scale, ssp, period) {
  file.path(
    "rasters",
    "future",
    ssp,
    period,
    paste0(
      "SPEI",
      scale,
      "_models_worse_",
      ssp,
      "_",
      period,
      ".tif"
    )
  )
}

full_app_path <- function(relative_path) {
  file.path(APP_ROOT, relative_path)
}

# ------------------------------------------------------------
# 4. CHECK ALL SOURCE FILES BEFORE WRITING ANYTHING
# ------------------------------------------------------------

source_inventory <- list()
k <- 1L

for (scale in SPEI_SCALES) {

  source_inventory[[k]] <- tibble(
    source_type = "historical_ensemble",
    scale = scale,
    scenario = "baseline",
    source_file = historical_source_file(scale)
  )
  k <- k + 1L

  for (ssp in FUTURE_SCENARIOS) {

    source_inventory[[k]] <- tibble(
      source_type = "future_ensemble",
      scale = scale,
      scenario = ssp,
      source_file = future_ensemble_source_file(scale, ssp)
    )
    k <- k + 1L

    source_inventory[[k]] <- tibble(
      source_type = "screening",
      scale = scale,
      scenario = ssp,
      source_file = screening_source_file(scale, ssp)
    )
    k <- k + 1L
  }
}

source_inventory <- bind_rows(source_inventory) |>
  mutate(
    source_file = normalise_slashes(source_file),
    exists = file.exists(source_file)
  )

missing_source_files <- source_inventory |>
  filter(!exists)

cat("\nSPEI SOURCE FILE CHECK\n")
cat("======================\n")
cat("Expected source files: ", nrow(source_inventory), "\n", sep = "")
cat("Source files found:    ", sum(source_inventory$exists), "\n", sep = "")

if (nrow(missing_source_files) > 0) {

  cat("\nMISSING SPEI SOURCE FILES\n")
  print(missing_source_files, n = Inf)

  readr::write_csv(
    source_inventory,
    file.path(AUDIT_DIR, "SPEI_source_file_audit.csv"),
    na = ""
  )

  stop(
    "\nSPEI preparation stopped because one or more D-drive source ",
    "files are missing. No app rasters or configuration files were changed.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 5. VALIDATE EXPECTED SOURCE LAYERS
# ------------------------------------------------------------

expected_historical_layer <- "hist_pct_spei_le_m1"

expected_screening_layers <- c(
  "2011_2040_change_pctpt",
  "2011_2040_models_worse",
  "2041_2070_change_pctpt",
  "2041_2070_models_worse",
  "2071_2100_change_pctpt",
  "2071_2100_models_worse"
)

source_layer_audit <- list()
k <- 1L

for (scale in SPEI_SCALES) {

  hist_file <- historical_source_file(scale)
  hist_r <- terra::rast(hist_file)

  source_layer_audit[[k]] <- tibble(
    source_type = "historical_ensemble",
    scale = scale,
    scenario = "baseline",
    source_file = normalise_slashes(hist_file),
    layers = terra::nlyr(hist_r),
    expected_layers_present =
      expected_historical_layer %in% names(hist_r),
    layer_names = paste(names(hist_r), collapse = " | ")
  )
  k <- k + 1L

  for (ssp in FUTURE_SCENARIOS) {

    fut_file <- future_ensemble_source_file(scale, ssp)
    fut_r <- terra::rast(fut_file)

    expected_future_layers <- paste0(
      PERIOD_TABLE$ensemble_prefix,
      "_pct_spei_le_m1"
    )

    source_layer_audit[[k]] <- tibble(
      source_type = "future_ensemble",
      scale = scale,
      scenario = ssp,
      source_file = normalise_slashes(fut_file),
      layers = terra::nlyr(fut_r),
      expected_layers_present =
        all(expected_future_layers %in% names(fut_r)),
      layer_names = paste(names(fut_r), collapse = " | ")
    )
    k <- k + 1L

    screen_file <- screening_source_file(scale, ssp)
    screen_r <- terra::rast(screen_file)

    source_layer_audit[[k]] <- tibble(
      source_type = "screening",
      scale = scale,
      scenario = ssp,
      source_file = normalise_slashes(screen_file),
      layers = terra::nlyr(screen_r),
      expected_layers_present =
        all(expected_screening_layers %in% names(screen_r)),
      layer_names = paste(names(screen_r), collapse = " | ")
    )
    k <- k + 1L
  }
}

source_layer_audit <- bind_rows(source_layer_audit)

readr::write_csv(
  source_layer_audit,
  file.path(AUDIT_DIR, "SPEI_source_layer_audit.csv"),
  na = ""
)

bad_source_layers <- source_layer_audit |>
  filter(!expected_layers_present)

if (nrow(bad_source_layers) > 0) {

  cat("\nSOURCE LAYER-NAME PROBLEMS\n")
  print(bad_source_layers, n = Inf)

  stop(
    "\nSPEI preparation stopped because expected bands were not found ",
    "in one or more source rasters. See outputs/audit/",
    "SPEI_source_layer_audit.csv.",
    call. = FALSE
  )
}

cat("All expected SPEI source bands were found.\n")

# ------------------------------------------------------------
# 6. SAFE SINGLE-BAND WRITER
# ------------------------------------------------------------

check_range <- function(product_type, min_value, max_value) {

  if (is.na(min_value) || is.na(max_value)) {
    return(FALSE)
  }

  if (product_type == "absolute_frequency") {
    return(min_value >= -0.001 && max_value <= 100.001)
  }

  if (product_type == "change_percentage_points") {
    return(min_value >= -100.001 && max_value <= 100.001)
  }

  if (product_type == "models_worse") {
    return(min_value >= -0.001 && max_value <= 5.001)
  }

  FALSE
}

write_single_band <- function(
    source_file,
    source_layer,
    destination_relative,
    product_type,
    scale,
    scenario,
    period
) {

  destination_full <- full_app_path(destination_relative)

  dir.create(
    dirname(destination_full),
    recursive = TRUE,
    showWarnings = FALSE
  )

  src <- terra::rast(source_file)

  if (!source_layer %in% names(src)) {
    stop(
      "Required layer '",
      source_layer,
      "' was not found in ",
      source_file,
      call. = FALSE
    )
  }

  x <- src[[source_layer]]

  if (file.exists(destination_full) && !OVERWRITE_APP_RASTERS) {

    message(
      "Output exists; validating without overwrite: ",
      destination_full
    )

  } else {

    terra::writeRaster(
      x,
      destination_full,
      overwrite = TRUE,
      datatype = "FLT4S",
      NAflag = NODATA_VALUE,
      gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
    )
  }

  check <- terra::rast(destination_full)

  mm <- terra::global(
    check,
    fun = c("min", "max"),
    na.rm = TRUE
  )

  min_value <- as.numeric(mm$min[[1]])
  max_value <- as.numeric(mm$max[[1]])

  res_xy <- terra::res(check)

  tibble(
    scale = scale,
    scenario = scenario,
    period = period,
    product_type = product_type,
    source_file = normalise_slashes(source_file),
    source_layer = source_layer,
    output_relative = normalise_slashes(destination_relative),
    output_full = normalise_slashes(destination_full),
    output_exists = file.exists(destination_full),
    raster_opens = TRUE,
    layers = terra::nlyr(check),
    x_resolution = res_xy[[1]],
    y_resolution = res_xy[[2]],
    crs_present = nzchar(terra::crs(check)),
    min_value = min_value,
    max_value = max_value,
    range_ok = check_range(
      product_type,
      min_value,
      max_value
    )
  )
}

# ------------------------------------------------------------
# 7. WRITE BASELINE ABSOLUTE DROUGHT FREQUENCY
# ------------------------------------------------------------

output_audit <- list()
k <- 1L

cat("\nWRITING SPEI APP RASTERS\n")
cat("========================\n")

for (scale in SPEI_SCALES) {

  cat("SPEI-", scale, " baseline\n", sep = "")

  output_audit[[k]] <- write_single_band(
    source_file = historical_source_file(scale),
    source_layer = expected_historical_layer,
    destination_relative = baseline_relative_path(scale),
    product_type = "absolute_frequency",
    scale = scale,
    scenario = "baseline",
    period = BASELINE_PERIOD
  )

  k <- k + 1L
}

# ------------------------------------------------------------
# 8. WRITE FUTURE ABSOLUTE + SUPPORTING CHANGE/AGREEMENT
# ------------------------------------------------------------

for (scale in SPEI_SCALES) {

  for (ssp in FUTURE_SCENARIOS) {

    future_file <- future_ensemble_source_file(scale, ssp)
    screening_file <- screening_source_file(scale, ssp)

    for (i in seq_len(nrow(PERIOD_TABLE))) {

      period <- PERIOD_TABLE$period[[i]]
      ensemble_prefix <- PERIOD_TABLE$ensemble_prefix[[i]]
      screening_prefix <- PERIOD_TABLE$screening_prefix[[i]]

      cat(
        "SPEI-", scale,
        " / ", ssp,
        " / ", period,
        "\n",
        sep = ""
      )

      # --------------------------------------------------------
      # Absolute future drought frequency
      # --------------------------------------------------------

      output_audit[[k]] <- write_single_band(
        source_file = future_file,
        source_layer = paste0(
          ensemble_prefix,
          "_pct_spei_le_m1"
        ),
        destination_relative =
          future_absolute_relative_path(
            scale,
            ssp,
            period
          ),
        product_type = "absolute_frequency",
        scale = scale,
        scenario = ssp,
        period = period
      )

      k <- k + 1L

      # --------------------------------------------------------
      # Scientifically preferred model-by-model change product
      # --------------------------------------------------------

      if (WRITE_SUPPORTING_CHANGE_AGREEMENT) {

        output_audit[[k]] <- write_single_band(
          source_file = screening_file,
          source_layer = paste0(
            screening_prefix,
            "_change_pctpt"
          ),
          destination_relative =
            future_change_relative_path(
              scale,
              ssp,
              period
            ),
          product_type = "change_percentage_points",
          scale = scale,
          scenario = ssp,
          period = period
        )

        k <- k + 1L

        # ------------------------------------------------------
        # Number of the five ESMs showing worsening
        # ------------------------------------------------------

        output_audit[[k]] <- write_single_band(
          source_file = screening_file,
          source_layer = paste0(
            screening_prefix,
            "_models_worse"
          ),
          destination_relative =
            future_agreement_relative_path(
              scale,
              ssp,
              period
            ),
          product_type = "models_worse",
          scale = scale,
          scenario = ssp,
          period = period
        )

        k <- k + 1L
      }
    }
  }
}

output_audit <- bind_rows(output_audit)

readr::write_csv(
  output_audit,
  file.path(AUDIT_DIR, "SPEI_app_raster_audit.csv"),
  na = ""
)

bad_outputs <- output_audit |>
  filter(
    !output_exists |
      !raster_opens |
      layers != 1 |
      !crs_present |
      !range_ok
  )

cat("\nAPP OUTPUT CHECK\n")
cat("================\n")
cat("App rasters written/checked: ", nrow(output_audit), "\n", sep = "")
cat("Valid app rasters:           ",
    nrow(output_audit) - nrow(bad_outputs), "\n", sep = "")

if (nrow(bad_outputs) > 0) {

  cat("\nSPEI APP RASTER PROBLEMS\n")
  print(bad_outputs, n = Inf)

  stop(
    "\nSPEI raster preparation stopped before configuration was modified. ",
    "See outputs/audit/SPEI_app_raster_audit.csv.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 9. BUILD THE 26 ABSOLUTE RASTER CATALOGUE ROWS
# ------------------------------------------------------------
#
# Register:
#   2 baseline rasters
#   2 SPEI scales x 4 SSPs x 3 future periods = 24
#
# Change and model-agreement rasters remain available on disk for
# later Forestry use but are not normal selectable climate variables
# yet.
# ------------------------------------------------------------

variable_lookup <- tibble::tribble(
  ~scale, ~variable_id,          ~display_name,
  3L,     "SPEI3_DROUGHT_FREQ", "SPEI-3 drought frequency",
  6L,     "SPEI6_DROUGHT_FREQ", "SPEI-6 drought frequency"
)

catalogue_rows <- list()
k <- 1L

for (scale in SPEI_SCALES) {

  variable_id <- variable_lookup |>
    filter(scale == !!scale) |>
    pull(variable_id)

  # Baseline
  baseline_rel <- baseline_relative_path(scale)

  baseline_audit <- output_audit |>
    filter(
      .data$scale == scale,
      scenario == "baseline",
      period == BASELINE_PERIOD,
      product_type == "absolute_frequency"
    )

  catalogue_rows[[k]] <- tibble(
    dataset_id = paste0(
      variable_id,
      "_BASE_19812010"
    ),
    variable_id = variable_id,
    scenario = "baseline",
    period = BASELINE_PERIOD,
    file_path = normalise_slashes(baseline_rel),
    file_format = "tif",
    units = "% of months",
    dataset_type = "continuous",
    resolution = paste0(
      format(
        round(baseline_audit$x_resolution[[1]], 6),
        trim = TRUE,
        scientific = FALSE
      ),
      " x ",
      format(
        round(baseline_audit$y_resolution[[1]], 6),
        trim = TRUE,
        scientific = FALSE
      ),
      " degrees"
    ),
    nodata_value = NODATA_VALUE,
    enabled = TRUE
  )

  k <- k + 1L

  # Future
  for (ssp in FUTURE_SCENARIOS) {
    for (period in PERIOD_TABLE$period) {

      rel <- future_absolute_relative_path(
        scale,
        ssp,
        period
      )

      current_audit <- output_audit |>
        filter(
          .data$scale == scale,
          scenario == ssp,
          .data$period == period,
          product_type == "absolute_frequency"
        )

      catalogue_rows[[k]] <- tibble(
        dataset_id = paste0(
          variable_id,
          "_",
          str_to_upper(ssp),
          "_",
          str_replace_all(period, "-", "")
        ),
        variable_id = variable_id,
        scenario = ssp,
        period = period,
        file_path = normalise_slashes(rel),
        file_format = "tif",
        units = "% of months",
        dataset_type = "continuous",
        resolution = paste0(
          format(
            round(current_audit$x_resolution[[1]], 6),
            trim = TRUE,
            scientific = FALSE
          ),
          " x ",
          format(
            round(current_audit$y_resolution[[1]], 6),
            trim = TRUE,
            scientific = FALSE
          ),
          " degrees"
        ),
        nodata_value = NODATA_VALUE,
        enabled = TRUE
      )

      k <- k + 1L
    }
  }
}

catalogue_rows <- bind_rows(catalogue_rows)

if (nrow(catalogue_rows) != 26) {
  stop(
    "Internal script error: expected 26 SPEI catalogue rows, got ",
    nrow(catalogue_rows),
    ".",
    call. = FALSE
  )
}

readr::write_csv(
  catalogue_rows,
  file.path(AUDIT_DIR, "SPEI_catalogue_rows_to_register.csv"),
  na = ""
)

# ------------------------------------------------------------
# 10. VARIABLE METADATA
# ------------------------------------------------------------

metadata_rows <- tibble::tribble(
  ~variable_id,          ~display_name,                ~description,                                                                 ~units,         ~summary_method, ~risk_direction, ~baseline_variable_id, ~classification_method,       ~interpretation,                                                                                                                          ~limitations,
  "SPEI3_DROUGHT_FREQ",  "SPEI-3 drought frequency",  "Percentage of months in the 30-year climatology with SPEI-3 <= -1.",       "% of months",  "mean",          "high",          "SPEI3_DROUGHT_FREQ",  "pending_fixed_thresholds", "Higher values indicate more frequent moderate-or-worse drought conditions accumulated over approximately three months.", "CHELSA-CMIP6-derived five-model ensemble using Hargreaves-Samani PET. SPEI-3 is a climate-screening indicator for shorter-term/seasonal drought and shallow moisture stress; it is not a site-specific drought prediction or ecological impact threshold.",
  "SPEI6_DROUGHT_FREQ",  "SPEI-6 drought frequency",  "Percentage of months in the 30-year climatology with SPEI-6 <= -1.",       "% of months",  "mean",          "high",          "SPEI6_DROUGHT_FREQ",  "pending_fixed_thresholds", "Higher values indicate more frequent moderate-or-worse drought conditions accumulated over approximately six months.",   "CHELSA-CMIP6-derived five-model ensemble using Hargreaves-Samani PET. SPEI-6 is a climate-screening indicator for more persistent drought and longer-duration moisture stress; it is not a site-specific drought prediction or ecological impact threshold."
)

# ------------------------------------------------------------
# 11. CONFIG HELPERS
# ------------------------------------------------------------

read_config <- function(filename) {
  readr::read_csv(
    file.path(CONFIG_DIR, filename),
    show_col_types = FALSE
  )
}

upsert_rows <- function(existing, replacement, keys) {

  missing_existing <- setdiff(keys, names(existing))
  missing_replacement <- setdiff(keys, names(replacement))

  if (length(missing_existing) > 0) {
    stop(
      "Existing config table is missing key(s): ",
      paste(missing_existing, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_replacement) > 0) {
    stop(
      "Replacement table is missing key(s): ",
      paste(missing_replacement, collapse = ", "),
      call. = FALSE
    )
  }

  kept <- existing |>
    anti_join(
      replacement |>
        distinct(across(all_of(keys))),
      by = keys
    )

  out <- bind_rows(
    kept,
    replacement
  )

  preferred_order <- c(
    names(existing),
    setdiff(names(out), names(existing))
  )

  out |>
    select(all_of(preferred_order))
}

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

backup_config <- function(filename) {

  source_path <- file.path(
    CONFIG_DIR,
    filename
  )

  backup_path <- file.path(
    BACKUP_DIR,
    paste0(
      tools::file_path_sans_ext(filename),
      "_before_SPEI_",
      timestamp,
      ".",
      tools::file_ext(filename)
    )
  )

  ok <- file.copy(
    source_path,
    backup_path,
    overwrite = FALSE
  )

  if (!ok) {
    stop(
      "Could not create configuration backup: ",
      backup_path,
      call. = FALSE
    )
  }

  normalise_slashes(backup_path)
}

# ------------------------------------------------------------
# 12. READ EXISTING CONFIG
# ------------------------------------------------------------

raster_catalogue <- read_config("raster_catalogue.csv")
variable_metadata <- read_config("variable_metadata.csv")
theme_variables <- read_config("theme_variables.csv")
pathway_themes <- read_config("pathway_themes.csv")

required_raster_columns <- c(
  "dataset_id",
  "variable_id",
  "scenario",
  "period",
  "file_path",
  "units",
  "dataset_type",
  "nodata_value",
  "enabled"
)

missing_raster_columns <- setdiff(
  required_raster_columns,
  names(raster_catalogue)
)

if (length(missing_raster_columns) > 0) {
  stop(
    "raster_catalogue.csv is missing required column(s): ",
    paste(missing_raster_columns, collapse = ", "),
    call. = FALSE
  )
}

required_metadata_columns <- c(
  "variable_id",
  "display_name",
  "description",
  "units",
  "summary_method",
  "risk_direction",
  "baseline_variable_id",
  "classification_method",
  "interpretation",
  "limitations"
)

missing_metadata_columns <- setdiff(
  required_metadata_columns,
  names(variable_metadata)
)

if (length(missing_metadata_columns) > 0) {
  stop(
    "variable_metadata.csv is missing required column(s): ",
    paste(missing_metadata_columns, collapse = ", "),
    call. = FALSE
  )
}

required_theme_columns <- c(
  "theme",
  "variable_id",
  "display_order",
  "default_selected"
)

missing_theme_columns <- setdiff(
  required_theme_columns,
  names(theme_variables)
)

if (length(missing_theme_columns) > 0) {
  stop(
    "theme_variables.csv is missing required column(s): ",
    paste(missing_theme_columns, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 13. FIND EXISTING DROUGHT/FIRE THEME
# ------------------------------------------------------------

theme_names <- unique(
  c(
    as.character(theme_variables$theme),
    as.character(pathway_themes$theme)
  )
)

# Prefer an existing theme containing both "drought" and "fire".
target_theme_matches <- theme_names[
  str_detect(
    str_to_lower(theme_names),
    "drought.*fire|fire.*drought"
  )
]

# If none, use an existing drought theme.
if (length(target_theme_matches) == 0) {
  target_theme_matches <- theme_names[
    str_detect(
      str_to_lower(theme_names),
      "drought"
    )
  ]
}

if (length(target_theme_matches) > 0) {
  TARGET_THEME <- target_theme_matches[[1]]
  TARGET_THEME_IS_NEW <- FALSE
} else {
  TARGET_THEME <- "Drought and Fire"
  TARGET_THEME_IS_NEW <- TRUE
}

cat("\nCONFIGURATION TARGET\n")
cat("====================\n")
cat("SPEI theme: ", TARGET_THEME, "\n", sep = "")
cat("New theme required: ", TARGET_THEME_IS_NEW, "\n", sep = "")

existing_theme_orders <- theme_variables |>
  filter(theme == TARGET_THEME) |>
  pull(display_order)

start_order <- if (
  length(existing_theme_orders) == 0 ||
  all(is.na(existing_theme_orders))
) {
  0
} else {
  max(existing_theme_orders, na.rm = TRUE)
}

theme_rows <- variable_lookup |>
  arrange(scale) |>
  transmute(
    theme = TARGET_THEME,
    variable_id,
    display_order = start_order + row_number(),
    default_selected = FALSE
  )

# If a new theme is required, add it to General Climate Screening.
available_pathways <- unique(
  as.character(pathway_themes$pathway)
)

general_pathway_matches <- available_pathways[
  str_detect(
    str_to_lower(available_pathways),
    "general.*climate"
  )
]

pathway_rows <- tibble()

if (TARGET_THEME_IS_NEW) {

  if (length(general_pathway_matches) == 0) {
    stop(
      "A new drought theme is required, but the General Climate ",
      "Screening pathway could not be identified.",
      call. = FALSE
    )
  }

  GENERAL_PATHWAY <- general_pathway_matches[[1]]

  existing_orders <- pathway_themes |>
    filter(pathway == GENERAL_PATHWAY) |>
    pull(display_order)

  next_order <- if (
    length(existing_orders) == 0 ||
    all(is.na(existing_orders))
  ) {
    1
  } else {
    max(existing_orders, na.rm = TRUE) + 1
  }

  pathway_rows <- tibble(
    pathway = GENERAL_PATHWAY,
    theme = TARGET_THEME,
    display_order = next_order,
    default_enabled = FALSE
  )
}

# ------------------------------------------------------------
# 14. BUILD UPDATED CONFIG IN MEMORY
# ------------------------------------------------------------

raster_out <- upsert_rows(
  raster_catalogue,
  catalogue_rows,
  c(
    "variable_id",
    "scenario",
    "period"
  )
)

metadata_out <- upsert_rows(
  variable_metadata,
  metadata_rows,
  "variable_id"
)

theme_out <- upsert_rows(
  theme_variables,
  theme_rows,
  c(
    "theme",
    "variable_id"
  )
)

pathway_out <- if (nrow(pathway_rows) > 0) {
  upsert_rows(
    pathway_themes,
    pathway_rows,
    c(
      "pathway",
      "theme"
    )
  )
} else {
  pathway_themes
}

# ------------------------------------------------------------
# 15. DUPLICATE AND COMPLETENESS CHECK
# ------------------------------------------------------------

duplicate_enabled <- raster_out |>
  filter(enabled) |>
  count(
    variable_id,
    scenario,
    period,
    name = "n"
  ) |>
  filter(n > 1)

if (nrow(duplicate_enabled) > 0) {

  cat("\nDUPLICATE ENABLED RASTER LOOKUPS\n")
  print(duplicate_enabled, n = Inf)

  stop(
    "SPEI registration stopped because the updated raster catalogue ",
    "would contain duplicate enabled variable/scenario/period records. ",
    "No configuration files were changed.",
    call. = FALSE
  )
}

spei_catalogue_check <- raster_out |>
  filter(
    variable_id %in% variable_lookup$variable_id
  )

missing_catalogue_files <- spei_catalogue_check |>
  filter(
    !file.exists(
      file.path(
        APP_ROOT,
        file_path
      )
    )
  )

if (nrow(missing_catalogue_files) > 0) {

  cat("\nSPEI CATALOGUE PATHS WITH MISSING FILES\n")
  print(
    missing_catalogue_files |>
      select(
        variable_id,
        scenario,
        period,
        file_path
      ),
    n = Inf
  )

  stop(
    "SPEI registration stopped because one or more catalogue paths ",
    "do not resolve to a file. No configuration files were changed.",
    call. = FALSE
  )
}

if (nrow(spei_catalogue_check) != 26) {
  stop(
    "Expected exactly 26 registered SPEI drought-frequency rows after ",
    "upsert; found ",
    nrow(spei_catalogue_check),
    ". No configuration files were changed.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 16. BACK UP AND WRITE CONFIG
# ------------------------------------------------------------

if (UPDATE_CONFIG) {

  cat("\nCreating configuration backups...\n")

  backup_files <- c(
    backup_config("raster_catalogue.csv"),
    backup_config("variable_metadata.csv"),
    backup_config("theme_variables.csv"),
    backup_config("pathway_themes.csv")
  )

  cat(
    paste0(
      "  ",
      backup_files,
      collapse = "\n"
    ),
    "\n"
  )

  readr::write_csv(
    raster_out,
    file.path(
      CONFIG_DIR,
      "raster_catalogue.csv"
    ),
    na = ""
  )

  readr::write_csv(
    metadata_out,
    file.path(
      CONFIG_DIR,
      "variable_metadata.csv"
    ),
    na = ""
  )

  readr::write_csv(
    theme_out,
    file.path(
      CONFIG_DIR,
      "theme_variables.csv"
    ),
    na = ""
  )

  readr::write_csv(
    pathway_out,
    file.path(
      CONFIG_DIR,
      "pathway_themes.csv"
    ),
    na = ""
  )

  cat("Configuration files updated.\n")

} else {

  cat(
    "\nUPDATE_CONFIG = FALSE - configuration files were not modified.\n"
  )
}

# ------------------------------------------------------------
# 17. SUPPORTING RASTER INVENTORY
# ------------------------------------------------------------

supporting_inventory <- output_audit |>
  filter(
    product_type %in% c(
      "change_percentage_points",
      "models_worse"
    )
  ) |>
  mutate(
    catalogue_status = "AVAILABLE_FOR_LATER_FORESTRY_USE_NOT_REGISTERED"
  )

readr::write_csv(
  supporting_inventory,
  file.path(
    AUDIT_DIR,
    "SPEI_supporting_change_agreement_inventory.csv"
  ),
  na = ""
)

# ------------------------------------------------------------
# 18. FINAL REPORT
# ------------------------------------------------------------

cat("\n============================================\n")
cat("SPEI APP PREPARATION + REGISTRATION COMPLETE\n")
cat("============================================\n")
cat("Source root:\n  ", normalise_slashes(SOURCE_ROOT), "\n", sep = "")
cat("App root:\n  ", normalise_slashes(APP_ROOT), "\n\n", sep = "")

cat("Absolute baseline rasters written: 2\n")
cat("Absolute future rasters written:   24\n")

if (WRITE_SUPPORTING_CHANGE_AGREEMENT) {
  cat("Change rasters written:            24\n")
  cat("Model-agreement rasters written:   24\n")
  cat("TOTAL app SPEI rasters:            74\n")
} else {
  cat("TOTAL app SPEI rasters:            26\n")
}

cat("\nRegistered selectable variables:\n")
cat("  SPEI3_DROUGHT_FREQ - SPEI-3 drought frequency\n")
cat("  SPEI6_DROUGHT_FREQ - SPEI-6 drought frequency\n")

cat("\nRegistered raster catalogue rows: 26\n")
cat("Theme: ", TARGET_THEME, "\n", sep = "")

cat("\nChange and model-agreement rasters are on disk but are NOT\n")
cat("registered as normal selectable variables yet.\n")

cat("\nNo risk thresholds were added or changed.\n")
cat("SPEI threat classes remain to be developed/validated with SFD.\n")

cat(
  "\nAudit files:\n  ",
  normalise_slashes(AUDIT_DIR),
  "\n",
  sep = ""
)

cat("============================================\n")
