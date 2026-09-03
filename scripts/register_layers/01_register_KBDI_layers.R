# ============================================================
# 01_register_KBDI_layers.R
# Sabah Climate Risk Explorer
#
# Purpose
#   1. Check that all 100 app-ready KBDI rasters have been moved
#      to the correct baseline or future scenario-period folders.
#   2. Open and validate every raster.
#   3. Register the 52 absolute KBDI rasters in raster_catalogue.csv.
#   4. Add/update KBDI variable metadata.
#   5. Add the KBDI variables to the existing drought/fire theme
#      used by General Climate Screening (or create a suitable
#      theme if one does not exist).
#   6. Back up configuration files before changing them.
#   7. Write audit tables to outputs/audit/.
#
# IMPORTANT
#   The 48 future-minus-baseline CHANGE rasters are checked but are
#   NOT registered in raster_catalogue.csv by this script.
#
#   Reason: the current app expects one enabled raster per
#   variable_id + scenario + period. Registering an absolute raster
#   and a change raster under the same variable/scenario/period
#   would create duplicate lookups. The app can calculate AOI change
#   from the registered baseline and future absolute rasters.
#
# Run from:
#   C:/Users/User/Documents/Climate-risk-screening-app
#
# Suggested save location:
#   scripts/register_layers/01_register_KBDI_layers.R
#
# Expected KBDI storage:
#   rasters/baseline/KBDI...historical_1981-2010.tif
#   rasters/future/<ssp>/<period>/KBDI...<ssp>_<period>.tif
#   rasters/future/<ssp>/<period>/KBDI...change_<ssp>_<period>_vs_1981-2010.tif
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(terra)
})

# ------------------------------------------------------------
# 0. SETTINGS
# ------------------------------------------------------------

CONFIG_DIR <- "config"
RASTER_ROOT <- "rasters"
AUDIT_DIR <- file.path("outputs", "audit")
BACKUP_DIR <- file.path(CONFIG_DIR, "backups")

WRITE_CHANGES <- TRUE

# Stop before editing config if raster files are missing, misplaced,
# unreadable, multi-band, geometrically inconsistent, or obviously
# outside their expected value range.
STOP_ON_RASTER_PROBLEM <- TRUE

# ------------------------------------------------------------
# 1. PROJECT CHECK
# ------------------------------------------------------------

if (!file.exists("app.R")) {
  stop(
    "app.R was not found. Run this script from the root of the ",
    "Climate-risk-screening-app project.",
    call. = FALSE
  )
}

required_config_files <- c(
  "raster_catalogue.csv",
  "variable_metadata.csv",
  "theme_variables.csv",
  "pathway_themes.csv"
)

missing_config_files <- required_config_files[
  !file.exists(file.path(CONFIG_DIR, required_config_files))
]

if (length(missing_config_files) > 0) {
  stop(
    "Missing configuration file(s): ",
    paste(missing_config_files, collapse = ", "),
    call. = FALSE
  )
}

dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(BACKUP_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. KBDI VARIABLE DEFINITIONS
# ------------------------------------------------------------

kbdi_variables <- tibble::tribble(
  ~variable_id,          ~display_name,                         ~units,                  ~description,
  "KBDImax",             "Mean annual maximum KBDI",           "KBDI index (0-800)",   "Mean across years of the annual maximum Keetch-Byram Drought Index (KBDI) for the climatological period.",
  "KBDIp95",             "95th percentile KBDI",               "KBDI index (0-800)",   "Upper-end KBDI condition represented by the 95th percentile for the climatological period.",
  "KBDI400days_eq365",   "Equivalent days/year KBDI >= 400",   "equivalent days/year", "Equivalent number of days per 365-day year with KBDI at or above 400.",
  "KBDI600days_eq365",   "Equivalent days/year KBDI >= 600",   "equivalent days/year", "Equivalent number of days per 365-day year with KBDI at or above 600."
) |>
  mutate(
    summary_method = "mean",
    risk_direction = "high",
    baseline_variable_id = variable_id,
    classification_method = "pending_fixed_thresholds",
    interpretation = case_when(
      variable_id == "KBDImax" ~
        "Higher values indicate more severe typical annual peak climatic drying conditions relevant to fire-weather screening.",
      variable_id == "KBDIp95" ~
        "Higher values indicate more severe upper-end climatic drying conditions relevant to fire-weather screening.",
      variable_id == "KBDI400days_eq365" ~
        "Higher values indicate more frequent elevated KBDI conditions. This is a climatic drying/fire-weather indicator, not a probability of fire occurrence.",
      variable_id == "KBDI600days_eq365" ~
        "Higher values indicate more frequent severe KBDI conditions. This is a climatic drying/fire-weather indicator, not a probability of fire occurrence.",
      TRUE ~ NA_character_
    ),
    limitations = paste(
      "NASA NEX-GDDP-CMIP6-derived KBDI screening product using a five-GCM ensemble.",
      "KBDI represents climatic fuel-moisture deficit/fire-weather pressure and does not by itself represent the probability of a fire occurring.",
      "The KBDI >=400 and >=600 thresholds are screening thresholds and should not be treated as Sabah forest-management threat classes until validated.",
      "Use alongside fire-probability/susceptibility information and local forest/site conditions where relevant."
    )
  )

metrics <- kbdi_variables$variable_id
future_scenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")
future_periods <- c("2011-2040", "2041-2070", "2071-2100")
baseline_period <- "1981-2010"

# ------------------------------------------------------------
# 3. BUILD THE EXPECTED 100-FILE INVENTORY
# ------------------------------------------------------------

baseline_expected <- tidyr::crossing(
  metric = metrics
) |>
  mutate(
    app_scenario = "baseline",
    source_scenario = "historical",
    period = baseline_period,
    product_type = "ensemble_mean",
    filename = paste0(
      metric,
      "_historical_",
      baseline_period,
      ".tif"
    ),
    # The app has only one historical baseline climatology, so baseline
    # rasters are stored directly in rasters/baseline/ rather than in a
    # separate 1981-2010 subfolder.
    expected_path = file.path(
      RASTER_ROOT,
      "baseline",
      filename
    )
  )

future_absolute_expected <- tidyr::crossing(
  metric = metrics,
  source_scenario = future_scenarios,
  period = future_periods
) |>
  mutate(
    app_scenario = source_scenario,
    product_type = "ensemble_mean",
    filename = paste0(
      metric,
      "_",
      source_scenario,
      "_",
      period,
      ".tif"
    ),
    expected_path = file.path(
      RASTER_ROOT,
      "future",
      source_scenario,
      period,
      filename
    )
  )

future_change_expected <- tidyr::crossing(
  metric = metrics,
  source_scenario = future_scenarios,
  period = future_periods
) |>
  mutate(
    app_scenario = source_scenario,
    product_type = "change_from_1981-2010",
    filename = paste0(
      metric,
      "_change_",
      source_scenario,
      "_",
      period,
      "_vs_",
      baseline_period,
      ".tif"
    ),
    expected_path = file.path(
      RASTER_ROOT,
      "future",
      source_scenario,
      period,
      filename
    )
  )

expected <- bind_rows(
  baseline_expected,
  future_absolute_expected,
  future_change_expected
) |>
  arrange(
    factor(product_type, c("ensemble_mean", "change_from_1981-2010")),
    factor(metric, metrics),
    app_scenario,
    period
  )

if (nrow(expected) != 100) {
  stop(
    "Internal script error: expected inventory should contain 100 files, not ",
    nrow(expected),
    ".",
    call. = FALSE
  )
}

cat("\nKBDI expected inventory\n")
cat("=======================\n")
cat("Baseline absolute rasters: ", nrow(baseline_expected), "\n", sep = "")
cat("Future absolute rasters:   ", nrow(future_absolute_expected), "\n", sep = "")
cat("Future change rasters:     ", nrow(future_change_expected), "\n", sep = "")
cat("TOTAL expected:            ", nrow(expected), "\n\n", sep = "")

# ------------------------------------------------------------
# 4. CHECK FILE LOCATIONS
# ------------------------------------------------------------

normalise_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

all_kbdi_files <- list.files(
  RASTER_ROOT,
  pattern = "^KBDI.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

all_kbdi_lookup <- tibble(
  actual_path = normalise_slashes(all_kbdi_files),
  filename = basename(all_kbdi_files)
)

location_audit <- expected |>
  mutate(
    expected_path = normalise_slashes(expected_path),
    exists_expected = file.exists(expected_path)
  ) |>
  left_join(
    all_kbdi_lookup |>
      group_by(filename) |>
      summarise(
        found_count = n(),
        found_anywhere = paste(actual_path, collapse = " | "),
        .groups = "drop"
      ),
    by = "filename"
  ) |>
  mutate(
    found_count = coalesce(found_count, 0L),
    location_status = case_when(
      exists_expected & found_count == 1 ~ "OK",
      exists_expected & found_count > 1 ~ "DUPLICATE_FILENAME",
      !exists_expected & found_count >= 1 ~ "WRONG_FOLDER",
      TRUE ~ "MISSING"
    )
  )

location_problems <- location_audit |>
  filter(location_status != "OK")

expected_basenames <- expected$filename

extra_kbdi_files <- all_kbdi_lookup |>
  filter(!filename %in% expected_basenames)

cat("KBDI files found under rasters/: ", nrow(all_kbdi_lookup), "\n", sep = "")
cat("Expected files in correct folders: ",
    sum(location_audit$location_status == "OK"), " / 100\n", sep = "")

if (nrow(extra_kbdi_files) > 0) {
  cat(
    "\nNOTE: Additional KBDI .tif files were found under rasters/ ",
    "that are not part of the 100-file app inventory:\n",
    sep = ""
  )
  print(extra_kbdi_files, n = Inf)
}

if (nrow(location_problems) > 0) {
  cat("\nKBDI FILE LOCATION PROBLEMS\n")
  print(
    location_problems |>
      select(
        metric,
        app_scenario,
        period,
        product_type,
        filename,
        expected_path,
        found_anywhere,
        location_status
      ),
    n = Inf
  )
  
  readr::write_csv(
    location_audit,
    file.path(AUDIT_DIR, "KBDI_file_location_audit.csv"),
    na = ""
  )
  
  if (STOP_ON_RASTER_PROBLEM) {
    stop(
      "\nKBDI registration stopped. Fix the missing/misplaced/duplicate files ",
      "shown above, then run this script again. No config files were changed.",
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------
# 5. OPEN AND VALIDATE ALL 100 RASTERS
# ------------------------------------------------------------

reference_path <- baseline_expected |>
  filter(metric == "KBDImax") |>
  pull(expected_path)

reference_raster <- terra::rast(reference_path)

safe_naflag <- function(r) {
  out <- tryCatch(
    terra::NAflag(r),
    error = function(e) NA_real_
  )
  if (length(out) == 0) NA_real_ else as.numeric(out[[1]])
}

check_value_range <- function(metric, product_type, min_value, max_value) {
  
  if (is.na(min_value) || is.na(max_value)) {
    return(FALSE)
  }
  
  # Broad tolerances are intentional; this catches gross processing errors,
  # not tiny floating-point excursions.
  if (metric %in% c("KBDImax", "KBDIp95")) {
    if (product_type == "ensemble_mean") {
      return(min_value >= -1 && max_value <= 801)
    } else {
      return(min_value >= -801 && max_value <= 801)
    }
  }
  
  if (metric %in% c("KBDI400days_eq365", "KBDI600days_eq365")) {
    if (product_type == "ensemble_mean") {
      return(min_value >= -1 && max_value <= 366)
    } else {
      return(min_value >= -366 && max_value <= 366)
    }
  }
  
  TRUE
}

check_one_raster <- function(metric, app_scenario, period, product_type, expected_path) {
  
  tryCatch(
    {
      r <- terra::rast(expected_path)
      
      mm <- terra::global(
        r,
        fun = c("min", "max"),
        na.rm = TRUE
      )
      
      min_value <- as.numeric(mm$min[[1]])
      max_value <- as.numeric(mm$max[[1]])
      
      same_geometry <- terra::compareGeom(
        reference_raster,
        r,
        stopOnError = FALSE
      )
      
      res_xy <- terra::res(r)
      
      tibble(
        metric = metric,
        app_scenario = app_scenario,
        period = period,
        product_type = product_type,
        expected_path = normalise_slashes(expected_path),
        raster_opens = TRUE,
        n_layers = terra::nlyr(r),
        n_rows = terra::nrow(r),
        n_cols = terra::ncol(r),
        x_resolution = res_xy[[1]],
        y_resolution = res_xy[[2]],
        is_lonlat = terra::is.lonlat(r),
        crs_present = nzchar(terra::crs(r)),
        nodata_flag = safe_naflag(r),
        min_value = min_value,
        max_value = max_value,
        same_geometry_as_reference = isTRUE(same_geometry),
        range_ok = check_value_range(
          metric,
          product_type,
          min_value,
          max_value
        ),
        error_message = NA_character_
      )
    },
    error = function(e) {
      tibble(
        metric = metric,
        app_scenario = app_scenario,
        period = period,
        product_type = product_type,
        expected_path = normalise_slashes(expected_path),
        raster_opens = FALSE,
        n_layers = NA_integer_,
        n_rows = NA_integer_,
        n_cols = NA_integer_,
        x_resolution = NA_real_,
        y_resolution = NA_real_,
        is_lonlat = NA,
        crs_present = FALSE,
        nodata_flag = NA_real_,
        min_value = NA_real_,
        max_value = NA_real_,
        same_geometry_as_reference = FALSE,
        range_ok = FALSE,
        error_message = conditionMessage(e)
      )
    }
  )
}

cat("\nOpening and checking all 100 KBDI rasters...\n")

raster_audit <- pmap_dfr(
  expected |>
    select(metric, app_scenario, period, product_type, expected_path),
  check_one_raster
)

readr::write_csv(
  raster_audit,
  file.path(AUDIT_DIR, "KBDI_raster_audit.csv"),
  na = ""
)

raster_problems <- raster_audit |>
  filter(
    !raster_opens |
      n_layers != 1 |
      !crs_present |
      !same_geometry_as_reference |
      !range_ok
  )

cat("Rasters opening successfully: ",
    sum(raster_audit$raster_opens), " / 100\n", sep = "")
cat("Single-band rasters:           ",
    sum(raster_audit$n_layers == 1, na.rm = TRUE), " / 100\n", sep = "")
cat("Same raster geometry:          ",
    sum(raster_audit$same_geometry_as_reference, na.rm = TRUE), " / 100\n", sep = "")
cat("Values within broad checks:    ",
    sum(raster_audit$range_ok, na.rm = TRUE), " / 100\n", sep = "")

if (nrow(raster_problems) > 0) {
  
  cat("\nRASTER VALIDATION PROBLEMS\n")
  print(
    raster_problems |>
      select(
        metric,
        app_scenario,
        period,
        product_type,
        expected_path,
        raster_opens,
        n_layers,
        crs_present,
        same_geometry_as_reference,
        min_value,
        max_value,
        range_ok,
        error_message
      ),
    n = Inf
  )
  
  if (STOP_ON_RASTER_PROBLEM) {
    stop(
      "\nKBDI registration stopped because one or more rasters failed validation. ",
      "See outputs/audit/KBDI_raster_audit.csv. No config files were changed.",
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------
# 6. BUILD THE 52 CATALOGUE ROWS
# ------------------------------------------------------------
#
# We register:
#   4 baseline rasters
#   48 future absolute rasters
#
# We do NOT register the 48 change rasters here.
# ------------------------------------------------------------

absolute_inventory <- expected |>
  filter(product_type == "ensemble_mean") |>
  left_join(
    kbdi_variables |>
      select(variable_id, units),
    by = c("metric" = "variable_id")
  ) |>
  left_join(
    raster_audit |>
      filter(product_type == "ensemble_mean") |>
      select(
        metric,
        app_scenario,
        period,
        x_resolution,
        y_resolution,
        nodata_flag
      ),
    by = c("metric", "app_scenario", "period")
  ) |>
  mutate(
    variable_id = metric,
    dataset_id = case_when(
      app_scenario == "baseline" ~ paste0(
        metric,
        "_BASE_",
        str_replace_all(period, "-", "")
      ),
      TRUE ~ paste0(
        metric,
        "_",
        str_to_upper(app_scenario),
        "_",
        str_replace_all(period, "-", "")
      )
    ),
    scenario = app_scenario,
    file_path = normalise_slashes(expected_path),
    file_format = "tif",
    dataset_type = "continuous",
    resolution = if_else(
      !is.na(x_resolution) & !is.na(y_resolution),
      paste0(
        format(round(x_resolution, 6), trim = TRUE, scientific = FALSE),
        " x ",
        format(round(y_resolution, 6), trim = TRUE, scientific = FALSE),
        " degrees"
      ),
      NA_character_
    ),
    # Keep the app convention. The raster audit separately records
    # the source GeoTIFF NAflag.
    nodata_value = -9999,
    enabled = TRUE
  ) |>
  select(
    dataset_id,
    variable_id,
    scenario,
    period,
    file_path,
    file_format,
    units,
    dataset_type,
    resolution,
    nodata_value,
    enabled
  )

if (nrow(absolute_inventory) != 52) {
  stop(
    "Internal script error: expected 52 absolute catalogue rows, got ",
    nrow(absolute_inventory),
    ".",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 7. CONFIG HELPERS
# ------------------------------------------------------------

read_config <- function(filename) {
  readr::read_csv(
    file.path(CONFIG_DIR, filename),
    show_col_types = FALSE
  )
}

upsert_rows <- function(existing, replacement, keys) {
  
  missing_keys_existing <- setdiff(keys, names(existing))
  missing_keys_replacement <- setdiff(keys, names(replacement))
  
  if (length(missing_keys_existing) > 0) {
    stop(
      "Existing table is missing upsert key(s): ",
      paste(missing_keys_existing, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(missing_keys_replacement) > 0) {
    stop(
      "Replacement rows are missing upsert key(s): ",
      paste(missing_keys_replacement, collapse = ", "),
      call. = FALSE
    )
  }
  
  kept <- existing |>
    anti_join(
      replacement |>
        distinct(across(all_of(keys))),
      by = keys
    )
  
  out <- bind_rows(kept, replacement)
  
  # Preserve the original column order and append genuinely new columns.
  preferred_order <- c(
    names(existing),
    setdiff(names(out), names(existing))
  )
  
  out |>
    select(all_of(preferred_order))
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

backup_config <- function(filename) {
  
  source_path <- file.path(CONFIG_DIR, filename)
  
  backup_path <- file.path(
    BACKUP_DIR,
    paste0(
      tools::file_path_sans_ext(filename),
      "_before_KBDI_",
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
      "Could not create backup of ",
      source_path,
      ". No configuration files have been written.",
      call. = FALSE
    )
  }
  
  normalise_slashes(backup_path)
}

# ------------------------------------------------------------
# 8. READ CURRENT CONFIG
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
    "config/raster_catalogue.csv is missing required column(s): ",
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
    "config/variable_metadata.csv is missing required column(s): ",
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
    "config/theme_variables.csv is missing required column(s): ",
    paste(missing_theme_columns, collapse = ", "),
    call. = FALSE
  )
}

required_pathway_columns <- c(
  "pathway",
  "theme",
  "display_order",
  "default_enabled"
)

missing_pathway_columns <- setdiff(
  required_pathway_columns,
  names(pathway_themes)
)

if (length(missing_pathway_columns) > 0) {
  stop(
    "config/pathway_themes.csv is missing required column(s): ",
    paste(missing_pathway_columns, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 9. FIND THE CURRENT GENERAL DROUGHT/FIRE THEME
# ------------------------------------------------------------

available_theme_names <- unique(
  c(
    as.character(theme_variables$theme),
    as.character(pathway_themes$theme)
  )
)

drought_fire_matches <- available_theme_names[
  str_detect(
    str_to_lower(available_theme_names),
    "drought.*fire|fire.*drought"
  )
]

if (length(drought_fire_matches) > 0) {
  target_theme <- drought_fire_matches[[1]]
  target_theme_is_new <- FALSE
} else {
  target_theme <- "Fire & Climatic Drying"
  target_theme_is_new <- TRUE
}

available_pathways <- unique(as.character(pathway_themes$pathway))

general_matches <- available_pathways[
  str_detect(
    str_to_lower(available_pathways),
    "general.*climate"
  )
]

if (length(general_matches) > 0) {
  general_pathway <- general_matches[[1]]
} else {
  stop(
    "Could not identify the General Climate Screening pathway in ",
    "config/pathway_themes.csv. No configuration files were changed.",
    call. = FALSE
  )
}

cat("\nConfiguration target\n")
cat("====================\n")
cat("General pathway: ", general_pathway, "\n", sep = "")
cat("Drought/fire theme: ", target_theme, "\n", sep = "")
cat("New theme required: ", target_theme_is_new, "\n\n", sep = "")

# ------------------------------------------------------------
# 10. BUILD METADATA AND THEME ROWS
# ------------------------------------------------------------

metadata_rows <- kbdi_variables |>
  select(
    variable_id,
    display_name,
    description,
    units,
    summary_method,
    risk_direction,
    baseline_variable_id,
    classification_method,
    interpretation,
    limitations
  )

existing_target_orders <- theme_variables |>
  filter(theme == target_theme) |>
  pull(display_order)

start_order <- if (
  length(existing_target_orders) == 0 ||
  all(is.na(existing_target_orders))
) {
  0
} else {
  max(existing_target_orders, na.rm = TRUE)
}

theme_rows <- kbdi_variables |>
  transmute(
    theme = target_theme,
    variable_id,
    display_order = start_order + row_number(),
    default_selected = FALSE
  )

# Make sure the target theme is linked to General Climate Screening.
general_theme_link <- pathway_themes |>
  filter(
    pathway == general_pathway,
    theme == target_theme
  )

pathway_rows <- tibble()

if (nrow(general_theme_link) == 0) {
  
  existing_pathway_orders <- pathway_themes |>
    filter(pathway == general_pathway) |>
    pull(display_order)
  
  next_pathway_order <- if (
    length(existing_pathway_orders) == 0 ||
    all(is.na(existing_pathway_orders))
  ) {
    1
  } else {
    max(existing_pathway_orders, na.rm = TRUE) + 1
  }
  
  pathway_rows <- tibble(
    pathway = general_pathway,
    theme = target_theme,
    display_order = next_pathway_order,
    default_enabled = FALSE
  )
}

# ------------------------------------------------------------
# 11. UPSERT CONFIG TABLES
# ------------------------------------------------------------

raster_out <- upsert_rows(
  raster_catalogue,
  absolute_inventory,
  c("variable_id", "scenario", "period")
)

metadata_out <- upsert_rows(
  variable_metadata,
  metadata_rows,
  "variable_id"
)

theme_out <- upsert_rows(
  theme_variables,
  theme_rows,
  c("theme", "variable_id")
)

pathway_out <- if (nrow(pathway_rows) > 0) {
  upsert_rows(
    pathway_themes,
    pathway_rows,
    c("pathway", "theme")
  )
} else {
  pathway_themes
}

# ------------------------------------------------------------
# 12. FINAL DUPLICATE AND FILE CHECKS BEFORE WRITE
# ------------------------------------------------------------

duplicate_catalogue_records <- raster_out |>
  filter(enabled) |>
  count(
    variable_id,
    scenario,
    period,
    name = "n"
  ) |>
  filter(n > 1)

if (nrow(duplicate_catalogue_records) > 0) {
  cat("\nDUPLICATE ENABLED CATALOGUE LOOKUPS FOUND\n")
  print(duplicate_catalogue_records, n = Inf)
  
  stop(
    "Registration stopped because raster_catalogue.csv would contain ",
    "duplicate enabled variable/scenario/period lookups. No config files ",
    "were changed.",
    call. = FALSE
  )
}

registered_kbdi_rows <- raster_out |>
  filter(variable_id %in% metrics)

registered_missing_files <- registered_kbdi_rows |>
  filter(!file.exists(file_path))

if (nrow(registered_missing_files) > 0) {
  cat("\nREGISTERED KBDI FILES THAT DO NOT EXIST\n")
  print(
    registered_missing_files |>
      select(variable_id, scenario, period, file_path),
    n = Inf
  )
  
  stop(
    "Registration stopped because one or more KBDI catalogue paths ",
    "would point to missing files.",
    call. = FALSE
  )
}

# Exact completeness check for the four KBDI variables:
expected_registered_combinations <- tidyr::crossing(
  variable_id = metrics,
  scenario = c("baseline", future_scenarios),
  period = c(baseline_period, future_periods)
) |>
  filter(
    (scenario == "baseline" & period == baseline_period) |
      (scenario != "baseline" & period %in% future_periods)
  )

actual_registered_combinations <- registered_kbdi_rows |>
  filter(enabled) |>
  distinct(variable_id, scenario, period)

missing_registered_combinations <- anti_join(
  expected_registered_combinations,
  actual_registered_combinations,
  by = c("variable_id", "scenario", "period")
)

if (nrow(missing_registered_combinations) > 0) {
  cat("\nMISSING EXPECTED KBDI CATALOGUE COMBINATIONS\n")
  print(missing_registered_combinations, n = Inf)
  
  stop(
    "Registration stopped because the proposed catalogue is incomplete.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 13. WRITE AUDIT SUMMARIES
# ------------------------------------------------------------

change_rasters_available <- expected |>
  filter(product_type == "change_from_1981-2010") |>
  transmute(
    metric,
    scenario = app_scenario,
    period,
    file_path = normalise_slashes(expected_path),
    file_exists = file.exists(expected_path),
    catalogue_status = "CHECKED_NOT_REGISTERED"
  )

readr::write_csv(
  change_rasters_available,
  file.path(AUDIT_DIR, "KBDI_change_rasters_available_not_registered.csv"),
  na = ""
)

registration_preview <- absolute_inventory |>
  mutate(registration_action = "UPSERT_ABSOLUTE_RASTER")

readr::write_csv(
  registration_preview,
  file.path(AUDIT_DIR, "KBDI_catalogue_rows_to_register.csv"),
  na = ""
)

# ------------------------------------------------------------
# 14. BACK UP AND WRITE CONFIG
# ------------------------------------------------------------

if (WRITE_CHANGES) {
  
  cat("\nCreating configuration backups...\n")
  
  backup_files <- c(
    backup_config("raster_catalogue.csv"),
    backup_config("variable_metadata.csv"),
    backup_config("theme_variables.csv"),
    backup_config("pathway_themes.csv")
  )
  
  cat("Backups created:\n")
  cat(paste0("  ", backup_files, collapse = "\n"), "\n")
  
  readr::write_csv(
    raster_out,
    file.path(CONFIG_DIR, "raster_catalogue.csv"),
    na = ""
  )
  
  readr::write_csv(
    metadata_out,
    file.path(CONFIG_DIR, "variable_metadata.csv"),
    na = ""
  )
  
  readr::write_csv(
    theme_out,
    file.path(CONFIG_DIR, "theme_variables.csv"),
    na = ""
  )
  
  readr::write_csv(
    pathway_out,
    file.path(CONFIG_DIR, "pathway_themes.csv"),
    na = ""
  )
  
  cat("\nConfiguration files updated.\n")
  
} else {
  
  cat(
    "\nWRITE_CHANGES = FALSE, so configuration files were NOT modified.\n"
  )
}

# ------------------------------------------------------------
# 15. FINAL REPORT
# ------------------------------------------------------------

cat("\n============================================\n")
cat("KBDI APP REGISTRATION COMPLETE\n")
cat("============================================\n")
cat("KBDI rasters checked:                100\n")
cat("Absolute rasters registered:          52\n")
cat("Change rasters checked/not registered: 48\n")
cat("KBDI variables registered:             4\n")
cat("Target theme: ", target_theme, "\n", sep = "")
cat("General pathway: ", general_pathway, "\n", sep = "")
cat("\nRisk thresholds were NOT added or changed.\n")
cat(
  "KBDI >=400 and >=600 remain screening metrics until threat-class ",
  "thresholds are explicitly agreed/validated.\n",
  sep = ""
)
cat("\nAudit files written to: ", normalise_slashes(AUDIT_DIR), "\n", sep = "")
cat("============================================\n")
