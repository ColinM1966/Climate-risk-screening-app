# ============================================================
# LOAD AND VALIDATE CONFIGURATION FILES
# Sabah Climate Risk Explorer
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# HELPER: CONVERT TEXT VALUES TO LOGICAL
# ------------------------------------------------------------

to_logical <- function(x) {

  tolower(
    str_squish(
      as.character(x)
    )
  ) %in% c(
    "true",
    "t",
    "1",
    "yes",
    "y"
  )
}

# ------------------------------------------------------------
# HELPER: CHECK REQUIRED COLUMNS
# ------------------------------------------------------------

check_required_columns <- function(
    data,
    required_columns,
    table_name
) {

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0) {

    stop(
      paste0(
        "Missing required columns in ",
        table_name,
        ":\n",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# HELPER: CHECK DUPLICATE IDS
# ------------------------------------------------------------

check_duplicate_ids <- function(
    data,
    id_columns,
    table_name
) {

  duplicate_rows <- data %>%
    count(
      across(
        all_of(id_columns)
      ),
      name = "record_count"
    ) %>%
    filter(
      record_count > 1
    )

  if (nrow(duplicate_rows) > 0) {

    warning(
      paste0(
        "Duplicate records were found in ",
        table_name,
        " using fields: ",
        paste(
          id_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )

    print(
      duplicate_rows
    )
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# LOAD ONE CSV FILE
# ------------------------------------------------------------

read_config_file <- function(
    file_path,
    table_name
) {

  if (!file.exists(file_path)) {

    stop(
      paste0(
        "Configuration file not found:\n",
        file_path
      ),
      call. = FALSE
    )
  }

  data <- read_csv(
    file_path,
    show_col_types = FALSE,
    trim_ws = TRUE
  )

  if (nrow(data) == 0) {

    warning(
      paste0(
        table_name,
        " contains no rows."
      ),
      call. = FALSE
    )
  }

  data
}

# ------------------------------------------------------------
# MAIN CONFIGURATION LOADER
# ------------------------------------------------------------

load_app_config <- function(
    config_dir = "config"
) {

  config_paths <- list(

    raster_catalogue = file.path(
      config_dir,
      "raster_catalogue.csv"
    ),

    variable_metadata = file.path(
      config_dir,
      "variable_metadata.csv"
    ),

    theme_variables = file.path(
      config_dir,
      "theme_variables.csv"
    ),

    pathway_themes = file.path(
      config_dir,
      "pathway_themes.csv"
    ),

    risk_thresholds = file.path(
      config_dir,
      "risk_thresholds.csv"
    )
  )

  # ----------------------------------------------------------
  # READ TABLES
  # ----------------------------------------------------------

  raster_catalogue <- read_config_file(
    config_paths$raster_catalogue,
    "raster_catalogue.csv"
  )

  variable_metadata <- read_config_file(
    config_paths$variable_metadata,
    "variable_metadata.csv"
  )

  theme_variables <- read_config_file(
    config_paths$theme_variables,
    "theme_variables.csv"
  )

  pathway_themes <- read_config_file(
    config_paths$pathway_themes,
    "pathway_themes.csv"
  )

  risk_thresholds <- read_config_file(
    config_paths$risk_thresholds,
    "risk_thresholds.csv"
  )

  # ----------------------------------------------------------
  # VALIDATE REQUIRED COLUMNS
  # ----------------------------------------------------------

  check_required_columns(
    raster_catalogue,
    c(
      "dataset_id",
      "variable_id",
      "scenario",
      "period",
      "file_path",
      "units",
      "dataset_type",
      "nodata_value",
      "enabled"
    ),
    "raster_catalogue.csv"
  )

  check_required_columns(
    variable_metadata,
    c(
      "variable_id",
      "display_name",
      "description",
      "units",
      "summary_method",
      "risk_direction"
    ),
    "variable_metadata.csv"
  )

  check_required_columns(
    theme_variables,
    c(
      "theme",
      "variable_id",
      "display_order",
      "default_selected"
    ),
    "theme_variables.csv"
  )

  check_required_columns(
    pathway_themes,
    c(
      "pathway",
      "theme",
      "display_order",
      "default_enabled"
    ),
    "pathway_themes.csv"
  )

  check_required_columns(
    risk_thresholds,
    c(
      "variable_id",
      "class",
      "lower_bound",
      "upper_bound",
      "lower_inclusive",
      "upper_inclusive",
      "score",
      "units"
    ),
    "risk_thresholds.csv"
  )

  # ----------------------------------------------------------
  # CLEAN TEXT FIELDS
  # ----------------------------------------------------------

  raster_catalogue <- raster_catalogue %>%
    mutate(
      dataset_id = str_squish(
        as.character(dataset_id)
      ),

      variable_id = str_squish(
        as.character(variable_id)
      ),

      scenario = str_to_lower(
        str_squish(
          as.character(scenario)
        )
      ),

      period = str_squish(
        as.character(period)
      ),

      file_path = str_squish(
        as.character(file_path)
      ),

      enabled = to_logical(
        enabled
      )
    )

  variable_metadata <- variable_metadata %>%
    mutate(
      variable_id = str_squish(
        as.character(variable_id)
      ),

      display_name = str_squish(
        as.character(display_name)
      ),

      units = str_squish(
        as.character(units)
      ),

      risk_direction = str_to_lower(
        str_squish(
          as.character(risk_direction)
        )
      )
    )

  theme_variables <- theme_variables %>%
    mutate(
      theme = str_squish(
        as.character(theme)
      ),

      variable_id = str_squish(
        as.character(variable_id)
      ),

      display_order = as.numeric(
        display_order
      ),

      default_selected = to_logical(
        default_selected
      )
    )

  pathway_themes <- pathway_themes %>%
    mutate(
      pathway = str_squish(
        as.character(pathway)
      ),

      theme = str_squish(
        as.character(theme)
      ),

      display_order = as.numeric(
        display_order
      ),

      default_enabled = to_logical(
        default_enabled
      )
    )

  risk_thresholds <- risk_thresholds %>%
    mutate(
      variable_id = str_squish(
        as.character(variable_id)
      ),

      class = str_squish(
        as.character(class)
      ),

      lower_bound = as.numeric(
        lower_bound
      ),

      upper_bound = as.numeric(
        upper_bound
      ),

      lower_inclusive = to_logical(
        lower_inclusive
      ),

      upper_inclusive = to_logical(
        upper_inclusive
      ),

      score = as.numeric(
        score
      ),

      units = str_squish(
        as.character(units)
      )
    )

  # ----------------------------------------------------------
  # CHECK DUPLICATE RECORDS
  # ----------------------------------------------------------

  check_duplicate_ids(
    raster_catalogue,
    c(
      "dataset_id"
    ),
    "raster_catalogue.csv"
  )

  check_duplicate_ids(
    variable_metadata,
    c(
      "variable_id"
    ),
    "variable_metadata.csv"
  )

  check_duplicate_ids(
    theme_variables,
    c(
      "theme",
      "variable_id"
    ),
    "theme_variables.csv"
  )

  check_duplicate_ids(
    pathway_themes,
    c(
      "pathway",
      "theme"
    ),
    "pathway_themes.csv"
  )

  check_duplicate_ids(
    risk_thresholds,
    c(
      "variable_id",
      "class"
    ),
    "risk_thresholds.csv"
  )

  # ----------------------------------------------------------
  # CHECK LINKS BETWEEN TABLES
  # ----------------------------------------------------------

  known_variables <- variable_metadata$variable_id

  missing_metadata_variables <- setdiff(
    raster_catalogue$variable_id,
    known_variables
  )

  if (length(missing_metadata_variables) > 0) {

    warning(
      paste0(
        "The following raster catalogue variables are missing ",
        "from variable_metadata.csv:\n",
        paste(
          missing_metadata_variables,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  missing_theme_variables <- setdiff(
    theme_variables$variable_id,
    known_variables
  )

  if (length(missing_theme_variables) > 0) {

    warning(
      paste0(
        "The following theme variables are missing from ",
        "variable_metadata.csv:\n",
        paste(
          missing_theme_variables,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  known_themes <- unique(
    theme_variables$theme
  )

  missing_pathway_themes <- setdiff(
    pathway_themes$theme,
    known_themes
  )

  if (length(missing_pathway_themes) > 0) {

    warning(
      paste0(
        "The following pathway themes are missing from ",
        "theme_variables.csv:\n",
        paste(
          missing_pathway_themes,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  missing_threshold_variables <- setdiff(
    risk_thresholds$variable_id,
    known_variables
  )

  if (length(missing_threshold_variables) > 0) {

    warning(
      paste0(
        "The following risk-threshold variables are missing ",
        "from variable_metadata.csv:\n",
        paste(
          missing_threshold_variables,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # CHECK RASTER FILE PATHS
  # ----------------------------------------------------------

  raster_catalogue <- raster_catalogue %>%
    mutate(
      file_exists = file.exists(
        file_path
      )
    )

  missing_enabled_files <- raster_catalogue %>%
    filter(
      enabled,
      !file_exists
    )

  if (nrow(missing_enabled_files) > 0) {

    warning(
      paste0(
        nrow(missing_enabled_files),
        " enabled raster catalogue records refer to files ",
        "that are not currently available locally."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # RETURN ALL TABLES AS ONE LIST
  # ----------------------------------------------------------

  config <- list(

    raster_catalogue =
      raster_catalogue,

    variable_metadata =
      variable_metadata,

    theme_variables =
      theme_variables,

    pathway_themes =
      pathway_themes,

    risk_thresholds =
      risk_thresholds
  )

  class(config) <- c(
    "sabah_climate_config",
    class(config)
  )

  config
}

# ------------------------------------------------------------
# OPTIONAL SUMMARY FUNCTION
# ------------------------------------------------------------

print_config_summary <- function(config) {

  cat("\n")
  cat("========================================\n")
  cat("SABAH CLIMATE RISK APP CONFIGURATION\n")
  cat("========================================\n")

  cat(
    "\nRaster catalogue records:",
    nrow(config$raster_catalogue),
    "\n"
  )

  cat(
    "Enabled raster records:",
    sum(
      config$raster_catalogue$enabled,
      na.rm = TRUE
    ),
    "\n"
  )

  cat(
    "Locally available raster files:",
    sum(
      config$raster_catalogue$file_exists,
      na.rm = TRUE
    ),
    "\n"
  )

  cat(
    "Variables:",
    nrow(config$variable_metadata),
    "\n"
  )

  cat(
    "Themes:",
    length(
      unique(
        config$theme_variables$theme
      )
    ),
    "\n"
  )

  cat(
    "User pathways:",
    length(
      unique(
        config$pathway_themes$pathway
      )
    ),
    "\n"
  )

  cat(
    "Risk-threshold records:",
    nrow(config$risk_thresholds),
    "\n"
  )

  cat("\nAvailable scenarios:\n")

  print(
    sort(
      unique(
        config$raster_catalogue$scenario
      )
    )
  )

  cat("\nAvailable periods:\n")

  print(
    sort(
      unique(
        config$raster_catalogue$period
      )
    )
  )

  invisible(config)
}
