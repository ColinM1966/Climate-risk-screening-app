# ============================================================
# TEST PROTOTYPE RASTER LOOKUPS
#
# File:
#   scripts/test_raster_lookup.R
#
# Run from the project root:
#   source("scripts/test_raster_lookup.R")
#
# Required files:
#   config/raster_catalogue.csv
#   R/utils/find_raster.R
#
# Output:
#   outputs/tests/raster_lookup_test.csv
# ============================================================


# ------------------------------------------------------------
# 1. REQUIRED PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "readr",
  "dplyr",
  "purrr",
  "tibble",
  "terra"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "The following required packages are not installed:\n",
      paste(missing_packages, collapse = ", "),
      "\n\nInstall them with:\n",
      "install.packages(c(",
      paste0('"', missing_packages, '"', collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

library(readr)
library(dplyr)
library(purrr)
library(tibble)
library(terra)


# ------------------------------------------------------------
# 2. PROJECT PATHS
# ------------------------------------------------------------

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

catalogue_path <- file.path(
  project_root,
  "config",
  "raster_catalogue.csv"
)

find_raster_script <- file.path(
  project_root,
  "R",
  "utils",
  "find_raster.R"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "tests"
)

output_file <- file.path(
  output_dir,
  "raster_lookup_test.csv"
)


# ------------------------------------------------------------
# 3. CHECK REQUIRED PROJECT FILES
# ------------------------------------------------------------

required_files <- c(
  catalogue_path,
  find_raster_script
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "The following required project files were not found:\n\n",
      paste(missing_files, collapse = "\n"),
      "\n\nRun this script from the root of the RStudio project."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. LOAD find_raster()
# ------------------------------------------------------------

source(find_raster_script)

if (!exists("find_raster", mode = "function")) {
  stop(
    paste0(
      "The function find_raster() was not found after sourcing:\n",
      find_raster_script
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 5. READ THE RASTER CATALOGUE
# ------------------------------------------------------------

raster_catalogue <- read_csv(
  catalogue_path,
  show_col_types = FALSE
)

if (nrow(raster_catalogue) == 0) {
  stop(
    "The raster catalogue contains no records.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 6. CHECK REQUIRED CATALOGUE COLUMNS
# ------------------------------------------------------------

required_columns <- c(
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

missing_columns <- setdiff(
  required_columns,
  names(raster_catalogue)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The raster catalogue is missing these required columns:\n",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 7. STANDARDISE CATALOGUE VALUES
# ------------------------------------------------------------

raster_catalogue <- raster_catalogue |>
  mutate(
    dataset_id = trimws(as.character(dataset_id)),
    variable_id = trimws(as.character(variable_id)),
    scenario = trimws(tolower(as.character(scenario))),
    period = trimws(as.character(period)),
    file_path = trimws(as.character(file_path)),
    units = trimws(as.character(units)),
    dataset_type = trimws(as.character(dataset_type))
  )

if (!is.logical(raster_catalogue$enabled)) {
  raster_catalogue <- raster_catalogue |>
    mutate(
      enabled = tolower(
        trimws(as.character(enabled))
      ) %in% c(
        "true",
        "t",
        "yes",
        "y",
        "1"
      )
    )
}


# ------------------------------------------------------------
# 8. CHECK FOR BLANK REQUIRED VALUES
# ------------------------------------------------------------

fields_that_cannot_be_blank <- c(
  "dataset_id",
  "variable_id",
  "scenario",
  "period",
  "file_path"
)

blank_value_check <- map_dfr(
  fields_that_cannot_be_blank,
  function(current_field) {
    
    current_values <- raster_catalogue[[current_field]]
    
    tibble(
      row_number = which(
        is.na(current_values) |
          trimws(current_values) == ""
      ),
      field = current_field
    )
  }
)

if (nrow(blank_value_check) > 0) {
  message(
    "\nBlank values were found in required catalogue fields:"
  )
  
  print(
    blank_value_check,
    n = Inf
  )
  
  stop(
    "Correct the blank catalogue values before continuing.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 9. CHECK FOR DUPLICATE ENABLED LOOKUPS
# ------------------------------------------------------------

duplicate_records <- raster_catalogue |>
  filter(enabled) |>
  count(
    variable_id,
    scenario,
    period,
    name = "number_of_records"
  ) |>
  filter(number_of_records > 1)

if (nrow(duplicate_records) > 0) {
  message(
    "\nDuplicate enabled raster records were found:"
  )
  
  print(
    duplicate_records,
    n = Inf
  )
  
  stop(
    paste0(
      "Each enabled variable, scenario and period combination ",
      "must have exactly one catalogue record."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 10. TEST SETTINGS
# ------------------------------------------------------------

test_scenario <- "ssp245"
test_period <- "2041-2070"


# ------------------------------------------------------------
# 11. SELECT THE PROTOTYPE RECORDS
# ------------------------------------------------------------

prototype_records <- raster_catalogue |>
  filter(
    enabled,
    scenario == test_scenario,
    period == test_period
  ) |>
  arrange(variable_id)

if (nrow(prototype_records) == 0) {
  stop(
    paste0(
      "No enabled catalogue records were found for:\n",
      "Scenario: ",
      test_scenario,
      "\nPeriod: ",
      test_period
    ),
    call. = FALSE
  )
}

prototype_variables <- prototype_records |>
  distinct(variable_id) |>
  arrange(variable_id) |>
  pull(variable_id)

message(
  paste0(
    "\nTesting ",
    length(prototype_variables),
    " prototype raster lookups for ",
    test_scenario,
    " / ",
    test_period,
    "."
  )
)


# ------------------------------------------------------------
# 12. TEST ONE CDD LOOKUP
# ------------------------------------------------------------

test_cdd <- find_raster(
  raster_catalogue = raster_catalogue,
  variable_id = "CDD",
  scenario = test_scenario,
  period = test_period
)

print(
  test_cdd,
  n = Inf,
  width = Inf
)

if (nrow(test_cdd) != 1) {
  stop(
    paste0(
      "The CDD lookup returned ",
      nrow(test_cdd),
      " records. Exactly one record was expected."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 13. RESOLVE CATALOGUE FILE PATHS
# ------------------------------------------------------------

resolve_raster_path <- function(
    catalogue_file_path,
    project_root
) {
  
  catalogue_file_path <- trimws(
    as.character(catalogue_file_path)
  )
  
  is_windows_absolute <- grepl(
    "^[A-Za-z]:[/\\\\]",
    catalogue_file_path
  )
  
  is_unix_absolute <- startsWith(
    catalogue_file_path,
    "/"
  )
  
  is_network_path <- grepl(
    "^[/\\\\]{2}",
    catalogue_file_path
  )
  
  if (
    is_windows_absolute ||
    is_unix_absolute ||
    is_network_path
  ) {
    return(
      normalizePath(
        catalogue_file_path,
        winslash = "/",
        mustWork = FALSE
      )
    )
  }
  
  normalizePath(
    file.path(
      project_root,
      catalogue_file_path
    ),
    winslash = "/",
    mustWork = FALSE
  )
}


# ------------------------------------------------------------
# 14. READ STORED MINIMUM AND MAXIMUM METADATA
# ------------------------------------------------------------

get_metadata_range <- function(current_raster) {
  
  raster_range <- suppressWarnings(
    tryCatch(
      {
        terra::minmax(
          current_raster,
          compute = FALSE
        )
      },
      error = function(e) {
        NULL
      }
    )
  )
  
  if (
    is.null(raster_range) ||
    length(raster_range) == 0 ||
    all(is.na(raster_range))
  ) {
    return(
      list(
        minimum = NA_real_,
        maximum = NA_real_
      )
    )
  }
  
  minimum_value <- suppressWarnings(
    min(
      raster_range[1, ],
      na.rm = TRUE
    )
  )
  
  maximum_value <- suppressWarnings(
    max(
      raster_range[2, ],
      na.rm = TRUE
    )
  )
  
  if (!is.finite(minimum_value)) {
    minimum_value <- NA_real_
  }
  
  if (!is.finite(maximum_value)) {
    maximum_value <- NA_real_
  }
  
  list(
    minimum = minimum_value,
    maximum = maximum_value
  )
}


# ------------------------------------------------------------
# 15. TEST ALL PROTOTYPE RASTER LOOKUPS
# ------------------------------------------------------------

lookup_test <- map_dfr(
  prototype_variables,
  function(current_variable) {
    
    message(
      paste0(
        "Testing: ",
        current_variable
      )
    )
    
    raster_record <- tryCatch(
      {
        find_raster(
          raster_catalogue = raster_catalogue,
          variable_id = current_variable,
          scenario = test_scenario,
          period = test_period
        )
      },
      error = function(e) {
        structure(
          list(
            error_message = conditionMessage(e)
          ),
          class = "lookup_error"
        )
      }
    )
    
    if (inherits(raster_record, "lookup_error")) {
      return(
        tibble(
          dataset_id = NA_character_,
          variable_id = current_variable,
          scenario = test_scenario,
          period = test_period,
          file_path = NA_character_,
          file_exists = FALSE,
          raster_opens = FALSE,
          number_of_layers = NA_integer_,
          number_of_rows = NA_integer_,
          number_of_columns = NA_integer_,
          crs_present = FALSE,
          has_values = FALSE,
          minimum = NA_real_,
          maximum = NA_real_,
          result = paste0(
            "LOOKUP ERROR: ",
            raster_record$error_message
          )
        )
      )
    }
    
    if (nrow(raster_record) != 1) {
      return(
        tibble(
          dataset_id = NA_character_,
          variable_id = current_variable,
          scenario = test_scenario,
          period = test_period,
          file_path = NA_character_,
          file_exists = FALSE,
          raster_opens = FALSE,
          number_of_layers = NA_integer_,
          number_of_rows = NA_integer_,
          number_of_columns = NA_integer_,
          crs_present = FALSE,
          has_values = FALSE,
          minimum = NA_real_,
          maximum = NA_real_,
          result = paste0(
            "LOOKUP FAILED: returned ",
            nrow(raster_record),
            " records"
          )
        )
      )
    }
    
    full_path <- resolve_raster_path(
      catalogue_file_path = raster_record$file_path[[1]],
      project_root = project_root
    )
    
    raster_file_exists <- file.exists(full_path)
    
    if (!raster_file_exists) {
      return(
        tibble(
          dataset_id = raster_record$dataset_id[[1]],
          variable_id = current_variable,
          scenario = test_scenario,
          period = test_period,
          file_path = full_path,
          file_exists = FALSE,
          raster_opens = FALSE,
          number_of_layers = NA_integer_,
          number_of_rows = NA_integer_,
          number_of_columns = NA_integer_,
          crs_present = FALSE,
          has_values = FALSE,
          minimum = NA_real_,
          maximum = NA_real_,
          result = "FILE NOT FOUND"
        )
      )
    }
    
    raster_result <- tryCatch(
      {
        current_raster <- terra::rast(full_path)
        
        raster_crs <- terra::crs(
          current_raster,
          proj = TRUE
        )
        
        crs_present <- (
          !is.na(raster_crs) &&
            nzchar(raster_crs)
        )
        
        raster_has_values <- terra::hasValues(
          current_raster
        )
        
        metadata_range <- get_metadata_range(
          current_raster
        )
        
        result_message <- case_when(
          terra::nlyr(current_raster) < 1 ~
            "RASTER HAS NO LAYERS",
          
          terra::nrow(current_raster) < 1 ~
            "RASTER HAS NO ROWS",
          
          terra::ncol(current_raster) < 1 ~
            "RASTER HAS NO COLUMNS",
          
          !crs_present ~
            "RASTER OPENED BUT CRS IS MISSING",
          
          !raster_has_values ~
            "RASTER OPENED BUT HAS NO VALUES",
          
          TRUE ~
            "PASS"
        )
        
        tibble(
          dataset_id = raster_record$dataset_id[[1]],
          variable_id = current_variable,
          scenario = test_scenario,
          period = test_period,
          file_path = full_path,
          file_exists = TRUE,
          raster_opens = TRUE,
          number_of_layers = terra::nlyr(current_raster),
          number_of_rows = terra::nrow(current_raster),
          number_of_columns = terra::ncol(current_raster),
          crs_present = crs_present,
          has_values = raster_has_values,
          minimum = metadata_range$minimum,
          maximum = metadata_range$maximum,
          result = result_message
        )
      },
      error = function(e) {
        tibble(
          dataset_id = raster_record$dataset_id[[1]],
          variable_id = current_variable,
          scenario = test_scenario,
          period = test_period,
          file_path = full_path,
          file_exists = TRUE,
          raster_opens = FALSE,
          number_of_layers = NA_integer_,
          number_of_rows = NA_integer_,
          number_of_columns = NA_integer_,
          crs_present = FALSE,
          has_values = FALSE,
          minimum = NA_real_,
          maximum = NA_real_,
          result = paste0(
            "RASTER OPEN ERROR: ",
            conditionMessage(e)
          )
        )
      }
    )
    
    raster_result
  }
)


# ------------------------------------------------------------
# 16. PRINT RESULTS
# ------------------------------------------------------------

message(
  "\nRaster lookup test results:"
)

print(
  lookup_test,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 17. SAVE RESULTS
# ------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  lookup_test,
  output_file
)


# ------------------------------------------------------------
# 18. CHECK FOR FAILED TESTS
# ------------------------------------------------------------

failed_tests <- lookup_test |>
  filter(result != "PASS")

if (nrow(failed_tests) > 0) {
  message(
    "\nThe following raster lookup tests failed:"
  )
  
  print(
    failed_tests,
    n = Inf,
    width = Inf
  )
  
  stop(
    paste0(
      "\n",
      nrow(failed_tests),
      " raster lookup test(s) failed.\n\n",
      "Results were saved to:\n",
      output_file
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 19. SUCCESS MESSAGE
# ------------------------------------------------------------

message(
  paste0(
    "\nAll ",
    nrow(lookup_test),
    " prototype raster lookups passed."
  )
)

message(
  "\nResults saved to:"
)

message(
  output_file
)
