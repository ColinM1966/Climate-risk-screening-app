# ============================================================
# TEST JAMBONGAN AOI AGAINST ALL SIX PROTOTYPE RASTERS
#
# Save as:
#   examples/test_Jambongan_aoi_processing.R
#
# Run from the root of the RStudio project:
#
#   source("examples/test_Jambongan_aoi_processing.R")
#
# Required project files:
#   config/raster_catalogue.csv
#   config/variable_metadata.csv
#   data/examples/Jambongan.gpkg
#   R/utils/find_raster.R
#   R/processing/prepare_aoi.R
#   R/processing/process_continuous_raster.R
#
# Outputs:
#   outputs/tests/Jambongan_aoi_summary.csv
#   outputs/tests/Jambongan_aoi_prepared.gpkg
#   outputs/tests/Jambongan_<variable>_cropped.tif
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "sf",
  "terra",
  "dplyr",
  "purrr",
  "readr",
  "tibble",
  "fs"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(
    installed.packages()
  )
]

if (length(missing_packages) > 0) {

  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}

library(sf)
library(terra)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(fs)


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

metadata_path <- file.path(
  project_root,
  "config",
  "variable_metadata.csv"
)

aoi_path <- file.path(
  project_root,
  "data",
  "examples",
  "Jambongan.gpkg"
)

find_raster_script <- file.path(
  project_root,
  "R",
  "utils",
  "find_raster.R"
)

prepare_aoi_script <- file.path(
  project_root,
  "R",
  "processing",
  "prepare_aoi.R"
)

process_raster_script <- file.path(
  project_root,
  "R",
  "processing",
  "process_continuous_raster.R"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "tests"
)

fs::dir_create(
  output_dir
)


# ------------------------------------------------------------
# 3. CONFIRM REQUIRED FILES EXIST
# ------------------------------------------------------------

required_files <- c(
  catalogue_path,
  metadata_path,
  aoi_path,
  find_raster_script,
  prepare_aoi_script,
  process_raster_script
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {

  stop(
    paste0(
      "These required files were not found:\n\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. LOAD PROJECT FUNCTIONS
# ------------------------------------------------------------

source(
  find_raster_script
)

source(
  prepare_aoi_script
)

source(
  process_raster_script
)

required_functions <- c(
  "find_raster",
  "prepare_aoi",
  "process_continuous_raster"
)

missing_functions <- required_functions[
  !vapply(
    required_functions,
    exists,
    logical(1),
    mode = "function"
  )
]

if (length(missing_functions) > 0) {

  stop(
    paste0(
      "These functions were not created after sourcing ",
      "the project scripts:\n\n",
      paste(
        missing_functions,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 5. LOAD CONFIGURATION TABLES
# ------------------------------------------------------------

raster_catalogue <- readr::read_csv(
  catalogue_path,
  show_col_types = FALSE
)

variable_metadata <- readr::read_csv(
  metadata_path,
  show_col_types = FALSE
)

required_catalogue_columns <- c(
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

missing_catalogue_columns <- setdiff(
  required_catalogue_columns,
  names(raster_catalogue)
)

if (length(missing_catalogue_columns) > 0) {

  stop(
    paste0(
      "The raster catalogue is missing these columns:\n\n",
      paste(
        missing_catalogue_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

required_metadata_columns <- c(
  "variable_id"
)

missing_metadata_columns <- setdiff(
  required_metadata_columns,
  names(variable_metadata)
)

if (length(missing_metadata_columns) > 0) {

  stop(
    paste0(
      "The variable metadata table is missing these columns:\n\n",
      paste(
        missing_metadata_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 6. TEST SETTINGS
# ------------------------------------------------------------

test_scenario <- "ssp245"

test_period <- "2041-2070"

prototype_variables <- c(
  "Bio017",
  "Bio05",
  "CDD",
  "Fire",
  "PPETConDryMth",
  "PPETmin"
)

message(
  "\nTesting ",
  length(prototype_variables),
  " prototype variables for ",
  test_scenario,
  " / ",
  test_period,
  "."
)

available_test_records <- raster_catalogue |>
  dplyr::filter(
    .data$variable_id %in% prototype_variables,
    .data$scenario == test_scenario,
    .data$period == test_period,
    .data$enabled %in% TRUE
  )

missing_test_variables <- setdiff(
  prototype_variables,
  available_test_records$variable_id
)

if (length(missing_test_variables) > 0) {

  stop(
    paste0(
      "The following prototype variables do not have enabled ",
      test_scenario,
      " / ",
      test_period,
      " catalogue records:\n\n",
      paste(
        missing_test_variables,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

duplicate_test_records <- available_test_records |>
  dplyr::count(
    .data$variable_id,
    name = "record_count"
  ) |>
  dplyr::filter(
    .data$record_count != 1
  )

if (nrow(duplicate_test_records) > 0) {

  print(
    duplicate_test_records,
    n = Inf
  )

  stop(
    paste0(
      "Each prototype variable must have exactly one enabled ",
      test_scenario,
      " / ",
      test_period,
      " catalogue record."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 7. LOAD AND CHECK THE JAMBONGAN AOI
# ------------------------------------------------------------

jambongan_aoi <- sf::st_read(
  aoi_path,
  quiet = TRUE
)

if (nrow(jambongan_aoi) == 0) {

  stop(
    "Jambongan.gpkg contains no features.",
    call. = FALSE
  )
}

if (is.na(sf::st_crs(jambongan_aoi))) {

  stop(
    "Jambongan.gpkg has no defined coordinate reference system.",
    call. = FALSE
  )
}

invalid_before <- which(
  !sf::st_is_valid(jambongan_aoi)
)

if (length(invalid_before) > 0) {

  message(
    "Repairing ",
    length(invalid_before),
    " invalid AOI geometries."
  )

  jambongan_aoi <- sf::st_make_valid(
    jambongan_aoi
  )
}

if (any(sf::st_is_empty(jambongan_aoi))) {

  jambongan_aoi <- jambongan_aoi[
    !sf::st_is_empty(jambongan_aoi),
  ]
}

if (nrow(jambongan_aoi) == 0) {

  stop(
    "No non-empty AOI features remain after geometry checking.",
    call. = FALSE
  )
}

message(
  "\nOriginal Jambongan AOI:"
)

print(
  jambongan_aoi
)

message(
  "\nAOI CRS:"
)

print(
  sf::st_crs(jambongan_aoi)
)

message(
  "\nAOI bounding box:"
)

print(
  sf::st_bbox(jambongan_aoi)
)


# ------------------------------------------------------------
# 8. PREPARE THE AOI
#
# This wrapper supports the likely argument names used by the
# current prepare_aoi() function.
# ------------------------------------------------------------

call_prepare_aoi <- function(
    aoi_object
) {

  function_arguments <- names(
    formals(prepare_aoi)
  )

  if ("aoi_sf" %in% function_arguments) {

    return(
      prepare_aoi(
        aoi_sf = aoi_object
      )
    )
  }

  if ("aoi" %in% function_arguments) {

    return(
      prepare_aoi(
        aoi = aoi_object
      )
    )
  }

  if ("x" %in% function_arguments) {

    return(
      prepare_aoi(
        x = aoi_object
      )
    )
  }

  prepare_aoi(
    aoi_object
  )
}

prepared_aoi <- call_prepare_aoi(
  jambongan_aoi
)

if (!inherits(prepared_aoi, "sf")) {

  stop(
    "prepare_aoi() did not return an sf object.",
    call. = FALSE
  )
}

if (nrow(prepared_aoi) == 0) {

  stop(
    "prepare_aoi() returned an empty sf object.",
    call. = FALSE
  )
}

if (is.na(sf::st_crs(prepared_aoi))) {

  stop(
    "The prepared AOI has no defined CRS.",
    call. = FALSE
  )
}

if (any(!sf::st_is_valid(prepared_aoi))) {

  stop(
    "The prepared AOI still contains invalid geometry.",
    call. = FALSE
  )
}

if (any(sf::st_is_empty(prepared_aoi))) {

  stop(
    "The prepared AOI contains empty geometry.",
    call. = FALSE
  )
}

prepared_aoi_output <- file.path(
  output_dir,
  "Jambongan_aoi_prepared.gpkg"
)

if (file.exists(prepared_aoi_output)) {

  file.remove(
    prepared_aoi_output
  )
}

sf::st_write(
  prepared_aoi,
  prepared_aoi_output,
  quiet = TRUE
)


# ------------------------------------------------------------
# 9. RESOLVE RELATIVE RASTER PATHS
# ------------------------------------------------------------

resolve_project_path <- function(
    path_value
) {

  if (
    length(path_value) != 1 ||
    is.na(path_value) ||
    trimws(path_value) == ""
  ) {

    stop(
      "The raster catalogue contains a missing or empty file path.",
      call. = FALSE
    )
  }

  path_value <- trimws(
    path_value
  )

  is_absolute_path <- grepl(
    "^[A-Za-z]:[/\\\\]",
    path_value
  ) ||
    grepl(
      "^/",
      path_value
    ) ||
    grepl(
      "^\\\\\\\\",
      path_value
    )

  if (is_absolute_path) {

    return(
      normalizePath(
        path_value,
        winslash = "/",
        mustWork = FALSE
      )
    )
  }

  normalizePath(
    file.path(
      project_root,
      path_value
    ),
    winslash = "/",
    mustWork = FALSE
  )
}


# ------------------------------------------------------------
# 10. CALCULATE EXACT AOI STATISTICS
#
# These calculations provide an independent test even if the
# project's process_continuous_raster() function returns a
# different object structure.
# ------------------------------------------------------------

calculate_aoi_statistics <- function(
    raster_object,
    aoi_sf
) {

  if (!inherits(raster_object, "SpatRaster")) {

    stop(
      "raster_object must be a terra SpatRaster.",
      call. = FALSE
    )
  }

  if (terra::nlyr(raster_object) != 1) {

    stop(
      paste0(
        "Expected a single-layer raster but found ",
        terra::nlyr(raster_object),
        " layers."
      ),
      call. = FALSE
    )
  }

  raster_crs <- terra::crs(
    raster_object
  )

  if (is.na(raster_crs) || raster_crs == "") {

    stop(
      "The raster has no CRS.",
      call. = FALSE
    )
  }

  aoi_for_raster <- sf::st_transform(
    aoi_sf,
    raster_crs
  )

  aoi_vector <- terra::vect(
    aoi_for_raster
  )

  extracted <- terra::extract(
    raster_object,
    aoi_vector,
    exact = TRUE,
    cells = TRUE
  )

  value_columns <- setdiff(
    names(extracted),
    c(
      "ID",
      "cell",
      "fraction",
      "weight"
    )
  )

  if (length(value_columns) == 0) {

    stop(
      "No raster value column was returned by terra::extract().",
      call. = FALSE
    )
  }

  value_column <- value_columns[[1]]

  values <- extracted[[value_column]]

  weights <- if (
    "fraction" %in% names(extracted)
  ) {

    extracted$fraction

  } else if (
    "weight" %in% names(extracted)
  ) {

    extracted$weight

  } else {

    rep(
      1,
      length(values)
    )
  }

  valid <- is.finite(values) &
    is.finite(weights) &
    weights > 0

  if (!any(valid)) {

    return(
      tibble::tibble(
        mean = NA_real_,
        weighted_mean = NA_real_,
        minimum = NA_real_,
        median = NA_real_,
        p90 = NA_real_,
        maximum = NA_real_,
        valid_cells = 0L
      )
    )
  }

  valid_values <- values[
    valid
  ]

  valid_weights <- weights[
    valid
  ]

  weighted_mean_value <- stats::weighted.mean(
    x = valid_values,
    w = valid_weights,
    na.rm = TRUE
  )

  valid_cell_count <- if (
    "cell" %in% names(extracted)
  ) {

    length(
      unique(
        extracted$cell[
          valid
        ]
      )
    )

  } else {

    length(valid_values)
  }

  tibble::tibble(
    mean = mean(
      valid_values,
      na.rm = TRUE
    ),
    weighted_mean = weighted_mean_value,
    minimum = min(
      valid_values,
      na.rm = TRUE
    ),
    median = stats::median(
      valid_values,
      na.rm = TRUE
    ),
    p90 = as.numeric(
      stats::quantile(
        valid_values,
        probs = 0.90,
        na.rm = TRUE,
        names = FALSE
      )
    ),
    maximum = max(
      valid_values,
      na.rm = TRUE
    ),
    valid_cells = as.integer(
      valid_cell_count
    )
  )
}


# ------------------------------------------------------------
# 11. CALL THE PROJECT PROCESSING FUNCTION
#
# This wrapper accommodates the likely argument names used in
# process_continuous_raster.R during development.
# ------------------------------------------------------------

call_process_continuous_raster <- function(
    raster_path_value,
    raster_record,
    aoi_object,
    current_output_dir
) {

  function_arguments <- names(
    formals(process_continuous_raster)
  )

  supplied_arguments <- list()

  if ("raster_path" %in% function_arguments) {

    supplied_arguments$raster_path <- raster_path_value
  }

  if ("raster_file" %in% function_arguments) {

    supplied_arguments$raster_file <- raster_path_value
  }

  if ("raster_record" %in% function_arguments) {

    supplied_arguments$raster_record <- raster_record
  }

  if ("variable_id" %in% function_arguments) {

    supplied_arguments$variable_id <-
      raster_record$variable_id[[1]]
  }

  if ("scenario" %in% function_arguments) {

    supplied_arguments$scenario <-
      raster_record$scenario[[1]]
  }

  if ("period" %in% function_arguments) {

    supplied_arguments$period <-
      raster_record$period[[1]]
  }

  if ("aoi" %in% function_arguments) {

    supplied_arguments$aoi <- aoi_object
  }

  if ("aoi_sf" %in% function_arguments) {

    supplied_arguments$aoi_sf <- aoi_object
  }

  if ("output_dir" %in% function_arguments) {

    supplied_arguments$output_dir <- current_output_dir
  }

  if ("write_cropped" %in% function_arguments) {

    supplied_arguments$write_cropped <- TRUE
  }

  if (length(supplied_arguments) == 0) {

    return(
      process_continuous_raster(
        raster_path_value,
        aoi_object
      )
    )
  }

  do.call(
    process_continuous_raster,
    supplied_arguments
  )
}


# ------------------------------------------------------------
# 12. STANDARD FAILED-RESULT ROW
# ------------------------------------------------------------

failed_result_row <- function(
    variable_id,
    dataset_id = NA_character_,
    units = NA_character_,
    raster_path = NA_character_,
    file_exists = FALSE,
    raster_opens = FALSE,
    project_function_passed = FALSE,
    result
) {

  tibble::tibble(
    variable_id = variable_id,
    dataset_id = dataset_id,
    scenario = test_scenario,
    period = test_period,
    units = units,
    raster_path = raster_path,
    file_exists = file_exists,
    raster_opens = raster_opens,
    project_function_passed = project_function_passed,
    mean = NA_real_,
    weighted_mean = NA_real_,
    minimum = NA_real_,
    median = NA_real_,
    p90 = NA_real_,
    maximum = NA_real_,
    valid_cells = 0L,
    cropped_raster = NA_character_,
    result = result
  )
}


# ------------------------------------------------------------
# 13. PROCESS ALL SIX PROTOTYPE RASTERS
# ------------------------------------------------------------

aoi_results <- purrr::map_dfr(
  prototype_variables,
  function(current_variable) {

    message(
      "\nProcessing variable: ",
      current_variable
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

        message(
          "Raster lookup failed for ",
          current_variable,
          ": ",
          conditionMessage(e)
        )

        NULL
      }
    )

    if (is.null(raster_record)) {

      return(
        failed_result_row(
          variable_id = current_variable,
          result = "find_raster() returned an error."
        )
      )
    }

    if (!inherits(raster_record, "data.frame")) {

      return(
        failed_result_row(
          variable_id = current_variable,
          result = "find_raster() did not return a data frame."
        )
      )
    }

    if (nrow(raster_record) != 1) {

      return(
        failed_result_row(
          variable_id = current_variable,
          result = paste0(
            "Lookup returned ",
            nrow(raster_record),
            " records."
          )
        )
      )
    }

    raster_path_value <- tryCatch(
      {

        resolve_project_path(
          raster_record$file_path[[1]]
        )
      },
      error = function(e) {

        NA_character_
      }
    )

    if (
      is.na(raster_path_value) ||
      !file.exists(raster_path_value)
    ) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          result = "Raster file not found."
        )
      )
    }

    current_raster <- tryCatch(
      {

        terra::rast(
          raster_path_value
        )
      },
      error = function(e) {

        message(
          "terra failed to open ",
          current_variable,
          ": ",
          conditionMessage(e)
        )

        NULL
      }
    )

    if (is.null(current_raster)) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          result = "terra could not open the raster."
        )
      )
    }

    if (terra::nlyr(current_raster) != 1) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          result = paste0(
            "Raster contains ",
            terra::nlyr(current_raster),
            " layers; one layer was expected."
          )
        )
      )
    }

    raster_crs <- terra::crs(
      current_raster
    )

    if (is.na(raster_crs) || raster_crs == "") {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          result = "Raster has no CRS."
        )
      )
    }

    variable_output_dir <- file.path(
      output_dir,
      current_variable
    )

    fs::dir_create(
      variable_output_dir
    )

    project_function_error <- NA_character_

    project_function_passed <- tryCatch(
      {

        call_process_continuous_raster(
          raster_path_value = raster_path_value,
          raster_record = raster_record,
          aoi_object = prepared_aoi,
          current_output_dir = variable_output_dir
        )

        TRUE
      },
      error = function(e) {

        project_function_error <<- conditionMessage(e)

        message(
          "Project function warning for ",
          current_variable,
          ": ",
          project_function_error
        )

        FALSE
      }
    )

    aoi_for_raster <- tryCatch(
      {

        sf::st_transform(
          prepared_aoi,
          raster_crs
        )
      },
      error = function(e) {

        NULL
      }
    )

    if (is.null(aoi_for_raster)) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          project_function_passed = project_function_passed,
          result = "The AOI could not be transformed to the raster CRS."
        )
      )
    }

    aoi_vector <- terra::vect(
      aoi_for_raster
    )

    overlap_test <- terra::relate(
      terra::ext(current_raster),
      terra::ext(aoi_vector),
      relation = "intersects"
    )

    if (!isTRUE(overlap_test)) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          project_function_passed = project_function_passed,
          result = "The Jambongan AOI does not overlap the raster extent."
        )
      )
    }

    cropped_raster <- tryCatch(
      {

        terra::crop(
          current_raster,
          aoi_vector,
          snap = "out"
        ) |>
          terra::mask(
            aoi_vector
          )
      },
      error = function(e) {

        message(
          "Crop or mask failed for ",
          current_variable,
          ": ",
          conditionMessage(e)
        )

        NULL
      }
    )

    if (is.null(cropped_raster)) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          project_function_passed = project_function_passed,
          result = "Raster crop or mask failed."
        )
      )
    }

    cropped_output <- file.path(
      output_dir,
      paste0(
        "Jambongan_",
        current_variable,
        "_cropped.tif"
      )
    )

    tryCatch(
      {

        terra::writeRaster(
          cropped_raster,
          cropped_output,
          overwrite = TRUE,
          NAflag = -9999,
          gdal = c(
            "COMPRESS=LZW",
            "TILED=YES"
          )
        )
      },
      error = function(e) {

        stop(
          paste0(
            "Could not write the cropped raster for ",
            current_variable,
            ": ",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )

    statistics <- tryCatch(
      {

        calculate_aoi_statistics(
          raster_object = current_raster,
          aoi_sf = prepared_aoi
        )
      },
      error = function(e) {

        message(
          "Statistics failed for ",
          current_variable,
          ": ",
          conditionMessage(e)
        )

        NULL
      }
    )

    if (is.null(statistics)) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          project_function_passed = project_function_passed,
          result = "AOI statistics calculation failed."
        )
      )
    }

    if (statistics$valid_cells[[1]] == 0) {

      return(
        failed_result_row(
          variable_id = current_variable,
          dataset_id = raster_record$dataset_id[[1]],
          units = raster_record$units[[1]],
          raster_path = raster_path_value,
          file_exists = TRUE,
          raster_opens = TRUE,
          project_function_passed = project_function_passed,
          result = "No valid raster cells were found inside the AOI."
        )
      )
    }

    result_message <- if (project_function_passed) {

      "PASS"

    } else {

      paste0(
        "Independent processing passed, but ",
        "process_continuous_raster() failed: ",
        project_function_error
      )
    }

    tibble::tibble(
      variable_id = current_variable,
      dataset_id = raster_record$dataset_id[[1]],
      scenario = raster_record$scenario[[1]],
      period = raster_record$period[[1]],
      units = raster_record$units[[1]],
      raster_path = raster_path_value,
      file_exists = TRUE,
      raster_opens = TRUE,
      project_function_passed = project_function_passed,
      mean = statistics$mean[[1]],
      weighted_mean = statistics$weighted_mean[[1]],
      minimum = statistics$minimum[[1]],
      median = statistics$median[[1]],
      p90 = statistics$p90[[1]],
      maximum = statistics$maximum[[1]],
      valid_cells = statistics$valid_cells[[1]],
      cropped_raster = normalizePath(
        cropped_output,
        winslash = "/",
        mustWork = FALSE
      ),
      result = result_message
    )
  }
)


# ------------------------------------------------------------
# 14. ADD VARIABLE DISPLAY NAMES AND DESCRIPTIONS
# ------------------------------------------------------------

metadata_for_join <- variable_metadata

if (!"display_name" %in% names(metadata_for_join)) {

  metadata_for_join$display_name <-
    metadata_for_join$variable_id
}

if (!"description" %in% names(metadata_for_join)) {

  metadata_for_join$description <- NA_character_
}

metadata_for_join <- metadata_for_join |>
  dplyr::select(
    .data$variable_id,
    .data$display_name,
    .data$description
  ) |>
  dplyr::distinct(
    .data$variable_id,
    .keep_all = TRUE
  )

aoi_results <- aoi_results |>
  dplyr::left_join(
    metadata_for_join,
    by = "variable_id"
  ) |>
  dplyr::relocate(
    .data$display_name,
    .after = .data$variable_id
  ) |>
  dplyr::relocate(
    .data$description,
    .after = .data$display_name
  ) |>
  dplyr::arrange(
    match(
      .data$variable_id,
      prototype_variables
    )
  )


# ------------------------------------------------------------
# 15. PRINT AND SAVE RESULTS
# ------------------------------------------------------------

message(
  "\nJambongan AOI processing results:"
)

print(
  aoi_results,
  n = Inf,
  width = Inf
)

summary_output <- file.path(
  output_dir,
  "Jambongan_aoi_summary.csv"
)

readr::write_csv(
  aoi_results,
  summary_output,
  na = ""
)


# ------------------------------------------------------------
# 16. FINAL TEST
# ------------------------------------------------------------

failed_results <- aoi_results |>
  dplyr::filter(
    .data$result != "PASS"
  )

if (nrow(failed_results) > 0) {

  message(
    "\nFailed Jambongan AOI tests:"
  )

  print(
    failed_results,
    n = Inf,
    width = Inf
  )

  stop(
    paste0(
      nrow(failed_results),
      " of ",
      nrow(aoi_results),
      " Jambongan AOI raster tests failed.\n",
      "Review outputs/tests/Jambongan_aoi_summary.csv."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 17. SUCCESS MESSAGE
# ------------------------------------------------------------

message(
  "\n========================================"
)

message(
  "\nJAMBONGAN AOI PROCESSING TEST COMPLETE"
)

message(
  "\nAll six Jambongan AOI raster tests passed."
)

message(
  "\nVariables tested:"
)

message(
  paste(
    prototype_variables,
    collapse = ", "
  )
)

message(
  "\nScenario and period:"
)

message(
  test_scenario,
  " / ",
  test_period
)

message(
  "\nSummary saved to:\n",
  summary_output
)

message(
  "\nPrepared AOI saved to:\n",
  prepared_aoi_output
)

message(
  "\nCropped rasters saved under:\n",
  output_dir
)

message(
  "\n========================================"
)
