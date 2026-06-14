# ============================================================
# PROCESS CONTINUOUS RASTER
# Sabah Climate Risk Explorer
#
# Used for continuous-value rasters such as:
# - CDD
# - Bio05
# - Bio17
# - P:PET
# - Heat Index days
# - Livestock heat-stress days
# - Crop-yield change
# - Fire probability
#
# The function:
# 1. reads the raster;
# 2. transforms the AOI to the raster CRS;
# 3. crops and masks the raster;
# 4. saves a cropped GeoTIFF;
# 5. uses exactextractr for fractional-cell summaries;
# 6. returns one summary row per AOI.
# ============================================================

library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# SAFE SUMMARY FUNCTIONS
# ------------------------------------------------------------

safe_mean <- function(x) {

  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  mean(
    x,
    na.rm = TRUE
  )
}

safe_median <- function(x) {

  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  median(
    x,
    na.rm = TRUE
  )
}

safe_quantile <- function(
    x,
    probability
) {

  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  as.numeric(
    quantile(
      x,
      probs = probability,
      na.rm = TRUE,
      names = FALSE
    )
  )
}

safe_min <- function(x) {

  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  min(
    x,
    na.rm = TRUE
  )
}

safe_max <- function(x) {

  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  max(
    x,
    na.rm = TRUE
  )
}

# ------------------------------------------------------------
# SAFE FILE NAME
# ------------------------------------------------------------

make_safe_filename <- function(x) {

  x %>%
    as.character() %>%
    str_squish() %>%
    str_replace_all(
      "[^A-Za-z0-9_-]+",
      "_"
    ) %>%
    str_remove_all(
      "^_|_$"
    )
}

# ------------------------------------------------------------
# CHECK RASTER AND AOI OVERLAP
# ------------------------------------------------------------

check_raster_overlap <- function(
    raster_object,
    aoi_sf
) {

  raster_extent <- as.polygons(
    ext(raster_object),
    crs = crs(raster_object)
  )

  aoi_vect <- vect(
    aoi_sf
  )

  overlap_result <- relate(
    aoi_vect,
    raster_extent,
    relation = "intersects"
  )

  any(overlap_result)
}

# ------------------------------------------------------------
# PROCESS ONE CONTINUOUS RASTER
# ------------------------------------------------------------

process_continuous_raster <- function(
    raster_file,
    aoi_sf,
    variable_id,
    scenario,
    period,
    output_dir,
    dataset_id = NA_character_,
    units = NA_character_,
    output_prefix = NULL,
    write_cropped_raster = TRUE,
    nodata_value = -9999,
    overwrite = TRUE
) {

  # ----------------------------------------------------------
  # CHECK INPUTS
  # ----------------------------------------------------------

  if (!file.exists(raster_file)) {

    stop(
      paste0(
        "Raster file not found:\n",
        raster_file
      ),
      call. = FALSE
    )
  }

  if (!inherits(aoi_sf, "sf")) {

    stop(
      "aoi_sf must be an sf object.",
      call. = FALSE
    )
  }

  if (nrow(aoi_sf) == 0) {

    stop(
      "The AOI contains no features.",
      call. = FALSE
    )
  }

  if (is.na(st_crs(aoi_sf))) {

    stop(
      "The AOI has no defined CRS.",
      call. = FALSE
    )
  }

  if (
    is.null(variable_id) ||
      is.na(variable_id) ||
      variable_id == ""
  ) {

    stop(
      "variable_id must be supplied.",
      call. = FALSE
    )
  }

  if (
    is.null(scenario) ||
      is.na(scenario) ||
      scenario == ""
  ) {

    stop(
      "scenario must be supplied.",
      call. = FALSE
    )
  }

  if (
    is.null(period) ||
      is.na(period) ||
      period == ""
  ) {

    stop(
      "period must be supplied.",
      call. = FALSE
    )
  }

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # ----------------------------------------------------------
  # READ RASTER
  # ----------------------------------------------------------

  r <- rast(
    raster_file
  )

  if (nlyr(r) != 1) {

    stop(
      paste0(
        "Expected one raster layer, but found ",
        nlyr(r),
        " layers in:\n",
        raster_file
      ),
      call. = FALSE
    )
  }

  names(r) <- "raster_value"

  # ----------------------------------------------------------
  # TRANSFORM AOI TO RASTER CRS
  # ----------------------------------------------------------

  aoi_raster_crs <- st_transform(
    aoi_sf,
    crs(r)
  )

  aoi_raster_crs <- st_make_valid(
    aoi_raster_crs
  )

  aoi_raster_crs <- aoi_raster_crs[
    !st_is_empty(
      st_geometry(
        aoi_raster_crs
      )
    ),
  ]

  if (nrow(aoi_raster_crs) == 0) {

    stop(
      "No valid AOI geometry remained after transformation.",
      call. = FALSE
    )
  }

  if (
    !check_raster_overlap(
      raster_object = r,
      aoi_sf = aoi_raster_crs
    )
  ) {

    stop(
      paste0(
        "The AOI does not overlap raster:\n",
        raster_file
      ),
      call. = FALSE
    )
  }

  aoi_vect <- vect(
    aoi_raster_crs
  )

  # ----------------------------------------------------------
  # CROP AND MASK RASTER
  # ----------------------------------------------------------

  r_crop <- crop(
    r,
    aoi_vect,
    snap = "out"
  )

  if (ncell(r_crop) == 0) {

    stop(
      "Raster cropping returned no cells.",
      call. = FALSE
    )
  }

  r_mask <- mask(
    r_crop,
    aoi_vect
  )

  # ----------------------------------------------------------
  # CREATE OUTPUT RASTER NAME
  # ----------------------------------------------------------

  if (is.null(output_prefix)) {

    output_prefix <- if (
      "AOI_NAME" %in% names(aoi_sf)
    ) {

      paste(
        unique(
          aoi_sf$AOI_NAME
        ),
        collapse = "_"
      )

    } else {

      "AOI"
    }
  }

  output_prefix <- make_safe_filename(
    output_prefix
  )

  variable_safe <- make_safe_filename(
    variable_id
  )

  scenario_safe <- make_safe_filename(
    scenario
  )

  period_safe <- make_safe_filename(
    period
  )

  cropped_raster_file <- file.path(
    output_dir,
    paste0(
      output_prefix,
      "_",
      variable_safe,
      "_",
      scenario_safe,
      "_",
      period_safe,
      "_cropped.tif"
    )
  )

  # ----------------------------------------------------------
  # WRITE CROPPED RASTER
  # ----------------------------------------------------------

  if (write_cropped_raster) {

    writeRaster(
      r_mask,
      cropped_raster_file,
      overwrite = overwrite,
      datatype = "FLT4S",
      NAflag = nodata_value,
      gdal = c(
        "COMPRESS=LZW",
        "TILED=YES"
      )
    )
  }

  # ----------------------------------------------------------
  # EXACT EXTRACTION
  # ----------------------------------------------------------

  extraction_list <- exact_extract(
    r,
    aoi_raster_crs,
    include_area = TRUE,
    progress = FALSE
  )

  if (
    length(extraction_list) !=
      nrow(aoi_raster_crs)
  ) {

    stop(
      paste0(
        "The number of extraction results does not match ",
        "the number of AOI features."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # SUMMARISE EACH AOI
  # ----------------------------------------------------------

  results <- map2_dfr(
    extraction_list,
    seq_along(
      extraction_list
    ),
    function(
        extraction,
        aoi_index
    ) {

      aoi_row <- aoi_raster_crs[
        aoi_index,
      ]

      aoi_id <- if (
        "AOI_ID" %in% names(aoi_row)
      ) {

        as.character(
          aoi_row$AOI_ID
        )

      } else {

        paste0(
          "AOI_",
          aoi_index
        )
      }

      aoi_name <- if (
        "AOI_NAME" %in% names(aoi_row)
      ) {

        as.character(
          aoi_row$AOI_NAME
        )

      } else {

        aoi_id
      }

      aoi_area_ha <- if (
        "AOI_AREA_HA" %in% names(aoi_row)
      ) {

        as.numeric(
          aoi_row$AOI_AREA_HA
        )

      } else {

        aoi_area_projected <- st_transform(
          aoi_row,
          32650
        )

        as.numeric(
          st_area(
            aoi_area_projected
          )
        ) / 10000
      }

      # ------------------------------------------------------
      # NO EXTRACTION VALUES
      # ------------------------------------------------------

      if (
        is.null(extraction) ||
          nrow(extraction) == 0
      ) {

        return(
          tibble(
            AOI_ID = aoi_id,
            AOI_NAME = aoi_name,
            AOI_AREA_HA = aoi_area_ha,

            dataset_id = dataset_id,
            variable_id = variable_id,
            scenario = scenario,
            period = period,
            units = units,

            unweighted_mean = NA_real_,
            weighted_mean = NA_real_,
            median = NA_real_,
            p75 = NA_real_,
            p90 = NA_real_,
            min = NA_real_,
            max = NA_real_,

            n_intersecting_cells = 0,
            n_valid_cells = 0,
            valid_covered_area_ha = 0,
            raster_coverage_pct = 0,

            source_raster = raster_file,
            cropped_raster = if (
              write_cropped_raster
            ) {
              cropped_raster_file
            } else {
              NA_character_
            }
          )
        )
      }

      # ------------------------------------------------------
      # IDENTIFY RASTER VALUE COLUMN
      # ------------------------------------------------------

      information_columns <- c(
        "coverage_fraction",
        "area",
        "x",
        "y",
        "cell"
      )

      value_columns <- setdiff(
        names(extraction),
        information_columns
      )

      if (length(value_columns) == 0) {

        stop(
          paste0(
            "No raster-value column was returned for AOI: ",
            aoi_name,
            "\nColumns returned:\n",
            paste(
              names(extraction),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      }

      raster_value_column <-
        value_columns[1]

      extraction <- extraction %>%
        rename(
          value =
            all_of(
              raster_value_column
            )
        ) %>%
        mutate(
          covered_area_m2 =
            area *
            coverage_fraction,

          valid_covered_area_m2 =
            if_else(
              !is.na(value),
              covered_area_m2,
              0
            ),

          weighted_value =
            if_else(
              !is.na(value),
              value *
                covered_area_m2,
              0
            )
        )

      # ------------------------------------------------------
      # CALCULATE STATISTICS
      # ------------------------------------------------------

      valid_area_m2 <- sum(
        extraction$valid_covered_area_m2,
        na.rm = TRUE
      )

      weighted_mean_value <- if (
        valid_area_m2 > 0
      ) {

        sum(
          extraction$weighted_value,
          na.rm = TRUE
        ) /
          valid_area_m2

      } else {

        NA_real_
      }

      valid_values <- extraction$value[
        !is.na(
          extraction$value
        )
      ]

      valid_covered_area_ha <-
        valid_area_m2 / 10000

      raster_coverage_pct <- if (
        !is.na(aoi_area_ha) &&
          aoi_area_ha > 0
      ) {

        100 *
          valid_covered_area_ha /
          aoi_area_ha

      } else {

        NA_real_
      }

      raster_coverage_pct <- pmin(
        pmax(
          raster_coverage_pct,
          0
        ),
        100
      )

      tibble(
        AOI_ID = aoi_id,
        AOI_NAME = aoi_name,
        AOI_AREA_HA = aoi_area_ha,

        dataset_id = dataset_id,
        variable_id = variable_id,
        scenario = scenario,
        period = period,
        units = units,

        unweighted_mean =
          safe_mean(
            valid_values
          ),

        weighted_mean =
          weighted_mean_value,

        median =
          safe_median(
            valid_values
          ),

        p75 =
          safe_quantile(
            valid_values,
            0.75
          ),

        p90 =
          safe_quantile(
            valid_values,
            0.90
          ),

        min =
          safe_min(
            valid_values
          ),

        max =
          safe_max(
            valid_values
          ),

        n_intersecting_cells =
          nrow(extraction),

        n_valid_cells =
          sum(
            !is.na(
              extraction$value
            )
          ),

        valid_covered_area_ha =
          valid_covered_area_ha,

        raster_coverage_pct =
          raster_coverage_pct,

        source_raster =
          raster_file,

        cropped_raster = if (
          write_cropped_raster
        ) {
          cropped_raster_file
        } else {
          NA_character_
        }
      )
    }
  )

  # ----------------------------------------------------------
  # ROUND OUTPUT VALUES
  # ----------------------------------------------------------

  results %>%
    mutate(
      across(
        c(
          AOI_AREA_HA,
          unweighted_mean,
          weighted_mean,
          median,
          p75,
          p90,
          min,
          max,
          valid_covered_area_ha,
          raster_coverage_pct
        ),
        ~ round(
          .x,
          5
        )
      )
    )
}

# ------------------------------------------------------------
# PROCESS A CATALOGUE RECORD
# ------------------------------------------------------------

process_catalogue_raster <- function(
    raster_record,
    aoi_sf,
    output_dir,
    write_cropped_raster = TRUE,
    overwrite = TRUE
) {

  required_fields <- c(
    "dataset_id",
    "variable_id",
    "scenario",
    "period",
    "file_path",
    "units",
    "nodata_value"
  )

  missing_fields <- setdiff(
    required_fields,
    names(raster_record)
  )

  if (length(missing_fields) > 0) {

    stop(
      paste0(
        "The raster catalogue record is missing fields:\n",
        paste(
          missing_fields,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  if (nrow(raster_record) != 1) {

    stop(
      "raster_record must contain exactly one row.",
      call. = FALSE
    )
  }

  process_continuous_raster(
    raster_file =
      raster_record$file_path[[1]],

    aoi_sf =
      aoi_sf,

    variable_id =
      raster_record$variable_id[[1]],

    scenario =
      raster_record$scenario[[1]],

    period =
      raster_record$period[[1]],

    output_dir =
      output_dir,

    dataset_id =
      raster_record$dataset_id[[1]],

    units =
      raster_record$units[[1]],

    nodata_value =
      raster_record$nodata_value[[1]],

    write_cropped_raster =
      write_cropped_raster,

    overwrite =
      overwrite
  )
}
