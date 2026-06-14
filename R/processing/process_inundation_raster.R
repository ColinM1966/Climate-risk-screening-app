# ============================================================
# PROCESS INUNDATION RASTER
# Sabah Climate Risk Explorer
#
# Intended for binary inundation rasters:
#   1 = inundated
#   0 = not inundated
#   NA / -9999 = NoData
#
# The function:
# 1. reads the raster;
# 2. transforms the AOI to the raster CRS;
# 3. converts the raster to a clean binary layer;
# 4. crops and masks the raster;
# 5. saves a cropped GeoTIFF;
# 6. calculates exposed area and percentage using exactextractr;
# 7. returns one summary row per AOI.
# ============================================================

library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# SAFE FILE NAME
# ------------------------------------------------------------

make_safe_inundation_filename <- function(x) {

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

check_inundation_overlap <- function(
    raster_object,
    aoi_sf
) {

  raster_extent_polygon <- as.polygons(
    ext(raster_object),
    crs = crs(raster_object)
  )

  aoi_vect <- vect(
    aoi_sf
  )

  overlap_result <- relate(
    aoi_vect,
    raster_extent_polygon,
    relation = "intersects"
  )

  any(overlap_result)
}

# ------------------------------------------------------------
# PROCESS ONE INUNDATION RASTER
# ------------------------------------------------------------

process_inundation_raster <- function(
    raster_file,
    aoi_sf,
    variable_id,
    scenario,
    period,
    output_dir,
    dataset_id = NA_character_,
    units = "binary",
    inundated_value = 1,
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
        "Inundation raster file not found:\n",
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

  # ----------------------------------------------------------
  # CREATE CLEAN BINARY RASTER
  #
  # inundated_value becomes 1
  # all other valid values become 0
  # source NA values remain NA
  # ----------------------------------------------------------

  exposed <- ifel(
    is.na(r),
    NA,
    ifel(
      r == inundated_value,
      1,
      0
    )
  )

  names(exposed) <- "inundated"

  # ----------------------------------------------------------
  # TRANSFORM AOI TO RASTER CRS
  # ----------------------------------------------------------

  aoi_raster_crs <- st_transform(
    aoi_sf,
    crs(exposed)
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
    !check_inundation_overlap(
      raster_object = exposed,
      aoi_sf = aoi_raster_crs
    )
  ) {

    stop(
      paste0(
        "The AOI does not overlap the inundation raster:\n",
        raster_file
      ),
      call. = FALSE
    )
  }

  aoi_vect <- vect(
    aoi_raster_crs
  )

  # ----------------------------------------------------------
  # CROP AND MASK
  # ----------------------------------------------------------

  exposed_crop <- crop(
    exposed,
    aoi_vect,
    snap = "out"
  )

  if (ncell(exposed_crop) == 0) {

    stop(
      "Raster cropping returned no cells.",
      call. = FALSE
    )
  }

  exposed_mask <- mask(
    exposed_crop,
    aoi_vect
  )

  # ----------------------------------------------------------
  # CREATE OUTPUT NAME
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

  output_prefix <- make_safe_inundation_filename(
    output_prefix
  )

  variable_safe <- make_safe_inundation_filename(
    variable_id
  )

  scenario_safe <- make_safe_inundation_filename(
    scenario
  )

  period_safe <- make_safe_inundation_filename(
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
      exposed_mask,
      cropped_raster_file,
      overwrite = overwrite,
      datatype = "INT2S",
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
    exposed,
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
      # NO INTERSECTING CELLS
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

            raster_area_ha = 0,
            inundated_area_ha = 0,
            pct_inundated = 0,

            n_intersecting_cells = 0,
            n_valid_cells = 0,
            n_inundated_cells = 0,
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
      # IDENTIFY VALUE COLUMN
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
          inundated =
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
              !is.na(inundated),
              covered_area_m2,
              0
            ),

          inundated_covered_area_m2 =
            if_else(
              !is.na(inundated) &
                inundated == 1,
              covered_area_m2,
              0
            )
        )

      # ------------------------------------------------------
      # CALCULATE AREAS
      # ------------------------------------------------------

      valid_area_m2 <- sum(
        extraction$valid_covered_area_m2,
        na.rm = TRUE
      )

      inundated_area_m2 <- sum(
        extraction$inundated_covered_area_m2,
        na.rm = TRUE
      )

      raster_area_ha <-
        valid_area_m2 / 10000

      inundated_area_ha <-
        inundated_area_m2 / 10000

      pct_inundated <- if (
        valid_area_m2 > 0
      ) {

        100 *
          inundated_area_m2 /
          valid_area_m2

      } else {

        0
      }

      pct_inundated <- pmin(
        pmax(
          pct_inundated,
          0
        ),
        100
      )

      raster_coverage_pct <- if (
        !is.na(aoi_area_ha) &&
          aoi_area_ha > 0
      ) {

        100 *
          raster_area_ha /
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

      n_valid_cells <- sum(
        !is.na(
          extraction$inundated
        )
      )

      n_inundated_cells <- sum(
        !is.na(
          extraction$inundated
        ) &
          extraction$inundated == 1 &
          extraction$coverage_fraction > 0
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

        raster_area_ha =
          raster_area_ha,

        inundated_area_ha =
          inundated_area_ha,

        pct_inundated =
          pct_inundated,

        n_intersecting_cells =
          nrow(extraction),

        n_valid_cells =
          n_valid_cells,

        n_inundated_cells =
          n_inundated_cells,

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
  # ROUND OUTPUTS
  # ----------------------------------------------------------

  results %>%
    mutate(
      across(
        c(
          AOI_AREA_HA,
          raster_area_ha,
          inundated_area_ha,
          pct_inundated,
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
# PROCESS ONE CATALOGUE RECORD
# ------------------------------------------------------------

process_inundation_catalogue_raster <- function(
    raster_record,
    aoi_sf,
    output_dir,
    inundated_value = 1,
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

  process_inundation_raster(
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

    inundated_value =
      inundated_value,

    write_cropped_raster =
      write_cropped_raster,

    overwrite =
      overwrite
  )
}
