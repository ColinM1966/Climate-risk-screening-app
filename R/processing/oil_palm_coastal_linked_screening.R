# ============================================================
# OIL PALM - CONDITIONAL COASTAL INUNDATION LINKED SCREENING
# Sabah Climate Risk Explorer
#
# This is deliberately SEPARATE from the Oil Palm crop-water
# climate-stress score.
#
# It is intended for binary inundation/exposure rasters:
#   1 = exposed / inundated
#   0 = not exposed
#   NA / -9999 = NoData
#
# The linked screening:
#   - detects registered coastal inundation layers;
#   - checks whether the current AOI overlaps their coverage;
#   - retains each layer's own scenario and time-period structure;
#   - reports exposed area and percentage separately;
#   - does NOT contribute to the Oil Palm core score.
# ============================================================

OIL_PALM_COASTAL_ID_REGEX <- paste(
  c(
    "slr",
    "sea[ _-]?level",
    "storm[ _-]?surge",
    "coastal[ _-]?inund",
    "inund[ _-]?coast"
  ),
  collapse = "|"
)


oil_palm_coastal_display_name <- function(variable_id) {
  x <- as.character(variable_id)

  if (grepl("storm[ _-]?surge", x, ignore.case = TRUE)) {
    return("Storm-surge inundation")
  }

  if (grepl("slr|sea[ _-]?level", x, ignore.case = TRUE)) {
    return("Sea-level-rise inundation")
  }

  if (grepl("inund", x, ignore.case = TRUE)) {
    return("Coastal inundation")
  }

  gsub("_", " ", x, fixed = TRUE)
}


oil_palm_coastal_catalogue <- function(
    raster_catalogue,
    variable_metadata = NULL
) {
  if (is.null(raster_catalogue) || nrow(raster_catalogue) == 0) {
    return(tibble::tibble())
  }

  cat <- raster_catalogue

  if (!"enabled" %in% names(cat)) {
    cat$enabled <- TRUE
  }

  cat <- cat |>
    dplyr::filter(.data$enabled)

  if (nrow(cat) == 0) {
    return(tibble::tibble())
  }

  metadata_labels <- NULL

  if (
    !is.null(variable_metadata) &&
    nrow(variable_metadata) > 0 &&
    all(c("variable_id", "display_name") %in% names(variable_metadata))
  ) {
    metadata_labels <- variable_metadata |>
      dplyr::select(
        .data$variable_id,
        metadata_display_name = .data$display_name
      ) |>
      dplyr::distinct(.data$variable_id, .keep_all = TRUE)

    cat <- cat |>
      dplyr::left_join(
        metadata_labels,
        by = "variable_id"
      )
  } else {
    cat$metadata_display_name <- NA_character_
  }

  if (!"dataset_id" %in% names(cat)) {
    cat$dataset_id <- NA_character_
  }

  search_text <- paste(
    ifelse(is.na(cat$variable_id), "", cat$variable_id),
    ifelse(is.na(cat$dataset_id), "", cat$dataset_id),
    ifelse(
      is.na(cat$metadata_display_name),
      "",
      cat$metadata_display_name
    )
  )

  cat <- cat[
    grepl(
      OIL_PALM_COASTAL_ID_REGEX,
      search_text,
      ignore.case = TRUE,
      perl = TRUE
    ),
    ,
    drop = FALSE
  ]

  if (nrow(cat) == 0) {
    return(tibble::tibble())
  }

  cat |>
    dplyr::mutate(
      coastal_display_name = dplyr::if_else(
        !is.na(.data$metadata_display_name) &
          nzchar(.data$metadata_display_name),
        .data$metadata_display_name,
        vapply(
          .data$variable_id,
          oil_palm_coastal_display_name,
          character(1)
        )
      ),
      coastal_return_period_years = suppressWarnings(
        as.integer(
          sub(
            ".*RP0*([0-9]+)$",
            "\\1",
            .data$variable_id,
            perl = TRUE
          )
        )
      )
    ) |>
    dplyr::arrange(
      dplyr::coalesce(.data$coastal_return_period_years, Inf),
      .data$coastal_display_name,
      .data$scenario,
      .data$period
    )
}


oil_palm_coastal_record_overlaps_aoi <- function(
    raster_record,
    aoi_sf
) {
  if (
    is.null(aoi_sf) ||
    nrow(aoi_sf) == 0 ||
    nrow(raster_record) != 1 ||
    !"file_path" %in% names(raster_record)
  ) {
    return(FALSE)
  }

  raster_path <- raster_record$file_path[[1]]

  if (
    is.null(raster_path) ||
    is.na(raster_path) ||
    !nzchar(raster_path) ||
    !file.exists(raster_path)
  ) {
    return(FALSE)
  }

  tryCatch(
    {
      r <- terra::rast(raster_path)

      if (terra::nlyr(r) > 1) {
        r <- r[[1]]
      }

      raster_crs <- terra::crs(r)

      if (is.null(raster_crs) || !nzchar(raster_crs)) {
        return(FALSE)
      }

      aoi_raster <- sf::st_transform(
        sf::st_make_valid(aoi_sf),
        raster_crs
      )

      raster_extent <- terra::as.polygons(
        terra::ext(r),
        crs = raster_crs
      )

      overlap <- terra::relate(
        terra::vect(aoi_raster),
        raster_extent,
        relation = "intersects"
      )

      any(overlap)
    },
    error = function(e) {
      FALSE
    }
  )
}


oil_palm_coastal_records_for_aoi <- function(
    raster_catalogue,
    variable_metadata,
    aoi_sf
) {
  records <- oil_palm_coastal_catalogue(
    raster_catalogue = raster_catalogue,
    variable_metadata = variable_metadata
  )

  if (
    nrow(records) == 0 ||
    is.null(aoi_sf) ||
    nrow(aoi_sf) == 0
  ) {
    return(records[0, , drop = FALSE])
  }

  keep <- vapply(
    seq_len(nrow(records)),
    function(i) {
      oil_palm_coastal_record_overlaps_aoi(
        raster_record = records[i, , drop = FALSE],
        aoi_sf = aoi_sf
      )
    },
    logical(1)
  )

  records[keep, , drop = FALSE]
}


oil_palm_prepare_aoi_for_inundation <- function(
    aoi_sf,
    aoi_name = "AOI"
) {
  out <- sf::st_make_valid(aoi_sf)

  if (!"AOI_ID" %in% names(out)) {
    out$AOI_ID <- paste0("AOI_", seq_len(nrow(out)))
  }

  if (!"AOI_NAME" %in% names(out)) {
    out$AOI_NAME <- aoi_name
  }

  if (!"AOI_AREA_HA" %in% names(out)) {
    area_sf <- sf::st_transform(out, 32650)
    out$AOI_AREA_HA <- as.numeric(
      sf::st_area(area_sf)
    ) / 10000
  }

  out
}


oil_palm_run_coastal_exposure <- function(
    raster_record,
    aoi_sf,
    aoi_name = "AOI",
    output_dir = file.path(
      "outputs",
      "oil_palm",
      "coastal_exposure"
    ),
    write_cropped_raster = TRUE
) {
  if (nrow(raster_record) != 1) {
    stop(
      "Exactly one coastal inundation catalogue record must be selected.",
      call. = FALSE
    )
  }

  required <- c(
    "dataset_id",
    "variable_id",
    "scenario",
    "period",
    "file_path",
    "units",
    "nodata_value"
  )

  missing <- setdiff(
    required,
    names(raster_record)
  )

  if (length(missing) > 0) {
    stop(
      paste(
        "The coastal inundation catalogue record is missing:",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  aoi_ready <- oil_palm_prepare_aoi_for_inundation(
    aoi_sf = aoi_sf,
    aoi_name = aoi_name
  )

  # Preserve the user's original AOI area before the generic
  # inundation processor removes present-day sea.
  original_aoi_area_ha <- sum(
    aoi_ready$AOI_AREA_HA,
    na.rm = TRUE
  )
  results <- process_inundation_catalogue_raster(
    raster_record = raster_record,
    aoi_sf = aoi_ready,
    output_dir = output_dir,
    inundated_value = 1,
    write_cropped_raster = write_cropped_raster,
    overwrite = TRUE
  )

  total_aoi_ha <- sum(
    results$AOI_AREA_HA,
    na.rm = TRUE
  )

  total_raster_ha <- sum(
    results$raster_area_ha,
    na.rm = TRUE
  )

  total_exposed_ha <- sum(
    results$inundated_area_ha,
    na.rm = TRUE
  )

  pct_aoi_exposed <- if (
    is.finite(total_aoi_ha) &&
    total_aoi_ha > 0
  ) {
    100 * total_exposed_ha / total_aoi_ha
  } else {
    NA_real_
  }

  pct_covered_area_exposed <- if (
    is.finite(total_raster_ha) &&
    total_raster_ha > 0
  ) {
    100 * total_exposed_ha / total_raster_ha
  } else {
    NA_real_
  }

  raster_coverage_pct <- if (
    is.finite(total_aoi_ha) &&
    total_aoi_ha > 0
  ) {
    100 * total_raster_ha / total_aoi_ha
  } else {
    NA_real_
  }

  summary <- tibble::tibble(
    AOI_NAME = aoi_name,
    dataset_id = raster_record$dataset_id[[1]],
    variable_id = raster_record$variable_id[[1]],
    layer_name = if (
      "coastal_display_name" %in% names(raster_record)
    ) {
      raster_record$coastal_display_name[[1]]
    } else {
      oil_palm_coastal_display_name(
        raster_record$variable_id[[1]]
      )
    },
    scenario = raster_record$scenario[[1]],
    period = raster_record$period[[1]],
    AOI_area_ha = original_aoi_area_ha,
    present_day_land_area_ha = total_aoi_ha,
    existing_sea_area_ha = pmax(
      0,
      original_aoi_area_ha - total_aoi_ha
    ),
    raster_covered_area_ha = total_raster_ha,
    exposed_area_ha = total_exposed_ha,
    pct_AOI_exposed = pct_aoi_exposed,
    pct_present_day_land_exposed = pct_aoi_exposed,
    pct_covered_area_exposed = pct_covered_area_exposed,
    raster_coverage_pct = raster_coverage_pct,
    raster_coverage_land_pct = raster_coverage_pct,
    cropped_raster = dplyr::first(
      stats::na.omit(results$cropped_raster)
    )
  )

  list(
    summary = summary,
    feature_results = results,
    raster_record = raster_record
  )
}
