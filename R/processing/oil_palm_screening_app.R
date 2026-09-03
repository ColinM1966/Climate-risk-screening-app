# ============================================================
# OIL PALM CLIMATE-STRESS SCREENING HELPERS
# For the Sabah Climate Risk Explorer
# ============================================================
#
# This module is designed to be sourced from app.R.
# It uses the existing raster_catalogue and AOI workflow but does
# NOT modify the existing single-variable climate analysis.
#
# Core indicators:
#   PPETmin       lower future value = worsening water-stress severity
#   PPETConDryMth higher future value = worsening water-stress duration
#   CDD           higher future value = worsening dry-spell persistence
#
# IMPORTANT:
#   The output is a climate-stress SCREENING result.
#   It is not a prediction of fresh fruit bunch (FFB) yield loss.
# ============================================================

OIL_PALM_CORE_VARIABLES <- c(
  "PPETmin",
  "PPETConDryMth",
  "CDD"
)

OIL_PALM_BASELINE_SCENARIO <- "baseline"
OIL_PALM_BASELINE_PERIOD <- "1981-2010"


oil_palm_indicator_metadata <- tibble::tribble(
  ~variable_id,       ~indicator,                                  ~direction,  ~units_fallback,
  "PPETmin",          "Minimum P:PET",                             "decrease", "ratio",
  "PPETConDryMth",    "Consecutive months P:PET < 1",             "increase", "months",
  "CDD",              "Consecutive dry days",                     "increase", "days"
)


# ------------------------------------------------------------
# FIND ONE EXISTING RASTER IN THE CATALOGUE
# ------------------------------------------------------------

oil_palm_get_raster_path <- function(
    raster_catalogue,
    variable_id,
    scenario,
    period
) {
  matched <- raster_catalogue |>
    dplyr::filter(
      enabled,
      .data$variable_id == !!variable_id,
      .data$scenario == !!scenario,
      .data$period == !!period
    )

  if (nrow(matched) == 0) {
    stop(
      "No raster catalogue entry for ",
      variable_id, " / ", scenario, " / ", period
    )
  }

  existing <- matched$file_path[file.exists(matched$file_path)]

  if (length(existing) == 0) {
    stop(
      "Raster is catalogued but the file is unavailable for ",
      variable_id, " / ", scenario, " / ", period
    )
  }

  existing[1]
}


# ------------------------------------------------------------
# LOAD A SINGLE-LAYER RASTER
# ------------------------------------------------------------

oil_palm_load_raster <- function(path) {
  r <- terra::rast(path)

  if (terra::nlyr(r) > 1) {
    r <- r[[1]]
  }

  r
}


# ------------------------------------------------------------
# ALIGN A RASTER TO A TEMPLATE
# ------------------------------------------------------------

oil_palm_align_raster <- function(
    r,
    template,
    method = "bilinear"
) {
  same_geometry <- terra::compareGeom(
    r,
    template,
    crs = TRUE,
    ext = TRUE,
    rowcol = TRUE,
    res = TRUE,
    stopOnError = FALSE
  )

  if (isTRUE(same_geometry)) {
    return(r)
  }

  terra::project(
    r,
    template,
    method = method
  )
}


# ------------------------------------------------------------
# CROP AND MASK A RASTER TO THE ACTIVE AOI
# ------------------------------------------------------------

oil_palm_crop_to_aoi <- function(r, aoi) {
  raster_crs <- terra::crs(r)

  aoi_for_raster <- sf::st_transform(
    aoi,
    raster_crs
  )

  aoi_vect <- terra::vect(aoi_for_raster)

  cropped <- terra::crop(
    r,
    aoi_vect
  )

  terra::mask(
    cropped,
    aoi_vect
  )
}


# ------------------------------------------------------------
# SIMPLE AOI STATISTICS
# ------------------------------------------------------------

oil_palm_raster_stats <- function(r) {
  values <- terra::values(
    r,
    mat = FALSE
  )

  values <- values[is.finite(values)]

  if (length(values) == 0) {
    return(
      list(
        mean = NA_real_,
        minimum = NA_real_,
        maximum = NA_real_
      )
    )
  }

  list(
    mean = mean(values),
    minimum = min(values),
    maximum = max(values)
  )
}


# ------------------------------------------------------------
# CONVERT CHANGE INTO POSITIVE "WORSENING"
# ------------------------------------------------------------

oil_palm_worsening <- function(
    baseline,
    future,
    direction
) {
  if (direction == "decrease") {
    # PPETmin: lower future P:PET = drier / worse
    raw <- baseline - future
  } else if (direction == "increase") {
    # PPETConDryMth and CDD: higher future value = worse
    raw <- future - baseline
  } else {
    stop("Unknown oil-palm indicator direction: ", direction)
  }

  terra::ifel(
    raw > 0,
    raw,
    0
  )
}


# ------------------------------------------------------------
# SCALE WORSENING USING A FIXED SABAH REFERENCE
# ------------------------------------------------------------
#
# v4 uses ONE common reference scale for every SSP and time period.
# This makes the 0-100 score comparable across future combinations.
#
# The reference value for each indicator is calculated once as the
# 95th percentile (by default) of the MAXIMUM projected worsening at
# each Sabah grid cell across all available future SSP/period
# combinations. A score of 100 therefore represents the upper end of
# the future-change envelope used by this app; it is NOT percentage
# yield loss.
# ------------------------------------------------------------

oil_palm_scale_with_reference <- function(
    worsening_raster,
    reference_value
) {
  if (!is.finite(reference_value) || reference_value <= 0) {
    return(worsening_raster * 0)
  }

  scaled <- worsening_raster / reference_value

  terra::clamp(
    scaled,
    lower = 0,
    upper = 1,
    values = TRUE
  )
}

# ------------------------------------------------------------
# SCALE ABSOLUTE STRESS USING FIXED SABAH LOW/HIGH REFERENCES
# ------------------------------------------------------------
# v5 adds an absolute climate-stress score so areas that are
# already stressed under the baseline are distinguished from areas
# that only show a large amount of change.
# ------------------------------------------------------------

oil_palm_scale_absolute <- function(
    value_raster,
    low_reference,
    high_reference,
    direction
) {
  if (
    !is.finite(low_reference) ||
    !is.finite(high_reference) ||
    high_reference <= low_reference
  ) {
    return(value_raster * 0)
  }

  if (direction == 'decrease') {
    scaled <- (high_reference - value_raster) / (high_reference - low_reference)
  } else if (direction == 'increase') {
    scaled <- (value_raster - low_reference) / (high_reference - low_reference)
  } else {
    stop('Unknown oil-palm indicator direction: ', direction)
  }

  terra::clamp(
    scaled,
    lower = 0,
    upper = 1,
    values = TRUE
  )
}


oil_palm_sample_values <- function(
    r,
    sample_size = 20000
) {
  sampled <- tryCatch(
    {
      terra::spatSample(
        r,
        size = sample_size,
        method = 'regular',
        na.rm = TRUE,
        values = TRUE,
        as.df = TRUE
      )
    },
    error = function(e) NULL
  )

  if (is.null(sampled) || nrow(sampled) == 0) {
    sampled <- tryCatch(
      {
        terra::spatSample(
          r,
          size = sample_size,
          method = 'random',
          na.rm = TRUE,
          values = TRUE,
          as.df = TRUE
        )
      },
      error = function(e) NULL
    )
  }

  if (is.null(sampled) || nrow(sampled) == 0) {
    values <- terra::values(r, mat = FALSE)
  } else {
    values <- sampled[[ncol(sampled)]]
  }

  values <- values[is.finite(values)]
  as.numeric(values)
}


# Cache the common reference scale for the current R session so that
# a comparison does not rebuild the Sabah-wide future envelope every
# time the user changes SSPs or periods.
oil_palm_reference_cache <- new.env(parent = emptyenv())


oil_palm_reference_cache_key <- function(
    raster_catalogue,
    upper_quantile = 0.95
) {
  rows <- raster_catalogue |>
    dplyr::filter(
      enabled,
      .data$variable_id %in% OIL_PALM_CORE_VARIABLES,
      file.exists(.data$file_path)
    ) |>
    dplyr::arrange(
      .data$variable_id,
      .data$scenario,
      .data$period,
      .data$file_path
    )

  if (nrow(rows) == 0) {
    return(paste0('empty_', upper_quantile))
  }

  info <- file.info(rows$file_path)

  paste(
    upper_quantile,
    paste(
      rows$variable_id,
      rows$scenario,
      rows$period,
      rows$file_path,
      as.numeric(info$mtime),
      sep = '::',
      collapse = '||'
    ),
    sep = '||'
  )
}


oil_palm_clear_reference_cache <- function() {
  rm(list = ls(envir = oil_palm_reference_cache),
     envir = oil_palm_reference_cache)
  invisible(TRUE)
}


# ------------------------------------------------------------
# AVAILABLE FUTURE SCENARIO/PERIOD COMBINATIONS
# ------------------------------------------------------------

oil_palm_available_combinations <- function(raster_catalogue) {
  available <- raster_catalogue |>
    dplyr::filter(
      enabled,
      .data$variable_id %in% OIL_PALM_CORE_VARIABLES,
      .data$scenario != OIL_PALM_BASELINE_SCENARIO,
      file.exists(.data$file_path)
    ) |>
    dplyr::distinct(
      .data$variable_id,
      .data$scenario,
      .data$period
    ) |>
    dplyr::count(
      .data$scenario,
      .data$period,
      name = "n_variables"
    ) |>
    dplyr::filter(
      .data$n_variables == length(OIL_PALM_CORE_VARIABLES)
    ) |>
    dplyr::arrange(
      .data$scenario,
      .data$period
    )

  baseline_ok <- raster_catalogue |>
    dplyr::filter(
      enabled,
      .data$variable_id %in% OIL_PALM_CORE_VARIABLES,
      .data$scenario == OIL_PALM_BASELINE_SCENARIO,
      .data$period == OIL_PALM_BASELINE_PERIOD,
      file.exists(.data$file_path)
    ) |>
    dplyr::distinct(.data$variable_id) |>
    nrow() == length(OIL_PALM_CORE_VARIABLES)

  if (!baseline_ok) {
    available <- available[0, , drop = FALSE]
  }

  available
}


# ------------------------------------------------------------
# BUILD / GET COMMON SABAH REFERENCE SCALE
# ------------------------------------------------------------

oil_palm_build_reference_scale <- function(
    raster_catalogue,
    upper_quantile = 0.95,
    force = FALSE,
    sample_size = 20000
) {
  cache_key <- oil_palm_reference_cache_key(
    raster_catalogue,
    upper_quantile = upper_quantile
  )

  if (
    !isTRUE(force) &&
    exists('key', envir = oil_palm_reference_cache, inherits = FALSE) &&
    exists('value', envir = oil_palm_reference_cache, inherits = FALSE) &&
    identical(
      get('key', envir = oil_palm_reference_cache),
      cache_key
    )
  ) {
    return(get('value', envir = oil_palm_reference_cache))
  }

  available <- oil_palm_available_combinations(raster_catalogue)

  if (nrow(available) == 0) {
    stop(
      'A common Oil Palm reference scale could not be built because ',
      'no complete future SSP/time-period combinations were found.'
    )
  }

  reference_rows <- vector(
    'list',
    nrow(oil_palm_indicator_metadata)
  )

  for (i in seq_len(nrow(oil_palm_indicator_metadata))) {
    meta <- oil_palm_indicator_metadata[i, ]
    variable_id <- meta$variable_id[[1]]
    direction <- meta$direction[[1]]

    baseline_path <- oil_palm_get_raster_path(
      raster_catalogue = raster_catalogue,
      variable_id = variable_id,
      scenario = OIL_PALM_BASELINE_SCENARIO,
      period = OIL_PALM_BASELINE_PERIOD
    )

    baseline_full <- oil_palm_load_raster(baseline_path)
    worsening_rasters <- vector('list', nrow(available))

    for (j in seq_len(nrow(available))) {
      future_path <- oil_palm_get_raster_path(
        raster_catalogue = raster_catalogue,
        variable_id = variable_id,
        scenario = available$scenario[[j]],
        period = available$period[[j]]
      )

      future_full <- oil_palm_load_raster(future_path)

      method <- if (variable_id == 'PPETConDryMth') {
        'near'
      } else {
        'bilinear'
      }

      future_full <- oil_palm_align_raster(
        future_full,
        baseline_full,
        method = method
      )

      worsening_rasters[[j]] <- oil_palm_worsening(
        baseline = baseline_full,
        future = future_full,
        direction = direction
      )
    }

    worsening_stack <- terra::rast(worsening_rasters)

    # Future envelope: at every Sabah cell retain the greatest
    # projected worsening found in any available SSP/period.
    future_envelope <- terra::app(
      worsening_stack,
      fun = 'max',
      na.rm = TRUE
    )

    reference_value <- tryCatch(
      {
        as.numeric(
          terra::global(
            future_envelope,
            fun = 'quantile',
            probs = upper_quantile,
            na.rm = TRUE
          )[1, 1]
        )
      },
      error = function(e) NA_real_
    )

    if (!is.finite(reference_value) || reference_value <= 0) {
      reference_value <- tryCatch(
        {
          as.numeric(
            terra::global(
              future_envelope,
              fun = 'max',
              na.rm = TRUE
            )[1, 1]
          )
        },
        error = function(e) NA_real_
      )
    }

    if (!is.finite(reference_value) || reference_value <= 0) {
      stop(
        'A positive common reference value could not be calculated for ',
        meta$indicator[[1]], '.'
      )
    }

    value_rasters <- list(baseline_full)

    for (j in seq_len(nrow(available))) {
      future_path <- oil_palm_get_raster_path(
        raster_catalogue = raster_catalogue,
        variable_id = variable_id,
        scenario = available$scenario[[j]],
        period = available$period[[j]]
      )

      future_full <- oil_palm_load_raster(future_path)
      future_full <- oil_palm_align_raster(
        future_full,
        baseline_full,
        method = method
      )

      value_rasters[[length(value_rasters) + 1]] <- future_full
    }

    sampled_values <- unlist(
      lapply(
        value_rasters,
        oil_palm_sample_values,
        sample_size = sample_size
      ),
      use.names = FALSE
    )

    sampled_values <- sampled_values[is.finite(sampled_values)]

    if (length(sampled_values) == 0) {
      stop(
        'Absolute-stress reference values could not be sampled for ',
        meta$indicator[[1]], '.'
      )
    }

    low_reference <- as.numeric(
      stats::quantile(
        sampled_values,
        probs = 0.05,
        na.rm = TRUE,
        names = FALSE
      )
    )

    high_reference <- as.numeric(
      stats::quantile(
        sampled_values,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      )
    )

    if (
      !is.finite(low_reference) ||
      !is.finite(high_reference) ||
      high_reference <= low_reference
    ) {
      low_reference <- min(sampled_values, na.rm = TRUE)
      high_reference <- max(sampled_values, na.rm = TRUE)
    }

    if (
      !is.finite(low_reference) ||
      !is.finite(high_reference) ||
      high_reference <= low_reference
    ) {
      stop(
        'Valid absolute-stress reference values could not be calculated for ',
        meta$indicator[[1]], '.'
      )
    }

    reference_rows[[i]] <- tibble::tibble(
      Variable_ID = variable_id,
      Indicator = meta$indicator[[1]],
      Change_reference_value = reference_value,
      Reference_quantile = upper_quantile,
      Absolute_low_reference = low_reference,
      Absolute_high_reference = high_reference,
      Future_combinations = nrow(available),
      Change_reference_method = paste(
        'Sabah-wide',
        paste0(round(upper_quantile * 100), 'th percentile'),
        'of the maximum projected worsening at each cell across all',
        'available future SSP/time-period combinations'
      ),
      Absolute_reference_method = paste(
        'Sabah-wide 5th and 95th percentiles of actual indicator values',
        'sampled from the baseline plus all available future',
        'SSP/time-period combinations'
      )
    )
  }

  reference_scale <- dplyr::bind_rows(reference_rows)

  assign('key', cache_key, envir = oil_palm_reference_cache)
  assign('value', reference_scale, envir = oil_palm_reference_cache)

  reference_scale
}


oil_palm_reference_value <- function(
    reference_scale,
    variable_id
) {
  value <- reference_scale |>
    dplyr::filter(.data$Variable_ID == !!variable_id) |>
    dplyr::pull(.data$Change_reference_value)

  if (length(value) == 0 || !is.finite(value[1]) || value[1] <= 0) {
    stop('No valid common Oil Palm change reference value for ', variable_id, '.')
  }

  as.numeric(value[1])
}


oil_palm_absolute_reference_values <- function(
    reference_scale,
    variable_id
) {
  row <- reference_scale |>
    dplyr::filter(.data$Variable_ID == !!variable_id)

  if (nrow(row) == 0) {
    stop('No absolute-stress reference values for ', variable_id, '.')
  }

  low_reference <- as.numeric(row$Absolute_low_reference[[1]])
  high_reference <- as.numeric(row$Absolute_high_reference[[1]])

  if (
    !is.finite(low_reference) ||
    !is.finite(high_reference) ||
    high_reference <= low_reference
  ) {
    stop('Invalid absolute-stress reference values for ', variable_id, '.')
  }

  list(
    low = low_reference,
    high = high_reference
  )
}


# ------------------------------------------------------------
# BUILD OIL-PALM SCREENING RESULT
# ------------------------------------------------------------

oil_palm_build_screening <- function(
    raster_catalogue,
    aoi,
    aoi_name,
    future_scenario,
    future_period,
    upper_quantile = 0.95,
    reference_scale = NULL
) {
  if (is.null(aoi)) {
    stop('No AOI is loaded.')
  }

  if (is.null(reference_scale)) {
    reference_scale <- oil_palm_build_reference_scale(
      raster_catalogue = raster_catalogue,
      upper_quantile = upper_quantile
    )
  }

  indicator_rows <- list()
  score_rasters <- list()
  baseline_stress_rasters <- list()
  future_stress_rasters <- list()
  agreement_rasters <- list()

  template <- NULL

  for (i in seq_len(nrow(oil_palm_indicator_metadata))) {
    meta <- oil_palm_indicator_metadata[i, ]

    variable_id <- meta$variable_id[[1]]
    direction <- meta$direction[[1]]

    baseline_path <- oil_palm_get_raster_path(
      raster_catalogue = raster_catalogue,
      variable_id = variable_id,
      scenario = OIL_PALM_BASELINE_SCENARIO,
      period = OIL_PALM_BASELINE_PERIOD
    )

    future_path <- oil_palm_get_raster_path(
      raster_catalogue = raster_catalogue,
      variable_id = variable_id,
      scenario = future_scenario,
      period = future_period
    )

    baseline_full <- oil_palm_load_raster(baseline_path)
    future_full <- oil_palm_load_raster(future_path)

    method <- if (variable_id == 'PPETConDryMth') {
      'near'
    } else {
      'bilinear'
    }

    future_full <- oil_palm_align_raster(
      future_full,
      baseline_full,
      method = method
    )

    if (is.null(template)) {
      template <- baseline_full
    }

    worsening_full <- oil_palm_worsening(
      baseline = baseline_full,
      future = future_full,
      direction = direction
    )

    reference_value <- oil_palm_reference_value(
      reference_scale,
      variable_id
    )

    absolute_refs <- oil_palm_absolute_reference_values(
      reference_scale,
      variable_id
    )

    score_full <- oil_palm_scale_with_reference(
      worsening_raster = worsening_full,
      reference_value = reference_value
    )

    baseline_stress_full <- oil_palm_scale_absolute(
      value_raster = baseline_full,
      low_reference = absolute_refs$low,
      high_reference = absolute_refs$high,
      direction = direction
    )

    future_stress_full <- oil_palm_scale_absolute(
      value_raster = future_full,
      low_reference = absolute_refs$low,
      high_reference = absolute_refs$high,
      direction = direction
    )

    score_full <- oil_palm_align_raster(
      score_full,
      template,
      method = 'bilinear'
    )

    agreement_full <- terra::ifel(
      worsening_full > 0,
      1,
      0
    )

    agreement_full <- oil_palm_align_raster(
      agreement_full,
      template,
      method = 'near'
    )

    score_rasters[[variable_id]] <- score_full
    # Store aligned COPIES for the cross-indicator composite raster.
    # Keep baseline_stress_full/future_stress_full on their native grids
    # so the AOI statistics below are not changed by resampling.
    baseline_stress_rasters[[variable_id]] <- oil_palm_align_raster(
      baseline_stress_full,
      template,
      method = 'bilinear'
    )

    future_stress_rasters[[variable_id]] <- oil_palm_align_raster(
      future_stress_full,
      template,
      method = 'bilinear'
    )
    agreement_rasters[[variable_id]] <- agreement_full

    baseline_aoi <- oil_palm_crop_to_aoi(
      baseline_full,
      aoi
    )

    future_aoi <- oil_palm_crop_to_aoi(
      future_full,
      aoi
    )

    baseline_stats <- oil_palm_raster_stats(baseline_aoi)
    future_stats <- oil_palm_raster_stats(future_aoi)
    indicator_score_stats <- oil_palm_raster_stats(
      oil_palm_crop_to_aoi(score_full, aoi)
    )

    baseline_stress_stats <- oil_palm_raster_stats(
      oil_palm_crop_to_aoi(baseline_stress_full, aoi)
    )

    future_stress_stats <- oil_palm_raster_stats(
      oil_palm_crop_to_aoi(future_stress_full, aoi)
    )

    if (
      !is.finite(baseline_stats$mean) ||
      !is.finite(future_stats$mean)
    ) {
      stop(
        'No valid raster values were found in the AOI for ',
        meta$indicator[[1]], '.'
      )
    }

    raw_change <- future_stats$mean - baseline_stats$mean

    worsens_aoi <- if (direction == 'decrease') {
      raw_change < 0
    } else {
      raw_change > 0
    }

    units_value <- raster_catalogue |>
      dplyr::filter(
        enabled,
        .data$variable_id == !!variable_id,
        .data$scenario == !!future_scenario,
        .data$period == !!future_period
      ) |>
      dplyr::pull(.data$units)

    if (
      length(units_value) == 0 ||
      is.na(units_value[1]) ||
      units_value[1] == ''
    ) {
      units_value <- meta$units_fallback[[1]]
    } else {
      units_value <- units_value[1]
    }

    indicator_rows[[i]] <- tibble::tibble(
      Variable_ID = variable_id,
      Indicator = meta$indicator[[1]],
      Baseline = baseline_stats$mean,
      Future = future_stats$mean,
      Change = raw_change,
      Worsens = worsens_aoi,
      Indicator_stress_score = indicator_score_stats$mean * 100,
      Baseline_indicator_stress_score = baseline_stress_stats$mean * 100,
      Future_indicator_stress_score = future_stress_stats$mean * 100,
      Reference_value = reference_value,
      Units = units_value,
      Baseline_raster = baseline_path,
      Future_raster = future_path
    )
  }

  indicator_table <- dplyr::bind_rows(indicator_rows)

  score_stack <- terra::rast(score_rasters)
  baseline_stress_stack <- terra::rast(baseline_stress_rasters)
  future_stress_stack <- terra::rast(future_stress_rasters)
  agreement_stack <- terra::rast(agreement_rasters)

  relative_score_full <- terra::app(
    score_stack,
    fun = mean,
    na.rm = TRUE
  ) * 100

  names(relative_score_full) <- 'oil_palm_climate_stress_change_score'

  baseline_absolute_score_full <- terra::app(
    baseline_stress_stack,
    fun = mean,
    na.rm = TRUE
  ) * 100

  names(baseline_absolute_score_full) <- 'oil_palm_baseline_climate_stress_score'

  future_absolute_score_full <- terra::app(
    future_stress_stack,
    fun = mean,
    na.rm = TRUE
  ) * 100

  names(future_absolute_score_full) <- 'oil_palm_future_climate_stress_score'

  agreement_full <- terra::app(
    agreement_stack,
    fun = sum,
    na.rm = TRUE
  )

  names(agreement_full) <- 'oil_palm_stress_agreement'

  relative_score_aoi <- oil_palm_crop_to_aoi(
    relative_score_full,
    aoi
  )

  baseline_absolute_score_aoi <- oil_palm_crop_to_aoi(
    baseline_absolute_score_full,
    aoi
  )

  future_absolute_score_aoi <- oil_palm_crop_to_aoi(
    future_absolute_score_full,
    aoi
  )

  agreement_aoi <- oil_palm_crop_to_aoi(
    agreement_full,
    aoi
  )

  score_stats <- oil_palm_raster_stats(
    relative_score_aoi
  )

  baseline_absolute_score_stats <- oil_palm_raster_stats(
    baseline_absolute_score_aoi
  )

  future_absolute_score_stats <- oil_palm_raster_stats(
    future_absolute_score_aoi
  )

  agreement_stats <- oil_palm_raster_stats(
    agreement_aoi
  )

  n_worsening <- sum(
    indicator_table$Worsens,
    na.rm = TRUE
  )

  interpretation <- dplyr::case_when(
    n_worsening == 3 ~ paste(
      'All three water-stress indicators worsen for this AOI.',
      'The climate signal is consistent with increasing oil-palm water stress.',
      'This is a screening result, not a predicted yield loss.'
    ),
    n_worsening == 2 ~ paste(
      'Two of the three water-stress indicators worsen for this AOI.',
      'The climate signal suggests increasing oil-palm water stress,',
      'although not all indicators agree.',
      'This is a screening result, not a predicted yield loss.'
    ),
    n_worsening == 1 ~ paste(
      'One of the three water-stress indicators worsens for this AOI.',
      'The climate signal is mixed and should be interpreted cautiously.',
      'This is a screening result, not a predicted yield loss.'
    ),
    TRUE ~ paste(
      'None of the three core water-stress indicators worsen for this AOI',
      'under the selected scenario and period.',
      'This does not mean that oil-palm production is free from other climate risks.'
    )
  )

  list(
    aoi_name = aoi_name,
    scenario_id = future_scenario,
    scenario = if (exists('get_scenario_label')) {
      get_scenario_label(future_scenario)
    } else {
      future_scenario
    },
    period_id = future_period,
    period = if (exists('get_period_label')) {
      get_period_label(future_period)
    } else {
      future_period
    },
    baseline_period = OIL_PALM_BASELINE_PERIOD,
    indicator_table = indicator_table,
    n_worsening = n_worsening,
    agreement_mean = agreement_stats$mean,
    relative_score_mean = score_stats$mean,
    relative_score_min = score_stats$minimum,
    relative_score_max = score_stats$maximum,
    baseline_absolute_score_mean = baseline_absolute_score_stats$mean,
    future_absolute_score_mean = future_absolute_score_stats$mean,
    interpretation = interpretation,
    score_raster = future_absolute_score_aoi,
    change_score_raster = relative_score_aoi,
    baseline_score_raster = baseline_absolute_score_aoi,
    agreement_raster = agreement_aoi,
    reference_scale = reference_scale,
    score_is_comparable = TRUE
  )
}


# ------------------------------------------------------------
# BUILD ONE LIGHTWEIGHT OIL-PALM COMPARISON ROW SET
# ------------------------------------------------------------
# This calculates AOI statistics and the COMPARABLE 0-100 score using
# the common Sabah reference scale. It does not create a full statewide
# score raster for each comparison row, which keeps multi-SSP runs much
# lighter than repeatedly running the full map analysis.
# ------------------------------------------------------------

oil_palm_build_indicator_table_only <- function(
    raster_catalogue,
    aoi,
    future_scenario,
    future_period,
    reference_scale
) {
  indicator_rows <- vector(
    'list',
    nrow(oil_palm_indicator_metadata)
  )

  template <- NULL

  for (i in seq_len(nrow(oil_palm_indicator_metadata))) {
    meta <- oil_palm_indicator_metadata[i, ]

    variable_id <- meta$variable_id[[1]]
    direction <- meta$direction[[1]]

    baseline_path <- oil_palm_get_raster_path(
      raster_catalogue = raster_catalogue,
      variable_id = variable_id,
      scenario = OIL_PALM_BASELINE_SCENARIO,
      period = OIL_PALM_BASELINE_PERIOD
    )

    future_path <- oil_palm_get_raster_path(
      raster_catalogue = raster_catalogue,
      variable_id = variable_id,
      scenario = future_scenario,
      period = future_period
    )

    baseline_full <- oil_palm_load_raster(baseline_path)
    future_full <- oil_palm_load_raster(future_path)

    method <- if (variable_id == 'PPETConDryMth') {
      'near'
    } else {
      'bilinear'
    }

    future_full <- oil_palm_align_raster(
      future_full,
      baseline_full,
      method = method
    )

    if (is.null(template)) {
      template <- baseline_full
    }

    baseline_aoi <- oil_palm_crop_to_aoi(
      baseline_full,
      aoi
    )

    future_aoi <- oil_palm_crop_to_aoi(
      future_full,
      aoi
    )

    baseline_stats <- oil_palm_raster_stats(baseline_aoi)
    future_stats <- oil_palm_raster_stats(future_aoi)

    if (
      !is.finite(baseline_stats$mean) ||
      !is.finite(future_stats$mean)
    ) {
      stop(
        'No valid raster values were found in the AOI for ',
        meta$indicator[[1]], '.'
      )
    }

    worsening_full <- oil_palm_worsening(
      baseline = baseline_full,
      future = future_full,
      direction = direction
    )

    reference_value <- oil_palm_reference_value(
      reference_scale,
      variable_id
    )

    absolute_refs <- oil_palm_absolute_reference_values(
      reference_scale,
      variable_id
    )

    score_full <- oil_palm_scale_with_reference(
      worsening_raster = worsening_full,
      reference_value = reference_value
    )

    baseline_stress_full <- oil_palm_scale_absolute(
      value_raster = baseline_full,
      low_reference = absolute_refs$low,
      high_reference = absolute_refs$high,
      direction = direction
    )

    future_stress_full <- oil_palm_scale_absolute(
      value_raster = future_full,
      low_reference = absolute_refs$low,
      high_reference = absolute_refs$high,
      direction = direction
    )

    score_full <- oil_palm_align_raster(
      score_full,
      template,
      method = 'bilinear'
    )

    score_stats <- oil_palm_raster_stats(
      oil_palm_crop_to_aoi(score_full, aoi)
    )

    baseline_stress_stats <- oil_palm_raster_stats(
      oil_palm_crop_to_aoi(baseline_stress_full, aoi)
    )

    future_stress_stats <- oil_palm_raster_stats(
      oil_palm_crop_to_aoi(future_stress_full, aoi)
    )

    raw_change <- future_stats$mean - baseline_stats$mean

    worsens_aoi <- if (direction == 'decrease') {
      raw_change < 0
    } else {
      raw_change > 0
    }

    units_value <- raster_catalogue |>
      dplyr::filter(
        enabled,
        .data$variable_id == !!variable_id,
        .data$scenario == !!future_scenario,
        .data$period == !!future_period
      ) |>
      dplyr::pull(.data$units)

    if (
      length(units_value) == 0 ||
      is.na(units_value[1]) ||
      units_value[1] == ''
    ) {
      units_value <- meta$units_fallback[[1]]
    } else {
      units_value <- units_value[1]
    }

    indicator_rows[[i]] <- tibble::tibble(
      Variable_ID = variable_id,
      Indicator = meta$indicator[[1]],
      Baseline = baseline_stats$mean,
      Future = future_stats$mean,
      Change = raw_change,
      Worsens = worsens_aoi,
      Indicator_stress_score = score_stats$mean * 100,
      Baseline_indicator_stress_score = baseline_stress_stats$mean * 100,
      Future_indicator_stress_score = future_stress_stats$mean * 100,
      Reference_value = reference_value,
      Units = units_value,
      Baseline_raster = baseline_path,
      Future_raster = future_path
    )
  }

  indicator_table <- dplyr::bind_rows(indicator_rows)

  relative_score_mean <- mean(
    indicator_table$Indicator_stress_score,
    na.rm = TRUE
  )

  baseline_absolute_score_mean <- mean(
    indicator_table$Baseline_indicator_stress_score,
    na.rm = TRUE
  )

  future_absolute_score_mean <- mean(
    indicator_table$Future_indicator_stress_score,
    na.rm = TRUE
  )

  list(
    scenario_id = future_scenario,
    scenario = if (exists('get_scenario_label')) {
      get_scenario_label(future_scenario)
    } else {
      future_scenario
    },
    period_id = future_period,
    period = if (exists('get_period_label')) {
      get_period_label(future_period)
    } else {
      future_period
    },
    indicator_table = indicator_table,
    n_worsening = sum(indicator_table$Worsens, na.rm = TRUE),
    relative_score_mean = relative_score_mean,
    baseline_absolute_score_mean = baseline_absolute_score_mean,
    future_absolute_score_mean = future_absolute_score_mean
  )
}


# ------------------------------------------------------------
# BUILD OIL-PALM SCENARIO / CLIMATOLOGY COMPARISON
# ------------------------------------------------------------
#
# v4: the 0-100 Relative climate-stress-change score IS included in
# comparisons. Every row uses the SAME Sabah reference values, so the
# scores can be compared directly between SSPs and climatologies.
# ------------------------------------------------------------

oil_palm_build_comparison <- function(
    raster_catalogue,
    aoi,
    aoi_name,
    future_scenarios,
    future_periods,
    upper_quantile = 0.95,
    reference_scale = NULL
) {
  if (is.null(aoi)) {
    stop('No AOI is loaded.')
  }

  if (length(future_scenarios) == 0 || length(future_periods) == 0) {
    stop('Select at least one future scenario and one time period.')
  }

  available <- oil_palm_available_combinations(raster_catalogue) |>
    dplyr::filter(
      .data$scenario %in% future_scenarios,
      .data$period %in% future_periods
    )

  if (nrow(available) == 0) {
    stop(
      'None of the selected Oil Palm scenario/time-period combinations ',
      'has all three required raster layers.'
    )
  }

  if (is.null(reference_scale)) {
    reference_scale <- oil_palm_build_reference_scale(
      raster_catalogue = raster_catalogue,
      upper_quantile = upper_quantile
    )
  }

  screening_results <- vector('list', nrow(available))

  for (i in seq_len(nrow(available))) {
    screening_results[[i]] <- oil_palm_build_indicator_table_only(
      raster_catalogue = raster_catalogue,
      aoi = aoi,
      future_scenario = available$scenario[[i]],
      future_period = available$period[[i]],
      reference_scale = reference_scale
    )
  }

  first_table <- screening_results[[1]]$indicator_table

  baseline_long <- first_table |>
    dplyr::transmute(
      AOI = aoi_name,
      Scenario = if (exists('get_scenario_label')) {
        get_scenario_label(OIL_PALM_BASELINE_SCENARIO)
      } else {
        'Baseline'
      },
      Scenario_ID = OIL_PALM_BASELINE_SCENARIO,
      Period = if (exists('get_period_label')) {
        get_period_label(OIL_PALM_BASELINE_PERIOD)
      } else {
        OIL_PALM_BASELINE_PERIOD
      },
      Period_ID = OIL_PALM_BASELINE_PERIOD,
      Variable_ID = .data$Variable_ID,
      Indicator = .data$Indicator,
      Value = .data$Baseline,
      Change_from_baseline = 0,
      Worsens = NA,
      Indicator_stress_score = 0,
      Reference_value = .data$Reference_value,
      Units = .data$Units,
      Baseline = .data$Baseline,
      Future = .data$Baseline
    )

  future_long <- dplyr::bind_rows(
    lapply(
      screening_results,
      function(result) {
        result$indicator_table |>
          dplyr::transmute(
            AOI = aoi_name,
            Scenario = result$scenario,
            Scenario_ID = result$scenario_id,
            Period = result$period,
            Period_ID = result$period_id,
            Variable_ID = .data$Variable_ID,
            Indicator = .data$Indicator,
            Value = .data$Future,
            Change_from_baseline = .data$Change,
            Worsens = .data$Worsens,
            Indicator_stress_score = .data$Indicator_stress_score,
            Reference_value = .data$Reference_value,
            Units = .data$Units,
            Baseline = .data$Baseline,
            Future = .data$Future
          )
      }
    )
  )

  long_table <- dplyr::bind_rows(
    baseline_long,
    future_long
  )

  baseline_wide <- tibble::tibble(
    AOI = aoi_name,
    Scenario = if (exists('get_scenario_label')) {
      get_scenario_label(OIL_PALM_BASELINE_SCENARIO)
    } else {
      'Baseline'
    },
    Scenario_ID = OIL_PALM_BASELINE_SCENARIO,
    Period = if (exists('get_period_label')) {
      get_period_label(OIL_PALM_BASELINE_PERIOD)
    } else {
      OIL_PALM_BASELINE_PERIOD
    },
    Period_ID = OIL_PALM_BASELINE_PERIOD,
    PPETmin = first_table$Baseline[
      first_table$Variable_ID == 'PPETmin'
    ][1],
    PPETConDryMth = first_table$Baseline[
      first_table$Variable_ID == 'PPETConDryMth'
    ][1],
    CDD = first_table$Baseline[
      first_table$Variable_ID == 'CDD'
    ][1],
    Baseline_stress_score = mean(first_table$Baseline_indicator_stress_score, na.rm = TRUE),
    Future_stress_score = mean(first_table$Baseline_indicator_stress_score, na.rm = TRUE),
    Indicators_worsening = NA_integer_,
    Relative_stress_change_score = 0
  )

  future_wide <- dplyr::bind_rows(
    lapply(
      screening_results,
      function(result) {
        tab <- result$indicator_table

        tibble::tibble(
          AOI = aoi_name,
          Scenario = result$scenario,
          Scenario_ID = result$scenario_id,
          Period = result$period,
          Period_ID = result$period_id,
          PPETmin = tab$Future[tab$Variable_ID == 'PPETmin'][1],
          PPETConDryMth = tab$Future[
            tab$Variable_ID == 'PPETConDryMth'
          ][1],
          CDD = tab$Future[tab$Variable_ID == 'CDD'][1],
          Baseline_stress_score = result$baseline_absolute_score_mean,
          Future_stress_score = result$future_absolute_score_mean,
          Indicators_worsening = result$n_worsening,
          Relative_stress_change_score = result$relative_score_mean
        )
      }
    )
  )

  comparison_table <- dplyr::bind_rows(
    baseline_wide,
    future_wide
  )

  score_table <- comparison_table |>
    dplyr::select(
      .data$AOI,
      .data$Scenario,
      .data$Scenario_ID,
      .data$Period,
      .data$Period_ID,
      .data$Relative_stress_change_score
    )

  export_table <- long_table |>
    dplyr::left_join(
      score_table,
      by = c(
        "AOI",
        "Scenario",
        "Scenario_ID",
        "Period",
        "Period_ID"
      )
    )

  list(
    aoi_name = aoi_name,
    baseline_scenario_id = OIL_PALM_BASELINE_SCENARIO,
    baseline_period_id = OIL_PALM_BASELINE_PERIOD,
    selected_scenarios = future_scenarios,
    selected_periods = future_periods,
    n_combinations = nrow(available),
    available_combinations = available,
    comparison_table = comparison_table,
    score_table = score_table,
    long_table = long_table,
    export_table = export_table,
    reference_scale = reference_scale,
    score_is_comparable = TRUE
  )
}


# >>> PPETMIN_PHYSICAL_SCALING_V16_OIL_PALM >>>
# ------------------------------------------------------------
# PPETmin physical moisture-deficit scaling for Oil Palm
# ------------------------------------------------------------

oil_palm_ppetmin_stress_fraction <- function(x) {

  if (inherits(
    x,
    "SpatRaster"
  )) {

    return(
      terra::clamp(
        1 - x,
        lower = 0,
        upper = 1,
        values = TRUE
      )
    )
  }

  x <- as.numeric(x)

  pmax(
    0,
    pmin(
      1,
      1 - x
    )
  )
}


oil_palm_ppetmin_stress_score <- function(x) {
  oil_palm_ppetmin_stress_fraction(
    x
  ) * 100
}


if (!exists(
  ".oil_palm_worsening_before_ppet_physical_v16",
  inherits = FALSE
)) {
  .oil_palm_worsening_before_ppet_physical_v16 <-
    oil_palm_worsening
}

if (!exists(
  ".oil_palm_reference_value_before_ppet_physical_v16",
  inherits = FALSE
)) {
  .oil_palm_reference_value_before_ppet_physical_v16 <-
    oil_palm_reference_value
}

if (!exists(
  ".oil_palm_absolute_reference_values_before_ppet_physical_v16",
  inherits = FALSE
)) {
  .oil_palm_absolute_reference_values_before_ppet_physical_v16 <-
    oil_palm_absolute_reference_values
}

if (
  exists(
    "oil_palm_build_reference_scale",
    mode = "function"
  ) &&
  !exists(
    ".oil_palm_build_reference_scale_before_ppet_physical_v16",
    inherits = FALSE
  )
) {
  .oil_palm_build_reference_scale_before_ppet_physical_v16 <-
    oil_palm_build_reference_scale
}


# PPETmin is the only core Oil Palm indicator with direction
# "decrease". For that branch, calculate change in PHYSICAL
# water-deficit stress rather than change in the raw ratio.
oil_palm_worsening <- function(
    baseline,
    future,
    direction
) {

  if (identical(
    direction,
    "decrease"
  )) {

    baseline_stress <-
      oil_palm_ppetmin_stress_fraction(
        baseline
      )

    future_stress <-
      oil_palm_ppetmin_stress_fraction(
        future
      )

    delta <-
      future_stress -
      baseline_stress

    return(
      terra::clamp(
        delta,
        lower = 0,
        upper = 1,
        values = TRUE
      )
    )
  }

  .oil_palm_worsening_before_ppet_physical_v16(
    baseline = baseline,
    future = future,
    direction = direction
  )
}


# A fixed reference of 1 leaves the physical PPETmin stress-change
# fraction unchanged in oil_palm_scale_with_reference().
oil_palm_reference_value <- function(
    reference_scale,
    variable_id
) {

  if (identical(
    variable_id,
    "PPETmin"
  )) {
    return(1)
  }

  .oil_palm_reference_value_before_ppet_physical_v16(
    reference_scale = reference_scale,
    variable_id = variable_id
  )
}


# Existing oil_palm_scale_absolute() with low=0, high=1 and
# direction="decrease" becomes clamp(1 - P:PET, 0, 1).
oil_palm_absolute_reference_values <- function(
    reference_scale,
    variable_id
) {

  if (identical(
    variable_id,
    "PPETmin"
  )) {
    return(
      list(
        low = 0,
        high = 1
      )
    )
  }

  .oil_palm_absolute_reference_values_before_ppet_physical_v16(
    reference_scale = reference_scale,
    variable_id = variable_id
  )
}


if (exists(
  ".oil_palm_build_reference_scale_before_ppet_physical_v16",
  inherits = FALSE
)) {

  oil_palm_build_reference_scale <- function(...) {

    scale <-
      .oil_palm_build_reference_scale_before_ppet_physical_v16(...)

    row <- which(
      scale$Variable_ID ==
        "PPETmin"
    )

    if (length(row) == 1) {

      if (
        "Change_reference_value" %in%
          names(scale)
      ) {
        scale$Change_reference_value[
          row
        ] <- 1
      }

      if (
        "Absolute_low_reference" %in%
          names(scale)
      ) {
        scale$Absolute_low_reference[
          row
        ] <- 0
      }

      if (
        "Absolute_high_reference" %in%
          names(scale)
      ) {
        scale$Absolute_high_reference[
          row
        ] <- 1
      }

      if (
        "Change_reference_method" %in%
          names(scale)
      ) {
        scale$Change_reference_method[
          row
        ] <- paste(
          "Physical PPETmin stress change:",
          "max(0, future stress - baseline stress),",
          "where stress = max(0, min(1, 1-P:PET))"
        )
      }

      if (
        "Absolute_reference_method" %in%
          names(scale)
      ) {
        scale$Absolute_reference_method[
          row
        ] <- paste(
          "Physical P:PET balance scale:",
          "P:PET >= 1 = 0 stress;",
          "below 1, stress fraction = 1-P:PET"
        )
      }
    }

    scale
  }
}

# <<< PPETMIN_PHYSICAL_SCALING_V16_OIL_PALM <<<

