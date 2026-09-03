# ============================================================
# FOREST RESTORATION CLIMATE SCREENING
# Version 15
# Sabah Climate Risk Explorer
#
# Core dimensions:
#   1. Establishment water stress
#   2. Fire exposure
#
# Optional dimensions are reported separately and do not alter
# either core dimension score.
#
# Scoring rule:
#   Baseline and future are scored on the SAME fixed Sabah-wide 1981-2010 baseline scale.
#   Climate-stress change = Future score - Baseline score.
#
# IMPORTANT:
#   Scores are screening indices, not seedling mortality,
#   restoration success probability, or a prescribed pathway.
# ============================================================

RESTORATION_BASELINE_SCENARIO <- "baseline"
RESTORATION_BASELINE_PERIOD <- "1981-2010"

RESTORATION_WATER_VARIABLES <- c(
  "PPETmin",
  "PPETConDryMth",
  "CDD"
)

RESTORATION_WATER_DIRECTIONS <- c(
  PPETmin = "decrease",
  PPETConDryMth = "increase",
  CDD = "increase"
)

RESTORATION_FIRE_CANDIDATES <- c(
  "Fire",
  "FIRE_PROB"
)

RESTORATION_OPTIONAL_DIMENSIONS <- list(
  extreme_rain_erosion = list(
    label = "Extreme rainfall / erosion pressure",
    candidates = c("Rx5day", "RX5DAY", "Rx1day", "RX1DAY"),
    direction = "increase",
    interpretation = paste(
      "Higher extreme-rainfall exposure may increase erosion, soil disturbance,",
      "access problems and damage to recently restored sites."
    )
  ),
  flood_waterlogging = list(
    label = "Flood / waterlogging exposure",
    candidates = c("FLOOD_SUSC", "FloodSusceptibility", "FLOOD", "Flood"),
    direction = "increase",
    interpretation = paste(
      "Flooding and prolonged waterlogging may constrain establishment in lowland",
      "or riparian sites and may require hydrological assessment."
    )
  ),
  landslide_trigger = list(
    label = "Landslide / slope-instability pressure",
    candidates = c("LANDSLIDE_SUSC", "LandslideSusceptibility", "LANDSLIDE", "Landslide"),
    direction = "increase",
    interpretation = paste(
      "Slope instability can constrain restoration feasibility, access and",
      "intervention intensity on steep terrain."
    )
  ),
  worker_heat = list(
    label = "Field-worker heat / workability",
    candidates = c("WBGTmax", "WBGT-Sun_Max", "WBGT_SUN_MAX", "WBGT-Shade_Max", "WBGT_SHADE_MAX"),
    direction = "increase",
    interpretation = paste(
      "Higher heat exposure can reduce safe outdoor work time for planting,",
      "maintenance and monitoring teams."
    )
  )
)

restoration_reference_cache <- new.env(parent = emptyenv())

restoration_clamp01 <- function(x) {
  pmin(1, pmax(0, x))
}

restoration_stress_change <- function(baseline_score, future_score) {
  future_score - baseline_score
}

restoration_stress_class <- function(score) {
  if (!is.finite(score)) return("Not available")
  if (score < 33.333) return("Lower relative exposure")
  if (score < 66.667) return("Moderate relative exposure")
  "Higher relative exposure"
}

restoration_get_record <- function(
    raster_catalogue,
    variable_candidates,
    scenario,
    period
) {
  x <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      tolower(.data$variable_id) %in% tolower(variable_candidates),
      .data$scenario == !!scenario,
      .data$period == !!period,
      file.exists(.data$file_path)
    )

  if (nrow(x) == 0) return(NULL)

  # Respect candidate order where more than one alias is registered.
  candidate_rank <- match(
    tolower(x$variable_id),
    tolower(variable_candidates)
  )
  x <- x[order(candidate_rank), , drop = FALSE]
  x[1, , drop = FALSE]
}

restoration_actual_variable_id <- function(raster_catalogue, candidates) {
  ids <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      tolower(.data$variable_id) %in% tolower(candidates),
      file.exists(.data$file_path)
    ) |>
    dplyr::pull(.data$variable_id) |>
    unique()

  if (length(ids) == 0) return(NA_character_)

  ranks <- match(tolower(ids), tolower(candidates))
  ids[order(ranks)][1]
}

restoration_load_raster <- function(record) {
  if (is.null(record) || nrow(record) != 1) {
    stop("Exactly one raster record is required.", call. = FALSE)
  }

  path <- record$file_path[[1]]
  if (!file.exists(path)) stop("Raster not found: ", path, call. = FALSE)

  r <- terra::rast(path)
  if (terra::nlyr(r) > 1) r <- r[[1]]

  nodata <- suppressWarnings(as.numeric(record$nodata_value[[1]]))
  if (is.finite(nodata)) {
    r <- terra::ifel(r == nodata, NA, r)
  }
  r <- terra::ifel(r <= -1e30, NA, r)
  r
}

restoration_aoi_mean <- function(record, aoi) {
  r <- restoration_load_raster(record)
  aoi_r <- sf::st_transform(sf::st_make_valid(aoi), terra::crs(r))
  v <- terra::vect(aoi_r)
  x <- terra::crop(r, v)
  x <- terra::mask(x, v)
  vals <- terra::values(x, mat = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

restoration_reference_key <- function(
    raster_catalogue,
    variable_id,
    q_low,
    q_high
) {
  rows <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      tolower(.data$variable_id) == tolower(.env$variable_id),
      .data$scenario == RESTORATION_BASELINE_SCENARIO,
      .data$period == RESTORATION_BASELINE_PERIOD,
      file.exists(.data$file_path)
    ) |>
    dplyr::distinct(
      .data$file_path,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(
      .data$file_path
    )

  file_times <- suppressWarnings(
    as.numeric(
      file.info(
        rows$file_path
      )$mtime
    )
  )

  latest_mtime <- if (
    length(file_times) == 0 ||
    all(!is.finite(file_times))
  ) {
    0
  } else {
    max(
      file_times[
        is.finite(file_times)
      ]
    )
  }

  paste(
    "restoration_baseline_ref",
    make.names(variable_id),
    RESTORATION_BASELINE_SCENARIO,
    RESTORATION_BASELINE_PERIOD,
    sprintf(
      "q%04d",
      round(q_low * 10000)
    ),
    sprintf(
      "q%04d",
      round(q_high * 10000)
    ),
    paste0(
      "n",
      nrow(rows)
    ),
    paste0(
      "m",
      format(
        round(latest_mtime),
        scientific = FALSE,
        trim = TRUE
      )
    ),
    sep = "::"
  )
}

restoration_fixed_reference <- function(
    raster_catalogue,
    variable_id,
    q_low = 0.05,
    q_high = 0.95,
    sample_per_raster = 500000L
) {
  key <- restoration_reference_key(
    raster_catalogue,
    variable_id,
    q_low,
    q_high
  )

  if (exists(
    key,
    envir = restoration_reference_cache,
    inherits = FALSE
  )) {
    return(
      get(
        key,
        envir = restoration_reference_cache
      )
    )
  }

  # ----------------------------------------------------------
  # SABAH HISTORICAL REFERENCE ONLY
  # ----------------------------------------------------------
  rows <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      tolower(.data$variable_id) == tolower(.env$variable_id),
      .data$scenario == RESTORATION_BASELINE_SCENARIO,
      .data$period == RESTORATION_BASELINE_PERIOD,
      file.exists(.data$file_path)
    ) |>
    dplyr::distinct(
      .data$file_path,
      .keep_all = TRUE
    )

  if (nrow(rows) != 1) {
    stop(
      "Expected exactly one enabled 1981-2010 baseline raster for ",
      variable_id,
      " but found ",
      nrow(rows),
      call. = FALSE
    )
  }

  r <- restoration_load_raster(
    rows[1, , drop = FALSE]
  )

  if (terra::nlyr(r) != 1) {
    stop(
      "Historical reference raster must have one layer for ",
      variable_id,
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # DIRECT QUANTILES ACROSS RASTER CELLS
  # ----------------------------------------------------------
  # Do not use spatSample() here. terra::global() passes actual
  # raster values to the supplied quantile function.
  # `maxcell` uses a regular sample only when necessary for a
  # very large raster.
  # ----------------------------------------------------------

  max_cells <- min(
    as.integer(sample_per_raster),
    as.integer(terra::ncell(r))
  )

  q <- terra::global(
    r,
    fun = function(v, ...) {
      stats::quantile(
        v,
        probs = c(q_low, q_high),
        na.rm = TRUE,
        names = FALSE,
        type = 7
      )
    },
    maxcell = max_cells
  )

  q_values <- as.numeric(
    unlist(
      q[1, , drop = FALSE],
      use.names = FALSE
    )
  )

  q_values <- q_values[
    is.finite(q_values)
  ]

  if (length(q_values) != 2) {
    stop(
      "Expected exactly two historical quantiles for ",
      variable_id,
      " but obtained ",
      length(q_values),
      call. = FALSE
    )
  }

  out <- c(
    low = q_values[[1]],
    high = q_values[[2]]
  )

  # ----------------------------------------------------------
  # HARD VALIDATION AGAINST THE ACTUAL RASTER RANGE
  # ----------------------------------------------------------
  raster_range <- terra::global(
    r,
    fun = c("min", "max"),
    na.rm = TRUE
  )

  raster_min <- as.numeric(
    raster_range[1, "min"]
  )

  raster_max <- as.numeric(
    raster_range[1, "max"]
  )

  tolerance <- max(
    1e-10,
    abs(raster_max - raster_min) * 1e-8
  )

  valid_reference <- (
    is.finite(out[["low"]]) &&
    is.finite(out[["high"]]) &&
    out[["high"]] > out[["low"]] &&
    out[["low"]] >= raster_min - tolerance &&
    out[["high"]] <= raster_max + tolerance
  )

  if (!isTRUE(valid_reference)) {
    stop(
      "Invalid historical P5-P95 reference for ",
      variable_id,
      ". Raster range = ",
      signif(raster_min, 6),
      " to ",
      signif(raster_max, 6),
      "; calculated P5-P95 = ",
      signif(out[["low"]], 6),
      " to ",
      signif(out[["high"]], 6),
      call. = FALSE
    )
  }

  # Logical bounds for indicators with known finite domains.
  if (
    identical(variable_id, "PPETConDryMth") &&
    (out[["low"]] < 0 || out[["high"]] > 12)
  ) {
    stop(
      "Historical PPETConDryMth reference is outside 0-12 months.",
      call. = FALSE
    )
  }

  if (
    identical(variable_id, "CDD") &&
    (out[["low"]] < 0 || out[["high"]] > 366)
  ) {
    stop(
      "Historical CDD reference is outside 0-366 days.",
      call. = FALSE
    )
  }

  assign(
    key,
    out,
    envir = restoration_reference_cache
  )

  out
}

restoration_score_value <- function(value, reference, direction = "increase") {
  if (
    !is.finite(value) ||
    !is.finite(reference[["low"]]) ||
    !is.finite(reference[["high"]]) ||
    reference[["high"]] <= reference[["low"]]
  ) return(NA_real_)

  low <- reference[["low"]]
  high <- reference[["high"]]

  raw <- if (identical(direction, "decrease")) {
    (high - value) / (high - low)
  } else {
    (value - low) / (high - low)
  }

  100 * restoration_clamp01(raw)
}

restoration_fire_score <- function(value) {
  if (!is.finite(value)) return(NA_real_)
  100 * restoration_clamp01(value)
}

restoration_core_available_combinations <- function(raster_catalogue) {
  fire_id <- restoration_actual_variable_id(
    raster_catalogue,
    RESTORATION_FIRE_CANDIDATES
  )

  water_ids <- vapply(
    RESTORATION_WATER_VARIABLES,
    function(x) restoration_actual_variable_id(raster_catalogue, x),
    character(1)
  )

  if (is.na(fire_id) || any(is.na(water_ids))) {
    return(tibble::tibble(scenario = character(0), period = character(0)))
  }

  candidate <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      .data$scenario != RESTORATION_BASELINE_SCENARIO,
      file.exists(.data$file_path)
    ) |>
    dplyr::distinct(.data$scenario, .data$period)

  if (nrow(candidate) == 0) return(candidate)

  keep <- vapply(
    seq_len(nrow(candidate)),
    function(i) {
      scenario <- candidate$scenario[[i]]
      period <- candidate$period[[i]]

      core_candidates <- c(RESTORATION_WATER_VARIABLES, RESTORATION_FIRE_CANDIDATES)
      water_ok <- all(vapply(
        RESTORATION_WATER_VARIABLES,
        function(v) !is.null(restoration_get_record(
          raster_catalogue, v, scenario, period
        )),
        logical(1)
      ))
      fire_ok <- !is.null(restoration_get_record(
        raster_catalogue,
        RESTORATION_FIRE_CANDIDATES,
        scenario,
        period
      ))
      water_ok && fire_ok
    },
    logical(1)
  )

  candidate[keep, , drop = FALSE] |>
    dplyr::arrange(.data$scenario, .data$period)
}

restoration_baseline_available <- function(raster_catalogue) {
  water_ok <- all(vapply(
    RESTORATION_WATER_VARIABLES,
    function(v) !is.null(restoration_get_record(
      raster_catalogue,
      v,
      RESTORATION_BASELINE_SCENARIO,
      RESTORATION_BASELINE_PERIOD
    )),
    logical(1)
  ))

  fire_ok <- !is.null(restoration_get_record(
    raster_catalogue,
    RESTORATION_FIRE_CANDIDATES,
    RESTORATION_BASELINE_SCENARIO,
    RESTORATION_BASELINE_PERIOD
  ))

  water_ok && fire_ok
}

restoration_optional_availability <- function(raster_catalogue) {
  core_combos <- restoration_core_available_combinations(raster_catalogue)

  rows <- lapply(
    names(RESTORATION_OPTIONAL_DIMENSIONS),
    function(id) {
      meta <- RESTORATION_OPTIONAL_DIMENSIONS[[id]]
      actual_id <- restoration_actual_variable_id(raster_catalogue, meta$candidates)

      baseline_ok <- FALSE
      future_count <- 0L

      if (!is.na(actual_id)) {
        baseline_ok <- !is.null(restoration_get_record(
          raster_catalogue,
          meta$candidates,
          RESTORATION_BASELINE_SCENARIO,
          RESTORATION_BASELINE_PERIOD
        ))

        if (nrow(core_combos) > 0) {
          future_count <- sum(vapply(
            seq_len(nrow(core_combos)),
            function(i) !is.null(restoration_get_record(
              raster_catalogue,
              meta$candidates,
              core_combos$scenario[[i]],
              core_combos$period[[i]]
            )),
            logical(1)
          ))
        }
      }

      tibble::tibble(
        dimension_id = id,
        label = meta$label,
        variable_id = actual_id,
        baseline_available = baseline_ok,
        future_combinations = future_count,
        available = baseline_ok && future_count > 0
      )
    }
  )

  dplyr::bind_rows(rows)
}

restoration_site_context_label <- function(id) {
  labels <- c(
    general = "General / not yet known",
    mixed_dipterocarp = "Mixed dipterocarp forest",
    kerangas = "Kerangas (heath) forest",
    peat_swamp = "Peat swamp forest",
    ultramafic = "Ultramafic forest",
    riparian_lowland = "Riparian / lowland forest",
    steep_upland = "Steep / upland site"
  )
  if (id %in% names(labels)) labels[[id]] else id
}

restoration_context_interpretation <- function(context, site_flags = character(0)) {
  base <- switch(
    context,
    mixed_dipterocarp = paste(
      "Interpret climate constraints alongside existing canopy condition,",
      "regeneration and proximity to seed sources."
    ),
    kerangas = paste(
      "Kerangas sites can have strong substrate and moisture constraints;",
      "species-site matching remains essential."
    ),
    peat_swamp = paste(
      "For peat swamp restoration, climate drying and fire exposure should be",
      "interpreted together with drainage, water table and hydrological connectivity."
    ),
    ultramafic = paste(
      "The climate screen does not represent specialised ultramafic soil chemistry;",
      "species selection must still reflect substrate conditions."
    ),
    riparian_lowland = paste(
      "Flooding, prolonged inundation and hydrological connectivity may be as",
      "important as drought for riparian and lowland restoration."
    ),
    steep_upland = paste(
      "Extreme rainfall, erosion and slope instability should be reviewed carefully",
      "for steep or upland restoration sites."
    ),
    paste(
      "Interpret the climate results together with forest condition, regeneration,",
      "soil, hydrology, degradation drivers and landscape context."
    )
  )

  flag_text <- character(0)
  if ("open_degraded" %in% site_flags) {
    flag_text <- c(flag_text, paste(
      "The site is flagged as open/severely degraded, so fire exposure and",
      "establishment stress deserve particular attention."
    ))
  }
  if ("natural_regeneration" %in% site_flags) {
    flag_text <- c(flag_text, paste(
      "Natural regeneration is reported as present; the climate screen does not",
      "by itself justify moving to a more intensive planting pathway."
    ))
  }
  if ("drained" %in% site_flags) {
    flag_text <- c(flag_text, paste(
      "Drainage is reported; hydrological rehabilitation may need separate field",
      "assessment, especially on peat or wetland sites."
    ))
  }
  if ("seasonal_flooding" %in% site_flags) {
    flag_text <- c(flag_text, paste(
      "Seasonal flooding is reported; flood and hydrological constraints should be",
      "checked even if drought stress is also increasing."
    ))
  }
  if ("steep" %in% site_flags) {
    flag_text <- c(flag_text, paste(
      "Steep terrain is reported; extreme-rainfall and slope-instability layers",
      "should be included where available."
    ))
  }
  if ("fire_driver" %in% site_flags) {
    flag_text <- c(flag_text, paste(
      "Fire is reported as an existing degradation driver; climate-related fire",
      "exposure should be considered alongside fuel, ignition and suppression capacity."
    ))
  }

  paste(c(base, flag_text), collapse = " ")
}

restoration_run_optional_dimension <- function(
    raster_catalogue,
    aoi,
    dimension_id,
    scenario,
    period
) {
  meta <- RESTORATION_OPTIONAL_DIMENSIONS[[dimension_id]]
  if (is.null(meta)) return(NULL)

  baseline_record <- restoration_get_record(
    raster_catalogue,
    meta$candidates,
    RESTORATION_BASELINE_SCENARIO,
    RESTORATION_BASELINE_PERIOD
  )
  future_record <- restoration_get_record(
    raster_catalogue,
    meta$candidates,
    scenario,
    period
  )

  if (is.null(baseline_record) || is.null(future_record)) return(NULL)

  variable_id <- future_record$variable_id[[1]]
  reference <- restoration_fixed_reference(raster_catalogue, variable_id)
  b <- restoration_aoi_mean(baseline_record, aoi)
  f <- restoration_aoi_mean(future_record, aoi)
  bs <- restoration_score_value(b, reference, meta$direction)
  fs <- restoration_score_value(f, reference, meta$direction)

  tibble::tibble(
    Dimension_ID = dimension_id,
    Dimension = meta$label,
    Variable_ID = variable_id,
    Baseline_value = b,
    Future_value = f,
    Baseline_score = bs,
    Future_score = fs,
    Stress_change = restoration_stress_change(bs, fs),
    Interpretation = meta$interpretation
  )
}

restoration_run_single <- function(
    raster_catalogue,
    aoi,
    aoi_name,
    scenario,
    period,
    context = "general",
    site_flags = character(0),
    optional_dimensions = character(0)
) {
  if (!restoration_baseline_available(raster_catalogue)) {
    stop("Restoration core baseline rasters are incomplete.", call. = FALSE)
  }

  available <- restoration_core_available_combinations(raster_catalogue)
  if (!any(available$scenario == scenario & available$period == period)) {
    stop(
      "The selected scenario/period does not contain all restoration core rasters.",
      call. = FALSE
    )
  }

  water_rows <- lapply(
    RESTORATION_WATER_VARIABLES,
    function(variable_id) {
      baseline_record <- restoration_get_record(
        raster_catalogue, variable_id,
        RESTORATION_BASELINE_SCENARIO,
        RESTORATION_BASELINE_PERIOD
      )
      future_record <- restoration_get_record(
        raster_catalogue, variable_id, scenario, period
      )

      reference <- restoration_fixed_reference(raster_catalogue, variable_id)
      baseline_value <- restoration_aoi_mean(baseline_record, aoi)
      future_value <- restoration_aoi_mean(future_record, aoi)
      direction <- RESTORATION_WATER_DIRECTIONS[[variable_id]]
      baseline_score <- restoration_score_value(baseline_value, reference, direction)
      future_score <- restoration_score_value(future_value, reference, direction)

      tibble::tibble(
        Variable_ID = variable_id,
        Indicator = dplyr::case_when(
          variable_id == "PPETmin" ~ "Minimum P:PET",
          variable_id == "PPETConDryMth" ~ "Consecutive months P:PET < 1",
          variable_id == "CDD" ~ "Consecutive dry days",
          TRUE ~ variable_id
        ),
        Direction_of_concern = direction,
        Baseline_value = baseline_value,
        Future_value = future_value,
        Raw_change = future_value - baseline_value,
        Baseline_stress_score = baseline_score,
        Future_stress_score = future_score,
        Stress_change = restoration_stress_change(baseline_score, future_score),
        Reference_low = reference[["low"]],
        Reference_high = reference[["high"]]
      )
    }
  )

  water_table <- dplyr::bind_rows(water_rows)
  water_baseline_score <- mean(water_table$Baseline_stress_score, na.rm = TRUE)
  water_future_score <- mean(water_table$Future_stress_score, na.rm = TRUE)

  fire_baseline_record <- restoration_get_record(
    raster_catalogue,
    RESTORATION_FIRE_CANDIDATES,
    RESTORATION_BASELINE_SCENARIO,
    RESTORATION_BASELINE_PERIOD
  )
  fire_future_record <- restoration_get_record(
    raster_catalogue,
    RESTORATION_FIRE_CANDIDATES,
    scenario,
    period
  )

  fire_baseline_value <- restoration_aoi_mean(fire_baseline_record, aoi)
  fire_future_value <- restoration_aoi_mean(fire_future_record, aoi)
  fire_baseline_score <- restoration_fire_score(fire_baseline_value)
  fire_future_score <- restoration_fire_score(fire_future_value)

  core_table <- tibble::tibble(
    Dimension_ID = c("establishment_water", "fire_drought"),
    Dimension = c("Establishment water stress", "Fire exposure"),
    Baseline_score = c(water_baseline_score, fire_baseline_score),
    Future_score = c(water_future_score, fire_future_score),
    Stress_change = c(
      restoration_stress_change(water_baseline_score, water_future_score),
      restoration_stress_change(fire_baseline_score, fire_future_score)
    )
  )

  optional_rows <- lapply(
    optional_dimensions,
    function(id) restoration_run_optional_dimension(
      raster_catalogue, aoi, id, scenario, period
    )
  )
  optional_rows <- Filter(Negate(is.null), optional_rows)
  optional_table <- if (length(optional_rows) == 0) {
    tibble::tibble()
  } else {
    dplyr::bind_rows(optional_rows)
  }

  list(
    aoi_name = aoi_name,
    scenario_id = scenario,
    period_id = period,
    context = context,
    context_label = restoration_site_context_label(context),
    site_flags = site_flags,
    water_indicator_table = water_table,
    core_table = core_table,
    optional_table = optional_table,
    interpretation = restoration_context_interpretation(context, site_flags)
  )
}

restoration_run_comparison <- function(
    raster_catalogue,
    aoi,
    aoi_name,
    scenarios,
    periods,
    context = "general",
    site_flags = character(0),
    optional_dimensions = character(0)
) {
  available <- restoration_core_available_combinations(raster_catalogue) |>
    dplyr::filter(
      .data$scenario %in% scenarios,
      .data$period %in% periods
    )

  if (nrow(available) == 0) {
    stop("No complete restoration core combinations match the selection.", call. = FALSE)
  }

  results <- lapply(
    seq_len(nrow(available)),
    function(i) restoration_run_single(
      raster_catalogue = raster_catalogue,
      aoi = aoi,
      aoi_name = aoi_name,
      scenario = available$scenario[[i]],
      period = available$period[[i]],
      context = context,
      site_flags = site_flags,
      optional_dimensions = optional_dimensions
    )
  )

  # ----------------------------------------------------------
  # Helper: extract one water-indicator row.
  # ----------------------------------------------------------
  water_value <- function(tab, variable_id, field) {
    z <- tab[
      tab$Variable_ID == variable_id,
      field,
      drop = TRUE
    ]

    if (length(z) == 0) {
      return(NA_real_)
    }

    as.numeric(z[[1]])
  }

  # ----------------------------------------------------------
  # Baseline row.
  # The first result contains the same baseline AOI values and
  # fixed historical references used by every future result.
  # ----------------------------------------------------------
  first_result <- results[[1]]
  first_core <- first_result$core_table
  first_water <- first_result$water_indicator_table

  baseline_row <- tibble::tibble(
    AOI = aoi_name,
    Scenario = "Baseline",
    Scenario_ID = RESTORATION_BASELINE_SCENARIO,
    Period = "1981–2010",
    Period_ID = RESTORATION_BASELINE_PERIOD,

    PPETmin_value = water_value(first_water, "PPETmin", "Baseline_value"),
    PPETmin_raw_change = 0,
    PPETmin_component_score = water_value(first_water, "PPETmin", "Baseline_stress_score"),
    PPETmin_component_score_change = 0,
    PPETmin_reference_P05 = water_value(first_water, "PPETmin", "Reference_low"),
    PPETmin_reference_P95 = water_value(first_water, "PPETmin", "Reference_high"),

    PPETConDryMth_value = water_value(first_water, "PPETConDryMth", "Baseline_value"),
    PPETConDryMth_raw_change = 0,
    PPETConDryMth_component_score = water_value(first_water, "PPETConDryMth", "Baseline_stress_score"),
    PPETConDryMth_component_score_change = 0,
    PPETConDryMth_reference_P05 = water_value(first_water, "PPETConDryMth", "Reference_low"),
    PPETConDryMth_reference_P95 = water_value(first_water, "PPETConDryMth", "Reference_high"),

    CDD_value = water_value(first_water, "CDD", "Baseline_value"),
    CDD_raw_change = 0,
    CDD_component_score = water_value(first_water, "CDD", "Baseline_stress_score"),
    CDD_component_score_change = 0,
    CDD_reference_P05 = water_value(first_water, "CDD", "Reference_low"),
    CDD_reference_P95 = water_value(first_water, "CDD", "Reference_high"),

    Water_stress_score = first_core$Baseline_score[
      first_core$Dimension_ID == "establishment_water"
    ][1],
    Water_stress_change = 0,
    Fire_exposure_score = first_core$Baseline_score[
      first_core$Dimension_ID == "fire_drought"
    ][1],
    Fire_exposure_change = 0
  )

  # ----------------------------------------------------------
  # Future rows.
  # ----------------------------------------------------------
  future_rows <- lapply(
    seq_along(results),
    function(i) {
      x <- results[[i]]
      core <- x$core_table
      water <- x$water_indicator_table

      tibble::tibble(
        AOI = aoi_name,
        Scenario = x$scenario_id,
        Scenario_ID = x$scenario_id,
        Period = x$period_id,
        Period_ID = x$period_id,

        PPETmin_value = water_value(water, "PPETmin", "Future_value"),
        PPETmin_raw_change = water_value(water, "PPETmin", "Raw_change"),
        PPETmin_component_score = water_value(water, "PPETmin", "Future_stress_score"),
        PPETmin_component_score_change = water_value(water, "PPETmin", "Stress_change"),
        PPETmin_reference_P05 = water_value(water, "PPETmin", "Reference_low"),
        PPETmin_reference_P95 = water_value(water, "PPETmin", "Reference_high"),

        PPETConDryMth_value = water_value(water, "PPETConDryMth", "Future_value"),
        PPETConDryMth_raw_change = water_value(water, "PPETConDryMth", "Raw_change"),
        PPETConDryMth_component_score = water_value(water, "PPETConDryMth", "Future_stress_score"),
        PPETConDryMth_component_score_change = water_value(water, "PPETConDryMth", "Stress_change"),
        PPETConDryMth_reference_P05 = water_value(water, "PPETConDryMth", "Reference_low"),
        PPETConDryMth_reference_P95 = water_value(water, "PPETConDryMth", "Reference_high"),

        CDD_value = water_value(water, "CDD", "Future_value"),
        CDD_raw_change = water_value(water, "CDD", "Raw_change"),
        CDD_component_score = water_value(water, "CDD", "Future_stress_score"),
        CDD_component_score_change = water_value(water, "CDD", "Stress_change"),
        CDD_reference_P05 = water_value(water, "CDD", "Reference_low"),
        CDD_reference_P95 = water_value(water, "CDD", "Reference_high"),

        Water_stress_score = core$Future_score[
          core$Dimension_ID == "establishment_water"
        ][1],
        Water_stress_change = core$Stress_change[
          core$Dimension_ID == "establishment_water"
        ][1],
        Fire_exposure_score = core$Future_score[
          core$Dimension_ID == "fire_drought"
        ][1],
        Fire_exposure_change = core$Stress_change[
          core$Dimension_ID == "fire_drought"
        ][1]
      )
    }
  )

  comparison_table <- dplyr::bind_rows(
    c(
      list(baseline_row),
      future_rows
    )
  )

  list(
    aoi_name = aoi_name,
    context = context,
    context_label = restoration_site_context_label(context),
    site_flags = site_flags,
    comparison_table = comparison_table,
    results = results,
    interpretation = restoration_context_interpretation(context, site_flags)
  )
}

restoration_screening_server <- function(
    input,
    output,
    session,
    rv,
    raster_catalogue,
    get_scenario_label,
    get_period_label,
    safe_filename
) {
  single_result <- shiny::reactiveVal(NULL)
  comparison_result <- shiny::reactiveVal(NULL)

  combinations <- shiny::reactive({
    restoration_core_available_combinations(raster_catalogue)
  })

  optional_availability <- shiny::reactive({
    restoration_optional_availability(raster_catalogue)
  })

  output$restoration_screening_ui <- shiny::renderUI({
    combos <- combinations()
    optional <- optional_availability()

    if (!restoration_baseline_available(raster_catalogue)) {
      return(shiny::div(
        class = "selection-status-box selection-status-incomplete",
        shiny::strong("Restoration core rasters are incomplete"),
        shiny::div(
          class = "selection-status-detail",
          "The app needs baseline PPETmin, PPETConDryMth, CDD and Fire rasters."
        )
      ))
    }

    if (nrow(combos) == 0) {
      return(shiny::div(
        class = "selection-status-box selection-status-incomplete",
        shiny::strong("No complete future restoration combinations are available"),
        shiny::div(
          class = "selection-status-detail",
          "A future scenario/period must contain all three water-stress rasters and Fire."
        )
      ))
    }

    scenarios <- unique(combos$scenario)
    periods <- unique(combos$period)

    scenario_choices <- stats::setNames(
      scenarios,
      vapply(scenarios, get_scenario_label, character(1))
    )
    period_choices <- stats::setNames(
      periods,
      vapply(periods, get_period_label, character(1))
    )

    default_scenario <- if ("ssp370" %in% scenarios) "ssp370" else scenarios[1]
    default_period <- if ("2041-2070" %in% periods) "2041-2070" else periods[1]

    optional_ready <- optional |>
      dplyr::filter(.data$available)

    optional_choices <- if (nrow(optional_ready) > 0) {
      stats::setNames(optional_ready$dimension_id, optional_ready$label)
    } else {
      character(0)
    }

    optional_missing <- optional |>
      dplyr::filter(!.data$available)

    shiny::tagList(
      shiny::h4("3. Restoration context"),
      shiny::selectInput(
        "restoration_context",
        NULL,
        choices = c(
          "General / not yet known" = "general",
          "Mixed dipterocarp forest" = "mixed_dipterocarp",
          "Kerangas (heath) forest" = "kerangas",
          "Peat swamp forest" = "peat_swamp",
          "Ultramafic forest" = "ultramafic",
          "Riparian / lowland forest" = "riparian_lowland",
          "Steep / upland site" = "steep_upland"
        ),
        selected = "general"
      ),
      shiny::checkboxGroupInput(
        "restoration_site_flags",
        "Known site conditions (optional)",
        choices = c(
          "Open / severely degraded" = "open_degraded",
          "Natural regeneration is present" = "natural_regeneration",
          "Site is drained" = "drained",
          "Site is seasonally flooded" = "seasonal_flooding",
          "Steep terrain" = "steep",
          "Fire is an existing degradation driver" = "fire_driver"
        )
      ),
      shiny::helpText(
        paste(
          "Site context changes the interpretation only; it does not manufacture",
          "field data or change the climate scores."
        )
      ),
      shiny::h4("4. Core climate dimensions"),
      shiny::div(
        class = "selection-status-box selection-status-ready",
        shiny::div("✓ Establishment water stress"),
        shiny::div("✓ Fire exposure"),
        shiny::div(
          class = "selection-status-detail",
          paste(
            "The two core dimensions are reported separately.",
            "No overall restoration score is calculated."
          )
        )
      ),
      shiny::h4("5. Additional dimensions"),
      if (length(optional_choices) > 0) {
        shiny::checkboxGroupInput(
          "restoration_optional_dimensions",
          NULL,
          choices = optional_choices
        )
      } else {
        shiny::helpText("No additional restoration dimensions currently have both baseline and future rasters.")
      },
      if (nrow(optional_missing) > 0) {
        shiny::tags$details(
          shiny::tags$summary("Additional dimensions not yet available"),
          shiny::tags$ul(lapply(optional_missing$label, shiny::tags$li))
        )
      },
      shiny::helpText(
        "Additional dimensions are reported separately and do not alter the two core scores."
      ),
      shiny::h4("6. Select analysis mode"),
      shiny::radioButtons(
        "restoration_analysis_mode",
        NULL,
        choices = c(
          "Single scenario / time period" = "single",
          "Compare scenarios / time periods" = "compare"
        ),
        selected = "single"
      ),
      shiny::conditionalPanel(
        condition = "input.restoration_analysis_mode == 'single'",
        shiny::h4("7. Select scenario"),
        shiny::selectInput(
          "restoration_scenario",
          NULL,
          choices = scenario_choices,
          selected = default_scenario
        ),
        shiny::h4("8. Select time period"),
        shiny::selectInput(
          "restoration_period",
          NULL,
          choices = period_choices,
          selected = default_period
        )
      ),
      shiny::conditionalPanel(
        condition = "input.restoration_analysis_mode == 'compare'",
        shiny::h4("7. Compare scenarios"),
        shiny::selectInput(
          "restoration_comparison_scenarios",
          NULL,
          choices = scenario_choices,
          selected = scenarios,
          multiple = TRUE
        ),
        shiny::h4("8. Compare time periods"),
        shiny::selectInput(
          "restoration_comparison_periods",
          NULL,
          choices = period_choices,
          selected = periods,
          multiple = TRUE
        )
      ),
      shiny::hr(),
      shiny::h4("9. Run restoration screening"),
      shiny::helpText("Load or draw an AOI before running the restoration screening."),
      shiny::actionButton(
        "run_restoration_screening",
        "Run restoration screening",
        class = "btn-success",
        width = "100%"
      )
    )
  })

  shiny::observeEvent(
    input$restoration_scenario,
    {
      scenario <- input$restoration_scenario
      if (is.null(scenario)) return()
      combos <- combinations() |>
        dplyr::filter(.data$scenario == scenario)
      periods <- unique(combos$period)
      if (length(periods) == 0) return()
      choices <- stats::setNames(periods, vapply(periods, get_period_label, character(1)))
      selected <- if ("2041-2070" %in% periods) "2041-2070" else periods[1]
      shiny::updateSelectInput(
        session,
        "restoration_period",
        choices = choices,
        selected = selected
      )
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$run_restoration_screening,
    {
      shiny::req(rv$aoi, rv$aoi_name)

      mode <- input$restoration_analysis_mode
      context <- input$restoration_context
      flags <- input$restoration_site_flags
      optional <- input$restoration_optional_dimensions
      if (is.null(flags)) flags <- character(0)
      if (is.null(optional)) optional <- character(0)

      result <- tryCatch(
        shiny::withProgress(
          message = "Running restoration climate screening",
          value = 0,
          {
            shiny::incProgress(0.15, detail = "Reading core climate layers")

            if (identical(mode, "compare")) {
              shiny::req(
                input$restoration_comparison_scenarios,
                input$restoration_comparison_periods
              )
              x <- restoration_run_comparison(
                raster_catalogue = raster_catalogue,
                aoi = rv$aoi,
                aoi_name = rv$aoi_name,
                scenarios = input$restoration_comparison_scenarios,
                periods = input$restoration_comparison_periods,
                context = context,
                site_flags = flags,
                optional_dimensions = optional
              )
            } else {
              shiny::req(input$restoration_scenario, input$restoration_period)
              x <- restoration_run_single(
                raster_catalogue = raster_catalogue,
                aoi = rv$aoi,
                aoi_name = rv$aoi_name,
                scenario = input$restoration_scenario,
                period = input$restoration_period,
                context = context,
                site_flags = flags,
                optional_dimensions = optional
              )
            }

            shiny::incProgress(1, detail = "Complete")
            x
          }
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Restoration screening failed:", e$message),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )

      if (is.null(result)) return()

      if (identical(mode, "compare")) {
        comparison_result(result)
        single_result(NULL)
      } else {
        single_result(result)
        comparison_result(NULL)
      }

      shiny::showNotification("Restoration climate screening completed.", type = "message")
    }
  )

  output$restoration_results_ui <- shiny::renderUI({
    comp <- comparison_result()
    single <- single_result()

    if (is.null(comp) && is.null(single)) {
      return(shiny::tagList(
        shiny::h3("Forest Restoration Planning"),
        shiny::p("Select Forest Restoration Planning in the sidebar and run the screening."),
        shiny::div(
          class = "results-note",
          paste(
            "The restoration screen treats establishment water stress and fire exposure",
            "as separate core climate constraints. It does not prescribe a restoration pathway."
          )
        )
      ))
    }

    if (!is.null(comp)) {
      return(shiny::tagList(
        shiny::h3("Restoration Climate Constraints"),
        shiny::div(
          class = "oil-palm-summary-card",
          shiny::div(class = "oil-palm-summary-big", comp$context_label),
          shiny::div(paste("AOI:", comp$aoi_name, "| Baseline: 1981–2010"))
        ),
        shiny::p(comp$interpretation),
        shiny::h4("Core dimensions across selected futures"),
        DT::DTOutput("restoration_comparison_table"),
        shiny::br(),
        shiny::h4("Water-stress components"),
        shiny::p(
          paste(
            "Raw AOI means and component scores are shown below so the",
            "combined water-stress score can be checked directly.",
            "The current indicator weighting has not been changed."
          )
        ),
        DT::DTOutput("restoration_comparison_water_components_table"),
        shiny::div(
          class = "results-note",
          paste(
            "Baseline and future are scored against the same fixed Sabah-wide 1981-2010 reference distribution.",
            "Stress change = future score minus baseline score.",
            "Water stress and fire exposure are not averaged into one overall restoration score."
          )
        ),
        shiny::br(),
        shiny::downloadButton(
          "download_restoration_comparison_csv",
          "Download restoration comparison + component CSV"
        )
      ))
    }

    core_rows <- lapply(
      seq_len(nrow(single$core_table)),
      function(i) {
        x <- single$core_table[i, , drop = FALSE]
        shiny::tags$tr(
          shiny::tags$td(x$Dimension[[1]]),
          shiny::tags$td(sprintf("%.1f / 100", x$Baseline_score[[1]])),
          shiny::tags$td(sprintf("%.1f / 100", x$Future_score[[1]])),
          shiny::tags$td(sprintf("%+.1f", x$Stress_change[[1]]))
        )
      }
    )

    optional_ui <- if (nrow(single$optional_table) > 0) {
      shiny::tagList(
        shiny::h4("Additional dimensions"),
        shiny::tags$table(
          class = "table table-sm table-bordered report-table",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Dimension"),
            shiny::tags$th("Baseline score"),
            shiny::tags$th("Future score"),
            shiny::tags$th("Change")
          )),
          shiny::tags$tbody(lapply(seq_len(nrow(single$optional_table)), function(i) {
            x <- single$optional_table[i, , drop = FALSE]
            shiny::tags$tr(
              shiny::tags$td(x$Dimension[[1]]),
              shiny::tags$td(sprintf("%.1f", x$Baseline_score[[1]])),
              shiny::tags$td(sprintf("%.1f", x$Future_score[[1]])),
              shiny::tags$td(sprintf("%+.1f", x$Stress_change[[1]]))
            )
          }))
        ),
        shiny::div(
          class = "results-note",
          "Additional dimensions do not alter either core restoration score."
        )
      )
    } else NULL

    shiny::tagList(
      shiny::h3("Restoration Climate Constraints"),
      shiny::div(
        class = "oil-palm-summary-card",
        shiny::div(class = "oil-palm-summary-big", single$context_label),
        shiny::div(paste(
          "AOI:", single$aoi_name,
          "|", get_scenario_label(single$scenario_id),
          "|", get_period_label(single$period_id)
        ))
      ),
      shiny::p(single$interpretation),
      shiny::h4("Core dimensions"),
      shiny::tags$table(
        class = "table table-sm table-bordered report-table",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Dimension"),
          shiny::tags$th("Baseline"),
          shiny::tags$th("Future"),
          shiny::tags$th("Change")
        )),
        shiny::tags$tbody(core_rows)
      ),
      shiny::h4("Establishment water-stress indicators"),
      DT::DTOutput("restoration_water_indicator_table"),
      optional_ui,
      shiny::div(
        class = "results-note",
        paste(
          "The 0–100 values are screening scales, not seedling mortality,",
          "fire probability, restoration success, or a recommendation for ANR,",
          "enrichment planting or active reforestation."
        )
      ),
      shiny::br(),
      shiny::downloadButton(
        "download_restoration_single_csv",
        "Download restoration screening CSV"
      )
    )
  })

  output$restoration_water_indicator_table <- DT::renderDT({
    x <- single_result()
    shiny::req(x)
    tab <- x$water_indicator_table |>
      dplyr::select(
        .data$Indicator,
        .data$Baseline_value,
        .data$Future_value,
        .data$Raw_change,
        .data$Baseline_stress_score,
        .data$Future_stress_score,
        .data$Stress_change
      ) |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
    )
  })

  output$restoration_comparison_table <- DT::renderDT({
    x <- comparison_result()
    shiny::req(x)

    tab <- x$comparison_table
    tab$Scenario <- vapply(
      tab$Scenario_ID,
      get_scenario_label,
      character(1)
    )
    tab$Period <- vapply(
      tab$Period_ID,
      get_period_label,
      character(1)
    )

    tab <- tab |>
      dplyr::select(
        "Scenario",
        "Period",
        "Water_stress_score",
        "Water_stress_change",
        "Fire_exposure_score",
        "Fire_exposure_change"
      ) |>
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ round(.x, 1)
        )
      )

    names(tab) <- c(
      "Scenario",
      "Period",
      "Water stress score",
      "Water stress change",
      "Fire exposure score",
      "Fire exposure change"
    )

    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    )
  })

  # ----------------------------------------------------------
  # RESTORATION COMPARISON: WATER-STRESS COMPONENTS
  # ----------------------------------------------------------
  output$restoration_comparison_water_components_table <- DT::renderDT({
    x <- comparison_result()
    shiny::req(x)

    tab <- x$comparison_table
    tab$Scenario <- vapply(
      tab$Scenario_ID,
      get_scenario_label,
      character(1)
    )
    tab$Period <- vapply(
      tab$Period_ID,
      get_period_label,
      character(1)
    )

    tab <- tab |>
      dplyr::select(
        "Scenario",
        "Period",
        "PPETmin_value",
        "PPETmin_raw_change",
        "PPETmin_component_score",
        "PPETmin_component_score_change",
        "PPETConDryMth_value",
        "PPETConDryMth_raw_change",
        "PPETConDryMth_component_score",
        "PPETConDryMth_component_score_change",
        "CDD_value",
        "CDD_raw_change",
        "CDD_component_score",
        "CDD_component_score_change",
        "Water_stress_score",
        "Water_stress_change"
      ) |>
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      )

    names(tab) <- c(
      "Scenario",
      "Period",
      "Minimum P:PET",
      "P:PET raw change",
      "P:PET stress score",
      "P:PET score change",
      "Consecutive months P:PET < 1",
      "Dry-month raw change",
      "Dry-month stress score",
      "Dry-month score change",
      "CDD",
      "CDD raw change",
      "CDD stress score",
      "CDD score change",
      "Water stress score",
      "Water stress change"
    )

    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$download_restoration_single_csv <- shiny::downloadHandler(
    filename = function() {
      x <- single_result()
      shiny::req(x)
      paste0(
        "restoration_screening_",
        safe_filename(x$aoi_name), "_",
        x$scenario_id, "_", x$period_id, ".csv"
      )
    },
    content = function(file) {
      x <- single_result()
      shiny::req(x)

      core <- x$core_table |>
        dplyr::mutate(
          Section = "Core dimension",
          AOI = x$aoi_name,
          Scenario = x$scenario_id,
          Period = x$period_id,
          Context = x$context_label
        )

      readr::write_csv(core, file)
    }
  )

  output$download_restoration_comparison_csv <- shiny::downloadHandler(
    filename = function() {
      x <- comparison_result()
      shiny::req(x)
      paste0("restoration_comparison_", safe_filename(x$aoi_name), ".csv")
    },
    content = function(file) {
      x <- comparison_result()
      shiny::req(x)
      readr::write_csv(x$comparison_table, file)
    }
  )

  invisible(list(
    single_result = single_result,
    comparison_result = comparison_result,
    combinations = combinations,
    optional_availability = optional_availability
  ))
}


# >>> PPETMIN_PHYSICAL_SCALING_V16_RESTORATION >>>
# ------------------------------------------------------------
# PPETmin physical moisture-deficit scaling
# ------------------------------------------------------------

restoration_ppetmin_stress_score <- function(x) {

  x <- as.numeric(x)

  score <- (1 - x) * 100

  pmax(
    0,
    pmin(
      100,
      score
    )
  )
}


restoration_ppetmin_deficit_class <- function(x) {

  x <- as.numeric(x)

  dplyr::case_when(
    !is.finite(x) ~ NA_character_,
    x >= 1.00 ~ "No climatic water deficit",
    x >= 0.65 ~ "Low deficit",
    x >= 0.50 ~ "Moderate deficit",
    x >= 0.20 ~ "High deficit",
    x >= 0.05 ~ "Very high deficit",
    TRUE ~ "Extreme deficit"
  )
}


# PPETmin no longer uses a Sabah percentile reference.
# Returning low=0 and high=1 makes the existing generic
# decrease-direction scaler equivalent to clamp(1-P:PET, 0, 1).
if (
  exists(
    "restoration_fixed_reference",
    mode = "function"
  ) &&
  !exists(
    ".restoration_fixed_reference_before_ppet_physical_v16",
    inherits = FALSE
  )
) {

  .restoration_fixed_reference_before_ppet_physical_v16 <-
    restoration_fixed_reference


  restoration_fixed_reference <- function(
      raster_catalogue,
      variable_id,
      ...
  ) {

    if (identical(
      tolower(variable_id),
      "ppetmin"
    )) {
      return(
        c(
          low = 0,
          high = 1
        )
      )
    }

    .restoration_fixed_reference_before_ppet_physical_v16(
      raster_catalogue = raster_catalogue,
      variable_id = variable_id,
      ...
    )
  }
}


if (!exists(
  ".restoration_run_single_before_ppet_physical_v16",
  inherits = FALSE
)) {
  .restoration_run_single_before_ppet_physical_v16 <-
    restoration_run_single
}


restoration_run_single <- function(...) {

  result <-
    .restoration_run_single_before_ppet_physical_v16(...)

  if (
    is.null(result$water_indicator_table) ||
    !is.data.frame(result$water_indicator_table)
  ) {
    stop(
      paste(
        "Restoration PPETmin scaling could not be applied because",
        "water_indicator_table was not returned."
      ),
      call. = FALSE
    )
  }

  water <- result$water_indicator_table

  required_columns <- c(
    "Variable_ID",
    "Baseline_value",
    "Future_value",
    "Baseline_stress_score",
    "Future_stress_score",
    "Stress_change"
  )

  missing_columns <- setdiff(
    required_columns,
    names(water)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Restoration water table is missing required column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  ppet_row <- which(
    tolower(water$Variable_ID) ==
      "ppetmin"
  )

  if (length(ppet_row) != 1) {
    stop(
      "Expected exactly one PPETmin row in the Restoration water table; found ",
      length(ppet_row),
      ".",
      call. = FALSE
    )
  }

  baseline_ppet <-
    as.numeric(
      water$Baseline_value[
        ppet_row
      ]
    )

  future_ppet <-
    as.numeric(
      water$Future_value[
        ppet_row
      ]
    )

  baseline_score <-
    restoration_ppetmin_stress_score(
      baseline_ppet
    )

  future_score <-
    restoration_ppetmin_stress_score(
      future_ppet
    )

  water$Baseline_stress_score[
    ppet_row
  ] <- baseline_score

  water$Future_stress_score[
    ppet_row
  ] <- future_score

  water$Stress_change[
    ppet_row
  ] <- future_score - baseline_score


  if ("Reference_low" %in% names(water)) {
    water$Reference_low[
      ppet_row
    ] <- NA_real_
  }

  if ("Reference_high" %in% names(water)) {
    water$Reference_high[
      ppet_row
    ] <- NA_real_
  }


  if (!"Scaling_method" %in% names(water)) {
    water$Scaling_method <- NA_character_
  }

  water$Scaling_method[
    ppet_row
  ] <- paste(
    "Physical P:PET balance scale:",
    "P:PET >= 1 = 0 stress;",
    "below 1, stress = 100 * (1 - P:PET),",
    "bounded 0-100"
  )


  if (!"Baseline_stress_class" %in% names(water)) {
    water$Baseline_stress_class <- NA_character_
  }

  if (!"Future_stress_class" %in% names(water)) {
    water$Future_stress_class <- NA_character_
  }

  water$Baseline_stress_class[
    ppet_row
  ] <- restoration_ppetmin_deficit_class(
    baseline_ppet
  )

  water$Future_stress_class[
    ppet_row
  ] <- restoration_ppetmin_deficit_class(
    future_ppet
  )


  result$water_indicator_table <- water


  # Recalculate the combined water dimension with the CURRENT
  # unweighted mean. No weighting change is introduced here.
  if (
    !is.null(result$core_table) &&
    is.data.frame(result$core_table) &&
    all(
      c(
        "Dimension_ID",
        "Baseline_score",
        "Future_score",
        "Stress_change"
      ) %in% names(result$core_table)
    )
  ) {

    water_dimension <- which(
      result$core_table$Dimension_ID ==
        "establishment_water"
    )

    if (length(water_dimension) == 1) {

      baseline_water_score <- mean(
        water$Baseline_stress_score,
        na.rm = TRUE
      )

      future_water_score <- mean(
        water$Future_stress_score,
        na.rm = TRUE
      )

      result$core_table$Baseline_score[
        water_dimension
      ] <- baseline_water_score

      result$core_table$Future_score[
        water_dimension
      ] <- future_water_score

      result$core_table$Stress_change[
        water_dimension
      ] <- (
        future_water_score -
          baseline_water_score
      )
    }
  }


  result$ppetmin_scaling_method <- paste(
    "Physical P:PET balance scale:",
    "P:PET >= 1 = 0 stress;",
    "below 1, stress = 100 * (1 - P:PET)"
  )

  result$ppetmin_zero_stress_threshold <- 1

  result
}


if (
  exists(
    "restoration_run_comparison",
    mode = "function"
  ) &&
  !exists(
    ".restoration_run_comparison_before_ppet_physical_v16",
    inherits = FALSE
  )
) {

  .restoration_run_comparison_before_ppet_physical_v16 <-
    restoration_run_comparison


  restoration_run_comparison <- function(...) {

    result <-
      .restoration_run_comparison_before_ppet_physical_v16(...)

    if (
      !is.null(result$comparison_table) &&
      is.data.frame(result$comparison_table)
    ) {

      result$comparison_table$PPETmin_scaling_method <-
        paste(
          "Physical P:PET balance:",
          ">=1 = 0 stress;",
          "<1 = 100*(1-P:PET)"
        )

      result$comparison_table$PPETmin_zero_stress_threshold <-
        1

      if (
        "PPETmin_reference_P05" %in%
          names(result$comparison_table)
      ) {
        result$comparison_table$PPETmin_reference_P05 <-
          NA_real_
      }

      if (
        "PPETmin_reference_P95" %in%
          names(result$comparison_table)
      ) {
        result$comparison_table$PPETmin_reference_P95 <-
          NA_real_
      }
    }

    result
  }
}

# <<< PPETMIN_PHYSICAL_SCALING_V16_RESTORATION <<<

