# ============================================================
# OIL PALM CORE / OPTIONAL DIMENSION HELPERS
# Sabah Climate Risk Explorer
# ============================================================
#
# Design rule:
#   - Crop water stress remains the CORE Oil Palm score.
#   - Optional dimensions are reported separately.
#   - Optional dimensions NEVER alter the core crop-water score.
#   - No cross-dimension composite is produced here.
#
# Current catalogue fallback:
#   worker_heat -> WBGT (provisional until workability layers are registered)
#
# IMPORTANT OIL-PALM RULE:
#   Fire is NOT a general Oil Palm dimension. Established plantations on
#   mineral soils usually have limited continuous fuel and substantial
#   access/drainage breaks. Peat-related fire/hydrology should be treated
#   separately as a conditional site-specific module, not as a default
#   plantation climate-stress dimension.
#
# ============================================================

OIL_PALM_DIMENSION_APPLICATION_ID <- "oil_palm"

OIL_PALM_EXCLUDED_GENERAL_DIMENSIONS <- c("fire_drought")

OIL_PALM_DIMENSION_CANDIDATES <- list(
  oil_palm_water = c(
    "CWD_DEFICIT",
    "PPETmin",
    "CONDRYMONTH100",
    "PPETConDryMth",
    "CDD_PR1MM",
    "CDD",
    "VPD_MEAN"
  ),
  worker_heat = c(
    "WORKABILITY_LOSS_HEAVY",
    "WORKABILITY_LT75_DAYS",
    "WBGT30_DAYS",
    "WBGT_SHADE_28_DAYS",
    "HI41_DAYS",
    "WBGT"
  ),
  excess_rain_operations = c(
    "R20MM",
    "R50MM",
    "CWD_PR1MM"
  ),
  flood_waterlogging = c(
    "FLOOD_SUSC"
  )
)

OIL_PALM_DIMENSION_PRIMARY_PREFERENCE <- list(
  worker_heat = c(
    "WORKABILITY_LOSS_HEAVY",
    "WORKABILITY_LT75_DAYS",
    "WBGT30_DAYS",
    "WBGT_SHADE_28_DAYS",
    "WBGT"
  ),
  excess_rain_operations = c(
    "R20MM",
    "R50MM",
    "CWD_PR1MM"
  ),
  flood_waterlogging = c(
    "FLOOD_SUSC"
  )
)

OIL_PALM_DIMENSION_LABELS <- c(
  oil_palm_water = "Crop water stress",
  worker_heat = "Outdoor worker heat / workability",
  excess_rain_operations = "Excess rainfall / field access",
  flood_waterlogging = "Flood / waterlogging exposure"
)

OIL_PALM_VARIABLE_LABELS <- c(
  WORKABILITY_LOSS_HEAVY = "Potential heavy-work capacity loss",
  WORKABILITY_LT75_DAYS = "Days with heavy-work capacity below 75%",
  WBGT30_DAYS = "Days with WBGT at or above 30°C",
  WBGT_SHADE_28_DAYS = "Days with shade WBGT at or above 28°C",
  HI41_DAYS = "Days with Heat Index at or above 41°C",
  WBGT = "Maximum WBGT",
  R20MM = "Days with rainfall at or above 20 mm",
  R50MM = "Days with rainfall at or above 50 mm",
  CWD_PR1MM = "Longest wet spell",
  FLOOD_SUSC = "Flood susceptibility"
)

OIL_PALM_VARIABLE_DIRECTION <- c(
  WORKABILITY_LOSS_HEAVY = "increase",
  WORKABILITY_LT75_DAYS = "increase",
  WBGT30_DAYS = "increase",
  WBGT_SHADE_28_DAYS = "increase",
  HI41_DAYS = "increase",
  WBGT = "increase",
  R20MM = "increase",
  R50MM = "increase",
  CWD_PR1MM = "increase",
  FLOOD_SUSC = "increase"
)

# ------------------------------------------------------------
# CONFIG LOADERS
# ------------------------------------------------------------

oil_palm_load_dimension_config <- function(config_dir = "config") {
  app_dim_path <- file.path(config_dir, "application_dimensions.csv")
  indicator_path <- file.path(config_dir, "dimension_indicators.csv")

  if (!file.exists(app_dim_path)) {
    stop("Missing config/application_dimensions.csv")
  }

  if (!file.exists(indicator_path)) {
    stop("Missing config/dimension_indicators.csv")
  }

  list(
    application_dimensions = readr::read_csv(
      app_dim_path,
      show_col_types = FALSE,
      progress = FALSE
    ),
    dimension_indicators = readr::read_csv(
      indicator_path,
      show_col_types = FALSE,
      progress = FALSE
    )
  )
}

# ------------------------------------------------------------
# CATALOGUE HELPERS
# ------------------------------------------------------------

oil_palm_dimension_catalogue_variables <- function(raster_catalogue) {
  raster_catalogue |>
    dplyr::filter(.data$enabled) |>
    dplyr::distinct(.data$variable_id) |>
    dplyr::pull(.data$variable_id)
}

oil_palm_variable_has_baseline_and_future <- function(
    raster_catalogue,
    variable_id
) {
  rows <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      .data$variable_id == !!variable_id
    )

  if (nrow(rows) == 0) return(FALSE)

  has_baseline <- any(
    rows$scenario == OIL_PALM_BASELINE_SCENARIO &
      rows$period == OIL_PALM_BASELINE_PERIOD
  )

  has_future <- any(
    rows$scenario != OIL_PALM_BASELINE_SCENARIO
  )

  isTRUE(has_baseline && has_future)
}

oil_palm_choose_dimension_variable <- function(
    raster_catalogue,
    dimension_id
) {
  preferences <- OIL_PALM_DIMENSION_PRIMARY_PREFERENCE[[dimension_id]]

  if (is.null(preferences)) return(NA_character_)

  usable <- preferences[
    vapply(
      preferences,
      function(variable_id) {
        oil_palm_variable_has_baseline_and_future(
          raster_catalogue = raster_catalogue,
          variable_id = variable_id
        )
      },
      logical(1)
    )
  ]

  if (length(usable) == 0) return(NA_character_)
  usable[[1]]
}

# ------------------------------------------------------------
# DIMENSION AVAILABILITY AUDIT
# ------------------------------------------------------------

oil_palm_dimension_availability <- function(
    raster_catalogue,
    config_dir = "config"
) {
  config <- oil_palm_load_dimension_config(config_dir)

  app_dimensions <- config$application_dimensions |>
    dplyr::filter(
      .data$application_id == OIL_PALM_DIMENSION_APPLICATION_ID,
      !.data$dimension_id %in% OIL_PALM_EXCLUDED_GENERAL_DIMENSIONS
    ) |>
    dplyr::arrange(.data$display_order)

  if (nrow(app_dimensions) == 0) {
    stop("No oil_palm rows found in config/application_dimensions.csv")
  }

  catalogue_variables <- oil_palm_dimension_catalogue_variables(
    raster_catalogue
  )

  result <- lapply(
    seq_len(nrow(app_dimensions)),
    function(i) {
      row <- app_dimensions[i, ]
      dimension_id <- row$dimension_id[[1]]

      requested <- unique(c(
        config$dimension_indicators |>
          dplyr::filter(.data$dimension_id == !!dimension_id) |>
          dplyr::pull(.data$layer_id),
        OIL_PALM_DIMENSION_CANDIDATES[[dimension_id]]
      ))

      requested <- requested[!is.na(requested) & nzchar(requested)]
      present <- intersect(requested, catalogue_variables)
      missing <- setdiff(requested, catalogue_variables)

      if (identical(dimension_id, "oil_palm_water")) {
        core_ready <- all(
          OIL_PALM_CORE_VARIABLES %in% catalogue_variables
        ) && nrow(oil_palm_available_combinations(raster_catalogue)) > 0

        selected_variable <- NA_character_
        status <- if (core_ready) "ready" else "missing"
        status_note <- if (core_ready) {
          "Core score uses PPETmin, PPETConDryMth and CDD."
        } else {
          "One or more required core crop-water layers are missing."
        }
      } else {
        selected_variable <- oil_palm_choose_dimension_variable(
          raster_catalogue = raster_catalogue,
          dimension_id = dimension_id
        )

        status <- if (is.na(selected_variable)) {
          "missing"
        } else if (
          (dimension_id == "worker_heat" && selected_variable == "WBGT")
        ) {
          "provisional"
        } else {
          "ready"
        }

        status_note <- dplyr::case_when(
          dimension_id == "worker_heat" && selected_variable == "WBGT" ~
            paste(
              "Available now using Maximum WBGT as a provisional indicator.",
              "A workability-loss layer is preferred when registered."
            ),
          status == "ready" ~ "Required raster indicator is available.",
          dimension_id == "excess_rain_operations" ~
            "Requires R20MM, R50MM and/or CWD_PR1MM to be registered.",
          dimension_id == "flood_waterlogging" ~
            "Requires a flood-susceptibility layer (FLOOD_SUSC).",
          TRUE ~ "Required raster indicator is not yet registered."
        )
      }

      tibble::tibble(
        dimension_id = dimension_id,
        dimension_name = row$dimension_name[[1]],
        default_on = isTRUE(row$default_on[[1]]),
        user_toggle = isTRUE(row$user_toggle[[1]]),
        contributes_to_core_score = isTRUE(
          row$contributes_to_core_score[[1]]
        ),
        display_order = row$display_order[[1]],
        status = status,
        selected_variable = selected_variable,
        present_layers = paste(present, collapse = "; "),
        missing_layers = paste(missing, collapse = "; "),
        status_note = status_note
      )
    }
  )

  dplyr::bind_rows(result) |>
    dplyr::arrange(.data$display_order)
}

# ------------------------------------------------------------
# OPTIONAL DIMENSION AOI EXTRACTION
# ------------------------------------------------------------

oil_palm_optional_dimension_one <- function(
    raster_catalogue,
    aoi,
    dimension_id,
    future_scenario,
    future_period,
    availability = NULL
) {
  if (is.null(availability)) {
    availability <- oil_palm_dimension_availability(raster_catalogue)
  }

  dim_row <- availability |>
    dplyr::filter(.data$dimension_id == !!dimension_id)

  if (nrow(dim_row) == 0) {
    stop("Unknown Oil Palm dimension: ", dimension_id)
  }

  variable_id <- dim_row$selected_variable[[1]]

  if (is.na(variable_id) || !nzchar(variable_id)) {
    return(
      tibble::tibble(
        Dimension_ID = dimension_id,
        Dimension = dim_row$dimension_name[[1]],
        Status = "Layer not available",
        Indicator = NA_character_,
        Variable_ID = NA_character_,
        Scenario_ID = future_scenario,
        Period_ID = future_period,
        Baseline = NA_real_,
        Future = NA_real_,
        Change = NA_real_,
        Worsens = NA,
        Units = NA_character_,
        Note = dim_row$status_note[[1]]
      )
    )
  }

  baseline_match <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      .data$variable_id == !!variable_id,
      .data$scenario == OIL_PALM_BASELINE_SCENARIO,
      .data$period == OIL_PALM_BASELINE_PERIOD
    )

  future_match <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      .data$variable_id == !!variable_id,
      .data$scenario == !!future_scenario,
      .data$period == !!future_period
    )

  if (nrow(baseline_match) == 0 || nrow(future_match) == 0) {
    return(
      tibble::tibble(
        Dimension_ID = dimension_id,
        Dimension = dim_row$dimension_name[[1]],
        Status = "Not available for selected scenario / period",
        Indicator = OIL_PALM_VARIABLE_LABELS[[variable_id]],
        Variable_ID = variable_id,
        Scenario_ID = future_scenario,
        Period_ID = future_period,
        Baseline = NA_real_,
        Future = NA_real_,
        Change = NA_real_,
        Worsens = NA,
        Units = {
          future_units <- if (nrow(future_match) > 0) future_match$units[[1]] else NA_character_
          baseline_units <- if (nrow(baseline_match) > 0) baseline_match$units[[1]] else NA_character_
          dplyr::coalesce(future_units, baseline_units, NA_character_)
        },
        Note = dim_row$status_note[[1]]
      )
    )
  }

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

  future_full <- oil_palm_align_raster(
    future_full,
    baseline_full,
    method = "bilinear"
  )

  baseline_stats <- oil_palm_raster_stats(
    oil_palm_crop_to_aoi(baseline_full, aoi)
  )

  future_stats <- oil_palm_raster_stats(
    oil_palm_crop_to_aoi(future_full, aoi)
  )

  baseline_value <- baseline_stats$mean
  future_value <- future_stats$mean
  change_value <- future_value - baseline_value

  direction <- OIL_PALM_VARIABLE_DIRECTION[[variable_id]]
  worsens <- if (is.null(direction) || !is.finite(change_value)) {
    NA
  } else if (identical(direction, "increase")) {
    future_value > baseline_value
  } else {
    future_value < baseline_value
  }

  indicator_label <- OIL_PALM_VARIABLE_LABELS[[variable_id]]
  if (is.null(indicator_label)) indicator_label <- variable_id

  units_value <- future_match$units[[1]]
  if (is.null(units_value) || is.na(units_value) || !nzchar(units_value)) {
    units_value <- baseline_match$units[[1]]
  }

  tibble::tibble(
    Dimension_ID = dimension_id,
    Dimension = dim_row$dimension_name[[1]],
    Status = ifelse(
      dim_row$status[[1]] == "provisional",
      "Provisional",
      "Available"
    ),
    Indicator = indicator_label,
    Variable_ID = variable_id,
    Scenario_ID = future_scenario,
    Period_ID = future_period,
    Baseline = baseline_value,
    Future = future_value,
    Change = change_value,
    Worsens = worsens,
    Units = units_value,
    Note = dim_row$status_note[[1]]
  )
}

# ------------------------------------------------------------
# RUN SELECTED OPTIONAL DIMENSIONS - SINGLE
# ------------------------------------------------------------

oil_palm_run_optional_dimensions <- function(
    raster_catalogue,
    aoi,
    selected_dimensions,
    future_scenario,
    future_period,
    availability = NULL
) {
  selected_dimensions <- unique(selected_dimensions)
  selected_dimensions <- setdiff(selected_dimensions, "oil_palm_water")

  if (length(selected_dimensions) == 0) {
    return(tibble::tibble())
  }

  if (is.null(availability)) {
    availability <- oil_palm_dimension_availability(raster_catalogue)
  }

  purrr::map_dfr(
    selected_dimensions,
    function(dimension_id) {
      oil_palm_optional_dimension_one(
        raster_catalogue = raster_catalogue,
        aoi = aoi,
        dimension_id = dimension_id,
        future_scenario = future_scenario,
        future_period = future_period,
        availability = availability
      )
    }
  )
}

# ------------------------------------------------------------
# RUN SELECTED OPTIONAL DIMENSIONS - COMPARISON
# ------------------------------------------------------------

oil_palm_compare_optional_dimensions <- function(
    raster_catalogue,
    aoi,
    selected_dimensions,
    future_scenarios,
    future_periods,
    availability = NULL
) {
  selected_dimensions <- unique(selected_dimensions)
  selected_dimensions <- setdiff(selected_dimensions, "oil_palm_water")

  if (length(selected_dimensions) == 0) {
    return(tibble::tibble())
  }

  if (is.null(availability)) {
    availability <- oil_palm_dimension_availability(raster_catalogue)
  }

  combinations <- tidyr::crossing(
    Scenario_ID = future_scenarios,
    Period_ID = future_periods
  )

  purrr::pmap_dfr(
    combinations,
    function(Scenario_ID, Period_ID) {
      rows <- oil_palm_run_optional_dimensions(
        raster_catalogue = raster_catalogue,
        aoi = aoi,
        selected_dimensions = selected_dimensions,
        future_scenario = Scenario_ID,
        future_period = Period_ID,
        availability = availability
      )

      rows |>
        dplyr::mutate(
          Scenario = if (exists("get_scenario_label")) {
            get_scenario_label(Scenario_ID)
          } else {
            Scenario_ID
          },
          Period = if (exists("get_period_label")) {
            get_period_label(Period_ID)
          } else {
            Period_ID
          },
          .before = .data$Scenario_ID
        )
    }
  )
}

# ------------------------------------------------------------
# AUDIT TABLE FOR DEVELOPMENT
# ------------------------------------------------------------

oil_palm_dimension_layer_audit <- function(
    raster_catalogue,
    config_dir = "config"
) {
  availability <- oil_palm_dimension_availability(
    raster_catalogue = raster_catalogue,
    config_dir = config_dir
  )

  availability |>
    dplyr::mutate(
      core_or_optional = ifelse(
        .data$contributes_to_core_score,
        "Core",
        "Optional"
      ),
      ui_status = dplyr::case_when(
        .data$status == "ready" ~ "Enable",
        .data$status == "provisional" ~ "Enable - provisional indicator",
        TRUE ~ "Show as unavailable"
      )
    ) |>
    dplyr::select(
      .data$display_order,
      .data$dimension_id,
      .data$dimension_name,
      .data$core_or_optional,
      .data$status,
      .data$ui_status,
      .data$selected_variable,
      .data$present_layers,
      .data$missing_layers,
      .data$status_note
    )
}
