# ============================================================
# OIL PALM - DOA PEAT / FIRE SITE-SPECIFIC LINKED SCREENING
# Version 12.1
# Sabah Climate Risk Explorer
#
# Direct peat source:
#   data/boundaries/DoA_peat_extent.gpkg
# Layer:
#   pealandsabah
#
# Fire is NOT a general Oil Palm climate-risk dimension.
# This linked screening appears only where the active AOI
# intersects mapped DoA peat.
#
# The module reports:
#   - mapped DoA peat area within the AOI;
#   - percent of the AOI on mapped peat;
#   - baseline Fire probability over mapped peat only;
#   - future Fire probability over mapped peat only;
#   - change from baseline;
#   - p90 Fire probability as supporting context.
#
# No combined peat/fire 0-100 score is calculated.
# Drainage, water table, peat condition, ignition and plantation
# management are not represented by the current climate rasters.
# ============================================================


OIL_PALM_DOA_PEAT_PATH <- file.path(
  "data",
  "boundaries",
  "DoA_peat_extent.gpkg"
)

OIL_PALM_DOA_PEAT_LAYER <- "pealandsabah"


oil_palm_load_doa_peat <- function(
    path = OIL_PALM_DOA_PEAT_PATH,
    layer = OIL_PALM_DOA_PEAT_LAYER
) {
  if (!file.exists(path)) {
    stop(
      "DoA peat extent was not found: ",
      path,
      call. = FALSE
    )
  }

  x <- sf::st_read(
    path,
    layer = layer,
    quiet = TRUE
  )

  if (nrow(x) == 0) {
    stop(
      "The DoA peat extent contains no features.",
      call. = FALSE
    )
  }

  x <- sf::st_make_valid(x)

  # Use sf::st_geometry(x) rather than assuming the active
  # geometry column is literally named "geometry". GeoPackages
  # commonly use names such as geom.
  union_geom <- sf::st_union(
    sf::st_geometry(x)
  )

  out <- sf::st_sf(
    geometry = union_geom,
    crs = sf::st_crs(x)
  )

  sf::st_make_valid(out)
}


oil_palm_area_ha <- function(x) {
  if (
    is.null(x) ||
    nrow(x) == 0
  ) {
    return(0)
  }

  x_area <- if (
    !is.na(sf::st_crs(x)) &&
    sf::st_crs(x)$epsg == 32650
  ) {
    x
  } else {
    sf::st_transform(
      x,
      32650
    )
  }

  sum(
    as.numeric(
      sf::st_area(x_area)
    ),
    na.rm = TRUE
  ) / 10000
}


oil_palm_doa_peat_context <- function(
    aoi,
    peat_path = OIL_PALM_DOA_PEAT_PATH,
    peat_layer = OIL_PALM_DOA_PEAT_LAYER
) {
  if (
    is.null(aoi) ||
    nrow(aoi) == 0
  ) {
    return(
      list(
        summary = tibble::tibble(),
        intersection = NULL
      )
    )
  }

  peat <- oil_palm_load_doa_peat(
    path = peat_path,
    layer = peat_layer
  )

  aoi_valid <- sf::st_make_valid(
    aoi
  )

  aoi_area_ha <- oil_palm_area_ha(
    aoi_valid
  )

  peat <- sf::st_transform(
    peat,
    sf::st_crs(aoi_valid)
  )

  overlap <- suppressWarnings(
    sf::st_intersection(
      aoi_valid,
      peat
    )
  )

  peat_area_ha <- oil_palm_area_ha(
    overlap
  )

  pct_aoi_peat <- if (
    is.finite(aoi_area_ha) &&
    aoi_area_ha > 0
  ) {
    100 * peat_area_ha / aoi_area_ha
  } else {
    NA_real_
  }

  list(
    summary = tibble::tibble(
      peat_definition = "DoA-defined mapped peat",
      AOI_area_ha = aoi_area_ha,
      peat_area_ha = peat_area_ha,
      pct_AOI_peat = pct_aoi_peat
    ),
    intersection = overlap
  )
}


oil_palm_get_fire_record <- function(
    raster_catalogue,
    scenario,
    period
) {
  x <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      .data$scenario == !!scenario,
      .data$period == !!period,
      .data$variable_id %in% c(
        "Fire",
        "FIRE_PROB"
      )
    )

  if (nrow(x) == 0) {
    return(NULL)
  }

  x[1, , drop = FALSE]
}


oil_palm_fire_baseline_period <- function(
    raster_catalogue
) {
  x <- raster_catalogue |>
    dplyr::filter(
      .data$enabled,
      .data$scenario == "baseline",
      .data$variable_id %in% c(
        "Fire",
        "FIRE_PROB"
      )
    ) |>
    dplyr::distinct(
      .data$period
    ) |>
    dplyr::pull(
      .data$period
    )

  if (length(x) == 0) {
    return(NA_character_)
  }

  x[1]
}


oil_palm_clean_fire_raster <- function(
    raster_record
) {
  if (
    is.null(raster_record) ||
    nrow(raster_record) != 1
  ) {
    stop(
      "Exactly one Fire raster record is required.",
      call. = FALSE
    )
  }

  path <- raster_record$file_path[[1]]

  if (
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    stop(
      "Fire raster was not found: ",
      path,
      call. = FALSE
    )
  }

  r <- terra::rast(path)

  if (terra::nlyr(r) > 1) {
    r <- r[[1]]
  }

  nodata <- suppressWarnings(
    as.numeric(
      raster_record$nodata_value[[1]]
    )
  )

  if (is.finite(nodata)) {
    r <- terra::ifel(
      r == nodata,
      NA,
      r
    )
  }

  # Safety fallback for undeclared extreme negative NoData.
  r <- terra::ifel(
    r <= -1e30,
    NA,
    r
  )

  r
}


oil_palm_fire_stats_on_peat <- function(
    raster_record,
    peat_intersection
) {
  if (
    is.null(peat_intersection) ||
    nrow(peat_intersection) == 0
  ) {
    return(
      list(
        mean = NA_real_,
        minimum = NA_real_,
        maximum = NA_real_,
        p90 = NA_real_,
        valid_cells = 0L
      )
    )
  }

  r <- oil_palm_clean_fire_raster(
    raster_record
  )

  raster_crs <- terra::crs(r)

  peat_raster_crs <- sf::st_transform(
    sf::st_make_valid(
      peat_intersection
    ),
    raster_crs
  )

  peat_vect <- terra::vect(
    peat_raster_crs
  )

  clipped <- terra::crop(
    r,
    peat_vect
  )

  clipped <- terra::mask(
    clipped,
    peat_vect
  )

  values <- terra::values(
    clipped,
    mat = FALSE
  )

  values <- values[
    is.finite(values)
  ]

  if (length(values) == 0) {
    return(
      list(
        mean = NA_real_,
        minimum = NA_real_,
        maximum = NA_real_,
        p90 = NA_real_,
        valid_cells = 0L
      )
    )
  }

  list(
    mean = mean(values),
    minimum = min(values),
    maximum = max(values),
    p90 = as.numeric(
      stats::quantile(
        values,
        probs = 0.90,
        names = FALSE
      )
    ),
    valid_cells = length(values)
  )
}


oil_palm_run_peat_fire_single <- function(
    raster_catalogue,
    aoi,
    future_scenario,
    future_period
) {
  peat_context <- oil_palm_doa_peat_context(
    aoi = aoi
  )

  peat_summary <- peat_context$summary
  peat_intersection <- peat_context$intersection

  if (
    nrow(peat_summary) == 0 ||
    peat_summary$peat_area_ha[[1]] <= 0
  ) {
    return(
      list(
        mode = "single",
        peat_summary = peat_summary,
        fire_table = tibble::tibble(),
        intersection = peat_intersection
      )
    )
  }

  baseline_period <- oil_palm_fire_baseline_period(
    raster_catalogue
  )

  baseline_record <- oil_palm_get_fire_record(
    raster_catalogue = raster_catalogue,
    scenario = "baseline",
    period = baseline_period
  )

  future_record <- oil_palm_get_fire_record(
    raster_catalogue = raster_catalogue,
    scenario = future_scenario,
    period = future_period
  )

  if (is.null(baseline_record)) {
    stop(
      "No baseline Fire raster is registered.",
      call. = FALSE
    )
  }

  if (is.null(future_record)) {
    stop(
      "No Fire raster is registered for ",
      future_scenario,
      " / ",
      future_period,
      ".",
      call. = FALSE
    )
  }

  baseline_stats <- oil_palm_fire_stats_on_peat(
    raster_record = baseline_record,
    peat_intersection = peat_intersection
  )

  future_stats <- oil_palm_fire_stats_on_peat(
    raster_record = future_record,
    peat_intersection = peat_intersection
  )

  fire_table <- tibble::tibble(
    scenario = future_scenario,
    period = future_period,
    baseline_period = baseline_period,
    baseline_fire_mean = baseline_stats$mean,
    future_fire_mean = future_stats$mean,
    change_fire_mean =
      future_stats$mean -
      baseline_stats$mean,
    baseline_fire_p90 = baseline_stats$p90,
    future_fire_p90 = future_stats$p90,
    change_fire_p90 =
      future_stats$p90 -
      baseline_stats$p90,
    valid_future_cells =
      future_stats$valid_cells
  )

  list(
    mode = "single",
    peat_summary = peat_summary,
    fire_table = fire_table,
    intersection = peat_intersection
  )
}


oil_palm_run_peat_fire_comparison <- function(
    raster_catalogue,
    aoi,
    scenarios,
    periods
) {
  peat_context <- oil_palm_doa_peat_context(
    aoi = aoi
  )

  peat_summary <- peat_context$summary
  peat_intersection <- peat_context$intersection

  if (
    nrow(peat_summary) == 0 ||
    peat_summary$peat_area_ha[[1]] <= 0
  ) {
    return(
      list(
        mode = "compare",
        peat_summary = peat_summary,
        fire_table = tibble::tibble(),
        intersection = peat_intersection
      )
    )
  }

  baseline_period <- oil_palm_fire_baseline_period(
    raster_catalogue
  )

  baseline_record <- oil_palm_get_fire_record(
    raster_catalogue = raster_catalogue,
    scenario = "baseline",
    period = baseline_period
  )

  if (is.null(baseline_record)) {
    stop(
      "No baseline Fire raster is registered.",
      call. = FALSE
    )
  }

  baseline_stats <- oil_palm_fire_stats_on_peat(
    raster_record = baseline_record,
    peat_intersection = peat_intersection
  )

  comparison_grid <- tidyr::crossing(
    scenario = scenarios,
    period = periods
  )

  rows <- list()
  k <- 1L

  for (i in seq_len(nrow(comparison_grid))) {
    scenario_i <- comparison_grid$scenario[[i]]
    period_i <- comparison_grid$period[[i]]

    future_record <- oil_palm_get_fire_record(
      raster_catalogue = raster_catalogue,
      scenario = scenario_i,
      period = period_i
    )

    if (is.null(future_record)) {
      next
    }

    future_stats <- oil_palm_fire_stats_on_peat(
      raster_record = future_record,
      peat_intersection = peat_intersection
    )

    rows[[k]] <- tibble::tibble(
      scenario = scenario_i,
      period = period_i,
      baseline_period = baseline_period,
      baseline_fire_mean = baseline_stats$mean,
      future_fire_mean = future_stats$mean,
      change_fire_mean =
        future_stats$mean -
        baseline_stats$mean,
      baseline_fire_p90 = baseline_stats$p90,
      future_fire_p90 = future_stats$p90,
      change_fire_p90 =
        future_stats$p90 -
        baseline_stats$p90,
      valid_future_cells =
        future_stats$valid_cells
    )

    k <- k + 1L
  }

  list(
    mode = "compare",
    peat_summary = peat_summary,
    fire_table = dplyr::bind_rows(rows),
    intersection = peat_intersection
  )
}


oil_palm_peat_fire_server <- function(
    input,
    output,
    session,
    rv,
    raster_catalogue,
    get_scenario_label,
    get_period_label,
    safe_filename
) {
  result_val <- shiny::reactiveVal(
    NULL
  )

  peat_context_reactive <- shiny::reactive({
    if (is.null(rv$aoi)) {
      return(
        list(
          summary = tibble::tibble(),
          intersection = NULL
        )
      )
    }

    if (!file.exists(OIL_PALM_DOA_PEAT_PATH)) {
      return(
        list(
          summary = tibble::tibble(),
          intersection = NULL
        )
      )
    }

    oil_palm_doa_peat_context(
      aoi = rv$aoi
    )
  })

  output$oil_palm_peat_fire_linked_ui <- shiny::renderUI({
    if (!file.exists(OIL_PALM_DOA_PEAT_PATH)) {
      return(
        shiny::tagList(
          shiny::hr(),
          shiny::h5(
            "Site-specific linked screening"
          ),
          shiny::div(
            class = paste(
              "selection-status-box",
              "selection-status-incomplete"
            ),
            shiny::strong(
              "Peatland / fire considerations"
            ),
            shiny::div(
              class = "selection-status-detail",
              paste(
                "DoA peat extent not found at",
                OIL_PALM_DOA_PEAT_PATH
              )
            )
          )
        )
      )
    }

    if (is.null(rv$aoi)) {
      return(
        shiny::tagList(
          shiny::hr(),
          shiny::h5(
            "Site-specific linked screening"
          ),
          shiny::helpText(
            paste(
              "Peatland / fire considerations are checked",
              "after an AOI is loaded."
            )
          )
        )
      )
    }

    peat_context <- peat_context_reactive()
    peat_summary <- peat_context$summary

    if (
      nrow(peat_summary) == 0 ||
      peat_summary$peat_area_ha[[1]] <= 0
    ) {
      return(
        shiny::tagList(
          shiny::hr(),
          shiny::h5(
            "Site-specific linked screening"
          ),
          shiny::div(
            class = paste(
              "selection-status-box",
              "selection-status-ready"
            ),
            shiny::strong(
              "No mapped DoA peat intersects this AOI"
            ),
            shiny::div(
              class = "selection-status-detail",
              paste(
                "The peat/fire option is therefore not shown.",
                "This is based on data/boundaries/DoA_peat_extent.gpkg."
              )
            )
          )
        )
      )
    }

    include_peat <- isTRUE(
      input$oil_palm_include_peat_fire
    )

    shiny::tagList(
      shiny::hr(),
      shiny::h5(
        "Site-specific linked screening"
      ),
      shiny::checkboxInput(
        inputId =
          "oil_palm_include_peat_fire",
        label =
          "Peatland / fire considerations",
        value = include_peat
      ),
      shiny::div(
        class = paste(
          "selection-status-box",
          "selection-status-warning"
        ),
        shiny::strong(
          "Mapped DoA peat intersects this AOI"
        ),
        shiny::div(
          class = "selection-status-row",
          shiny::span(
            class =
              "selection-status-label",
            "Mapped peat"
          ),
          shiny::span(
            class =
              "selection-status-value",
            paste0(
              format(
                round(
                  peat_summary$peat_area_ha[[1]],
                  1
                ),
                big.mark = ","
              ),
              " ha (",
              round(
                peat_summary$pct_AOI_peat[[1]],
                1
              ),
              "% of AOI)"
            )
          )
        )
      ),
      shiny::helpText(
        paste(
          "Fire probability is assessed only over the mapped peat portion of the AOI.",
          "It is not a general plantation fire-risk score and does not alter",
          "the Oil Palm crop-water-stress score."
        )
      ),
      if (include_peat) {
        shiny::tagList(
          shiny::helpText(
            if (
              identical(
                input$oil_palm_analysis_mode,
                "compare"
              )
            ) {
              paste(
                "The peat/fire screen will use the SSPs and climatologies",
                "selected in the Oil Palm comparison."
              )
            } else {
              paste(
                "The peat/fire screen will use the selected Oil Palm",
                "scenario and climatology."
              )
            }
          ),
          shiny::actionButton(
            inputId =
              "run_oil_palm_peat_fire",
            label =
              "Run peat / fire screening",
            class = "btn-warning",
            width = "100%"
          )
        )
      }
    )
  })

  shiny::observeEvent(
    input$oil_palm_include_peat_fire,
    {
      if (
        !isTRUE(
          input$oil_palm_include_peat_fire
        )
      ) {
        result_val(NULL)

        leaflet::leafletProxy("map") |>
          leaflet::clearGroup(
            "Oil palm peat context"
          )
      }
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$run_oil_palm_peat_fire,
    {
      shiny::req(
        rv$aoi,
        isTRUE(
          input$oil_palm_include_peat_fire
        )
      )

      result <- tryCatch(
        {
          shiny::withProgress(
            message =
              "Running peat / fire screening",
            value = 0,
            {
              shiny::incProgress(
                0.2,
                detail =
                  "Intersecting AOI with mapped DoA peat"
              )

              if (
                identical(
                  input$oil_palm_analysis_mode,
                  "compare"
                )
              ) {
                shiny::req(
                  input$oil_palm_comparison_scenarios,
                  input$oil_palm_comparison_periods
                )

                x <- oil_palm_run_peat_fire_comparison(
                  raster_catalogue =
                    raster_catalogue,
                  aoi = rv$aoi,
                  scenarios =
                    input$oil_palm_comparison_scenarios,
                  periods =
                    input$oil_palm_comparison_periods
                )
              } else {
                shiny::req(
                  input$oil_palm_scenario,
                  input$oil_palm_period
                )

                x <- oil_palm_run_peat_fire_single(
                  raster_catalogue =
                    raster_catalogue,
                  aoi = rv$aoi,
                  future_scenario =
                    input$oil_palm_scenario,
                  future_period =
                    input$oil_palm_period
                )
              }

              shiny::incProgress(
                1,
                detail = "Complete"
              )

              x
            }
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste(
              "Peat / fire screening failed:",
              e$message
            ),
            type = "error",
            duration = NULL
          )

          NULL
        }
      )

      if (is.null(result)) {
        return()
      }

      result_val(
        result
      )

      leaflet::leafletProxy("map") |>
        leaflet::clearGroup(
          "Oil palm peat context"
        )

      if (
        !is.null(result$intersection) &&
        nrow(result$intersection) > 0
      ) {
        leaflet::leafletProxy("map") |>
          leaflet::addPolygons(
            data = result$intersection,
            weight = 2,
            opacity = 0.9,
            fillOpacity = 0.20,
            group = "Oil palm peat context",
            label =
              "DoA-defined mapped peat within AOI"
          )
      }

      shiny::showNotification(
        "Peat / fire screening completed.",
        type = "message"
      )
    }
  )

  output$oil_palm_peat_fire_results_ui <- shiny::renderUI({
    result <- result_val()

    if (is.null(result)) {
      return(NULL)
    }

    peat_summary <- result$peat_summary
    fire_table <- result$fire_table

    if (
      nrow(peat_summary) == 0 ||
      peat_summary$peat_area_ha[[1]] <= 0
    ) {
      return(NULL)
    }

    fire_ui <- if (
      nrow(fire_table) == 0
    ) {
      shiny::div(
        class = "results-note",
        paste(
          "No Fire scenario-period combinations were available",
          "for the selected Oil Palm analysis."
        )
      )
    } else {
      table_rows <- lapply(
        seq_len(nrow(fire_table)),
        function(i) {
          shiny::tags$tr(
            shiny::tags$td(
              get_scenario_label(
                fire_table$scenario[[i]]
              )
            ),
            shiny::tags$td(
              get_period_label(
                fire_table$period[[i]]
              )
            ),
            shiny::tags$td(
              ifelse(
                is.finite(
                  fire_table$baseline_fire_mean[[i]]
                ),
                sprintf(
                  "%.3f",
                  fire_table$baseline_fire_mean[[i]]
                ),
                "NA"
              )
            ),
            shiny::tags$td(
              ifelse(
                is.finite(
                  fire_table$future_fire_mean[[i]]
                ),
                sprintf(
                  "%.3f",
                  fire_table$future_fire_mean[[i]]
                ),
                "NA"
              )
            ),
            shiny::tags$td(
              ifelse(
                is.finite(
                  fire_table$change_fire_mean[[i]]
                ),
                sprintf(
                  "%+.3f",
                  fire_table$change_fire_mean[[i]]
                ),
                "NA"
              )
            ),
            shiny::tags$td(
              ifelse(
                is.finite(
                  fire_table$future_fire_p90[[i]]
                ),
                sprintf(
                  "%.3f",
                  fire_table$future_fire_p90[[i]]
                ),
                "NA"
              )
            )
          )
        }
      )

      shiny::tagList(
        shiny::h5(
          "Fire probability on mapped peat"
        ),
        shiny::tags$table(
          class = paste(
            "table table-sm table-bordered",
            "report-table"
          ),
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th(
                "Scenario"
              ),
              shiny::tags$th(
                "Period"
              ),
              shiny::tags$th(
                "Baseline mean"
              ),
              shiny::tags$th(
                "Future mean"
              ),
              shiny::tags$th(
                "Change"
              ),
              shiny::tags$th(
                "Future p90"
              )
            )
          ),
          shiny::tags$tbody(
            table_rows
          )
        )
      )
    }

    shiny::tags$div(
      id =
        "oil_palm_peat_fire_linked_report",
      class =
        "oil-palm-peat-fire-report-section",
      shiny::hr(),
      shiny::h4(
        "Site-specific linked screening"
      ),
      shiny::div(
        class = "oil-palm-summary-card",
        shiny::div(
          class = "oil-palm-summary-big",
          "Peatland / fire considerations"
        ),
        shiny::div(
          paste(
            "Fire exposure is evaluated only over the mapped DoA peat",
            "portion of the active AOI."
          )
        )
      ),
      shiny::h5(
        "Mapped DoA peat within AOI"
      ),
      shiny::tags$table(
        class = paste(
          "table table-sm table-bordered",
          "report-table"
        ),
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th(
              "AOI area"
            ),
            shiny::tags$th(
              "Mapped peat"
            ),
            shiny::tags$th(
              "% of AOI"
            )
          )
        ),
        shiny::tags$tbody(
          shiny::tags$tr(
            shiny::tags$td(
              paste0(
                format(
                  round(
                    peat_summary$AOI_area_ha[[1]],
                    1
                  ),
                  big.mark = ","
                ),
                " ha"
              )
            ),
            shiny::tags$td(
              paste0(
                format(
                  round(
                    peat_summary$peat_area_ha[[1]],
                    1
                  ),
                  big.mark = ","
                ),
                " ha"
              )
            ),
            shiny::tags$td(
              paste0(
                round(
                  peat_summary$pct_AOI_peat[[1]],
                  1
                ),
                "%"
              )
            )
          )
        )
      ),
      fire_ui,
      shiny::div(
        class = "results-note",
        paste(
          "This is a peat-context climate/fire screening, not a complete peat-fire risk model.",
          "Drainage, water table, peat condition, ignition sources and plantation management",
          "are not represented. Results are reported separately and do not contribute to",
          "the Oil Palm crop-water-stress score."
        )
      ),
      shiny::br(),
      shiny::downloadButton(
        outputId =
          "download_oil_palm_peat_fire_csv",
        label =
          "Download peat / fire CSV"
      )
    )
  })

  output$download_oil_palm_peat_fire_csv <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "oil_palm_peat_fire_",
        safe_filename(
          ifelse(
            is.null(rv$aoi_name),
            "AOI",
            rv$aoi_name
          )
        ),
        ".csv"
      )
    },
    content = function(file) {
      result <- result_val()
      shiny::req(result)

      if (nrow(result$fire_table) > 0) {
        export <- result$fire_table |>
          dplyr::mutate(
            AOI_area_ha =
              result$peat_summary$AOI_area_ha[[1]],
            peat_area_ha =
              result$peat_summary$peat_area_ha[[1]],
            pct_AOI_peat =
              result$peat_summary$pct_AOI_peat[[1]],
            .before = 1
          )
      } else {
        export <- result$peat_summary
      }

      readr::write_csv(
        export,
        file
      )
    }
  )

  invisible(
    list(
      result = result_val,
      peat_context = peat_context_reactive
    )
  )
}
