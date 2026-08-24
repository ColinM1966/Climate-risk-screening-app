# ============================================================
# AUDIT MASTER LAYER REGISTRY AGAINST THE LIVE APP CATALOGUE
# ============================================================
# Run from the Sabah Climate Risk Explorer project root.
# It does not modify the app configuration.
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fs)

audit_layer_registry <- function(
    raster_catalogue_path = "config/raster_catalogue.csv",
    layer_registry_path = "config/layer_registry.csv",
    output_dir = "outputs/layer_audit") {

  dir_create(output_dir)

  catalogue <- read_csv(raster_catalogue_path, show_col_types = FALSE)
  registry <- read_csv(layer_registry_path, show_col_types = FALSE)

  required_cat <- c("variable_id", "scenario", "period", "file_path")
  required_reg <- c("layer_id", "display_name", "current_status")

  stopifnot(all(required_cat %in% names(catalogue)))
  stopifnot(all(required_reg %in% names(registry)))

  if (!"enabled" %in% names(catalogue)) catalogue$enabled <- TRUE

  catalogue <- catalogue |>
    mutate(
      enabled = as.logical(enabled),
      file_exists = file.exists(file_path)
    )

  coverage <- catalogue |>
    filter(enabled) |>
    group_by(variable_id) |>
    summarise(
      catalogue_rows = n(),
      existing_files = sum(file_exists, na.rm = TRUE),
      scenarios = paste(sort(unique(scenario)), collapse = ";"),
      periods = paste(sort(unique(period)), collapse = ";"),
      .groups = "drop"
    )

  audit <- registry |>
    left_join(coverage, by = c("layer_id" = "variable_id")) |>
    mutate(
      live_catalogue_status = case_when(
        is.na(catalogue_rows) ~ "NOT_IN_RASTER_CATALOGUE",
        existing_files == 0 ~ "CATALOGUED_FILES_MISSING",
        existing_files < catalogue_rows ~ "PARTIAL_FILES_AVAILABLE",
        TRUE ~ "AVAILABLE"
      )
    )

  unregistered <- catalogue |>
    distinct(variable_id) |>
    anti_join(registry, by = c("variable_id" = "layer_id")) |>
    arrange(variable_id)

  coverage_grid <- catalogue |>
    filter(enabled) |>
    count(variable_id, scenario, period, wt = as.integer(file_exists), name = "existing_files") |>
    arrange(variable_id, scenario, period)

  write_csv(audit, file.path(output_dir, "layer_registry_live_audit.csv"))
  write_csv(unregistered, file.path(output_dir, "catalogue_layers_not_in_registry.csv"))
  write_csv(coverage_grid, file.path(output_dir, "scenario_period_coverage.csv"))

  message("Audit written to: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  invisible(list(audit = audit, unregistered = unregistered, coverage = coverage_grid))
}

# Run directly when sourcing interactively:
# audit_layer_registry()
