# ============================================================
# REGISTER SABAH MARINE VISION RASTERS IN CLIMATE RISK EXPLORER
# ============================================================
#
# Run from the Climate-risk-screening-app project root AFTER the
# required SMV rasters have been copied under:
#
#   rasters/marine/
#
# Supported folder layouts include:
#
# A. Simplified app layout
#   rasters/marine/historical/1980-2005/
#   rasters/marine/rcp45/2020-2039/
#   rasters/marine/rcp45/2040-2059/
#   rasters/marine/rcp45/2079-2098/
#   rasters/marine/rcp85/2020-2039/
#   rasters/marine/rcp85/2040-2059/
#   rasters/marine/rcp85/2079-2098/
#
# B. Original SMV climatology folders copied under rasters/marine/
#   rasters/marine/climatologies/historical_1980_2005/T_surface/...
#   rasters/marine/climatologies/rcp45_2040_2059/T_surface/...
#
# The script locates the stress-oriented single-band raster for each
# core SMV variable, verifies it opens with terra, and idempotently
# adds/updates the four app configuration tables.
# ============================================================

required_packages <- c("readr", "dplyr", "tibble", "stringr", "terra")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install these packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

SMV_MARINE_ROOT <- file.path("rasters", "marine")
SMV_CONFIG_DIR <- "config"

SMV_PERIODS <- tibble::tribble(
  ~source_scenario, ~app_scenario, ~period,
  "historical",     "baseline",    "1980-2005",
  "rcp45",          "rcp45",       "2020-2039",
  "rcp45",          "rcp45",       "2040-2059",
  "rcp45",          "rcp45",       "2079-2098",
  "rcp85",          "rcp85",       "2020-2039",
  "rcp85",          "rcp85",       "2040-2059",
  "rcp85",          "rcp85",       "2079-2098"
)

SMV_VARIABLES <- tibble::tribble(
  ~variable_id,      ~display_name,                              ~file_regex,                                      ~theme,                       ~risk_direction, ~summary_method, ~fallback_units,
  "T_surface",       "Sea surface temperature",                  "T_surface.*warmest_climatological_month\\.tif$",  "Ocean warming",              "high",          "mean",          "source units",
  "T_bottom",        "Bottom temperature",                       "T_bottom.*warmest_climatological_month\\.tif$",   "Ocean warming",              "high",          "mean",          "source units",
  "S_surface",       "Surface salinity",                         "S_surface.*annual_climatological_mean\\.tif$",     "Coastal hydrography",        "mixed",         "mean",          "source units",
  "O2_bottom",       "Bottom dissolved oxygen",                  "O2_bottom.*lowest_climatological_month\\.tif$",    "Marine deoxygenation",       "low",           "mean",          "source units",
  "pH_surface",      "Surface pH",                               "pH_surface.*lowest_climatological_month\\.tif$",   "Ocean acidification",        "low",           "mean",          "pH",
  "pH_bottom",       "Bottom pH",                                "pH_bottom.*lowest_climatological_month\\.tif$",    "Ocean acidification",        "low",           "mean",          "pH",
  "satarag_surface", "Surface aragonite saturation state",       "satarag_surface.*lowest_climatological_month\\.tif$", "Ocean acidification",     "low",           "mean",          "dimensionless",
  "netPP_total",     "Column-total net primary production",      "netPP_total.*annual_climatological_mean\\.tif$",   "Marine productivity",        "low",           "mean",          "source units"
)

SMV_THEME_ORDER <- tibble::tribble(
  ~theme,                    ~display_order,
  "Ocean warming",           1,
  "Marine deoxygenation",    2,
  "Ocean acidification",     3,
  "Marine productivity",     4,
  "Coastal hydrography",     5
)

normalize_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

project_relative_path <- function(path) {
  path_abs <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root_abs <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  prefix <- paste0(root_abs, "/")
  if (startsWith(path_abs, prefix)) {
    return(substr(path_abs, nchar(prefix) + 1L, nchar(path_abs)))
  }

  # Fallback: keep absolute path if file is outside project.
  path_abs
}

marine_path_matches_combo <- function(path, source_scenario, period) {
  x <- tolower(normalize_slashes(path))
  period_us <- gsub("-", "_", period)
  scenario <- tolower(source_scenario)

  layout_a <- grepl(
    paste0("/", scenario, "/", period, "/"),
    x,
    fixed = TRUE
  )

  layout_b <- grepl(
    paste0("/", scenario, "_", period_us, "/"),
    x,
    fixed = TRUE
  )

  layout_a || layout_b
}

read_config_or_empty <- function(path, required_columns) {
  if (file.exists(path)) {
    out <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    out <- tibble::tibble()
  }

  for (nm in required_columns) {
    if (!nm %in% names(out)) out[[nm]] <- rep(NA, nrow(out))
  }
  out
}

bind_to_schema <- function(existing, new_rows) {
  all_names <- union(names(existing), names(new_rows))
  for (nm in setdiff(all_names, names(existing))) existing[[nm]] <- rep(NA, nrow(existing))
  for (nm in setdiff(all_names, names(new_rows))) new_rows[[nm]] <- rep(NA, nrow(new_rows))
  dplyr::bind_rows(
    existing[, all_names, drop = FALSE],
    new_rows[, all_names, drop = FALSE]
  )
}

upsert_rows <- function(existing, new_rows, key_cols) {
  if (nrow(new_rows) == 0) return(existing)
  if (nrow(existing) == 0) return(new_rows)

  old_key <- do.call(paste, c(existing[key_cols], sep = "||"))
  new_key <- do.call(paste, c(new_rows[key_cols], sep = "||"))
  existing <- existing[!old_key %in% new_key, , drop = FALSE]
  bind_to_schema(existing, new_rows)
}

detect_raster_units <- function(path, fallback) {
  units_value <- tryCatch({
    u <- terra::units(terra::rast(path))
    u <- unique(u[!is.na(u) & nzchar(u)])
    if (length(u) > 0) as.character(u[1]) else NA_character_
  }, error = function(e) NA_character_)

  if (!is.na(units_value) && nzchar(units_value)) units_value else fallback
}

build_smv_raster_catalogue_rows <- function(marine_root = SMV_MARINE_ROOT) {
  if (!dir.exists(marine_root)) {
    stop("Marine raster folder not found: ", normalize_slashes(marine_root))
  }

  all_tifs <- list.files(
    marine_root,
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(all_tifs) == 0) {
    stop("No .tif files found under ", normalize_slashes(marine_root))
  }

  rows <- list()
  missing <- list()
  k <- 0L
  m <- 0L

  for (i in seq_len(nrow(SMV_PERIODS))) {
    combo <- SMV_PERIODS[i, ]

    combo_files <- all_tifs[vapply(
      all_tifs,
      marine_path_matches_combo,
      logical(1),
      source_scenario = combo$source_scenario[[1]],
      period = combo$period[[1]]
    )]

    for (j in seq_len(nrow(SMV_VARIABLES))) {
      meta <- SMV_VARIABLES[j, ]
      hits <- combo_files[
        stringr::str_detect(
          basename(combo_files),
          stringr::regex(meta$file_regex[[1]], ignore_case = TRUE)
        )
      ]

      if (length(hits) != 1) {
        m <- m + 1L
        missing[[m]] <- tibble::tibble(
          scenario = combo$app_scenario[[1]],
          period = combo$period[[1]],
          variable_id = meta$variable_id[[1]],
          matches_found = length(hits),
          expected_pattern = meta$file_regex[[1]]
        )
        next
      }

      # Verify raster is readable and single-band.
      r <- tryCatch(terra::rast(hits[[1]]), error = function(e) NULL)
      if (is.null(r)) {
        stop("Could not open marine raster: ", hits[[1]])
      }
      if (terra::nlyr(r) != 1) {
        stop(
          "Expected a single-band stress-oriented raster but found ",
          terra::nlyr(r), " bands: ", hits[[1]]
        )
      }

      k <- k + 1L
      rows[[k]] <- tibble::tibble(
        dataset_id = paste(
          "SMV",
          meta$variable_id[[1]],
          toupper(combo$app_scenario[[1]]),
          gsub("-", "", combo$period[[1]]),
          sep = "_"
        ),
        variable_id = meta$variable_id[[1]],
        scenario = combo$app_scenario[[1]],
        period = combo$period[[1]],
        file_path = project_relative_path(hits[[1]]),
        file_format = "tif",
        units = detect_raster_units(hits[[1]], meta$fallback_units[[1]]),
        dataset_type = "continuous",
        resolution = "~0.1 degree (~11 km)",
        nodata_value = -9999,
        enabled = TRUE
      )
    }
  }

  list(
    rows = dplyr::bind_rows(rows),
    missing = dplyr::bind_rows(missing)
  )
}

build_smv_variable_metadata_rows <- function() {
  SMV_VARIABLES |>
    dplyr::transmute(
      variable_id,
      display_name,
      description = dplyr::case_when(
        variable_id == "T_surface" ~ "Warmest climatological month sea-surface temperature from the Sabah Marine Vision regional marine climate projections.",
        variable_id == "T_bottom" ~ "Warmest climatological month bottom temperature from the Sabah Marine Vision regional marine climate projections.",
        variable_id == "S_surface" ~ "Annual climatological mean surface salinity; changes indicate shifts in coastal hydrography and freshwater influence.",
        variable_id == "O2_bottom" ~ "Lowest climatological month bottom dissolved oxygen; lower values indicate greater deoxygenation stress.",
        variable_id == "pH_surface" ~ "Lowest climatological month surface pH; lower values indicate greater ocean acidification exposure.",
        variable_id == "pH_bottom" ~ "Lowest climatological month bottom pH; lower values indicate greater acidification exposure for benthic habitats.",
        variable_id == "satarag_surface" ~ "Lowest climatological month surface aragonite saturation state; lower values indicate less favourable conditions for calcifying organisms.",
        variable_id == "netPP_total" ~ "Annual climatological mean column-total net primary production; declining values indicate reduced modelled marine productivity.",
        TRUE ~ display_name
      ),
      units = fallback_units,
      summary_method,
      risk_direction,
      baseline_variable_id = variable_id,
      classification_method = "screening_value_and_change; composite scoring pending marine calibration",
      interpretation = dplyr::case_when(
        risk_direction == "high" ~ "Higher values generally indicate greater climate stress for this dimension.",
        risk_direction == "low" ~ "Lower values generally indicate greater climate stress for this dimension.",
        TRUE ~ "Interpret direction of change in the local ecological and hydrographic context."
      ),
      limitations = paste(
        "Blue Communities POLCOMS-ERSEM regional marine model; approximately 0.1 degree (~11 km).",
        "Coastal extension addresses coarse-grid coastline gaps but does not create fine-scale marine climate information.",
        "Do not interpret a relative screening score as ecological damage or fisheries yield loss."
      )
    )
}

build_smv_theme_variable_rows <- function() {
  SMV_VARIABLES |>
    dplyr::group_by(theme) |>
    dplyr::mutate(display_order = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      theme,
      variable_id,
      display_order,
      default_selected = display_order == 1
    )
}

build_smv_pathway_theme_rows <- function() {
  SMV_THEME_ORDER |>
    dplyr::transmute(
      pathway = "Coastal & Marine Climate Stress",
      theme,
      display_order,
      default_enabled = display_order == 1
    )
}

register_smv_marine_layers <- function(
    marine_root = SMV_MARINE_ROOT,
    config_dir = SMV_CONFIG_DIR,
    require_complete = TRUE
) {
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  scan <- build_smv_raster_catalogue_rows(marine_root)

  if (nrow(scan$missing) > 0) {
    message("\nMissing or ambiguous SMV raster matches:")
    print(scan$missing, n = Inf)
    if (require_complete) {
      stop(
        "Marine registration stopped because one or more required raster ",
        "files were missing or ambiguous."
      )
    }
  }

  if (nrow(scan$rows) == 0) {
    stop("No usable marine raster catalogue rows were created.")
  }

  raster_path <- file.path(config_dir, "raster_catalogue.csv")
  metadata_path <- file.path(config_dir, "variable_metadata.csv")
  theme_path <- file.path(config_dir, "theme_variables.csv")
  pathway_path <- file.path(config_dir, "pathway_themes.csv")

  raster_existing <- read_config_or_empty(
    raster_path,
    c("dataset_id", "variable_id", "scenario", "period", "file_path", "units", "dataset_type", "nodata_value", "enabled")
  )
  metadata_existing <- read_config_or_empty(metadata_path, c("variable_id"))
  theme_existing <- read_config_or_empty(theme_path, c("theme", "variable_id"))
  pathway_existing <- read_config_or_empty(pathway_path, c("pathway", "theme"))

  raster_out <- upsert_rows(
    raster_existing,
    scan$rows,
    c("variable_id", "scenario", "period")
  )

  metadata_out <- upsert_rows(
    metadata_existing,
    build_smv_variable_metadata_rows(),
    "variable_id"
  )

  theme_out <- upsert_rows(
    theme_existing,
    build_smv_theme_variable_rows(),
    c("theme", "variable_id")
  )

  pathway_out <- upsert_rows(
    pathway_existing,
    build_smv_pathway_theme_rows(),
    c("pathway", "theme")
  )

  readr::write_csv(raster_out, raster_path, na = "")
  readr::write_csv(metadata_out, metadata_path, na = "")
  readr::write_csv(theme_out, theme_path, na = "")
  readr::write_csv(pathway_out, pathway_path, na = "")

  message("\nSMV marine registration complete.")
  message("Registered marine raster records: ", nrow(scan$rows))
  message("Expected for a complete core set: ", nrow(SMV_PERIODS) * nrow(SMV_VARIABLES))
  message("Screening application: Coastal & Marine Climate Stress")

  invisible(list(
    raster_rows = scan$rows,
    missing = scan$missing,
    raster_catalogue = raster_out,
    variable_metadata = metadata_out,
    theme_variables = theme_out,
    pathway_themes = pathway_out
  ))
}
