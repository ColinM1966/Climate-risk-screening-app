# ============================================================
# SABAH CLIMATE RISK EXPLORER
# Initial Shiny prototype
# ============================================================

library(shiny)
library(shinyjs)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(terra)
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# LOAD APPLICATION FUNCTIONS
# ------------------------------------------------------------

source(
  file.path(
    "R",
    "utils",
    "load_config.R"
  )
)

source(
  file.path(
    "R",
    "utils",
    "find_raster.R"
  )
)

source(
  file.path(
    "R",
    "processing",
    "prepare_aoi.R"
  )
)

source(
  file.path(
    "R",
    "processing",
    "process_continuous_raster.R"
  )
)

source(
  file.path(
    "R",
    "processing",
    "process_inundation_raster.R"
  )
)

# ------------------------------------------------------------
# OIL PALM IMPACT SCREENING
# ------------------------------------------------------------

source(
  file.path(
    "R",
    "processing",
    "oil_palm_screening_app.R"
  )
)

source(
  file.path(
    "R",
    "processing",
    "oil_palm_dimensions.R"
  )
)


# ------------------------------------------------------------
# OIL PALM CONDITIONAL COASTAL LINKED SCREENING
# ------------------------------------------------------------

source(
  file.path(
    "R",
    "processing",
    "oil_palm_coastal_linked_screening.R"
  )
)


# ------------------------------------------------------------
# OIL PALM DOA PEAT / FIRE LINKED SCREENING
# ------------------------------------------------------------

source(
  file.path(
    "R",
    "processing",
    "oil_palm_peat_fire_linked_screening.R"
  )
)


# ------------------------------------------------------------
# FOREST RESTORATION CLIMATE SCREENING
# ------------------------------------------------------------

source(
  file.path(
    "R",
    "processing",
    "restoration_screening_app.R"
  )
)

# ------------------------------------------------------------
# LOAD CONFIGURATION TABLES
# ------------------------------------------------------------

app_config <- load_app_config(
  config_dir = "config"
)

print_config_summary(
  app_config
)

raster_catalogue <- app_config$raster_catalogue
variable_metadata <- app_config$variable_metadata
theme_variables <- app_config$theme_variables
pathway_themes <- app_config$pathway_themes
risk_thresholds <- app_config$risk_thresholds

# ------------------------------------------------------------
# BASIC CHOICES
# ------------------------------------------------------------

pathway_choices <- pathway_themes |>
  dplyr::distinct(pathway) |>
  dplyr::arrange(pathway) |>
  dplyr::pull(pathway)

if (length(pathway_choices) == 0) {
  pathway_choices <- "General Climate Risk Screening"
}

# ------------------------------------------------------------
# SCREENING APPLICATION CHOICES
# ------------------------------------------------------------
get_screening_application_label <- function(pathway_name) {
  x <- stringr::str_to_lower(pathway_name)
  if (stringr::str_detect(x, "general.*climate")) return("General Climate Screening")
  if (stringr::str_detect(x, "restoration")) return("Forest Restoration Planning")
  if (stringr::str_detect(x, "humid.*heat|heat.*stress|workability")) return("Outdoor Workability & Heat Stress")
  if (stringr::str_detect(x, "coastal.*marine|marine.*climate|marine.*stress")) return("Coastal & Marine Climate Stress")
  pathway_name
}

general_pathway_matches <- pathway_choices[
  stringr::str_detect(stringr::str_to_lower(pathway_choices), "general.*climate")
]

general_pathway <- if (length(general_pathway_matches) > 0) {
  general_pathway_matches[1]
} else {
  pathway_choices[1]
}

other_pathways <- setdiff(pathway_choices, general_pathway)
other_pathways <- other_pathways[!stringr::str_detect(stringr::str_to_lower(other_pathways), "restoration")]

screening_application_choices <- c(
  stats::setNames(paste0("pathway::", general_pathway), "General Climate Screening"),
  "Oil Palm Climate Stress" = "oil_palm",
  "Forest Restoration Planning" = "restoration"
)

if (length(other_pathways) > 0) {
  screening_application_choices <- c(
    screening_application_choices,
    stats::setNames(
      paste0("pathway::", other_pathways),
      vapply(other_pathways, get_screening_application_label, character(1))
    )
  )
}

default_screening_application <- paste0("pathway::", general_pathway)

# ------------------------------------------------------------
# LABEL HELPERS
# ------------------------------------------------------------

scenario_labels <- c(
  baseline = "Baseline",
  ssp126 = "SSP1-2.6",
  ssp245 = "SSP2-4.5",
  ssp370 = "SSP3-7.0",
  ssp585 = "SSP5-8.5",
  rcp45 = "RCP4.5",
  rcp85 = "RCP8.5"
)

period_labels <- c(
  "1980-2005" = "1980–2005",
  "1981-2010" = "1981–2010",
  "2011-2040" = "2011–2040",
  "2020-2039" = "2020–2039",
  "2040-2059" = "2040–2059",
  "2041-2070" = "2041–2070",
  "2071-2100" = "2071–2100",
  "2079-2098" = "2079–2098"
)

get_scenario_label <- function(scenario_id) {
  
  if (scenario_id %in% names(scenario_labels)) {
    return(
      unname(
        scenario_labels[[scenario_id]]
      )
    )
  }
  
  scenario_id
}

get_period_label <- function(period_id) {
  
  if (period_id %in% names(period_labels)) {
    return(
      unname(
        period_labels[[period_id]]
      )
    )
  }
  
  period_id
}

get_variable_label <- function(selected_variable_id) {
  
  # Primary source: config/variable metadata.
  # Keep labels catalogue-driven wherever possible.
  variable_label <- variable_metadata |>
    dplyr::filter(
      variable_id == selected_variable_id
    ) |>
    dplyr::pull(
      display_name
    )
  
  if (
    length(variable_label) > 0 &&
    !is.na(variable_label[1]) &&
    variable_label[1] != ""
  ) {
    return(
      variable_label[1]
    )
  }
  
  # Fallback only.
  # This prevents raw IDs being shown if the config has not yet
  # been updated, but the preferred fix is still to add the label
  # to config/variable_metadata.
  fallback_variable_labels <- c(
    WBGTmax = "Maximum WBGT",
    Bio05 = "Maximum temperature of warmest month",
    Bio017 = "Precipitation of driest quarter",
    CDD = "Consecutive dry days",
    FIRE_PROB = "Fire probability",
    Fire = "Fire probability",
    PPETConDryMth = "Consecutive dry months (P/PET < 1)",
    PPETmin = "Minimum precipitation-to-PET ratio",
    T_surface = "Sea surface temperature",
    T_bottom = "Bottom temperature",
    S_surface = "Surface salinity",
    O2_bottom = "Bottom dissolved oxygen",
    pH_surface = "Surface pH",
    pH_bottom = "Bottom pH",
    satarag_surface = "Surface aragonite saturation state",
    netPP_total = "Column-total net primary production"
  )
  
  if (selected_variable_id %in% names(fallback_variable_labels)) {
    return(
      unname(
        fallback_variable_labels[[selected_variable_id]]
      )
    )
  }
  
  selected_variable_id
}

safe_filename <- function(x) {
  
  x |>
    stringr::str_replace_all("[^A-Za-z0-9_-]", "_") |>
    stringr::str_replace_all("_+", "_")
}


aoi_has_valid_raster_cells <- function(
    aoi,
    raster_path
) {
  
  if (
    is.null(aoi) ||
    is.null(raster_path) ||
    !file.exists(raster_path)
  ) {
    return(FALSE)
  }
  
  result <- tryCatch(
    {
      raster_object <- terra::rast(
        raster_path
      )
      
      print(
        paste(
          "Checking raster:",
          raster_path
        )
      )
      
      print(
        terra::ext(
          raster_object
        )
      )
      
      print(
        terra::crs(
          raster_object
        )
      )
      
      if (terra::nlyr(raster_object) > 1) {
        raster_object <- raster_object[[1]]
      }
      
      raster_crs <- terra::crs(
        raster_object
      )
      
      aoi_for_raster <- sf::st_transform(
        aoi,
        raster_crs
      )
      
      print(
        sf::st_bbox(
          aoi_for_raster
        )
      )
      
      aoi_vect <- terra::vect(
        aoi_for_raster
      )
      
      cropped <- terra::crop(
        raster_object,
        aoi_vect
      )
      
      masked <- terra::mask(
        cropped,
        aoi_vect
      )
      
      values <- terra::values(
        masked,
        mat = FALSE
      )
      
      any(
        is.finite(values)
      )
    },
    error = function(e) {
      FALSE
    }
  )
  
  isTRUE(result)
}

get_first_existing_column <- function(data, possible_names) {
  
  matched_names <- intersect(
    possible_names,
    names(data)
  )
  
  if (length(matched_names) == 0) {
    return(NA_real_)
  }
  
  data[[matched_names[1]]]
}

get_first_numeric_stat <- function(data, possible_names) {
  
  matched_names <- intersect(
    possible_names,
    names(data)
  )
  
  if (length(matched_names) > 0) {
    value <- data[[matched_names[1]]]
    return(
      as.numeric(value[1])
    )
  }
  
  numeric_columns <- names(data)[
    vapply(
      data,
      is.numeric,
      logical(1)
    )
  ]
  
  if (length(numeric_columns) == 0) {
    return(NA_real_)
  }
  
  value <- data[[numeric_columns[1]]]
  
  as.numeric(value[1])
}

get_analysis_value <- function(
    result_object,
    possible_names
) {
  
  if (is.null(result_object)) {
    return(NA_real_)
  }
  
  # Case 1: value is returned directly from process_continuous_raster().
  matching_names <- possible_names[
    possible_names %in% names(result_object)
  ]
  
  if (length(matching_names) >= 1) {
    value <- result_object[[matching_names[1]]]
    
    if (length(value) > 0) {
      return(
        as.numeric(value[[1]])
      )
    }
  }
  
  # Case 2: value is inside result_object$summary.
  if (
    is.list(result_object) &&
    "summary" %in% names(result_object)
  ) {
    summary_table <- as.data.frame(
      result_object$summary
    )
    
    matching_summary_names <- possible_names[
      possible_names %in% names(summary_table)
    ]
    
    if (length(matching_summary_names) >= 1) {
      value <- summary_table[[matching_summary_names[1]]]
      
      if (length(value) > 0) {
        return(
          as.numeric(value[[1]])
        )
      }
    }
  }
  
  # Case 3: result itself is a data frame.
  if (is.data.frame(result_object)) {
    matching_data_names <- possible_names[
      possible_names %in% names(result_object)
    ]
    
    if (length(matching_data_names) >= 1) {
      value <- result_object[[matching_data_names[1]]]
      
      if (length(value) > 0) {
        return(
          as.numeric(value[[1]])
        )
      }
    }
  }
  
  NA_real_
}

# ------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------

ui <- page_sidebar(
  
  title = "Sabah Climate Risk Explorer",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#164A73"
  ),
  
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(
      HTML(
        "
        html,
        body {
          height: 100%;
          overflow: hidden;
        }

        .bslib-sidebar-layout {
          height: 100vh;
          max-height: 100vh;
        }

        .bslib-sidebar-layout .sidebar,
        .bslib-sidebar-layout > .sidebar,
        .bslib-sidebar-layout > aside,
        aside.sidebar,
        aside.bslib-sidebar,
        .sidebar {
          overflow: hidden !important;
        }

        #sidebar_scroll_content {
          max-height: calc(100vh - 20px);
          overflow-y: auto !important;
          overflow-x: hidden !important;
          padding-right: 14px;
          padding-bottom: 90px;
          scrollbar-width: thin;
        }

        #sidebar_scroll_content::-webkit-scrollbar {
          width: 10px;
        }

        #sidebar_scroll_content::-webkit-scrollbar-thumb {
          background-color: #999999;
          border-radius: 6px;
        }

        #sidebar_scroll_content::-webkit-scrollbar-track {
          background-color: #f1f1f1;
        }

        #run_analysis_scroll_zone {
          margin-bottom: 20px;
          padding-bottom: 10px;
        }

        #run_analysis {
          margin-bottom: 20px;
        }

        #run_analysis:disabled {
          cursor: not-allowed;
        }

        #sidebar_scroll_content .form-group,
        #sidebar_scroll_content .shiny-input-container,
        #sidebar_scroll_content .selectize-control {
          width: 100%;
        }

        #sidebar_scroll_content .shiny-text-output,
        #sidebar_scroll_content .help-block,
        #sidebar_scroll_content .form-text {
          white-space: normal;
          word-wrap: break-word;
        }

        body {
          overflow-x: hidden;
        }

        .leaflet-container {
          max-width: 100%;
        }

        .dataTables_wrapper {
          overflow-x: auto;
        }

        .results-note {
          margin-top: 10px;
          padding: 10px;
          font-size: 13px;
          color: #ffffff;
          background-color: #111111;
          border-left: 4px solid #ffd700;
          border-radius: 4px;
        }

        .comparison-table-wrapper {
          width: 100%;
          max-height: 380px;
          overflow-y: auto;
          overflow-x: auto;
          margin-bottom: 16px;
          clear: both;
        }

        .comparison-download-row {
          clear: both;
          display: block;
          margin-top: 16px;
          margin-bottom: 16px;
          position: relative;
          z-index: 10;
        }


        .selection-status-box {
          margin-top: 10px;
          padding: 12px;
          background-color: #f8f9fa;
          border: 1px solid #d8dde2;
          border-radius: 6px;
          font-size: 13px;
        }

        .selection-status-ready {
          border-left: 5px solid #2e7d32;
        }

        .selection-status-incomplete {
          border-left: 5px solid #d9822b;
        }

        .selection-status-heading {
          display: flex;
          align-items: center;
          gap: 8px;
          font-weight: 600;
          font-size: 14px;
        }

        .selection-status-box hr {
          margin-top: 9px;
          margin-bottom: 9px;
        }

        .selection-status-row {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
          gap: 12px;
          margin-top: 5px;
        }

        .selection-status-label {
          flex: 0 0 42%;
          color: #5f6872;
          font-weight: 600;
        }

        .selection-status-value {
          flex: 1;
          text-align: right;
          overflow-wrap: anywhere;
        }

        .selection-status-detail {
          margin-top: 1px;
          margin-bottom: 5px;
          color: #6c757d;
          font-size: 12px;
          text-align: right;
        }

        .selection-status-good {
          color: #2e7d32;
          font-weight: 600;
          text-align: right;
        }

        .selection-status-warning {
          color: #a34f00;
          font-weight: 600;
          text-align: right;
        }

        .oil-palm-summary-card {
          padding: 16px;
          border: 1px solid #cfd8d3;
          border-left: 6px solid #2e7d32;
          border-radius: 8px;
          background: #f7fbf7;
          margin-bottom: 16px;
        }

        .oil-palm-summary-big {
          font-size: 24px;
          font-weight: 700;
          color: #1b5e20;
          margin-bottom: 4px;
        }

        .oil-palm-score-card {
          padding: 14px;
          border-radius: 8px;
          background: #eef6ef;
          border: 1px solid #c8ddca;
          margin-top: 12px;
          margin-bottom: 12px;
        }

        .oil-palm-score-number {
          font-size: 28px;
          font-weight: 700;
          color: #1b5e20;
        }
        "
      )
    )
  ),
  
  tags$script(
    HTML(
      "
      document.addEventListener('wheel', function(event) {
        var target = event.target;
        var runZone = target.closest('#run_analysis_scroll_zone');

        if (!runZone) {
          return;
        }

        var sidebarContent = document.querySelector('#sidebar_scroll_content');

        if (!sidebarContent) {
          return;
        }

        sidebarContent.scrollTop = sidebarContent.scrollTop + event.deltaY;
        event.preventDefault();
      }, { passive: false });
      "
    )
  ),
  
  tags$style(
    HTML(
      "
      .print-only {
        display: none;
      }

      .screening-report-actions {
        display: flex;
        gap: 8px;
        justify-content: flex-end;
        margin-bottom: 12px;
      }

      @media print {
        @page {
          size: A4 portrait;
          margin: 12mm;
        }

        html, body {
          height: auto !important;
          overflow: visible !important;
          background: #ffffff !important;
        }

        body * {
          visibility: hidden !important;
        }

        .active-print-report,
        .active-print-report * {
          visibility: visible !important;
        }

        .active-print-report {
          position: absolute !important;
          left: 0 !important;
          top: 0 !important;
          width: 100% !important;
          max-width: none !important;
          border: 0 !important;
          box-shadow: none !important;
          background: #ffffff !important;
        }

        .active-print-report .no-print,
        .active-print-report #oil_palm_comparison_plot_controls,
        .active-print-report #download_oil_palm_csv {
          display: none !important;
        }

        .active-print-report .print-only {
          display: block !important;
        }

        .active-print-report .results-note {
          color: #333333 !important;
          background: #ffffff !important;
          border-left: 3px solid #666666 !important;
        }

        .active-print-report .oil-palm-summary-card,
        .active-print-report .oil-palm-score-card,
        .active-print-report table,
        .active-print-report .shiny-plot-output {
          break-inside: avoid;
          page-break-inside: avoid;
        }

        .active-print-report table {
          width: 100% !important;
          font-size: 9.5pt !important;
        }

        .active-print-report .card-header {
          font-size: 18pt !important;
          font-weight: 700 !important;
        }
      }
      "
    )
  ),

  tags$script(
    HTML(
      "
      function printScreeningReport(targetId) {
        var target = document.getElementById(targetId);
        if (!target) {
          return;
        }

        document.querySelectorAll('.active-print-report').forEach(function(el) {
          el.classList.remove('active-print-report');
        });

        target.classList.add('active-print-report');
        window.print();

        setTimeout(function() {
          target.classList.remove('active-print-report');
        }, 500);
      }
      "
    )
  ),

  # OIL PALM PRINT V6 - external standalone report window
  tags$script(src = "print_screening_report_v7.js"),

  sidebar = sidebar(
    
    width = 360,
    
    div(
      id = "sidebar_scroll_content",
      
      h4("1. Select area"),
      
      helpText(
        "Choose one method to define the area you want to analyse."
      ),
      
      radioButtons(
        inputId = "aoi_method",
        label = NULL,
        choices = c(
          "Upload polygon" = "upload",
          "Draw polygon" = "draw",
          "Select point and buffer" = "point",
          "Use Jambongan test AOI" = "jambongan"
        ),
        selected = "upload"
      ),
      
      conditionalPanel(
        condition = "input.aoi_method == 'upload'",
        
        fileInput(
          inputId = "aoi_file",
          label = "Upload spatial file",
          accept = c(
            ".gpkg",
            ".geojson",
            ".json",
            ".kml",
            ".shp",
            ".shx",
            ".dbf",
            ".prj"
          ),
          multiple = TRUE
        ),
        
        helpText(
          "Upload a polygon GeoPackage, GeoJSON, KML, or complete shapefile."
        )
      ),
      
      conditionalPanel(
        condition = "input.aoi_method == 'draw'",
        
        helpText(
          "Use the polygon tool on the map. Click around the boundary, then click the first point again to finish. The polygon will become the active AOI."
        ),
        
        actionButton(
          inputId = "clear_drawn_aoi",
          label = "Clear drawn AOI",
          class = "btn-secondary",
          width = "100%"
        )
      ),
      
      conditionalPanel(
        condition = "input.aoi_method == 'jambongan'",
        
        actionButton(
          inputId = "use_jambongan",
          label = "Load Jambongan test AOI",
          class = "btn-secondary",
          width = "100%"
        ),
        
        helpText(
          "Loads the Jambongan example AOI for testing."
        )
      ),
      
      conditionalPanel(
        condition = "input.aoi_method == 'point'",
        
        numericInput(
          inputId = "buffer_km",
          label = "Buffer distance (km)",
          value = 10,
          min = 0.1,
          max = 100,
          step = 0.5
        ),
        
        helpText(
          "Enter a buffer distance, then click the map. The circular buffer will become the active AOI."
        ),
        
        actionButton(
          inputId = "clear_point_buffer",
          label = "Clear point buffer",
          class = "btn-secondary",
          width = "100%"
        )
      ),
      
      textOutput(
        "active_aoi_status"
      ),
      
      hr(),
      
      h4("2. Select screening application"),
      
      selectInput(
        inputId = "screening_application",
        label = NULL,
        choices = screening_application_choices,
        selected = default_screening_application
      ),
      
      helpText(
        paste(
          "Choose the decision or screening task first.",
          "The controls below change to suit that application."
        )
      ),
      
      div(
        style = "display:none;",
        selectInput(
          inputId = "pathway",
          label = NULL,
          choices = pathway_choices,
          selected = general_pathway
        ),
        checkboxInput(
          inputId = "run_comparison",
          label = NULL,
          value = FALSE
        )
      ),
      
      conditionalPanel(
        condition = "input.screening_application != 'oil_palm' && input.screening_application != 'restoration'",
        
        h4("3. Select theme"),
        selectInput("theme", NULL, choices = NULL),
        
        h4("4. Select variable"),
        selectInput("variable_id", NULL, choices = NULL),
        
        h4("5. Select analysis mode"),
        radioButtons(
          "analysis_mode",
          NULL,
          choices = c(
            "Single scenario / time period" = "single",
            "Compare scenarios / time periods" = "compare"
          ),
          selected = "single"
        ),
        
        conditionalPanel(
          condition = "input.analysis_mode == 'single'",
          h4("6. Select scenario"),
          selectInput("scenario", NULL, choices = NULL),
          h4("7. Select time period"),
          selectInput("period", NULL, choices = NULL)
        ),
        
        conditionalPanel(
          condition = "input.analysis_mode == 'compare'",
          h4("6. Compare scenarios"),
          selectInput(
            "comparison_scenarios",
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          h4("7. Compare time periods"),
          selectInput(
            "comparison_periods",
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          helpText(
            paste(
              "Time periods are 30-year climatologies.",
              "Unavailable scenario-period combinations are skipped automatically."
            )
          )
        ),
        
        h4("8. Output options"),
        checkboxInput(
          "create_cropped_raster",
          "Create cropped raster output",
          value = FALSE
        ),
        helpText(
          "Only turn this on if you need to display or download a clipped GeoTIFF."
        ),
        
        hr(),
        h4("9. Run analysis"),
        div(
          id = "run_analysis_scroll_zone",
          helpText("Load or draw an AOI before running analysis."),
          actionButton(
            "run_analysis",
            "Run analysis",
            class = "btn-primary",
            width = "100%"
          )
        ),
        br(), br(),
        uiOutput("selection_status")
      ),
      
      conditionalPanel(
        condition = "input.screening_application == 'restoration'",
        div(
          class = "restoration-screening-panel",
          uiOutput("restoration_screening_ui")
        )
      ),
      conditionalPanel(
        condition = "input.screening_application == 'oil_palm'",
        div(
          class = "oil-palm-screening-panel",
          h4("3. Oil palm climate stress"),
          helpText(
            paste(
              "Automatically uses Minimum P:PET, consecutive months with P:PET < 1,",
              "and consecutive dry days (CDD). Future conditions are compared with the 1981–2010 baseline."
            )
          ),
          
          uiOutput("oil_palm_dimensions_ui"),

          uiOutput("oil_palm_coastal_linked_ui"),

          uiOutput("oil_palm_peat_fire_linked_ui"),

          h4("4. Select analysis mode"),
          radioButtons(
            "oil_palm_analysis_mode",
            NULL,
            choices = c(
              "Single scenario / time period" = "single",
              "Compare scenarios / time periods" = "compare"
            ),
            selected = "single"
          ),
          
          conditionalPanel(
            condition = "input.oil_palm_analysis_mode == 'single'",
            h4("5. Select scenario"),
            selectInput("oil_palm_scenario", NULL, choices = NULL),
            h4("6. Select time period"),
            selectInput("oil_palm_period", NULL, choices = NULL)
          ),
          
          conditionalPanel(
            condition = "input.oil_palm_analysis_mode == 'compare'",
            h4("5. Compare scenarios"),
            selectInput(
              "oil_palm_comparison_scenarios",
              NULL,
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            h4("6. Compare time periods"),
            selectInput(
              "oil_palm_comparison_periods",
              NULL,
              choices = NULL,
              selected = NULL,
              multiple = TRUE
            ),
            helpText(
              paste(
                "The 1981–2010 baseline is included automatically.",
                "Only available SSP and climatology combinations are analysed."
              )
            )
          ),
          
          hr(),
          h4("7. Run oil palm screening"),
          helpText("Load or draw an AOI before running the oil palm screening."),
          actionButton(
            "run_oil_palm",
            "Run oil palm screening",
            class = "btn-success",
            width = "100%"
          ),
          br(), br(),
          uiOutput("oil_palm_selection_status")
        )
      )
    )
  ),
  
  navset_card_tab(
    
    nav_panel(
      title = "Map",
      
      leafletOutput(
        outputId = "map",
        height = "72vh"
      )
    ),
    
    nav_panel(
      title = "Results",
      
      card(
        card_header(
          "Analysis summary"
        ),
        
        verbatimTextOutput(
          outputId = "analysis_status"
        ),
        
        tags$hr(),
        
        tableOutput(
          outputId = "result_table"
        ),
        
        br(),
        
        div(
          class = "results-note",
          textOutput("results_note")
        ),
        
        br(),
        
        h4("Scenario and period comparison"),
        
        textOutput(
          outputId = "comparison_missing_note"
        ),
        
        div(
          class = "comparison-table-wrapper",
          DTOutput(
            outputId = "comparison_results"
          )
        ),
        
        div(
          class = "results-note",
          "Change from baseline is calculated as the selected row mean minus the baseline mean for the same AOI and variable. It is a simple comparison value, not a risk score."
        ),
        
        br(),
        
        h4("Comparison graph"),
        
        uiOutput(
          outputId = "comparison_graph_ui"
        ),
        
        div(
          class = "results-note",
          paste(
            "The graph shows mean values from the comparison table.",
            "It does not show the full range of values.",
            "Minimum and maximum values are shown in the table."
          )
        ),
        
        div(
          class = "results-note",
          "For Bio017 and PPETmin, lower values indicate drier conditions."
        ),
        
        br(),
        
        h4("Downloads"),
        
        uiOutput(
          outputId = "download_buttons"
        ),
        
        br(),
        
        textOutput(
          outputId = "cropped_raster_status"
        ),
        
        br(),
        
        div(
          class = "results-note",
          "Cropped raster output is optional. If selected, the app creates a clipped GeoTIFF for GIS use. If not selected, only table and graph outputs are produced."
        )
      )
    ),
    
    nav_panel(
      title = "Oil Palm",
      card(
        id = "oil_palm_report_print_area",
        card_header("Oil Palm Climate Stress"),
        uiOutput("oil_palm_print_metadata"),
        uiOutput("oil_palm_print_button_ui"),
        uiOutput("oil_palm_summary_ui"),
        h4("Screening results"),
        tableOutput("oil_palm_results_table"),
        uiOutput("oil_palm_peat_fire_results_ui"),
        uiOutput("oil_palm_coastal_results_ui"),
        uiOutput("oil_palm_optional_results_ui"),
        uiOutput("oil_palm_comparison_plot_controls"),
        uiOutput("oil_palm_comparison_plot_ui"),
        div(
          class = "results-note",
          paste(
            "The 0–100 Relative climate-stress-change score now uses one fixed Sabah-wide reference scale for all SSPs and time periods.",
            "It can therefore be compared between scenarios and climatologies. It is a relative climate-stress score, not a percentage yield loss."
          )
        ),
        br(),
        downloadButton(
          "download_oil_palm_csv",
          "Download oil palm screening CSV"
        )
      )
    ),
    
    nav_panel(
      title = "Restoration",
      card(
        card_header("Forest Restoration Planning"),
        uiOutput("restoration_results_ui")
      )
    ),

    nav_panel(
      title = "Developer Test",
      
      card(
        card_header(
          "Temporary AOI test"
        ),
        
        textOutput(
          "test_active_aoi_status"
        ),
        
        verbatimTextOutput(
          "aoi_test_selection"
        ),
        
        actionButton(
          inputId = "run_aoi_test",
          label = "Run test",
          class = "btn-primary"
        ),
        
        br(),
        br(),
        
        DTOutput(
          "aoi_test_results"
        )
      )
    ),
    
    nav_panel(
      title = "Data availability",
      
      card(
        card_header(
          "Available raster datasets"
        ),
        
        div(
          class = "results-note",
          p(
            "This table shows raster layers listed in the prototype catalogue. ",
            "Use the search box or column filters to find a variable, scenario or period. ",
            "The File exists and Enabled columns show whether each layer is present and available for use in the prototype."
          ),
          p(
            "Some layers, such as WBGT, are monthly climatology summaries. ",
            "Interpret each layer according to its variable definition, units and time period."
          )
        ),
        
        br(),
        
        DTOutput(
          "catalogue_table"
        )
      )
    ),
    
    nav_panel(
      title = "About",
      
      card(
        card_header(
          "About this prototype"
        ),
        
        p(
          "This is a prototype for AOI-based climate risk screening."
        ),
        
        p(
          "Results are screening summaries and should not be treated as a complete local assessment."
        ),
        
        p(
          "No combined risk score is produced."
        ),
        
        p(
          "Drawn polygons and point-buffer AOIs are intended for exploratory testing."
        ),
        
        p(
          "For formal reporting, use a checked boundary from a verified spatial file."
        )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(
    input,
    output,
    session
) {
  
  # ----------------------------------------------------------
  # REACTIVE VALUES
  # ----------------------------------------------------------
  
  rv <- reactiveValues(
    aoi = NULL,
    aoi_name = NULL,
    result = NULL,
    comparison_results = NULL,
    comparison_missing = NULL,
    cropped_raster = NULL,
    oil_palm_result = NULL,
    oil_palm_comparison = NULL,
    oil_palm_optional_results = NULL,
    oil_palm_score_raster = NULL,
    oil_palm_agreement_raster = NULL,
    oil_palm_coastal_result = NULL,
    oil_palm_coastal_record = NULL,
    oil_palm_coastal_raster = NULL
  )
  
  # ----------------------------------------------------------
  # CLEAR HELPERS
  # ----------------------------------------------------------
  
  clear_analysis_outputs <- function() {
    
    rv$result <- NULL
    rv$comparison_results <- NULL
    rv$comparison_missing <- NULL
    rv$cropped_raster <- NULL
    rv$oil_palm_result <- NULL
    rv$oil_palm_comparison <- NULL
    rv$oil_palm_optional_results <- NULL
    rv$oil_palm_score_raster <- NULL
    rv$oil_palm_agreement_raster <- NULL
    rv$oil_palm_coastal_result <- NULL
    rv$oil_palm_coastal_record <- NULL
    rv$oil_palm_coastal_raster <- NULL
  }
  
  clear_analysis_map <- function() {
    
    leafletProxy("map") |>
      clearGroup("Analysis result") |>
      clearGroup("Oil palm stress") |>
      clearGroup("Oil palm coastal exposure") |>
      removeControl(
        layerId = "analysis_result_legend"
      ) |>
      removeControl(
        layerId = "oil_palm_legend"
      ) |>
      removeControl(
        layerId = "oil_palm_coastal_legend"
      )
  }
  
  # ----------------------------------------------------------
  # LOAD ACTIVE AOI HELPER
  # ----------------------------------------------------------
  
  load_active_aoi <- function(
    aoi_object,
    aoi_name,
    clear_drawn_layer = FALSE
  ) {
    
    rv$aoi <- aoi_object
    rv$aoi_name <- aoi_name
    
    clear_analysis_outputs()
    
    if (isTRUE(clear_drawn_layer)) {
      leafletProxy("map") |>
        clearGroup("Drawn AOI")
    }
    
    clear_analysis_map()
    
    showNotification(
      paste(
        "AOI loaded:",
        rv$aoi_name
      ),
      type = "message"
    )
  }
  
  # ----------------------------------------------------------
  # CLEAR RASTER MAP WHEN CROPPED OUTPUT IS TURNED OFF
  # ----------------------------------------------------------
  # If a user has previously displayed a cropped raster and then
  # unticks the cropped-raster option, remove the raster layer and
  # legend immediately while keeping the AOI outline on the map.
  # ----------------------------------------------------------
  
  observeEvent(
    input$create_cropped_raster,
    {
      if (!isTRUE(input$create_cropped_raster)) {
        rv$cropped_raster <- NULL
        clear_analysis_map()
      }
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # ACTIVE AOI STATUS
  # ----------------------------------------------------------
  
  output$active_aoi_status <- renderText(
    {
      if (is.null(rv$aoi)) {
        return(
          paste(
            "No AOI currently loaded.",
            "Choose Upload polygon, Draw polygon,",
            "Point-buffer, or Jambongan before running analysis."
          )
        )
      }
      
      geometry_type <- unique(
        as.character(
          sf::st_geometry_type(
            rv$aoi
          )
        )
      )
      
      paste(
        "Active AOI:",
        rv$aoi_name,
        "| Features:",
        nrow(rv$aoi),
        "| Geometry:",
        paste(
          geometry_type,
          collapse = ", "
        )
      )
    }
  )
  
  output$test_active_aoi_status <- renderText(
    {
      if (is.null(rv$aoi)) {
        return(
          "No AOI currently loaded."
        )
      }
      
      geometry_type <- unique(
        as.character(
          sf::st_geometry_type(rv$aoi)
        )
      )
      
      paste(
        "Active AOI:",
        rv$aoi_name,
        "| Features:",
        nrow(rv$aoi),
        "| Geometry:",
        paste(
          geometry_type,
          collapse = ", "
        )
      )
    }
  )
  
  output$aoi_test_selection <- renderText(
    {
      if (is.null(rv$aoi)) {
        return("No AOI currently loaded.")
      }
      
      paste(
        paste("AOI:", rv$aoi_name),
        paste("Variable:", input$variable_id),
        paste("Scenario:", input$scenario),
        paste("Period:", input$period),
        sep = "\n"
      )
    }
  )
  
  # ----------------------------------------------------------
  # SCREENING APPLICATION -> EXISTING PATHWAY BRIDGE
  # ----------------------------------------------------------
  
  observeEvent(
    input$screening_application,
    {
      req(input$screening_application)
      
      if (
        input$screening_application != "oil_palm" &&
        input$screening_application != "restoration" &&
        stringr::str_starts(input$screening_application, "pathway::")
      ) {
        selected_pathway <- stringr::str_remove(
          input$screening_application,
          "^pathway::"
        )
        
        if (selected_pathway %in% pathway_choices) {
          updateSelectInput(
            session,
            "pathway",
            choices = pathway_choices,
            selected = selected_pathway
          )
        }
      }
    },
    ignoreInit = FALSE
  )
  
  # The existing comparison engine reads input$run_comparison.
  # Keep that hidden input synchronised with the clearer analysis-mode control.
  observeEvent(
    input$analysis_mode,
    {
      updateCheckboxInput(
        session,
        "run_comparison",
        value = identical(input$analysis_mode, "compare")
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # OIL PALM CORE / OPTIONAL DIMENSIONS
  # ----------------------------------------------------------

  oil_palm_dimension_status <- reactive({
    oil_palm_dimension_availability(
      raster_catalogue = raster_catalogue,
      config_dir = "config"
    )
  })

  output$oil_palm_dimensions_ui <- renderUI({
    status <- oil_palm_dimension_status()

    optional <- status |>
      dplyr::filter(!.data$contributes_to_core_score)

    usable <- optional |>
      dplyr::filter(.data$status %in% c("ready", "provisional"))

    unavailable <- optional |>
      dplyr::filter(.data$status == "missing")

    usable_choices <- stats::setNames(
      usable$dimension_id,
      ifelse(
        usable$status == "provisional",
        paste0(usable$dimension_name, " (provisional)"),
        usable$dimension_name
      )
    )

    ui_parts <- list(
      h4("4. Select dimensions"),
      div(
        class = "results-note",
        tags$strong("Crop water stress (core - always included)"),
        div(
          class = "text-muted",
          "PPETmin + consecutive P:PET < 1 months + CDD. This remains the core Oil Palm climate-stress score."
        )
      )
    )

    if (length(usable_choices) > 0) {
      ui_parts <- c(
        ui_parts,
        list(
          checkboxGroupInput(
            inputId = "oil_palm_optional_dimensions",
            label = "Additional dimensions",
            choices = usable_choices,
            selected = character(0)
          )
        )
      )
    }

    if (nrow(unavailable) > 0) {
      ui_parts <- c(
        ui_parts,
        list(
          tags$strong("Not yet available in the raster catalogue"),
          tags$ul(
            lapply(
              seq_len(nrow(unavailable)),
              function(i) {
                tags$li(
                  paste0(
                    unavailable$dimension_name[[i]],
                    " - ",
                    unavailable$status_note[[i]]
                  )
                )
              }
            )
          )
        )
      )
    }

    ui_parts <- c(
      ui_parts,
      list(
        helpText(
          paste(
            "Additional dimensions are reported separately.",
            "They do not change the core crop-water-stress score."
          )
        ),
        div(
          class = "results-note",
          tags$strong("Peat plantations: "),
          paste(
            "Fire is not included in the general Oil Palm screen.",
            "Peat-related drying, hydrology and fire should be assessed as a separate",
            "conditional site-specific consideration when peat context is available."
          )
        )
      )
    )

    do.call(tagList, ui_parts)
  })

  oil_palm_selected_optional_dimensions <- reactive({
    selected <- input$oil_palm_optional_dimensions
    if (is.null(selected)) character(0) else selected
  })

  # ----------------------------------------------------------
  # OIL PALM AVAILABLE SCENARIOS / PERIODS
  # ----------------------------------------------------------
  
  oil_palm_combinations <- reactive({
    oil_palm_available_combinations(raster_catalogue)
  })
  
  observe({
    combinations <- oil_palm_combinations()
    
    if (nrow(combinations) == 0) {
      updateSelectInput(session, "oil_palm_scenario", choices = character(0))
      updateSelectInput(session, "oil_palm_period", choices = character(0))
      updateSelectInput(session, "oil_palm_comparison_scenarios", choices = character(0))
      updateSelectInput(session, "oil_palm_comparison_periods", choices = character(0))
      return()
    }
    
    scenarios <- unique(combinations$scenario)
    scenario_choices <- stats::setNames(
      scenarios,
      vapply(scenarios, get_scenario_label, character(1))
    )
    
    selected_scenario <- if ("ssp370" %in% scenarios) {
      "ssp370"
    } else if ("ssp245" %in% scenarios) {
      "ssp245"
    } else {
      scenarios[1]
    }
    
    updateSelectInput(
      session,
      "oil_palm_scenario",
      choices = scenario_choices,
      selected = selected_scenario
    )
    
    updateSelectInput(
      session,
      "oil_palm_comparison_scenarios",
      choices = scenario_choices,
      selected = scenarios
    )
    
    periods <- unique(combinations$period)
    period_choices <- stats::setNames(
      periods,
      vapply(periods, get_period_label, character(1))
    )
    
    default_periods <- intersect(
      c("2011-2040", "2041-2070", "2071-2100"),
      periods
    )
    if (length(default_periods) == 0) default_periods <- periods
    
    updateSelectInput(
      session,
      "oil_palm_comparison_periods",
      choices = period_choices,
      selected = default_periods
    )
  })
  
  observeEvent(
    input$oil_palm_scenario,
    {
      req(input$oil_palm_scenario)
      
      combinations <- oil_palm_combinations() |>
        dplyr::filter(scenario == input$oil_palm_scenario)
      
      periods <- unique(combinations$period)
      period_choices <- stats::setNames(
        periods,
        vapply(periods, get_period_label, character(1))
      )
      
      selected_period <- if ("2041-2070" %in% periods) {
        "2041-2070"
      } else {
        periods[1]
      }
      
      updateSelectInput(
        session,
        "oil_palm_period",
        choices = period_choices,
        selected = selected_period
      )
    },
    ignoreInit = FALSE
  )
  
  oil_palm_ready <- reactive({
    if (is.null(rv$aoi)) return(FALSE)
    
    mode <- input$oil_palm_analysis_mode
    if (is.null(mode) || !nzchar(mode)) return(FALSE)
    
    if (mode == "single") {
      if (
        is.null(input$oil_palm_scenario) ||
        !nzchar(input$oil_palm_scenario) ||
        is.null(input$oil_palm_period) ||
        !nzchar(input$oil_palm_period)
      ) return(FALSE)
      
      matching <- oil_palm_combinations() |>
        dplyr::filter(
          scenario == input$oil_palm_scenario,
          period == input$oil_palm_period
        )
      
      return(nrow(matching) > 0)
    }
    
    selected_scenarios <- input$oil_palm_comparison_scenarios
    selected_periods <- input$oil_palm_comparison_periods
    
    if (
      is.null(selected_scenarios) || length(selected_scenarios) == 0 ||
      is.null(selected_periods) || length(selected_periods) == 0
    ) return(FALSE)
    
    matching <- oil_palm_combinations() |>
      dplyr::filter(
        scenario %in% selected_scenarios,
        period %in% selected_periods
      )
    
    nrow(matching) > 0
  })
  
  observe({
    shinyjs::toggleState(
      id = "run_oil_palm",
      condition = isTRUE(oil_palm_ready())
    )
  })
  
  output$oil_palm_selection_status <- renderUI({
    ready <- isTRUE(oil_palm_ready())
    mode <- input$oil_palm_analysis_mode
    
    status_text <- if (ready) {
      if (identical(mode, "compare")) {
        "Ready to compare Oil Palm scenarios and time periods"
      } else {
        "Ready to run Oil Palm screening"
      }
    } else if (is.null(rv$aoi)) {
      "Load or create an AOI first"
    } else if (nrow(oil_palm_combinations()) == 0) {
      "The three required Oil Palm climate layers are not available together in the raster catalogue"
    } else if (identical(mode, "compare")) {
      "Select at least one available SSP and time period"
    } else {
      "Choose an available future scenario and time period"
    }
    
    div(
      class = paste(
        "selection-status-box",
        if (ready) "selection-status-ready" else "selection-status-incomplete"
      ),
      strong(status_text)
    )
  })
  

  # ----------------------------------------------------------
  # OIL PALM - CONDITIONAL COASTAL INUNDATION LINKED SCREENING
  # ----------------------------------------------------------
  # This is intentionally separate from the Oil Palm core and
  # optional-dimension scores.
  # ----------------------------------------------------------

  oil_palm_coastal_registered_records <- reactive({
    oil_palm_coastal_catalogue(
      raster_catalogue = raster_catalogue,
      variable_metadata = variable_metadata
    )
  })

  oil_palm_coastal_overlap_records <- reactive({
    if (is.null(rv$aoi)) {
      return(
        oil_palm_coastal_registered_records()[0, , drop = FALSE]
      )
    }

    oil_palm_coastal_records_for_aoi(
      raster_catalogue = raster_catalogue,
      variable_metadata = variable_metadata,
      aoi_sf = rv$aoi
    )
  })

  output$oil_palm_coastal_linked_ui <- renderUI({
    registered <- oil_palm_coastal_registered_records()

    if (nrow(registered) == 0) {
      return(
        tagList(
          hr(),
          h5("Site-specific linked screening"),
          div(
            class = "selection-status-box selection-status-incomplete",
            strong("Coastal inundation / sea-level exposure"),
            div(
              class = "selection-status-detail",
              paste(
                "No enabled coastal inundation layer is currently registered.",
                "When a binary SLR/coastal-inundation layer is added to the raster catalogue,",
                "this option will become available automatically."
              )
            )
          )
        )
      )
    }

    if (is.null(rv$aoi)) {
      return(
        tagList(
          hr(),
          h5("Site-specific linked screening"),
          helpText(
            paste(
              "Coastal inundation / sea-level exposure is available where the AOI overlaps",
              "a registered coastal inundation layer. Load or draw the AOI first."
            )
          )
        )
      )
    }

    records <- oil_palm_coastal_overlap_records()

    if (nrow(records) == 0) {
      return(
        tagList(
          hr(),
          h5("Site-specific linked screening"),
          div(
            class = "selection-status-box selection-status-ready",
            strong("No registered coastal-inundation coverage overlaps this AOI"),
            div(
              class = "selection-status-detail",
              paste(
                "The coastal option is therefore not shown for this site.",
                "This does not imply zero coastal risk outside the coverage of the registered layers."
              )
            )
          )
        )
      )
    }

    include_coastal <- isTRUE(
      input$oil_palm_include_coastal
    )

    variable_ids <- unique(records$variable_id)

    variable_labels <- vapply(
      variable_ids,
      function(x) {
        label <- records$coastal_display_name[
          records$variable_id == x
        ][1]

        if (
          is.na(label) ||
          !nzchar(label)
        ) {
          label <- oil_palm_coastal_display_name(x)
        }

        label
      },
      character(1)
    )

    variable_choices <- stats::setNames(
      variable_ids,
      variable_labels
    )

    selected_variable <- input$oil_palm_coastal_variable

    if (
      is.null(selected_variable) ||
      !selected_variable %in% variable_ids
    ) {
      selected_variable <- variable_ids[1]
    }

    variable_records <- records |>
      dplyr::filter(
        .data$variable_id == selected_variable
      )

    scenarios <- unique(
      variable_records$scenario
    )

    selected_scenario <- input$oil_palm_coastal_scenario

    if (
      is.null(selected_scenario) ||
      !selected_scenario %in% scenarios
    ) {
      selected_scenario <- scenarios[1]
    }

    scenario_choices <- stats::setNames(
      scenarios,
      vapply(
        scenarios,
        get_scenario_label,
        character(1)
      )
    )

    period_records <- variable_records |>
      dplyr::filter(
        .data$scenario == selected_scenario
      )

    periods <- unique(
      period_records$period
    )

    selected_period <- input$oil_palm_coastal_period

    if (
      is.null(selected_period) ||
      !selected_period %in% periods
    ) {
      selected_period <- periods[1]
    }

    period_choices <- stats::setNames(
      periods,
      vapply(
        periods,
        get_period_label,
        character(1)
      )
    )

    tagList(
      hr(),
      h5("Site-specific linked screening"),
      checkboxInput(
        inputId = "oil_palm_include_coastal",
        label = "Coastal inundation / sea-level exposure",
        value = include_coastal
      ),
      helpText(
        paste(
          "This is a separate exposure screening.",
          "It does not alter the Oil Palm crop-water-stress score",
          "or any selected optional-dimension score."
        )
      ),
      if (include_coastal) {
        tagList(
          selectInput(
            inputId = "oil_palm_coastal_variable",
            label = "Exposure layer",
            choices = variable_choices,
            selected = selected_variable
          ),
          selectInput(
            inputId = "oil_palm_coastal_scenario",
            label = "Coastal dataset scenario",
            choices = scenario_choices,
            selected = selected_scenario
          ),
          selectInput(
            inputId = "oil_palm_coastal_period",
            label = "Coastal dataset time horizon",
            choices = period_choices,
            selected = selected_period
          ),
          helpText(
            paste(
              "These controls intentionally remain separate from the Oil Palm SSP/climatology controls",
              "because coastal datasets may use different scenarios and time horizons."
            )
          ),
          actionButton(
            inputId = "run_oil_palm_coastal",
            label = "Run coastal exposure screening",
            class = "btn-info",
            width = "100%"
          )
        )
      }
    )
  })

  observeEvent(
    input$oil_palm_include_coastal,
    {
      if (!isTRUE(input$oil_palm_include_coastal)) {
        rv$oil_palm_coastal_result <- NULL
        rv$oil_palm_coastal_record <- NULL
        rv$oil_palm_coastal_raster <- NULL

        leafletProxy("map") |>
          clearGroup("Oil palm coastal exposure") |>
          removeControl(
            layerId = "oil_palm_coastal_legend"
          )
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$run_oil_palm_coastal,
    {
      req(
        rv$aoi,
        isTRUE(input$oil_palm_include_coastal),
        input$oil_palm_coastal_variable,
        input$oil_palm_coastal_scenario,
        input$oil_palm_coastal_period
      )

      records <- oil_palm_coastal_overlap_records() |>
        dplyr::filter(
          .data$variable_id ==
            input$oil_palm_coastal_variable,
          .data$scenario ==
            input$oil_palm_coastal_scenario,
          .data$period ==
            input$oil_palm_coastal_period
        )

      if (nrow(records) == 0) {
        showNotification(
          "The selected coastal exposure layer is no longer available for this AOI.",
          type = "error",
          duration = NULL
        )
        return()
      }

      record <- records[1, , drop = FALSE]

      result <- tryCatch(
        {
          withProgress(
            message = "Running coastal exposure screening",
            value = 0,
            {
              incProgress(
                0.25,
                detail = "Checking AOI and inundation layer"
              )

              x <- oil_palm_run_coastal_exposure(
                raster_record = record,
                aoi_sf = rv$aoi,
                aoi_name = rv$aoi_name,
                output_dir = file.path(
                  "outputs",
                  "oil_palm",
                  "coastal_exposure"
                ),
                write_cropped_raster = TRUE
              )

              incProgress(
                1,
                detail = "Complete"
              )

              x
            }
          )
        },
        error = function(error) {
          showNotification(
            paste(
              "Coastal exposure screening failed:",
              error$message
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

      rv$oil_palm_coastal_result <- result
      rv$oil_palm_coastal_record <- record
      rv$oil_palm_coastal_raster <- NULL

      summary_row <- result$summary[1, ]

      cropped_path <- summary_row$cropped_raster[[1]]

      if (
        !is.na(cropped_path) &&
        nzchar(cropped_path) &&
        file.exists(cropped_path)
      ) {
        coastal_raster <- terra::rast(
          cropped_path
        )

        rv$oil_palm_coastal_raster <- coastal_raster

        coastal_palette <- leaflet::colorFactor(
          palette = c(
            "#FFFFFF00",
            "#2C7FB8"
          ),
          domain = c(
            0,
            1
          ),
          na.color = "#FFFFFF00"
        )

        leafletProxy("map") |>
          clearGroup("Oil palm coastal exposure") |>
          removeControl(
            layerId = "oil_palm_coastal_legend"
          ) |>
          addRasterImage(
            x = coastal_raster,
            colors = coastal_palette,
            opacity = 0.75,
            group = "Oil palm coastal exposure",
            project = TRUE,
            method = "ngb",
            maxBytes = 10 * 1024 * 1024
          ) |>
          addLegend(
            colors = "#2C7FB8",
            labels = "Potential inundation exposure",
            title = "Oil Palm: coastal exposure",
            position = "bottomright",
            opacity = 1,
            layerId = "oil_palm_coastal_legend"
          )
      }

      showNotification(
        paste(
          "Coastal exposure screening completed:",
          round(
            summary_row$pct_AOI_exposed[[1]],
            1
          ),
          "% of present-day land flagged as exposed."
        ),
        type = "message"
      )
    }
  )

  output$oil_palm_coastal_results_ui <- renderUI({
    if (is.null(rv$oil_palm_coastal_result)) {
      return(NULL)
    }

    s <- rv$oil_palm_coastal_result$summary[1, ]

    scenario_label <- get_scenario_label(
      s$scenario[[1]]
    )

    period_label <- get_period_label(
      s$period[[1]]
    )

    tags$div(
      id = "oil_palm_coastal_linked_report",
      class = "oil-palm-coastal-report-section",
      hr(),
      h4("Site-specific linked screening"),
      div(
        class = "oil-palm-summary-card",
        div(
          class = "oil-palm-summary-big",
          "Coastal inundation / sea-level exposure"
        ),
        div(
          paste(
            s$layer_name[[1]],
            "|",
            scenario_label,
            "|",
            period_label
          )
        )
      ),
      tags$table(
        class = "table table-sm table-bordered report-table oil-palm-coastal-table",
        tags$thead(
          tags$tr(
            tags$th("Measure"),
            tags$th("Result")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Potentially inundated present-day land"),
            tags$td(
              paste0(
                format(
                  round(
                    s$exposed_area_ha[[1]],
                    2
                  ),
                  big.mark = ","
                ),
                " ha"
              )
            )
          ),
          tags$tr(
            tags$td("Share of present-day land exposed"),
            tags$td(
              paste0(
                round(
                  s$pct_AOI_exposed[[1]],
                  1
                ),
                "%"
              )
            )
          ),
          tags$tr(
            tags$td("Raster coverage of present-day land"),
            tags$td(
              paste0(
                round(
                  s$raster_coverage_pct[[1]],
                  1
                ),
                "%"
              )
            )
          ),
          tags$tr(
            tags$td("Share of raster-covered land exposed"),
            tags$td(
              paste0(
                round(
                  s$pct_covered_area_exposed[[1]],
                  1
                ),
                "%"
              )
            )
          )
        )
      ),
      div(
        class = "results-note",
        paste(
          "This is a site-specific exposure result and is reported separately.",
          "It is not included in the Oil Palm crop-water-stress score.",
          "Interpretation depends on the assumptions, scenario and time horizon of the selected inundation dataset."
        )
      ),
      br(),
      downloadButton(
        outputId = "download_oil_palm_coastal_csv",
        label = "Download coastal exposure CSV"
      )
    )
  })

  output$download_oil_palm_coastal_csv <- downloadHandler(
    filename = function() {
      paste0(
        "oil_palm_coastal_exposure_",
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
      req(rv$oil_palm_coastal_result)

      readr::write_csv(
        rv$oil_palm_coastal_result$summary,
        file
      )
    }
  )


  # ----------------------------------------------------------
  # OIL PALM DOA PEAT / FIRE LINKED SCREENING
  # ----------------------------------------------------------

  oil_palm_peat_fire_module <- oil_palm_peat_fire_server(
    input = input,
    output = output,
    session = session,
    rv = rv,
    raster_catalogue = raster_catalogue,
    get_scenario_label = get_scenario_label,
    get_period_label = get_period_label,
    safe_filename = safe_filename
  )


  # ----------------------------------------------------------
  # FOREST RESTORATION CLIMATE SCREENING
  # ----------------------------------------------------------

  restoration_screening_module <- restoration_screening_server(
    input = input,
    output = output,
    session = session,
    rv = rv,
    raster_catalogue = raster_catalogue,
    get_scenario_label = get_scenario_label,
    get_period_label = get_period_label,
    safe_filename = safe_filename
  )
  # ----------------------------------------------------------
  # ENABLE / DISABLE RUN ANALYSIS BUTTON
  # ----------------------------------------------------------
  
  observe(
    {
      selection_complete <-
        !is.null(rv$aoi) &&
        !is.null(input$variable_id) &&
        nzchar(input$variable_id) &&
        !is.null(input$scenario) &&
        nzchar(input$scenario) &&
        !is.null(input$period) &&
        nzchar(input$period)
      
      raster_available <- FALSE
      
      if (selection_complete) {
        
        matched_rasters <- raster_catalogue |>
          dplyr::filter(
            enabled,
            variable_id == input$variable_id,
            scenario == input$scenario,
            period == input$period
          )
        
        raster_available <-
          nrow(matched_rasters) > 0 &&
          any(
            file.exists(
              matched_rasters$file_path
            )
          )
      }
      
      shinyjs::toggleState(
        id = "run_analysis",
        condition =
          selection_complete &&
          raster_available
      )
    }
  )
  
  # ----------------------------------------------------------
  # LOAD UPLOADED AOI
  # ----------------------------------------------------------
  
  observeEvent(
    input$aoi_file,
    {
      req(input$aoi_file)
      
      uploaded_names <- input$aoi_file$name
      uploaded_paths <- input$aoi_file$datapath
      
      upload_dir <- tempfile("uploaded_aoi_")
      
      dir.create(
        upload_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      copied_ok <- file.copy(
        from = uploaded_paths,
        to = file.path(
          upload_dir,
          uploaded_names
        ),
        overwrite = TRUE
      )
      
      if (!all(copied_ok)) {
        showNotification(
          "One or more uploaded AOI files could not be copied.",
          type = "error",
          duration = NULL
        )
        
        return()
      }
      
      spatial_file_index <- which(
        grepl(
          "\\.(gpkg|geojson|json|kml|shp)$",
          uploaded_names,
          ignore.case = TRUE
        )
      )
      
      if (length(spatial_file_index) == 0) {
        showNotification(
          paste(
            "No readable spatial file was found.",
            "Upload a GeoPackage, GeoJSON, KML,",
            "or a complete shapefile."
          ),
          type = "error",
          duration = NULL
        )
        
        return()
      }
      
      supported_names <- uploaded_names[
        spatial_file_index
      ]
      
      gpkg_position <- which(
        tolower(
          tools::file_ext(supported_names)
        ) == "gpkg"
      )
      
      if (length(gpkg_position) > 0) {
        selected_index <- spatial_file_index[
          gpkg_position[1]
        ]
      } else {
        selected_index <- spatial_file_index[1]
      }
      
      uploaded_name <- uploaded_names[
        selected_index
      ]
      
      spatial_path <- file.path(
        upload_dir,
        uploaded_name
      )
      
      file_extension <- tolower(
        tools::file_ext(spatial_path)
      )
      
      uploaded_aoi <- tryCatch(
        {
          if (file_extension == "gpkg") {
            
            gpkg_layers <- sf::st_layers(
              spatial_path
            )
            
            if (length(gpkg_layers$name) == 0) {
              stop(
                "The uploaded GeoPackage contains no readable layers."
              )
            }
            
            polygon_layer_index <- which(
              grepl(
                "POLYGON",
                toupper(
                  as.character(
                    gpkg_layers$geomtype
                  )
                )
              )
            )
            
            if (length(polygon_layer_index) == 0) {
              stop(
                paste(
                  "The uploaded GeoPackage contains no polygon layer.",
                  "Available layers:",
                  paste(
                    gpkg_layers$name,
                    collapse = ", "
                  )
                )
              )
            }
            
            selected_layer <- gpkg_layers$name[
              polygon_layer_index[1]
            ]
            
            uploaded_aoi <- sf::st_read(
              dsn = spatial_path,
              layer = selected_layer,
              quiet = TRUE
            )
            
          } else {
            
            uploaded_aoi <- sf::st_read(
              dsn = spatial_path,
              quiet = TRUE
            )
          }
          
          if (nrow(uploaded_aoi) == 0) {
            stop(
              "The uploaded file contains no features."
            )
          }
          
          if (is.na(sf::st_crs(uploaded_aoi))) {
            stop(
              "The uploaded file has no CRS."
            )
          }
          
          uploaded_aoi <- sf::st_make_valid(
            uploaded_aoi
          )
          
          uploaded_aoi <- prepare_aoi(
            uploaded_aoi
          )
          
          uploaded_aoi
        },
        error = function(e) {
          
          showNotification(
            paste(
              "AOI upload failed:",
              conditionMessage(e)
            ),
            type = "error",
            duration = NULL
          )
          
          NULL
        }
      )
      
      if (is.null(uploaded_aoi)) {
        return()
      }
      
      load_active_aoi(
        aoi_object = uploaded_aoi,
        aoi_name = tools::file_path_sans_ext(
          uploaded_name
        ),
        clear_drawn_layer = TRUE
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # LOAD DRAWN POLYGON AOI FROM MAP
  # ----------------------------------------------------------
  # User selects "Draw polygon", draws a polygon on the map,
  # and the drawn polygon is stored as rv$aoi.
  # ----------------------------------------------------------
  
  observeEvent(
    input$map_draw_new_feature,
    {
      
      req(
        input$aoi_method == "draw",
        input$map_draw_new_feature
      )
      
      drawn_feature <- input$map_draw_new_feature
      
      if (
        is.null(drawn_feature$geometry) ||
        drawn_feature$geometry$type != "Polygon"
      ) {
        showNotification(
          "Only polygon drawing is supported for AOI selection.",
          type = "error",
          duration = 8
        )
        
        return()
      }
      
      # Clear old AOI and analysis layers before loading the new drawn AOI.
      # Do not clear "Drawn AOI" here; the newly drawn feature is still held
      # by the leaflet draw layer and the active AOI display observer will
      # show the final AOI outline through the normal "AOI" group.
      leafletProxy("map") |>
        clearGroup("AOI") |>
        clearGroup("Analysis result") |>
        removeControl(
          layerId = "analysis_result_legend"
        )
      
      coordinates <- drawn_feature$geometry$coordinates[[1]]
      
      coordinate_matrix <- do.call(
        rbind,
        lapply(
          coordinates,
          function(x) {
            c(
              x[[1]],
              x[[2]]
            )
          }
        )
      )
      
      drawn_polygon <- sf::st_polygon(
        list(
          coordinate_matrix
        )
      )
      
      drawn_aoi <- sf::st_sf(
        aoi_name = "Drawn_AOI",
        geometry = sf::st_sfc(
          drawn_polygon,
          crs = 4326
        )
      )
      
      drawn_aoi <- sf::st_make_valid(
        drawn_aoi
      )
      
      drawn_aoi <- prepare_aoi(
        drawn_aoi
      )
      
      load_active_aoi(
        aoi_object = drawn_aoi,
        aoi_name = paste0(
          "Drawn_AOI_",
          format(
            Sys.time(),
            "%Y%m%d_%H%M%S"
          )
        ),
        clear_drawn_layer = FALSE
      )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # CLEAR DRAWN AOI
  # ----------------------------------------------------------
  # Clears the drawn AOI layer.
  # If the active AOI is a drawn AOI, it also clears the active AOI.
  # This avoids accidentally deleting uploaded, Jambongan, or point-buffer AOIs.
  # ----------------------------------------------------------
  
  observeEvent(
    input$clear_drawn_aoi,
    {
      
      leafletProxy("map") |>
        clearGroup("Drawn AOI") |>
        clearGroup("Analysis result") |>
        removeControl(
          layerId = "analysis_result_legend"
        )
      
      if (
        !is.null(rv$aoi_name) &&
        stringr::str_detect(
          rv$aoi_name,
          "^Drawn_AOI"
        )
      ) {
        
        rv$aoi <- NULL
        rv$aoi_name <- NULL
        
        clear_analysis_outputs()
        
        leafletProxy("map") |>
          clearGroup("AOI")
        
        showNotification(
          "Drawn AOI cleared.",
          type = "message"
        )
        
      } else {
        
        showNotification(
          "Drawn AOI layer cleared. The active AOI was not changed because it was not a drawn AOI.",
          type = "message"
        )
      }
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # LOAD TEMPORARY JAMBONGAN AOI
  # ----------------------------------------------------------
  
  observeEvent(
    input$use_jambongan,
    {
      jambongan_path <- file.path(
        "data",
        "examples",
        "Jambongan.gpkg"
      )
      
      validate(
        need(
          file.exists(jambongan_path),
          paste(
            "Jambongan test AOI was not found:",
            jambongan_path
          )
        )
      )
      
      jambongan_aoi <- sf::st_read(
        jambongan_path,
        quiet = TRUE
      )
      
      validate(
        need(
          nrow(jambongan_aoi) > 0,
          "The Jambongan GeoPackage contains no spatial features."
        )
      )
      
      load_active_aoi(
        aoi_object = prepare_aoi(
          jambongan_aoi
        ),
        aoi_name = "Jambongan",
        clear_drawn_layer = TRUE
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # LOAD POINT-AND-BUFFER AOI FROM MAP CLICK
  # ----------------------------------------------------------
  # This provides a simple working AOI option for testing.
  # User selects "Select point and buffer", enters a buffer distance,
  # and clicks the map. The clicked point is buffered and stored as rv$aoi.
  # ----------------------------------------------------------
  
  observeEvent(
    input$map_click,
    {
      req(
        input$aoi_method == "point",
        input$map_click,
        input$buffer_km
      )
      
      if (
        is.na(input$buffer_km) ||
        input$buffer_km <= 0
      ) {
        showNotification(
          "Enter a buffer distance greater than 0 km before clicking the map.",
          type = "error",
          duration = 8
        )
        
        return()
      }
      
      clicked_lng <- input$map_click$lng
      clicked_lat <- input$map_click$lat
      
      clicked_point <- sf::st_as_sf(
        data.frame(
          id = 1,
          longitude = clicked_lng,
          latitude = clicked_lat
        ),
        coords = c(
          "longitude",
          "latitude"
        ),
        crs = 4326
      )
      
      buffer_metres <- input$buffer_km * 1000
      
      buffered_aoi <- clicked_point |>
        sf::st_transform(
          3857
        ) |>
        sf::st_buffer(
          dist = buffer_metres
        ) |>
        sf::st_transform(
          4326
        ) |>
        sf::st_make_valid()
      
      buffered_aoi <- prepare_aoi(
        buffered_aoi
      )
      
      load_active_aoi(
        aoi_object = buffered_aoi,
        aoi_name = paste0(
          "Point_buffer_",
          input$buffer_km,
          "km_",
          format(
            Sys.time(),
            "%Y%m%d_%H%M%S"
          )
        ),
        clear_drawn_layer = TRUE
      )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # CLEAR POINT-BUFFER AOI
  # ----------------------------------------------------------
  # Clears the active AOI only if it was created using
  # point-and-buffer mode.
  # This avoids accidentally deleting or hiding uploaded,
  # drawn, or Jambongan AOIs.
  # ----------------------------------------------------------
  
  observeEvent(
    input$clear_point_buffer,
    {
      if (
        !is.null(rv$aoi_name) &&
        stringr::str_detect(
          rv$aoi_name,
          "^Point_buffer"
        )
      ) {
        
        leafletProxy("map") |>
          clearGroup("AOI") |>
          clearGroup("Analysis result") |>
          removeControl(
            layerId = "analysis_result_legend"
          )
        
        rv$aoi <- NULL
        rv$aoi_name <- NULL
        
        clear_analysis_outputs()
        
        showNotification(
          "Point buffer AOI cleared.",
          type = "message"
        )
        
      } else {
        
        showNotification(
          "The active AOI was not changed because it is not a point-buffer AOI.",
          type = "message"
        )
      }
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # INITIAL MAP
  # ----------------------------------------------------------
  
  output$map <- renderLeaflet(
    {
      leaflet(
        options = leafletOptions(
          minZoom = 6,
          maxZoom = 18
        )
      ) |>
        addProviderTiles(
          providers$OpenStreetMap,
          group = "OpenStreetMap"
        ) |>
        setView(
          lng = 117.0,
          lat = 5.3,
          zoom = 7
        ) |>
        addScaleBar(
          position = "bottomleft",
          options = scaleBarOptions(
            metric = TRUE,
            imperial = FALSE
          )
        ) |>
        addLayersControl(
          baseGroups = c(
            "OpenStreetMap"
          ),
          overlayGroups = c(
            "AOI",
            "Analysis result",
            "Drawn AOI"
          ),
          options = layersControlOptions(
            collapsed = TRUE
          )
        ) |>
        addDrawToolbar(
          targetGroup = "Drawn AOI",
          
          polygonOptions = drawPolygonOptions(
            shapeOptions = drawShapeOptions(
              color = "#7B2CBF",
              weight = 3,
              fillOpacity = 0.15
            ),
            showArea = TRUE,
            metric = TRUE
          ),
          
          rectangleOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          polylineOptions = FALSE,
          
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        )
    }
  )
  
  # ----------------------------------------------------------
  # DISPLAY ACTIVE AOI ON MAP
  # ----------------------------------------------------------
  
  observeEvent(
    list(
      rv$aoi,
      rv$aoi_name
    ),
    {
      req(
        rv$aoi,
        rv$aoi_name
      )
      
      map_aoi <- sf::st_transform(
        rv$aoi,
        4326
      )
      
      aoi_bbox <- sf::st_bbox(
        map_aoi
      )
      
      leafletProxy("map") |>
        clearGroup("AOI") |>
        addPolygons(
          data = map_aoi,
          group = "AOI",
          color = "#7B2CBF",
          weight = 3,
          fillOpacity = 0.15,
          label = rv$aoi_name
        ) |>
        fitBounds(
          lng1 = aoi_bbox[["xmin"]],
          lat1 = aoi_bbox[["ymin"]],
          lng2 = aoi_bbox[["xmax"]],
          lat2 = aoi_bbox[["ymax"]]
        )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # UPDATE THEMES FROM SELECTED PATHWAY
  # ----------------------------------------------------------
  
  observeEvent(
    input$pathway,
    {
      available_themes <- pathway_themes |>
        dplyr::filter(
          pathway == input$pathway
        ) |>
        dplyr::arrange(
          display_order
        ) |>
        dplyr::distinct(
          theme,
          .keep_all = TRUE
        )
      
      theme_choices <- available_themes$theme
      
      if (length(theme_choices) == 0) {
        theme_choices <- theme_variables |>
          dplyr::distinct(theme) |>
          dplyr::arrange(theme) |>
          dplyr::pull(theme)
      }
      
      default_theme <- available_themes |>
        dplyr::filter(
          default_enabled
        ) |>
        dplyr::pull(theme)
      
      if (length(default_theme) == 0) {
        default_theme <- theme_choices[1]
      }
      
      updateSelectInput(
        session = session,
        inputId = "theme",
        choices = theme_choices,
        selected = default_theme[1]
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # UPDATE VARIABLES FROM SELECTED THEME
  # ----------------------------------------------------------
  
  observeEvent(
    input$theme,
    {
      req(input$theme)
      
      theme_variable_ids <- theme_variables |>
        dplyr::filter(
          theme == input$theme
        ) |>
        dplyr::pull(
          variable_id
        )
      
      enabled_variable_ids <- raster_catalogue |>
        dplyr::filter(
          enabled
        ) |>
        dplyr::distinct(
          variable_id
        ) |>
        dplyr::pull(
          variable_id
        )
      
      available_variable_ids <- intersect(
        theme_variable_ids,
        enabled_variable_ids
      )
      
      # Bio05/Bio017 were originally injected into every theme as
      # prototype variables. Keep that convenience only for the general
      # climate screening pathway; specialist applications should show
      # only variables explicitly assigned to their selected theme.
      if (identical(input$pathway, general_pathway)) {
        pilot_variable_ids <- intersect(
          c("Bio05", "Bio017"),
          enabled_variable_ids
        )
        
        available_variable_ids <- unique(
          c(available_variable_ids, pilot_variable_ids)
        )
      }
      
      # Only fall back to all enabled variables if the theme has no
      # configured variable links at all. If the links exist but rasters
      # are unavailable, leave the list empty so the configuration issue
      # is visible rather than silently showing unrelated variables.
      if (
        length(available_variable_ids) == 0 &&
        length(theme_variable_ids) == 0
      ) {
        available_variable_ids <- enabled_variable_ids
      }
      
      available_variables <- tibble::tibble(
        variable_id = available_variable_ids
      ) |>
        dplyr::left_join(
          variable_metadata |>
            dplyr::select(
              variable_id,
              display_name
            ),
          by = "variable_id"
        ) |>
        dplyr::mutate(
          display_name = dplyr::case_when(
            variable_id == "Bio05" &
              (
                is.na(display_name) |
                  display_name == ""
              ) ~ "Bio05 - Maximum temperature of warmest month",
            variable_id == "Bio017" &
              (
                is.na(display_name) |
                  display_name == ""
              ) ~ "Bio017 - Precipitation of driest quarter",
            variable_id == "WBGTmax" &
              (
                is.na(display_name) |
                  display_name == ""
              ) ~ "Maximum WBGT",
            is.na(display_name) |
              display_name == "" ~ variable_id,
            TRUE ~ display_name
          )
        )
      
      variable_choices <- stats::setNames(
        available_variables$variable_id,
        available_variables$display_name
      )
      
      updateSelectInput(
        session = session,
        inputId = "variable_id",
        choices = variable_choices,
        selected = available_variables$variable_id[1]
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # UPDATE SCENARIOS FROM SELECTED VARIABLE
  # ----------------------------------------------------------
  
  observeEvent(
    input$variable_id,
    {
      req(input$variable_id)
      
      available_scenarios <- raster_catalogue |>
        dplyr::filter(
          enabled,
          variable_id == input$variable_id
        ) |>
        dplyr::distinct(
          scenario
        ) |>
        dplyr::pull(
          scenario
        )
      
      available_scenarios <- available_scenarios[
        available_scenarios %in% names(scenario_labels)
      ]
      
      scenario_choices <- stats::setNames(
        available_scenarios,
        vapply(
          available_scenarios,
          get_scenario_label,
          character(1)
        )
      )
      
      selected_scenario <- if (
        "ssp245" %in% available_scenarios
      ) {
        "ssp245"
      } else if (
        "baseline" %in% available_scenarios
      ) {
        "baseline"
      } else {
        available_scenarios[1]
      }
      
      updateSelectInput(
        session = session,
        inputId = "scenario",
        choices = scenario_choices,
        selected = selected_scenario
      )
      
      updateSelectInput(
        session = session,
        inputId = "comparison_scenarios",
        choices = scenario_choices,
        selected = available_scenarios
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # UPDATE PERIODS FROM SELECTED VARIABLE AND SCENARIO
  # ----------------------------------------------------------
  
  observeEvent(
    list(
      input$variable_id,
      input$scenario
    ),
    {
      req(
        input$variable_id,
        input$scenario
      )
      
      available_periods <- raster_catalogue |>
        dplyr::filter(
          enabled,
          variable_id == input$variable_id,
          scenario == input$scenario
        ) |>
        dplyr::distinct(
          period
        ) |>
        dplyr::pull(
          period
        )
      
      period_choices <- stats::setNames(
        available_periods,
        vapply(
          available_periods,
          get_period_label,
          character(1)
        )
      )
      
      preferred_periods <- if (input$scenario == "baseline") {
        c("1981-2010", "1980-2005")
      } else {
        c(
          "2041-2070",
          "2040-2059",
          "2011-2040",
          "2020-2039",
          "2071-2100",
          "2079-2098"
        )
      }
      
      selected_period <- intersect(
        preferred_periods,
        available_periods
      )[1]
      
      if (length(selected_period) == 0 || is.na(selected_period)) {
        selected_period <- available_periods[1]
      }
      
      updateSelectInput(
        session = session,
        inputId = "period",
        choices = period_choices,
        selected = selected_period
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # UPDATE COMPARISON PERIODS FROM SELECTED VARIABLE
  # ----------------------------------------------------------
  
  observeEvent(
    input$variable_id,
    {
      req(input$variable_id)
      
      available_periods <- raster_catalogue |>
        dplyr::filter(
          enabled,
          variable_id == input$variable_id
        ) |>
        dplyr::distinct(
          period
        ) |>
        dplyr::pull(
          period
        )
      
      period_choices <- stats::setNames(
        available_periods,
        vapply(
          available_periods,
          get_period_label,
          character(1)
        )
      )
      
      # Default to the appropriate baseline plus mid-century period.
      # Terrestrial: 1981-2010 / 2041-2070.
      # Sabah Marine Vision: 1980-2005 / 2040-2059.
      if ("1980-2005" %in% available_periods) {
        preferred_comparison_periods <- c("1980-2005", "2040-2059")
      } else {
        preferred_comparison_periods <- c("1981-2010", "2041-2070")
      }
      
      default_periods <- intersect(
        preferred_comparison_periods,
        available_periods
      )
      
      if (length(default_periods) == 0) {
        default_periods <- available_periods
      }
      
      updateSelectInput(
        session = session,
        inputId = "comparison_periods",
        choices = period_choices,
        selected = default_periods
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # SELECTION STATUS
  # ----------------------------------------------------------
  
  output$selection_status <- renderUI(
    {
      variable_selected <-
        !is.null(input$variable_id) &&
        nzchar(input$variable_id)
      
      scenario_selected <-
        !is.null(input$scenario) &&
        nzchar(input$scenario)
      
      period_selected <-
        !is.null(input$period) &&
        nzchar(input$period)
      
      aoi_selected <- !is.null(rv$aoi)
      
      selection_complete <-
        variable_selected &&
        scenario_selected &&
        period_selected
      
      matching_dataset <- NULL
      
      if (selection_complete) {
        matching_dataset <- raster_catalogue |>
          dplyr::filter(
            enabled,
            variable_id == input$variable_id,
            scenario == input$scenario,
            period == input$period
          )
      }
      
      raster_catalogued <-
        !is.null(matching_dataset) &&
        nrow(matching_dataset) > 0
      
      raster_file_exists <- FALSE
      
      if (raster_catalogued) {
        raster_file_exists <- any(
          file.exists(
            matching_dataset$file_path
          )
        )
      }
      
      ready_to_run <-
        aoi_selected &&
        selection_complete &&
        raster_catalogued &&
        raster_file_exists
      
      status_message <- if (ready_to_run) {
        "Ready to run analysis"
      } else if (!aoi_selected) {
        "Load or create an AOI to continue"
      } else if (!variable_selected) {
        "Select a climate variable"
      } else if (!scenario_selected) {
        "Select a scenario"
      } else if (!period_selected) {
        "Select a time period"
      } else if (!raster_catalogued) {
        "No raster catalogue entry matches this selection"
      } else if (!raster_file_exists) {
        "The selected raster is listed but the file is unavailable"
      } else {
        "Selection incomplete"
      }
      
      if (aoi_selected) {
        geometry_type <- paste(
          unique(
            as.character(
              sf::st_geometry_type(
                rv$aoi
              )
            )
          ),
          collapse = ", "
        )
        
        aoi_detail <- paste0(
          nrow(rv$aoi),
          ifelse(
            nrow(rv$aoi) == 1,
            " feature",
            " features"
          ),
          " | ",
          geometry_type
        )
      } else {
        aoi_detail <- NULL
      }
      
      raster_status <- if (!selection_complete) {
        "Waiting for selection"
      } else if (!raster_catalogued) {
        "Not catalogued"
      } else if (!raster_file_exists) {
        "File unavailable"
      } else {
        "Available"
      }
      
      div(
        class = paste(
          "selection-status-box",
          if (ready_to_run) {
            "selection-status-ready"
          } else {
            "selection-status-incomplete"
          }
        ),
        
        div(
          class = "selection-status-heading",
          icon(
            if (ready_to_run) {
              "circle-check"
            } else {
              "circle-info"
            }
          ),
          tags$span(
            status_message
          )
        ),
        
        tags$hr(),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "AOI"
          ),
          tags$span(
            class = "selection-status-value",
            if (aoi_selected) {
              rv$aoi_name
            } else {
              "Not loaded"
            }
          )
        ),
        
        if (aoi_selected) {
          div(
            class = "selection-status-detail",
            aoi_detail
          )
        },
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Application"
          ),
          tags$span(
            class = "selection-status-value",
            if (
              !is.null(input$pathway) &&
              nzchar(input$pathway)
            ) {
              get_screening_application_label(input$pathway)
            } else {
              "Not selected"
            }
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Theme"
          ),
          tags$span(
            class = "selection-status-value",
            if (
              !is.null(input$theme) &&
              nzchar(input$theme)
            ) {
              input$theme
            } else {
              "Not selected"
            }
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Variable"
          ),
          tags$span(
            class = "selection-status-value",
            if (variable_selected) {
              get_variable_label(
                input$variable_id
              )
            } else {
              "Not selected"
            }
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Scenario"
          ),
          tags$span(
            class = "selection-status-value",
            if (scenario_selected) {
              get_scenario_label(
                input$scenario
              )
            } else {
              "Not selected"
            }
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Period"
          ),
          tags$span(
            class = "selection-status-value",
            if (period_selected) {
              get_period_label(
                input$period
              )
            } else {
              "Not selected"
            }
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Raster"
          ),
          tags$span(
            class = if (
              raster_catalogued &&
              raster_file_exists
            ) {
              "selection-status-good"
            } else {
              "selection-status-warning"
            },
            raster_status
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Comparison"
          ),
          tags$span(
            class = "selection-status-value",
            if (isTRUE(input$run_comparison)) {
              "Selected"
            } else {
              "Not selected"
            }
          )
        ),
        
        div(
          class = "selection-status-row",
          tags$span(
            class = "selection-status-label",
            "Cropped GeoTIFF"
          ),
          tags$span(
            class = "selection-status-value",
            if (isTRUE(
              input$create_cropped_raster
            )) {
              "Selected"
            } else {
              "Not selected"
            }
          )
        )
      )
    }
  )
  
  # ----------------------------------------------------------
  # ANALYSIS STATUS TEXT
  # ----------------------------------------------------------
  
  output$analysis_status <- renderText(
    {
      if (is.null(rv$aoi)) {
        return(
          "No AOI is currently selected."
        )
      }
      
      if (is.null(rv$result)) {
        return(
          paste(
            "Active AOI:",
            rv$aoi_name,
            "\nNo analysis has been run for the current AOI."
          )
        )
      }
      
      paste(
        paste(
          "Analysis completed for:",
          rv$result$aoi_name
        ),
        paste(
          "Variable:",
          rv$result$display_name
        ),
        paste(
          "Scenario:",
          rv$result$scenario
        ),
        paste(
          "Period:",
          rv$result$period
        ),
        sep = "\n"
      )
    }
  )
  
  # ----------------------------------------------------------
  # MAIN ANALYSIS
  # ----------------------------------------------------------
  
  observeEvent(
    input$run_analysis,
    {
      req(
        rv$aoi,
        input$variable_id,
        input$scenario,
        input$period
      )
      
      clear_analysis_outputs()
      
      variable_id <- input$variable_id
      scenario_id <- input$scenario
      period_id <- input$period
      
      matched_dataset <- raster_catalogue |>
        dplyr::filter(
          enabled,
          variable_id == !!variable_id,
          scenario == !!scenario_id,
          period == !!period_id
        ) |>
        dplyr::slice(1)
      
      if (nrow(matched_dataset) == 0) {
        showNotification(
          "No matching raster was found for the selected variable, scenario and period.",
          type = "error",
          duration = 10
        )
        
        return()
      }
      
      raster_path <- matched_dataset$file_path[1]
      
      output_dir <- file.path(
        "outputs",
        safe_filename(rv$aoi_name),
        safe_filename(variable_id),
        safe_filename(scenario_id),
        safe_filename(period_id)
      )
      
      dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      if (!file.exists(raster_path)) {
        showNotification(
          paste(
            "Raster file was listed in the catalogue but could not be found:",
            raster_path
          ),
          type = "error",
          duration = NULL
        )
        
        return()
      }
      
      if (
        !aoi_has_valid_raster_cells(
          rv$aoi,
          raster_path
        )
      ) {
        
        message("No valid raster cells found.")
        message("Raster: ", raster_path)
        message("AOI: ", rv$aoi_name)
        message("Variable: ", variable_id)
        message("Scenario: ", scenario_id)
        message("Period: ", period_id)
        message(
          paste(
            "This may mean the AOI is outside the raster extent,",
            "over NoData cells, or the raster has a CRS/extent/NoData issue."
          )
        )
        
        showNotification(
          "No valid raster cells found. Try a larger buffer or click further inside Sabah land area.",
          type = "error",
          duration = 8
        )
        
        return()
      }
      
      withProgress(
        message = "Running climate analysis",
        value = 0,
        {
          incProgress(
            0.25,
            detail = "Finding raster"
          )
          
          incProgress(
            0.50,
            detail = "Preparing AOI"
          )
          
          analysis_result <- tryCatch(
            {
              process_continuous_raster(
                raster_path,
                rv$aoi,
                variable_id = variable_id,
                scenario = scenario_id,
                period = period_id,
                output_dir = output_dir
              )
            },
            error = function(error) {
              
              showNotification(
                paste(
                  "Continuous-raster processing failed:",
                  error$message
                ),
                type = "error",
                duration = 12
              )
              
              NULL
            }
          )
          
          incProgress(
            0.75,
            detail = "Calculating statistics"
          )
          
          if (is.null(analysis_result)) {
            return()
          }
          
          mean_value <- get_analysis_value(
            analysis_result,
            c(
              "mean",
              "unweighted_mean",
              "Mean",
              "mean_value",
              "mean_value_rounded",
              "average",
              "Average",
              "avg",
              "AVG"
            )
          )
          
          min_value <- get_analysis_value(
            analysis_result,
            c(
              "minimum",
              "min",
              "Minimum",
              "Min",
              "minimum_value"
            )
          )
          
          max_value <- get_analysis_value(
            analysis_result,
            c(
              "maximum",
              "max",
              "Maximum",
              "Max",
              "maximum_value"
            )
          )
          
          if (is.na(mean_value[1])) {
            showNotification(
              paste(
                "Mean value was not found in the processing result. Available result fields:",
                paste(
                  names(analysis_result),
                  collapse = ", "
                )
              ),
              type = "warning",
              duration = 12
            )
          }
          
          variable_label <- get_variable_label(
            variable_id
          )
          
          scenario_label <- get_scenario_label(
            scenario_id
          )
          
          period_label <- get_period_label(
            period_id
          )
          
          units_value <- matched_dataset$units[1]
          
          if (
            length(units_value) == 0 ||
            is.na(units_value) ||
            units_value == ""
          ) {
            units_value <- "Not specified"
          }
          
          rv$result <- list(
            aoi_name = rv$aoi_name,
            variable_id = variable_id,
            display_name = variable_label,
            scenario_id = scenario_id,
            scenario = scenario_label,
            period_id = period_id,
            period = period_label,
            mean = as.numeric(mean_value[1]),
            minimum = as.numeric(min_value[1]),
            maximum = as.numeric(max_value[1]),
            units = units_value,
            raster_file = raster_path
          )
          
          if (
            isTRUE(input$create_cropped_raster) &&
            is.list(analysis_result) &&
            "cropped_raster" %in% names(analysis_result)
          ) {
            rv$cropped_raster <- analysis_result$cropped_raster
          } else {
            rv$cropped_raster <- NULL
            clear_analysis_map()
          }
          
          incProgress(
            1,
            detail = "Complete"
          )
        }
      )
      
      if (isTRUE(input$run_comparison)) {
        
        comparison_grid <- tidyr::expand_grid(
          Scenario_ID = input$comparison_scenarios,
          Period_ID = input$comparison_periods
        )
        
        comparison_results <- list()
        comparison_missing <- list()
        
        for (i in seq_len(nrow(comparison_grid))) {
          
          selected_scenario <- comparison_grid$Scenario_ID[i]
          selected_period <- comparison_grid$Period_ID[i]
          
          raster_row <- raster_catalogue |>
            dplyr::filter(
              enabled,
              variable_id == !!variable_id,
              scenario == !!selected_scenario,
              period == !!selected_period
            ) |>
            dplyr::slice(1)
          
          if (
            nrow(raster_row) == 0 ||
            !file.exists(raster_row$file_path[1])
          ) {
            
            comparison_missing[[length(comparison_missing) + 1]] <-
              tibble::tibble(
                Variable_ID = variable_id,
                Scenario_ID = selected_scenario,
                Period_ID = selected_period
              )
            
            next
          }
          
          if (
            !aoi_has_valid_raster_cells(
              rv$aoi,
              raster_row$file_path[1]
            )
          ) {
            
            comparison_missing[[length(comparison_missing) + 1]] <-
              tibble::tibble(
                Variable_ID = variable_id,
                Scenario_ID = selected_scenario,
                Period_ID = selected_period
              )
            
            next
          }
          
          comparison_output_dir <- file.path(
            "outputs",
            safe_filename(rv$aoi_name),
            safe_filename(variable_id),
            safe_filename(selected_scenario),
            safe_filename(selected_period)
          )
          
          dir.create(
            comparison_output_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )
          
          comparison_result <- tryCatch(
            {
              process_continuous_raster(
                raster_row$file_path[1],
                rv$aoi,
                variable_id = variable_id,
                scenario = selected_scenario,
                period = selected_period,
                output_dir = comparison_output_dir
              )
            },
            error = function(error) {
              
              if (
                !stringr::str_detect(
                  error$message,
                  "does not overlap"
                )
              ) {
                showNotification(
                  paste(
                    "Comparison processing failed for",
                    selected_scenario,
                    selected_period,
                    ":",
                    error$message
                  ),
                  type = "warning",
                  duration = 8
                )
              }
              
              NULL
            }
          )
          
          if (is.null(comparison_result)) {
            
            comparison_missing[[length(comparison_missing) + 1]] <-
              tibble::tibble(
                Variable_ID = variable_id,
                Scenario_ID = selected_scenario,
                Period_ID = selected_period
              )
            
            next
          }
          
          mean_value <- get_analysis_value(
            comparison_result,
            c(
              "mean",
              "unweighted_mean",
              "Mean",
              "mean_value",
              "mean_value_rounded",
              "average",
              "Average",
              "avg",
              "AVG"
            )
          )
          
          min_value <- get_analysis_value(
            comparison_result,
            c(
              "minimum",
              "min",
              "Minimum",
              "Min",
              "minimum_value"
            )
          )
          
          max_value <- get_analysis_value(
            comparison_result,
            c(
              "maximum",
              "max",
              "Maximum",
              "Max",
              "maximum_value"
            )
          )
          
          if (is.na(mean_value[1])) {
            comparison_missing[[length(comparison_missing) + 1]] <-
              tibble::tibble(
                Variable_ID = variable_id,
                Scenario_ID = selected_scenario,
                Period_ID = selected_period
              )
            
            next
          }
          
          comparison_results[[length(comparison_results) + 1]] <-
            tibble::tibble(
              AOI = rv$aoi_name,
              Variable = get_variable_label(variable_id),
              Variable_ID = variable_id,
              Scenario = get_scenario_label(selected_scenario),
              Scenario_ID = selected_scenario,
              Period = get_period_label(selected_period),
              Period_ID = selected_period,
              Mean = round(
                as.numeric(mean_value[1]),
                2
              ),
              Minimum = round(
                as.numeric(min_value[1]),
                2
              ),
              Maximum = round(
                as.numeric(max_value[1]),
                2
              ),
              Change_from_baseline = NA_real_,
              Units = raster_row$units[1],
              Raster_file = raster_row$file_path[1]
            )
        }
        
        if (length(comparison_results) > 0) {
          
          rv$comparison_results <- dplyr::bind_rows(
            comparison_results
          )
          
          # Identify the baseline dynamically so terrestrial 1981-2010
          # and SMV marine 1980-2005 can coexist in the same app.
          baseline_mean <- rv$comparison_results |>
            dplyr::filter(
              Scenario_ID == "baseline"
            ) |>
            dplyr::pull(
              Mean
            )
          
          if (length(baseline_mean) == 1) {
            
            rv$comparison_results <- rv$comparison_results |>
              dplyr::mutate(
                Change_from_baseline = round(
                  Mean - baseline_mean,
                  2
                )
              )
          }
        }
        
        if (length(comparison_missing) > 0) {
          rv$comparison_missing <- dplyr::bind_rows(
            comparison_missing
          )
        } else {
          rv$comparison_missing <- tibble::tibble()
        }
      }
      
      showNotification(
        "Analysis completed.",
        type = "message"
      )
    }
  )
  
  # ----------------------------------------------------------
  # OIL PALM CLIMATE-STRESS SCREENING
  # ----------------------------------------------------------
  # v4 uses a single Sabah-wide reference scale for the 0-100 score.
  # The same scale is used in single and comparison modes, so scores
  # can be compared directly between SSPs and 30-year climatologies.
  # The score is a relative climate-stress-change index, not yield loss.
  # ----------------------------------------------------------
  
  observeEvent(
    input$run_oil_palm,
    {
      req(
        rv$aoi,
        input$oil_palm_analysis_mode
      )
      
      # Clear previous Oil Palm outputs without disturbing the AOI.
      rv$oil_palm_result <- NULL
      rv$oil_palm_comparison <- NULL
      rv$oil_palm_optional_results <- NULL
      rv$oil_palm_score_raster <- NULL
      rv$oil_palm_agreement_raster <- NULL
      
      leafletProxy("map") |>
        clearGroup("Oil palm stress") |>
        removeControl(layerId = "oil_palm_legend")
      
      if (identical(input$oil_palm_analysis_mode, "single")) {
        req(
          input$oil_palm_scenario,
          input$oil_palm_period
        )
        
        withProgress(
          message = "Running Oil Palm climate screening",
          value = 0,
          {
            incProgress(
              0.2,
              detail = "Loading baseline and future climate layers"
            )
            
            oil_result <- tryCatch(
              {
                oil_palm_build_screening(
                  raster_catalogue = raster_catalogue,
                  aoi = rv$aoi,
                  aoi_name = rv$aoi_name,
                  future_scenario = input$oil_palm_scenario,
                  future_period = input$oil_palm_period
                )
              },
              error = function(error) {
                showNotification(
                  paste(
                    "Oil Palm screening failed:",
                    error$message
                  ),
                  type = "error",
                  duration = NULL
                )
                
                NULL
              }
            )
            
            if (is.null(oil_result)) {
              return()
            }
            
            incProgress(
              0.75,
              detail = "Building agreement, future stress, and change layers"
            )
            
            rv$oil_palm_result <- oil_result
            rv$oil_palm_optional_results <- oil_palm_run_optional_dimensions(
              raster_catalogue = raster_catalogue,
              aoi = rv$aoi,
              selected_dimensions = oil_palm_selected_optional_dimensions(),
              future_scenario = input$oil_palm_scenario,
              future_period = input$oil_palm_period,
              availability = oil_palm_dimension_status()
            )
            rv$oil_palm_score_raster <- oil_result$score_raster
            rv$oil_palm_agreement_raster <- oil_result$agreement_raster
            
            score_values <- terra::values(
              rv$oil_palm_score_raster,
              mat = FALSE
            )
            
            score_values <- score_values[
              is.finite(score_values)
            ]
            
            if (length(score_values) > 0) {
              oil_palette <- leaflet::colorNumeric(
                palette = "YlOrRd",
                domain = c(0, 100),
                na.color = "transparent"
              )
              
              leafletProxy("map") |>
                clearGroup("Analysis result") |>
                clearGroup("Oil palm stress") |>
                removeControl(
                  layerId = "analysis_result_legend"
                ) |>
                removeControl(
                  layerId = "oil_palm_legend"
                ) |>
                addRasterImage(
                  x = rv$oil_palm_score_raster,
                  colors = oil_palette,
                  opacity = 0.75,
                  group = "Oil palm stress",
                  project = TRUE,
                  method = "bilinear",
                  maxBytes = 10 * 1024 * 1024
                ) |>
                addLegend(
                  pal = oil_palette,
                  values = c(0, 100),
                  title = "Oil Palm Future Climate Stress (0–100)",
                  position = "bottomright",
                  opacity = 1,
                  layerId = "oil_palm_legend"
                )
            }
            
            incProgress(
              1,
              detail = "Complete"
            )
          }
        )
        
        if (!is.null(rv$oil_palm_result)) {
          showNotification(
            "Oil Palm climate screening completed.",
            type = "message"
          )
        }
        
      } else {
        req(
          input$oil_palm_comparison_scenarios,
          input$oil_palm_comparison_periods
        )
        
        withProgress(
          message = "Comparing Oil Palm climate stress",
          value = 0,
          {
            incProgress(
              0.15,
              detail = "Checking available SSP and climatology combinations"
            )
            
            oil_comparison <- tryCatch(
              {
                oil_palm_build_comparison(
                  raster_catalogue = raster_catalogue,
                  aoi = rv$aoi,
                  aoi_name = rv$aoi_name,
                  future_scenarios = input$oil_palm_comparison_scenarios,
                  future_periods = input$oil_palm_comparison_periods
                )
              },
              error = function(error) {
                showNotification(
                  paste(
                    "Oil Palm comparison failed:",
                    error$message
                  ),
                  type = "error",
                  duration = NULL
                )
                
                NULL
              }
            )
            
            if (is.null(oil_comparison)) {
              return()
            }
            
            incProgress(
              0.9,
              detail = "Preparing comparison table and graph"
            )
            
            rv$oil_palm_comparison <- oil_comparison
            rv$oil_palm_optional_results <- oil_palm_compare_optional_dimensions(
              raster_catalogue = raster_catalogue,
              aoi = rv$aoi,
              selected_dimensions = oil_palm_selected_optional_dimensions(),
              future_scenarios = input$oil_palm_comparison_scenarios,
              future_periods = input$oil_palm_comparison_periods,
              availability = oil_palm_dimension_status()
            )
            
            # Comparison mode reports comparable 0-100 AOI scores.
            # A single raster is not shown because several SSP/period
            # combinations may be selected at the same time.
            leafletProxy("map") |>
              clearGroup("Oil palm stress") |>
              removeControl(layerId = "oil_palm_legend")
            
            incProgress(
              1,
              detail = "Complete"
            )
          }
        )
        
        if (!is.null(rv$oil_palm_comparison)) {
          showNotification(
            "Oil Palm scenario and time-period comparison completed.",
            type = "message"
          )
        }
      }
    }
  )
  
  # ----------------------------------------------------------
  # OIL PALM PRINTABLE REPORT
  # ----------------------------------------------------------

  output$oil_palm_print_button_ui <- renderUI({
    if (is.null(rv$oil_palm_result) && is.null(rv$oil_palm_comparison)) {
      return(NULL)
    }

    div(
      class = "screening-report-actions no-print",
      actionButton(
        inputId = "print_oil_palm_report",
        label = "Print / Save report",
        icon = icon("print"),
        class = "btn-primary",
        onclick = "printScreeningReport('oil_palm_report_print_area'); return false;"
      )
    )
  })

  output$oil_palm_print_metadata <- renderUI({
    if (is.null(rv$oil_palm_result) && is.null(rv$oil_palm_comparison)) {
      return(NULL)
    }

    aoi_name <- if (!is.null(rv$oil_palm_comparison)) {
      rv$oil_palm_comparison$aoi_name
    } else {
      rv$oil_palm_result$aoi_name
    }

    div(
      class = "print-only",
      h3("Sabah Climate Risk Explorer"),
      p(strong(paste("Area of interest:", aoi_name))),
      p(paste("Report generated:", format(Sys.time(), "%d %B %Y %H:%M"))),
      p(
        class = "text-muted",
        paste(
          "Screening-level climate information. Scores are comparative climate-stress indicators",
          "and should not be interpreted as percentage yield loss."
        )
      ),
      tags$hr()
    )
  })

  # ----------------------------------------------------------
  # OIL PALM SUMMARY
  # ----------------------------------------------------------
  
  output$oil_palm_summary_ui <- renderUI(
    {
      if (
        is.null(rv$oil_palm_result) &&
        is.null(rv$oil_palm_comparison)
      ) {
        return(
          div(
            class = "text-muted",
            "Select Oil Palm Climate Stress in the sidebar and run the screening."
          )
        )
      }
      
      if (!is.null(rv$oil_palm_comparison)) {
        comparison <- rv$oil_palm_comparison
        
        future_scores <- comparison$comparison_table |>
          dplyr::filter(
            .data$Scenario_ID != OIL_PALM_BASELINE_SCENARIO,
            is.finite(.data$Relative_stress_change_score)
          ) |>
          dplyr::pull(.data$Relative_stress_change_score)
        
        score_text <- if (length(future_scores) == 0) {
          "Not available"
        } else if (length(future_scores) == 1) {
          paste0(round(future_scores[1]), " / 100")
        } else {
          paste0(
            round(min(future_scores)),
            "–",
            round(max(future_scores)),
            " / 100 across selected futures"
          )
        }
        
        return(
          div(
            div(
              class = "oil-palm-summary-card",
              div(
                class = "oil-palm-summary-big",
                paste0(
                  comparison$n_combinations,
                  ifelse(
                    comparison$n_combinations == 1,
                    " future combination compared",
                    " future combinations compared"
                  )
                )
              ),
              div(
                paste(
                  "AOI:",
                  comparison$aoi_name,
                  "| Baseline: 1981–2010"
                )
              )
            ),
            div(
              class = "oil-palm-score-card",
              strong("Future climate-stress score"),
              div(
                class = "oil-palm-score-number",
                {
                  future_stress_scores <- comparison$comparison_table |>
                    dplyr::filter(
                      .data$Scenario_ID != OIL_PALM_BASELINE_SCENARIO,
                      is.finite(.data$Future_stress_score)
                    ) |>
                    dplyr::pull(.data$Future_stress_score)
                  
                  if (length(future_stress_scores) == 0) {
                    "Not available"
                  } else if (length(future_stress_scores) == 1) {
                    paste0(round(future_stress_scores[1]), " / 100")
                  } else {
                    paste0(
                      round(min(future_stress_scores)),
                      "–",
                      round(max(future_stress_scores)),
                      " / 100 across selected futures"
                    )
                  }
                }
              ),
              div(
                class = "text-muted",
                paste(
                  "This shows how climate-stressed the AOI is under the future",
                  "scenario and period, not just how much it changes."
                )
              )
            ),
            div(
              class = "oil-palm-score-card",
              strong("Additional climate-stress-change score"),
              div(
                class = "oil-palm-score-number",
                score_text
              ),
              div(
                class = "text-muted",
                paste(
                  "Every SSP and time period uses the same fixed Sabah-wide",
                  "reference scale, so these 0–100 change scores can be compared directly."
                )
              )
            ),
            p(
              paste(
                "Comparison mode shows the three oil-palm water-stress indicators,",
                "a future climate-stress score, and an additional",
                "climate-stress-change score for each available SSP and",
                "30-year climatology against the same historical baseline."
              )
            ),
            div(
              class = "results-note",
              paste(
                "The Future climate-stress score reflects how stressed the AOI is",
                "under the selected future conditions. The Additional",
                "climate-stress-change score reflects the amount of worsening",
                "relative to 1981–2010. Neither score is percentage yield loss."
              )
            )
          )
        )
      }
      result <- rv$oil_palm_result
      
      div(
        div(
          class = "oil-palm-summary-card",
          div(
            class = "oil-palm-summary-big",
            paste0(
              result$n_worsening,
              " of 3 water-stress indicators worsen"
            )
          ),
          div(
            paste(
              "AOI:",
              result$aoi_name,
              "|",
              result$scenario,
              "|",
              result$period
            )
          )
        ),
        
        div(
          class = "oil-palm-score-card",
          strong("Baseline climate-stress score"),
          div(
            class = "oil-palm-score-number",
            if (is.finite(result$baseline_absolute_score_mean)) {
              paste0(round(result$baseline_absolute_score_mean), " / 100")
            } else {
              "Not available"
            }
          ),
          div(
            class = "text-muted",
            "How climate-stressed the AOI is under the 1981–2010 baseline climate."
          )
        ),
        div(
          class = "oil-palm-score-card",
          strong("Future climate-stress score"),
          div(
            class = "oil-palm-score-number",
            if (is.finite(result$future_absolute_score_mean)) {
              paste0(round(result$future_absolute_score_mean), " / 100")
            } else {
              "Not available"
            }
          ),
          div(
            class = "text-muted",
            "How climate-stressed the AOI is under the selected future scenario and period."
          )
        ),
        div(
          class = "oil-palm-score-card",
          strong("Additional climate-stress-change score"),
          div(
            class = "oil-palm-score-number",
            if (is.finite(result$relative_score_mean)) {
              paste0(round(result$relative_score_mean), " / 100")
            } else {
              "Not available"
            }
          ),
          div(
            class = "text-muted",
            paste(
              "Uses the same fixed Sabah-wide reference scale as every other SSP",
              "and time period, so the change score is comparable across screenings.",
              "It is not percentage yield loss."
            )
          )
        ),
        
        h5("Interpretation"),
        p(result$interpretation)
      )
    }
  )
  
  # ----------------------------------------------------------
  # OIL PALM RESULTS TABLE
  # ----------------------------------------------------------
  
  output$oil_palm_results_table <- renderTable(
    {
      if (!is.null(rv$oil_palm_comparison)) {
        return(
          rv$oil_palm_comparison$comparison_table |>
            dplyr::transmute(
              Scenario = .data$Scenario,
              Period = .data$Period,
              `Minimum P:PET` = round(.data$PPETmin, 2),
              `Consecutive months P:PET < 1` = round(
                .data$PPETConDryMth,
                2
              ),
              `Consecutive dry days` = round(.data$CDD, 2),
              `Baseline climate-stress score` = round(
                .data$Baseline_stress_score,
                0
              ),
              `Future climate-stress score` = round(
                .data$Future_stress_score,
                0
              ),
              `Indicators worsening` = ifelse(
                is.na(.data$Indicators_worsening),
                "Reference",
                paste0(.data$Indicators_worsening, " / 3")
              ),
              `Additional climate-stress-change score` = round(
                .data$Relative_stress_change_score,
                0
              )
            )
        )
      }
      
      req(rv$oil_palm_result)
      
      rv$oil_palm_result$indicator_table |>
        dplyr::transmute(
          Indicator = .data$Indicator,
          Baseline = round(.data$Baseline, 2),
          Future = round(.data$Future, 2),
          Change = round(.data$Change, 2),
          `Climate stress worsens` = ifelse(
            .data$Worsens,
            "Yes",
            "No"
          ),
          Units = .data$Units
        )
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%"
  )
  
  # ----------------------------------------------------------
  # OIL PALM OPTIONAL DIMENSION RESULTS
  # ----------------------------------------------------------

  output$oil_palm_optional_results_ui <- renderUI({
    results <- rv$oil_palm_optional_results

    if (is.null(results) || nrow(results) == 0) {
      return(NULL)
    }

    tagList(
      h4("Additional dimensions"),
      div(
        class = "results-note",
        paste(
          "These dimensions are reported separately and do not change the core crop-water-stress score.",
          "Provisional means the best currently registered indicator is being used while a preferred impact layer is pending."
        )
      ),
      tableOutput("oil_palm_optional_results_table"),
      downloadButton(
        "download_oil_palm_optional_csv",
        "Download additional dimensions CSV"
      ),
      br(), br()
    )
  })

  output$oil_palm_optional_results_table <- renderTable({
    req(rv$oil_palm_optional_results)
    results <- rv$oil_palm_optional_results

    if ("Scenario" %in% names(results)) {
      results |>
        dplyr::transmute(
          Dimension = .data$Dimension,
          Status = .data$Status,
          Indicator = .data$Indicator,
          Scenario = .data$Scenario,
          Period = .data$Period,
          Baseline = round(.data$Baseline, 2),
          Future = round(.data$Future, 2),
          Change = round(.data$Change, 2),
          `Stress worsens` = dplyr::case_when(
            is.na(.data$Worsens) ~ "Not available",
            .data$Worsens ~ "Yes",
            TRUE ~ "No"
          ),
          Units = .data$Units
        )
    } else {
      results |>
        dplyr::transmute(
          Dimension = .data$Dimension,
          Status = .data$Status,
          Indicator = .data$Indicator,
          Baseline = round(.data$Baseline, 2),
          Future = round(.data$Future, 2),
          Change = round(.data$Change, 2),
          `Stress worsens` = dplyr::case_when(
            is.na(.data$Worsens) ~ "Not available",
            .data$Worsens ~ "Yes",
            TRUE ~ "No"
          ),
          Units = .data$Units
        )
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  output$download_oil_palm_optional_csv <- downloadHandler(
    filename = function() {
      paste0(
        safe_filename(rv$aoi_name),
        "_oil_palm_additional_dimensions.csv"
      )
    },
    content = function(file) {
      req(rv$oil_palm_optional_results)
      readr::write_csv(rv$oil_palm_optional_results, file)
    }
  )

  # ----------------------------------------------------------
  # OIL PALM COMPARISON GRAPH
  # ----------------------------------------------------------
  
  output$oil_palm_comparison_plot_controls <- renderUI(
    {
      if (is.null(rv$oil_palm_comparison)) {
        return(NULL)
      }
      
      tagList(
        h4("Comparison graph"),
        selectInput(
          inputId = "oil_palm_plot_indicator",
          label = "Indicator to plot",
          choices = c(
            "Future climate-stress score" = "FutureStressScore",
            "Additional climate-stress-change score" = "StressScore",
            "Minimum P:PET" = "PPETmin",
            "Consecutive months P:PET < 1" = "PPETConDryMth",
            "Consecutive dry days" = "CDD"
          ),
          selected = "FutureStressScore"
        )
      )
    }
  )
  
  output$oil_palm_comparison_plot_ui <- renderUI(
    {
      if (is.null(rv$oil_palm_comparison)) {
        return(NULL)
      }
      
      plotOutput(
        outputId = "oil_palm_comparison_plot",
        height = "430px"
      )
    }
  )
  
  output$oil_palm_comparison_plot <- renderPlot(
    {
      req(
        rv$oil_palm_comparison,
        input$oil_palm_plot_indicator
      )
      
      if (identical(input$oil_palm_plot_indicator, "StressScore")) {
        plot_data <- rv$oil_palm_comparison$comparison_table |>
          dplyr::mutate(
            Scenario_Period = paste(
              .data$Scenario,
              .data$Period,
              sep = " / "
            ),
            Plot_value = .data$Relative_stress_change_score
          )
        
        indicator_name <- "Additional climate-stress-change score"
        units_value <- "0–100"
        score_plot <- TRUE
      } else if (identical(input$oil_palm_plot_indicator, "FutureStressScore")) {
        plot_data <- rv$oil_palm_comparison$comparison_table |>
          dplyr::mutate(
            Scenario_Period = paste(
              .data$Scenario,
              .data$Period,
              sep = " / "
            ),
            Plot_value = .data$Future_stress_score
          )
        
        indicator_name <- "Future climate-stress score"
        units_value <- "0–100"
        score_plot <- TRUE
      } else {
        plot_data <- rv$oil_palm_comparison$long_table |>
          dplyr::filter(
            .data$Variable_ID == input$oil_palm_plot_indicator
          ) |>
          dplyr::mutate(
            Scenario_Period = paste(
              .data$Scenario,
              .data$Period,
              sep = " / "
            ),
            Plot_value = .data$Value
          )
        
        indicator_name <- unique(plot_data$Indicator)[1]
        units_value <- unique(plot_data$Units)[1]
        score_plot <- FALSE
      }
      
      validate(
        need(
          nrow(plot_data) > 0,
          "No comparison values are available for this indicator."
        )
      )
      
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      
      par(
        mar = c(4, 10, 3, 1) + 0.1,
        cex.main = 0.9,
        cex.lab = 0.85,
        cex.axis = 0.75
      )
      
      if (isTRUE(score_plot)) {
        barplot(
          height = plot_data$Plot_value,
          names.arg = plot_data$Scenario_Period,
          horiz = TRUE,
          las = 1,
          xlab = paste0(indicator_name, " (0–100)"),
          xlim = c(0, 100),
          main = paste(
            indicator_name,
            "within",
            rv$oil_palm_comparison$aoi_name
          )
        )
      } else {
        barplot(
          height = plot_data$Plot_value,
          names.arg = plot_data$Scenario_Period,
          horiz = TRUE,
          las = 1,
          xlab = paste0(
            indicator_name,
            if (
              !is.na(units_value) &&
              nzchar(units_value)
            ) {
              paste0(" (", units_value, ")")
            } else {
              ""
            }
          ),
          main = paste(
            indicator_name,
            "within",
            rv$oil_palm_comparison$aoi_name
          )
        )
      }
    },
    height = 430
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD OIL PALM CSV
  # ----------------------------------------------------------
  
  output$download_oil_palm_csv <- downloadHandler(
    filename = function() {
      if (!is.null(rv$oil_palm_comparison)) {
        return(
          paste0(
            safe_filename(rv$oil_palm_comparison$aoi_name),
            "_oil_palm_comparison.csv"
          )
        )
      }
      
      req(rv$oil_palm_result)
      
      paste0(
        safe_filename(rv$oil_palm_result$aoi_name),
        "_oil_palm_",
        rv$oil_palm_result$scenario_id,
        "_",
        rv$oil_palm_result$period_id,
        ".csv"
      )
    },
    
    content = function(file) {
      if (!is.null(rv$oil_palm_comparison)) {
        export <- rv$oil_palm_comparison$export_table |>
          dplyr::mutate(
            Note = paste(
              "Actual indicator values and the 0-100 Relative climate-stress-change score are comparable across rows.",
              "All SSPs and time periods use the same fixed Sabah-wide reference scale.",
              "The score is not a percentage yield loss."
            )
          )
        
        readr::write_csv(
          export,
          file
        )
        
        return()
      }
      
      req(rv$oil_palm_result)
      
      export <- rv$oil_palm_result$indicator_table |>
        dplyr::mutate(
          AOI = rv$oil_palm_result$aoi_name,
          Scenario = rv$oil_palm_result$scenario,
          Scenario_ID = rv$oil_palm_result$scenario_id,
          Period = rv$oil_palm_result$period,
          Period_ID = rv$oil_palm_result$period_id,
          Indicators_worsening = rv$oil_palm_result$n_worsening,
          Relative_stress_change_score = rv$oil_palm_result$relative_score_mean,
          Interpretation = rv$oil_palm_result$interpretation
        ) |>
        dplyr::select(
          AOI,
          Scenario,
          Scenario_ID,
          Period,
          Period_ID,
          Indicator,
          Variable_ID,
          Baseline,
          Future,
          Change,
          Worsens,
          Units,
          Indicators_worsening,
          Relative_stress_change_score,
          Interpretation,
          Baseline_raster,
          Future_raster
        )
      
      readr::write_csv(
        export,
        file
      )
    }
  )
  
  # ----------------------------------------------------------
  # SIMPLE RESULTS TABLE
  # ----------------------------------------------------------
  
  output$result_table <- renderTable(
    {
      if (is.null(rv$aoi)) {
        return(
          tibble::tibble(
            Field = "Status",
            Value = "No AOI is currently selected."
          )
        )
      }
      
      if (is.null(rv$result)) {
        return(
          tibble::tibble(
            Field = c(
              "AOI",
              "Status"
            ),
            Value = c(
              rv$aoi_name,
              "No analysis has been run for the current AOI."
            )
          )
        )
      }
      
      tibble::tibble(
        Field = c(
          "AOI",
          "Variable",
          "Scenario",
          "Period",
          "Mean",
          "Minimum",
          "Maximum",
          "Units"
        ),
        Value = c(
          rv$result$aoi_name,
          rv$result$display_name,
          rv$result$scenario,
          rv$result$period,
          round(
            rv$result$mean,
            2
          ),
          round(
            rv$result$minimum,
            2
          ),
          round(
            rv$result$maximum,
            2
          ),
          rv$result$units
        )
      )
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%"
  )
  
  # ----------------------------------------------------------
  # DYNAMIC DOWNLOAD BUTTONS
  # ----------------------------------------------------------
  
  output$download_buttons <- renderUI(
    {
      if (is.null(rv$result)) {
        return(
          div(
            class = "text-muted",
            "Run an analysis to enable downloads."
          )
        )
      }
      
      download_items <- list(
        downloadButton(
          outputId = "download_result_csv",
          label = "Download result CSV"
        ),
        
        br(),
        br(),
        
        downloadButton(
          outputId = "download_comparison_csv",
          label = "Download comparison CSV"
        )
      )
      
      if (!is.null(rv$cropped_raster)) {
        download_items <- c(
          download_items,
          list(
            br(),
            br(),
            downloadButton(
              outputId = "download_cropped_raster",
              label = "Download cropped raster"
            )
          )
        )
      }
      
      tagList(
        download_items
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD MAIN RESULT CSV
  # ----------------------------------------------------------
  
  output$download_result_csv <- downloadHandler(
    
    filename = function() {
      
      req(
        rv$result
      )
      
      safe_aoi_name <- safe_filename(
        rv$result$aoi_name
      )
      
      safe_variable_id <- safe_filename(
        rv$result$variable_id
      )
      
      paste0(
        safe_aoi_name,
        "_",
        safe_variable_id,
        "_",
        rv$result$scenario_id,
        "_",
        rv$result$period_id,
        "_result.csv"
      )
    },
    
    content = function(file) {
      
      req(
        rv$result
      )
      
      result_export <- tibble::tibble(
        AOI = rv$result$aoi_name,
        Variable = rv$result$display_name,
        Variable_ID = rv$result$variable_id,
        Scenario = rv$result$scenario,
        Scenario_ID = rv$result$scenario_id,
        Period = rv$result$period,
        Period_ID = rv$result$period_id,
        Mean = round(
          rv$result$mean,
          2
        ),
        Minimum = round(
          rv$result$minimum,
          2
        ),
        Maximum = round(
          rv$result$maximum,
          2
        ),
        Units = rv$result$units,
        Raster_file = rv$result$raster_file
      )
      
      readr::write_csv(
        result_export,
        file
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD CROPPED RASTER
  # ----------------------------------------------------------
  
  output$download_cropped_raster <- downloadHandler(
    
    filename = function() {
      
      req(
        rv$result
      )
      
      safe_aoi_name <- safe_filename(
        rv$result$aoi_name
      )
      
      safe_variable_id <- safe_filename(
        rv$result$variable_id
      )
      
      paste0(
        safe_aoi_name,
        "_",
        safe_variable_id,
        "_",
        rv$result$scenario_id,
        "_",
        rv$result$period_id,
        "_cropped.tif"
      )
    },
    
    content = function(file) {
      
      req(
        isTRUE(input$create_cropped_raster),
        rv$cropped_raster
      )
      
      cropped_raster <- rv$cropped_raster
      
      if (
        is.character(cropped_raster) &&
        length(cropped_raster) == 1 &&
        file.exists(cropped_raster)
      ) {
        
        file.copy(
          from = cropped_raster,
          to = file,
          overwrite = TRUE
        )
        
      } else if (
        inherits(
          cropped_raster,
          "SpatRaster"
        )
      ) {
        
        terra::writeRaster(
          cropped_raster,
          filename = file,
          overwrite = TRUE
        )
        
      } else {
        
        stop(
          "No cropped raster is available for download."
        )
      }
    }
  )
  
  # ----------------------------------------------------------
  # COMPARISON MISSING NOTE
  # ----------------------------------------------------------
  
  output$comparison_missing_note <- renderText(
    {
      if (
        is.null(rv$comparison_missing) ||
        nrow(rv$comparison_missing) == 0
      ) {
        return("")
      }
      
      "Some selected scenario-period combinations were skipped because no matching raster was found in the raster catalogue."
    }
  )
  
  # ----------------------------------------------------------
  # USER-FACING COMPARISON TABLE HELPER
  # ----------------------------------------------------------
  
  comparison_display_table <- reactive(
    {
      req(
        rv$comparison_results
      )
      
      rv$comparison_results |>
        dplyr::select(
          AOI,
          Variable,
          Scenario,
          Period,
          Mean,
          Change_from_baseline,
          Minimum,
          Maximum,
          Units
        )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD COMPARISON CSV HELPER
  # ----------------------------------------------------------
  # This is the full exported version.
  # It includes IDs and Raster_file for traceability.
  # ----------------------------------------------------------
  
  comparison_export_table <- reactive(
    {
      req(
        rv$comparison_results
      )
      
      rv$comparison_results |>
        dplyr::mutate(
          Change_from_baseline = as.numeric(
            Change_from_baseline
          )
        ) |>
        dplyr::select(
          AOI,
          Variable,
          Variable_ID,
          Scenario,
          Scenario_ID,
          Period,
          Period_ID,
          Mean,
          Minimum,
          Maximum,
          Change_from_baseline,
          Units,
          Raster_file
        )
    }
  )
  
  # ----------------------------------------------------------
  # COMPARISON RESULTS TABLE
  # ----------------------------------------------------------
  
  output$comparison_results <- renderDT(
    {
      if (is.null(rv$comparison_results)) {
        return(
          DT::datatable(
            tibble::tibble(
              Status = "No comparison results yet. Run an analysis first."
            ),
            rownames = FALSE,
            options = list(
              dom = "t",
              ordering = FALSE,
              paging = FALSE,
              searching = FALSE,
              info = FALSE
            )
          )
        )
      }
      
      DT::datatable(
        comparison_display_table(),
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          scrollY = "300px",
          scrollCollapse = TRUE,
          autoWidth = TRUE
        )
      )
    }
  )
  
  # ----------------------------------------------------------
  # COMPARISON GRAPH UI
  # ----------------------------------------------------------
  
  output$comparison_graph_ui <- renderUI(
    {
      if (is.null(rv$comparison_results)) {
        return(
          div(
            class = "text-muted",
            "Run a comparison analysis to show the graph."
          )
        )
      }
      
      plotOutput(
        outputId = "comparison_plot",
        height = "520px",
        width = "700px"
      )
    }
  )
  
  # ----------------------------------------------------------
  # COMPARISON GRAPH
  # ----------------------------------------------------------
  
  output$comparison_plot <- renderPlot(
    {
      req(
        rv$comparison_results
      )
      
      plot_data <- rv$comparison_results |>
        dplyr::mutate(
          Scenario_Period = paste(
            Scenario,
            Period,
            sep = " / "
          )
        )
      
      validate(
        need(
          nrow(plot_data) >= 1,
          "No comparison results are available to plot."
        )
      )
      
      old_par <- par(
        no.readonly = TRUE
      )
      
      on.exit(
        par(old_par)
      )
      
      par(
        mar = c(4, 8, 3, 1) + 0.1,
        cex.main = 0.9,
        cex.lab = 0.8,
        cex.axis = 0.75
      )
      
      barplot(
        height = plot_data$Mean,
        names.arg = plot_data$Scenario_Period,
        horiz = TRUE,
        las = 1,
        xlab = paste0(
          "Mean",
          " (",
          plot_data$Units[[1]],
          ")"
        ),
        main = paste(
          plot_data$Variable[[1]],
          "within",
          plot_data$AOI[[1]]
        )
      )
    },
    height = 520,
    width = 700
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD COMPARISON CSV
  # ----------------------------------------------------------
  # Updated export columns:
  # AOI, Variable, Variable_ID, Scenario, Scenario_ID,
  # Period, Period_ID, Mean, Minimum, Maximum,
  # Change_from_baseline, Units, Raster_file
  # ----------------------------------------------------------
  
  output$download_comparison_csv <- downloadHandler(
    
    filename = function() {
      
      safe_aoi_name <- safe_filename(
        rv$aoi_name
      )
      
      safe_variable_id <- safe_filename(
        input$variable_id
      )
      
      paste0(
        safe_aoi_name,
        "_",
        safe_variable_id,
        "_comparison.csv"
      )
    },
    
    content = function(file) {
      
      req(
        comparison_export_table()
      )
      
      readr::write_csv(
        comparison_export_table(),
        file
      )
    }
  )
  
  # ----------------------------------------------------------
  # RESULTS TABLE NOTE
  # ----------------------------------------------------------
  
  output$results_note <- renderText(
    {
      req(input$variable_id)
      
      switch(
        input$variable_id,
        
        WBGTmax =
          paste(
            "Maximum WBGT is a heat-stress indicator.",
            "Higher values indicate greater potential heat exposure and reduced outdoor workability.",
            "This layer represents monthly average maximum WBGT, not daily extreme WBGT."
          ),
        
        Bio017 =
          paste(
            "Bio017 is precipitation of the driest quarter.",
            "Lower values indicate drier conditions."
          ),
        
        PPETmin =
          paste(
            "PPETmin is the minimum precipitation-to-potential-evapotranspiration ratio.",
            "Lower values indicate drier conditions."
          ),
        
        Bio05 =
          paste(
            "Bio05 is maximum temperature of the warmest month.",
            "Higher values indicate hotter warm-month conditions."
          ),
        
        FIRE_PROB =
          "Higher values indicate greater modelled fire probability.",
        
        Fire =
          "Higher values indicate greater modelled fire probability.",
        
        ""
      )
    }
  )
  
  # ----------------------------------------------------------
  # CROPPED RASTER STATUS
  # ----------------------------------------------------------
  
  output$cropped_raster_status <- renderText(
    {
      if (is.null(rv$result)) {
        return("")
      }
      
      if (isTRUE(input$create_cropped_raster)) {
        
        if (!is.null(rv$cropped_raster)) {
          return(
            "Cropped raster output is available for download."
          )
        }
        
        return(
          "Cropped raster output was requested, but no cropped raster is currently available."
        )
      }
      
      "Cropped raster output was not requested for this analysis."
    }
  )
  
  # ----------------------------------------------------------
  # DISPLAY SELECTED ANALYSIS RASTER AND LEGEND
  # ----------------------------------------------------------
  
  observeEvent(
    rv$cropped_raster,
    {
      req(
        isTRUE(input$create_cropped_raster),
        rv$cropped_raster,
        input$variable_id
      )
      
      cropped_raster <- rv$cropped_raster
      
      if (
        is.character(cropped_raster) &&
        length(cropped_raster) == 1
      ) {
        validate(
          need(
            file.exists(cropped_raster),
            "The processed raster file could not be found."
          )
        )
        
        cropped_raster <- terra::rast(
          cropped_raster
        )
      }
      
      validate(
        need(
          inherits(
            cropped_raster,
            "SpatRaster"
          ),
          "The processed raster is not a terra SpatRaster."
        )
      )
      
      if (terra::nlyr(cropped_raster) > 1) {
        cropped_raster <- cropped_raster[[1]]
      }
      
      raster_values <- terra::values(
        cropped_raster,
        mat = FALSE
      )
      
      raster_values <- raster_values[
        is.finite(raster_values)
      ]
      
      validate(
        need(
          length(raster_values) > 0,
          "The processed raster contains no valid values."
        )
      )
      
      variable_name <- get_variable_label(
        input$variable_id
      )
      
      units_value <- if (
        !is.null(rv$result$units)
      ) {
        rv$result$units
      } else {
        "Not specified"
      }
      
      palette_name <- dplyr::case_when(
        input$variable_id == "Bio05" ~ "inferno",
        input$variable_id == "WBGTmax" ~ "inferno",
        input$variable_id == "Bio017" ~ "viridis",
        TRUE ~ "viridis"
      )
      
      palette_function <- leaflet::colorNumeric(
        palette = palette_name,
        domain = raster_values,
        na.color = "transparent"
      )
      
      legend_title <- paste0(
        variable_name,
        " (",
        units_value,
        ")"
      )
      
      clear_analysis_map()
      
      leafletProxy("map") |>
        addRasterImage(
          x = cropped_raster,
          colors = palette_function,
          opacity = 0.75,
          group = "Analysis result",
          project = TRUE,
          method = "bilinear",
          maxBytes = 10 * 1024 * 1024
        ) |>
        addLegend(
          pal = palette_function,
          values = raster_values,
          title = legend_title,
          group = "Analysis result",
          position = "bottomright",
          opacity = 1,
          layerId = "analysis_result_legend"
        )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # CATALOGUE TABLE
  # ----------------------------------------------------------
  
  output$catalogue_table <- renderDT(
    {
      display_catalogue <- raster_catalogue |>
        dplyr::mutate(
          Variable = vapply(
            variable_id,
            get_variable_label,
            character(1)
          ),
          Scenario = vapply(
            scenario,
            get_scenario_label,
            character(1)
          ),
          Period = vapply(
            period,
            get_period_label,
            character(1)
          ),
          Units = dplyr::if_else(
            is.na(units) | units == "",
            "Not specified",
            as.character(units)
          ),
          `File exists` = dplyr::if_else(
            file.exists(file_path),
            "Yes",
            "No"
          ),
          Enabled = dplyr::if_else(
            enabled,
            "Yes",
            "No"
          ),
          `Variable ID` = variable_id,
          `Scenario ID` = scenario,
          `Period ID` = period,
          `Raster file` = basename(file_path),
          `File path` = file_path
        )
      
      front_columns <- c(
        "Variable",
        "Scenario",
        "Period",
        "Units",
        "File exists",
        "Enabled"
      )
      
      technical_columns <- c(
        "Variable ID",
        "Scenario ID",
        "Period ID",
        "Raster file",
        "File path"
      )
      
      remaining_columns <- setdiff(
        names(display_catalogue),
        c(
          front_columns,
          technical_columns,
          "variable_id",
          "scenario",
          "period",
          "units",
          "enabled",
          "file_path"
        )
      )
      
      display_catalogue <- display_catalogue |>
        dplyr::select(
          dplyr::all_of(front_columns),
          dplyr::all_of(technical_columns),
          dplyr::any_of(remaining_columns)
        ) |>
        dplyr::arrange(
          Variable,
          Scenario,
          Period
        )
      
      DT::datatable(
        display_catalogue,
        rownames = FALSE,
        filter = "top",
        class = "stripe hover compact",
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 15, 25, 50),
          scrollX = TRUE,
          autoWidth = TRUE,
          searchHighlight = TRUE,
          columnDefs = list(
            list(
              targets = c(1, 2, 3, 4, 5),
              className = "dt-center"
            ),
            list(
              targets = 0,
              width = "260px"
            ),
            list(
              targets = 9,
              width = "220px"
            ),
            list(
              targets = 10,
              width = "360px"
            )
          )
        )
      )
    }
  )
  
  # ----------------------------------------------------------
  # TEMPORARY DEVELOPER TEST
  # ----------------------------------------------------------
  # The Run test button now runs its own AOI test only.
  # It does not click Run analysis and does not depend on rv$result.
  # ----------------------------------------------------------
  
  aoi_test_results <- eventReactive(
    input$run_aoi_test,
    {
      if (is.null(rv$aoi)) {
        showNotification(
          "Load an AOI before running the AOI test.",
          type = "error",
          duration = 8
        )
        
        return(
          tibble::tibble(
            Field = "Status",
            Result = "No AOI currently loaded."
          )
        )
      }
      
      req(
        input$variable_id,
        input$scenario,
        input$period
      )
      
      matched_dataset <- raster_catalogue |>
        dplyr::filter(
          enabled,
          variable_id == input$variable_id,
          scenario == input$scenario,
          period == input$period
        ) |>
        dplyr::slice(1)
      
      variable_label <- get_variable_label(
        input$variable_id
      )
      
      scenario_label <- get_scenario_label(
        input$scenario
      )
      
      period_label <- get_period_label(
        input$period
      )
      
      if (nrow(matched_dataset) == 0) {
        showNotification(
          "No matching raster was found for the AOI test.",
          type = "error",
          duration = 8
        )
        
        return(
          tibble::tibble(
            Field = c(
              "AOI",
              "Variable",
              "Scenario",
              "Period",
              "Mean",
              "Minimum",
              "Maximum",
              "Units"
            ),
            Result = c(
              rv$aoi_name,
              variable_label,
              scenario_label,
              period_label,
              "No matching raster",
              "No matching raster",
              "No matching raster",
              "Not available"
            )
          )
        )
      }
      
      raster_path <- matched_dataset$file_path[1]
      
      if (!file.exists(raster_path)) {
        showNotification(
          paste(
            "The selected AOI test raster file could not be found:",
            raster_path
          ),
          type = "error",
          duration = NULL
        )
        
        return(
          tibble::tibble(
            Field = c(
              "AOI",
              "Variable",
              "Scenario",
              "Period",
              "Mean",
              "Minimum",
              "Maximum",
              "Units"
            ),
            Result = c(
              rv$aoi_name,
              variable_label,
              scenario_label,
              period_label,
              "Raster file not found",
              "Raster file not found",
              "Raster file not found",
              matched_dataset$units[1]
            )
          )
        )
      }
      
      units_value <- matched_dataset$units[1]
      
      if (
        length(units_value) == 0 ||
        is.na(units_value) ||
        units_value == ""
      ) {
        units_value <- "Not specified"
      }
      
      test_output_dir <- file.path(
        "outputs",
        "aoi_tests",
        safe_filename(rv$aoi_name),
        safe_filename(input$variable_id),
        safe_filename(input$scenario),
        safe_filename(input$period)
      )
      
      dir.create(
        test_output_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      showNotification(
        "Running AOI test using the current AOI, variable, scenario and period.",
        type = "message"
      )
      
      analysis_result <- tryCatch(
        {
          process_continuous_raster(
            raster_path,
            rv$aoi,
            variable_id = input$variable_id,
            scenario = input$scenario,
            period = input$period,
            output_dir = test_output_dir
          )
        },
        error = function(error) {
          
          showNotification(
            paste(
              "AOI test processing failed:",
              error$message
            ),
            type = "error",
            duration = NULL
          )
          
          NULL
        }
      )
      
      if (is.null(analysis_result)) {
        return(
          tibble::tibble(
            Field = c(
              "AOI",
              "Variable",
              "Scenario",
              "Period",
              "Mean",
              "Minimum",
              "Maximum",
              "Units"
            ),
            Result = c(
              rv$aoi_name,
              variable_label,
              scenario_label,
              period_label,
              "Analysis failed",
              "Analysis failed",
              "Analysis failed",
              units_value
            )
          )
        )
      }
      
      mean_value <- get_analysis_value(
        analysis_result,
        c(
          "mean",
          "unweighted_mean",
          "Mean",
          "mean_value",
          "mean_value_rounded",
          "average",
          "Average",
          "avg",
          "AVG"
        )
      )
      
      min_value <- get_analysis_value(
        analysis_result,
        c(
          "minimum",
          "min",
          "Minimum",
          "Min",
          "minimum_value"
        )
      )
      
      max_value <- get_analysis_value(
        analysis_result,
        c(
          "maximum",
          "max",
          "Maximum",
          "Max",
          "maximum_value"
        )
      )
      
      tibble::tibble(
        Field = c(
          "AOI",
          "Variable",
          "Scenario",
          "Period",
          "Mean",
          "Minimum",
          "Maximum",
          "Units"
        ),
        Result = c(
          rv$aoi_name,
          variable_label,
          scenario_label,
          period_label,
          round(
            mean_value,
            2
          ),
          round(
            min_value,
            2
          ),
          round(
            max_value,
            2
          ),
          units_value
        )
      )
    }
  )
  
  output$aoi_test_results <- renderDT(
    {
      if (is.null(aoi_test_results())) {
        return(
          DT::datatable(
            tibble::tibble(
              Field = "Status",
              Result = "No test result yet. Click Run test."
            ),
            rownames = FALSE,
            options = list(
              dom = "t",
              ordering = FALSE,
              paging = FALSE,
              searching = FALSE,
              info = FALSE
            )
          )
        )
      }
      
      DT::datatable(
        aoi_test_results(),
        rownames = FALSE,
        options = list(
          dom = "t",
          ordering = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    }
  )
  
}

# ------------------------------------------------------------
# RUN APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
