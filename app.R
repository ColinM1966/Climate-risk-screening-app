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
# LABEL HELPERS
# ------------------------------------------------------------

scenario_labels <- c(
  baseline = "Baseline",
  ssp126 = "SSP1-2.6",
  ssp245 = "SSP2-4.5",
  ssp370 = "SSP3-7.0",
  ssp585 = "SSP5-8.5"
)

period_labels <- c(
  "1981-2010" = "1981–2010",
  "2011-2040" = "2011–2040",
  "2041-2070" = "2041–2070",
  "2071-2100" = "2071–2100"
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
    WBGTmax = "Maximum WBGT"
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
  
  sidebar = sidebar(
    
    width = 360,
    
    div(
      id = "sidebar_scroll_content",
      
      h4("1. Select area"),
      
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
        )
      ),
      
      conditionalPanel(
        condition = "input.aoi_method == 'draw'",
        
        helpText(
          "Use the polygon tool on the map to draw an AOI. Click points to trace the boundary, then click the first point again to finish the polygon. The finished polygon will become the active AOI."
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
          "Loads data/examples/Jambongan.gpkg."
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
          "Enter a buffer distance, then click the map to create a circular AOI around that point. The buffer will become the active AOI."
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
      
      h4("2. Select user pathway"),
      
      selectInput(
        inputId = "pathway",
        label = NULL,
        choices = pathway_choices,
        selected = pathway_choices[1]
      ),
      
      h4("3. Select theme"),
      
      selectInput(
        inputId = "theme",
        label = NULL,
        choices = NULL
      ),
      
      h4("4. Select variable"),
      
      selectInput(
        inputId = "variable_id",
        label = NULL,
        choices = NULL
      ),
      
      h4("5. Select scenario"),
      
      selectInput(
        inputId = "scenario",
        label = NULL,
        choices = NULL
      ),
      
      h4("6. Select time period"),
      
      selectInput(
        inputId = "period",
        label = NULL,
        choices = NULL
      ),
      
      h4("7. Comparison options"),
      
      checkboxInput(
        inputId = "run_comparison",
        label = "Run scenario and period comparison",
        value = FALSE
      ),
      
      h4("8. Output options"),
      
      checkboxInput(
        inputId = "create_cropped_raster",
        label = "Create cropped raster output",
        value = FALSE
      ),
      
      helpText(
        "Only turn this on if you need to display or download a clipped GeoTIFF. Leaving it off keeps the map simpler and avoids offering unnecessary raster outputs."
      ),
      
      conditionalPanel(
        condition = "input.run_comparison == true",
        
        selectInput(
          inputId = "comparison_scenarios",
          label = "Compare scenarios",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        
        selectInput(
          inputId = "comparison_periods",
          label = "Compare periods",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      ),
      
      hr(),
      
      div(
        id = "run_analysis_scroll_zone",
        
        actionButton(
          inputId = "run_analysis",
          label = "Run analysis",
          class = "btn-primary",
          width = "100%"
        )
      ),
      
      br(),
      br(),
      
      uiOutput(
        "selection_status"
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
          "The graph shows mean values only. Minimum and maximum values are shown in the table. Interpretation depends on the selected variable; for some variables, higher values indicate greater concern, while for others, lower values indicate drier conditions."
        ),
        
        div(
          class = "results-note",
          "This is especially important for Bio017 and PPETmin because they are lower-is-drier variables."
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
        
        DTOutput(
          "catalogue_table"
        )
      )
    ),
    
    nav_panel(
      title = "About",
      
      card(
        card_header(
          "Prototype status"
        ),
        
        p(
          paste(
            "This prototype demonstrates AOI-based climate raster",
            "screening using uploaded polygons, drawn polygons,",
            "point buffers, and a built-in Jambongan test AOI."
          )
        ),
        
        p(
          paste(
            "Draw polygon is active in this prototype.",
            "Draw an AOI on the map, then run analysis."
          )
        ),
        
        p(
          paste(
            "Results are screening summaries only.",
            "They describe raster values within the selected AOI",
            "and should not be interpreted as a final risk score."
          )
        ),
        
        p(
          paste(
            "Drawn-polygon and point-buffer AOIs are intended for",
            "rapid testing and exploratory screening.",
            "For formal reporting, users should use a checked boundary",
            "from a verified spatial file wherever possible."
          )
        ),
        
        p(
          paste(
            "No combined overall-risk score is produced at this stage.",
            "A combined score will only be added after the scoring method,",
            "weights and assumptions are agreed and documented."
          )
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
    cropped_raster = NULL
  )
  
  # ----------------------------------------------------------
  # CLEAR HELPERS
  # ----------------------------------------------------------
  
  clear_analysis_outputs <- function() {
    
    rv$result <- NULL
    rv$comparison_results <- NULL
    rv$comparison_missing <- NULL
    rv$cropped_raster <- NULL
  }
  
  clear_analysis_map <- function() {
    
    leafletProxy("map") |>
      clearGroup("Analysis result") |>
      removeControl(
        layerId = "analysis_result_legend"
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
  # ENABLE / DISABLE RUN ANALYSIS BUTTON
  # ----------------------------------------------------------
  
  observe(
    {
      shinyjs::toggleState(
        id = "run_analysis",
        condition = !is.null(rv$aoi)
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
      
      pilot_variable_ids <- intersect(
        c(
          "Bio05",
          "Bio017"
        ),
        enabled_variable_ids
      )
      
      available_variable_ids <- unique(
        c(
          available_variable_ids,
          pilot_variable_ids
        )
      )
      
      if (length(available_variable_ids) == 0) {
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
      
      selected_period <- if (
        input$scenario == "baseline" &&
        "1981-2010" %in% available_periods
      ) {
        "1981-2010"
      } else if (
        input$scenario != "baseline" &&
        "2041-2070" %in% available_periods
      ) {
        "2041-2070"
      } else {
        available_periods[1]
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
      
      default_periods <- intersect(
        c(
          "1981-2010",
          "2041-2070"
        ),
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
      tagList(
        strong("Current selection"),
        br(),
        paste("Variable:", input$variable_id),
        br(),
        paste("Scenario:", input$scenario),
        br(),
        paste("Period:", input$period)
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
          
          baseline_mean <- rv$comparison_results |>
            dplyr::filter(
              Scenario_ID == "baseline",
              Period_ID == "1981-2010"
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
      
      paste(
        "Skipped",
        nrow(rv$comparison_missing),
        "scenario-period combination(s) because the raster was unavailable, missing, or did not contain valid cells within the active AOI."
      )
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
      base_note <- paste(
        "Results summarise the selected raster within the active AOI.",
        "The raster resolution reflects the source climate dataset and should not be interpreted as fine-scale local variation."
      )
      
      if (
        !is.null(input$variable_id) &&
        input$variable_id == "Bio05"
      ) {
        return(
          paste(
            base_note,
            "For Bio05, higher values indicate hotter maximum temperature conditions."
          )
        )
      }
      
      if (
        !is.null(input$variable_id) &&
        input$variable_id == "Bio017"
      ) {
        return(
          paste(
            base_note,
            "For Bio017, lower values indicate lower precipitation in the driest quarter and therefore drier conditions."
          )
        )
      }
      
      if (
        !is.null(input$variable_id) &&
        input$variable_id == "PPETmin"
      ) {
        return(
          paste(
            base_note,
            "For PPETmin, lower values indicate drier moisture-balance conditions."
          )
        )
      }
      
      if (
        !is.null(input$variable_id) &&
        input$variable_id == "WBGTmax"
      ) {
        return(
          paste(
            base_note,
            "For WBGTmax, higher values indicate hotter heat-stress conditions. Interpret this as a screening indicator, not as a site-level occupational safety assessment."
          )
        )
      }
      
      base_note
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
          Scenario = vapply(
            scenario,
            get_scenario_label,
            character(1)
          ),
          Period = vapply(
            period,
            get_period_label,
            character(1)
          )
        ) |>
        dplyr::select(
          dplyr::any_of(
            c(
              "dataset_id",
              "variable_id",
              "Scenario",
              "Period",
              "file_path",
              "units",
              "enabled"
            )
          )
        )
      
      DT::datatable(
        display_catalogue,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
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
