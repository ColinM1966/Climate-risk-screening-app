# ============================================================
# SABAH CLIMATE RISK EXPLORER
# Initial Shiny prototype
# ============================================================

library(shiny)
library(shinyjs)
library(bslib)
library(leaflet)
library(sf)
library(terra)
library(dplyr)
library(readr)
library(stringr)
library(DT)

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
# 1. BASIC CHOICES
# ------------------------------------------------------------

pathway_choices <- pathway_themes %>%
  distinct(pathway) %>%
  arrange(pathway) %>%
  pull(pathway)

if (length(pathway_choices) == 0) {
  pathway_choices <- "General Climate Risk Screening"
}

# ------------------------------------------------------------
# 2. USER INTERFACE
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
        /* Make the full page fit the browser window */
        html,
        body {
          height: 100%;
          overflow: hidden;
        }

        /* Keep the bslib page layout full height */
        .bslib-sidebar-layout {
          height: 100vh;
          max-height: 100vh;
        }

        /*
        Do not put the scrollbar on the outer sidebar.
        The outer sidebar contains the bslib resize handle, so putting
        a scrollbar there makes the scrollbar overlap with the drag handle.
        */
        .bslib-sidebar-layout .sidebar,
        .bslib-sidebar-layout > .sidebar,
        .bslib-sidebar-layout > aside,
        aside.sidebar,
        aside.bslib-sidebar,
        .sidebar {
          overflow: hidden !important;
        }

        /*
        Scroll only the inner sidebar content.
        This keeps the scrollbar away from the resize handle.
        */
        #sidebar_scroll_content {
          max-height: calc(100vh - 20px);
          overflow-y: auto !important;
          overflow-x: hidden !important;
          padding-right: 14px;
          padding-bottom: 90px;
          scrollbar-width: thin;
        }

        /* Chrome / Edge scrollbar for the inner sidebar only */
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

        /* Run analysis area */
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

        /* Keep long inputs and text inside the inner sidebar */
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

        /* Prevent horizontal page scrolling */
        body {
          overflow-x: hidden;
        }

        .leaflet-container {
          max-width: 100%;
        }

        .dataTables_wrapper {
          overflow-x: auto;
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
          "Click the map to select the centre point."
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
      
      h4("4. Select variables"),
      
      selectInput(
        inputId = "variable_id",
        label = "Select variable",
        choices = NULL
      ),
      
      h4("5. Select scenario"),
      
      selectInput(
        inputId = "scenario",
        label = NULL,
        choices = c(
          "Baseline" = "baseline",
          "SSP2-4.5" = "ssp245"
        )
      ),
      
      h4("6. Select time period"),
      
      selectInput(
        inputId = "period",
        label = NULL,
        choices = c(
          "1981–2010" = "1981-2010",
          "2041–2070" = "2041-2070"
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
            "This prototype reads available variables, scenarios",
            "and periods from configuration files."
          )
        ),
        
        p(
          paste(
            "Raster processing, exact extraction, AOI upload,",
            "drawing, buffering and downloads will be connected",
            "in the next development stages."
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 3. SERVER
# ------------------------------------------------------------

server <- function(
    input,
    output,
    session
) {
  
  # ----------------------------------------------------------
  # REACTIVE APPLICATION VALUES
  # ----------------------------------------------------------
  
  rv <- reactiveValues(
    aoi = NULL,
    aoi_name = NULL,
    result = NULL,
    cropped_raster = NULL
  )
  
  
  # ----------------------------------------------------------
  # DISPLAY ACTIVE AOI STATUS
  # ----------------------------------------------------------
  
  output$active_aoi_status <- renderText(
    {
      
      if (is.null(rv$aoi)) {
        return(
          "No AOI currently loaded."
        )
      }
      
      paste(
        "Active AOI:",
        rv$aoi_name
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
      
      paste(
        "Active AOI:",
        rv$aoi_name
      )
    }
  )
  
  output$aoi_test_selection <- renderText(
    {
      
      if (is.null(rv$aoi)) {
        return(
          "No AOI currently loaded."
        )
      }
      
      paste(
        paste(
          "AOI:",
          rv$aoi_name
        ),
        paste(
          "Variable:",
          input$variable_id
        ),
        paste(
          "Scenario:",
          input$scenario
        ),
        paste(
          "Period:",
          input$period
        ),
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
        condition = !is.null(
          rv$aoi
        )
      )
    }
  )
  # ----------------------------------------------------------
  # LOAD UPLOADED AOI AS THE ACTIVE AOI
  # ----------------------------------------------------------
  
  observeEvent(
    input$aoi_file,
    {
      
      req(input$aoi_file)
      
      uploaded_names <- input$aoi_file$name
      uploaded_paths <- input$aoi_file$datapath
      
      # Keep all shapefile components together under their original names.
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
      
      # Prefer a GeoPackage when more than one supported file is present.
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
            
            message(
              "Reading GeoPackage layer: ",
              selected_layer
            )
            
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
      
      # Replace the previous AOI with this uploaded AOI.
      rv$aoi <- uploaded_aoi
      
      rv$aoi_name <- tools::file_path_sans_ext(
        uploaded_name
      )
      
      # Clear outputs created for the previous AOI.
      rv$result <- NULL
      rv$cropped_raster <- NULL
      
      leafletProxy("map") |>
        clearGroup("Analysis result") |>
        removeControl(
          layerId = "analysis_result_legend"
        )
      
      showNotification(
        paste(
          "AOI loaded:",
          rv$aoi_name
        ),
        type = "message"
      )
    },
    ignoreInit = TRUE
  )
  
  # ----------------------------------------------------------
  # RESOLVE AOI NAME FOR DISPLAY
  # ----------------------------------------------------------
  
  displayed_aoi_name <- reactive(
    {
      if (
        is.list(rv$result) &&
        !is.null(rv$result$aoi_name) &&
        length(rv$result$aoi_name) >= 1 &&
        !is.na(rv$result$aoi_name[[1]]) &&
        rv$result$aoi_name[[1]] != ""
      ) {
        return(
          as.character(rv$result$aoi_name[[1]])
        )
      }
      
      rv$aoi_name
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
      
      req(
        rv$result$aoi_name,
        rv$result$display_name,
        rv$result$scenario,
        rv$result$period,
        rv$result$mean,
        rv$result$minimum,
        rv$result$maximum,
        rv$result$units
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
        "data/examples/Jambongan.gpkg",
        quiet = TRUE
      )
      
      validate(
        need(
          nrow(jambongan_aoi) > 0,
          "The Jambongan GeoPackage contains no spatial features."
        )
      )
      
      rv$aoi <- prepare_aoi(
        jambongan_aoi
      )
      
      rv$aoi_name <- "Jambongan"
      rv$result <- NULL
      rv$cropped_raster <- NULL
      
      leafletProxy("map") |>
        clearGroup("Analysis result") |>
        removeControl(layerId = "analysis_result_legend")
      
      showNotification(
        paste(
          "AOI loaded:",
          rv$aoi_name
        ),
        type = "message"
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
      
      # Transform the active AOI to WGS 84 for Leaflet.
      map_aoi <- sf::st_transform(
        rv$aoi,
        4326
      )
      
      message(
        "\n---------------- MAP AOI VS ANALYSIS AOI ----------------"
      )
      
      message(
        "AOI name: ",
        rv$aoi_name
      )
      
      message(
        "Analysis AOI CRS: ",
        sf::st_crs(rv$aoi)$input
      )
      
      message(
        "Map AOI CRS: ",
        sf::st_crs(map_aoi)$input
      )
      
      message(
        "Analysis AOI feature count: ",
        nrow(rv$aoi)
      )
      
      message(
        "Map AOI feature count: ",
        nrow(map_aoi)
      )
      
      message(
        "Analysis AOI bbox:"
      )
      
      print(
        sf::st_bbox(
          rv$aoi
        )
      )
      
      message(
        "Map AOI bbox:"
      )
      
      print(
        sf::st_bbox(
          map_aoi
        )
      )
      
      message(
        "Analysis AOI geometry type:"
      )
      
      print(
        unique(
          sf::st_geometry_type(
            rv$aoi
          )
        )
      )
      
      message(
        "Map AOI geometry type:"
      )
      
      print(
        unique(
          sf::st_geometry_type(
            map_aoi
          )
        )
      )
      
      message(
        "---------------------------------------------------------\n"
      )
      
      
      # Calculate the AOI extent for map zooming.
      aoi_bbox <- sf::st_bbox(
        map_aoi
      )
      
      # Replace the previous AOI and zoom to the current AOI.
      leafletProxy(
        "map"
      ) |>
        clearGroup(
          "AOI"
        ) |>
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
  # DISPLAY SELECTED ANALYSIS RASTER AND LEGEND
  # ----------------------------------------------------------
  
  observeEvent(
    rv$cropped_raster,
    {
      
      req(
        rv$cropped_raster,
        input$variable_id,
        input$scenario,
        input$period
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
      
      selected_metadata <- variable_metadata |>
        dplyr::filter(
          variable_id == input$variable_id
        )
      
      selected_record <- raster_catalogue |>
        dplyr::filter(
          enabled,
          variable_id == input$variable_id,
          scenario == input$scenario,
          period == input$period
        )
      
      validate(
        need(
          nrow(selected_metadata) >= 1,
          "No metadata was found for the selected variable."
        ),
        need(
          nrow(selected_record) == 1,
          "No unique raster record was found."
        )
      )
      
      variable_name <- selected_metadata$display_name[[1]]
      
      if (
        is.na(variable_name) ||
        variable_name == ""
      ) {
        variable_name <- input$variable_id
      }
      
      units_value <- selected_record$units[[1]]
      
      if (
        is.na(units_value) ||
        units_value == ""
      ) {
        units_value <- "Not specified"
      }
      
      palette_name <- dplyr::case_when(
        input$variable_id == "Bio05" ~ "inferno",
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
      
      leafletProxy(
        "map"
      ) |>
        clearGroup(
          "Analysis result"
        ) |>
        removeControl(
          layerId = "analysis_result_legend"
        ) |>
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
  # INITIAL MAP
  # ----------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 18
      )
    ) %>%
      
      addProviderTiles(
        providers$OpenStreetMap,
        group = "OpenStreetMap"
      ) %>%
      
      setView(
        lng = 117.0,
        lat = 5.3,
        zoom = 7
      ) %>%
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(
          metric = TRUE,
          imperial = FALSE
        )
      ) %>%
      
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap"
        ),
        
        overlayGroups = c(
          "AOI",
          "Analysis result"
        ),
        
        options = layersControlOptions(
          collapsed = TRUE
        )
      )
  })
  
  # ----------------------------------------------------------
  # UPDATE THEMES FROM SELECTED PATHWAY
  # ----------------------------------------------------------
  
  observeEvent(
    input$pathway,
    {
      
      available_themes <- pathway_themes %>%
        filter(
          pathway == input$pathway
        ) %>%
        arrange(
          display_order
        ) %>%
        distinct(
          theme,
          .keep_all = TRUE
        )
      
      theme_choices <- available_themes$theme
      
      if (length(theme_choices) == 0) {
        
        theme_choices <- theme_variables %>%
          distinct(theme) %>%
          arrange(theme) %>%
          pull(theme)
      }
      
      default_theme <- available_themes %>%
        filter(
          default_enabled
        ) %>%
        pull(theme)
      
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
  # UPDATE VARIABLE FROM SELECTED THEME
  # ----------------------------------------------------------
  
  observeEvent(
    input$theme,
    {
      
      req(input$theme)
      
      theme_variable_ids <- theme_variables %>%
        filter(
          theme == input$theme
        ) %>%
        pull(
          variable_id
        )
      
      additional_variable_ids <- c(
        "Bio05",
        "Bio017"
      )
      
      enabled_variable_ids <- raster_catalogue %>%
        filter(
          enabled
        ) %>%
        distinct(
          variable_id
        ) %>%
        pull(
          variable_id
        )
      
      available_variable_ids <- union(
        theme_variable_ids,
        intersect(
          additional_variable_ids,
          enabled_variable_ids
        )
      )
      
      available_variables <- tibble(
        variable_id = available_variable_ids
      ) %>%
        left_join(
          variable_metadata %>%
            select(
              variable_id,
              display_name
            ),
          by = "variable_id"
        ) %>%
        mutate(
          display_name = if_else(
            is.na(display_name) |
              display_name == "",
            variable_id,
            display_name
          )
        )
      
      variable_choices <- setNames(
        available_variables$variable_id,
        available_variables$display_name
      )
      
      default_variable <- if (
        "Bio05" %in% available_variables$variable_id
      ) {
        "Bio05"
      } else if (
        "Bio017" %in% available_variables$variable_id
      ) {
        "Bio017"
      } else {
        available_variables$variable_id[[1]]
      }
      
      updateSelectInput(
        session = session,
        inputId = "variable_id",
        choices = variable_choices,
        selected = default_variable
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
      
      req(
        input$variable_id
      )
      
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
      
      scenario_labels <- c(
        baseline = "Baseline",
        ssp245 = "SSP2-4.5"
      )
      
      available_scenarios <- available_scenarios[
        available_scenarios %in% names(
          scenario_labels
        )
      ]
      
      validate(
        need(
          length(available_scenarios) > 0,
          "No scenario is available for the selected variable."
        )
      )
      
      scenario_choices <- stats::setNames(
        available_scenarios,
        scenario_labels[
          available_scenarios
        ]
      )
      
      selected_scenario <- if (
        "ssp245" %in% available_scenarios
      ) {
        "ssp245"
      } else {
        available_scenarios[[1]]
      }
      
      updateSelectInput(
        session = session,
        inputId = "scenario",
        choices = scenario_choices,
        selected = selected_scenario
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
      
      period_labels <- c(
        "1981-2010" = "1981–2010",
        "2041-2070" = "2041–2070"
      )
      
      available_periods <- available_periods[
        available_periods %in% names(
          period_labels
        )
      ]
      
      validate(
        need(
          length(available_periods) > 0,
          "No period is available for the selected scenario."
        )
      )
      
      period_choices <- stats::setNames(
        available_periods,
        period_labels[
          available_periods
        ]
      )
      
      preferred_period <- if (
        input$scenario == "baseline" &&
        "1981-2010" %in% available_periods
      ) {
        "1981-2010"
      } else if (
        input$scenario == "ssp245" &&
        "2041-2070" %in% available_periods
      ) {
        "2041-2070"
      } else {
        available_periods[[1]]
      }
      
      updateSelectInput(
        session = session,
        inputId = "period",
        choices = period_choices,
        selected = preferred_period
      )
    },
    ignoreInit = FALSE
  )
  
  # ----------------------------------------------------------
  # DISPLAY CURRENT SELECTION
  # ----------------------------------------------------------
  
  
  output$selection_status <- renderUI({
    
    if (
      is.null(input$variable_id) ||
      input$variable_id == ""
    ) {
      
      return(
        div(
          class = "text-muted",
          "Select a variable."
        )
      )
    }
    
    tagList(
      
      strong(
        "Current selection"
      ),
      
      tags$br(),
      
      paste0(
        "Active AOI: ",
        ifelse(
          is.null(rv$aoi_name),
          "None selected",
          rv$aoi_name
        )
      ),
      
      tags$br(),
      
      paste0(
        "Theme: ",
        input$theme
      ),
      
      tags$br(),
      
      paste0(
        "Variable: ",
        input$variable_id
      ),
      
      tags$br(),
      
      paste0(
        "Scenario: ",
        input$scenario
      ),
      
      tags$br(),
      
      paste0(
        "Period: ",
        input$period
      )
    )
  })
  
  # ----------------------------------------------------------
  # DATA AVAILABILITY TABLE
  # ----------------------------------------------------------
  
  
  output$catalogue_table <- renderDT({
    
    raster_catalogue %>%
      select(
        dataset_id,
        variable_id,
        scenario,
        period,
        file_path,
        enabled
      ) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          scrollY = "55vh",
          scrollCollapse = TRUE
        )
      )
  })
  
  # ----------------------------------------------------------
  # RUN ANALYSIS BUTTON
  # ----------------------------------------------------------
  
  observeEvent(
    input$run_analysis,
    {
      
      validation_errors <- character(0)
      
      if (
        is.null(rv$aoi) ||
        is.null(rv$aoi_name)
      ) {
        validation_errors <- c(
          validation_errors,
          "Select or upload an AOI before running the analysis."
        )
      }
      
      if (
        is.null(input$variable_id) ||
        input$variable_id == ""
      ) {
        validation_errors <- c(
          validation_errors,
          "Select a variable."
        )
      }
      
      if (
        is.null(input$scenario) ||
        input$scenario == ""
      ) {
        validation_errors <- c(
          validation_errors,
          "Select a scenario."
        )
      }
      
      if (
        is.null(input$period) ||
        input$period == ""
      ) {
        validation_errors <- c(
          validation_errors,
          "Select a time period."
        )
      }
      
      if (length(validation_errors) > 0) {
        
        showNotification(
          paste(
            validation_errors,
            collapse = "\n"
          ),
          type = "error",
          duration = 10
        )
        
        return()
      }
      
      withProgress(
        message = "Running climate analysis",
        value = 0,
        {
          
          setProgress(
            value = 0.25,
            detail = "Finding raster"
          )
          
          selected_record <- tryCatch(
            {
              find_raster(
                raster_catalogue = raster_catalogue,
                variable_id = input$variable_id,
                scenario = input$scenario,
                period = input$period
              )
            },
            error = function(e) {
              
              showNotification(
                paste(
                  "Raster lookup failed:",
                  conditionMessage(e)
                ),
                type = "error",
                duration = NULL
              )
              
              NULL
            }
          )
          
          if (is.null(selected_record)) {
            return()
          }
          
          if (!is.data.frame(selected_record)) {
            
            showNotification(
              "find_raster() did not return a data-frame record.",
              type = "error",
              duration = NULL
            )
            
            return()
          }
          
          if (nrow(selected_record) != 1) {
            
            showNotification(
              "No unique raster was found for this selection.",
              type = "error",
              duration = NULL
            )
            
            return()
          }
          
          if (
            !"file_path" %in% names(selected_record) ||
            is.na(selected_record$file_path[[1]]) ||
            selected_record$file_path[[1]] == ""
          ) {
            
            showNotification(
              "The selected raster record has no valid file path.",
              type = "error",
              duration = NULL
            )
            
            return()
          }
          
          raster_path <- file.path(
            getwd(),
            selected_record$file_path[[1]]
          )
          
          raster_path <- normalizePath(
            raster_path,
            winslash = "/",
            mustWork = FALSE
          )
          
          if (!file.exists(raster_path)) {
            
            showNotification(
              "The selected raster file could not be found.",
              type = "error",
              duration = NULL
            )
            
            return()
          }
          
          setProgress(
            value = 0.50,
            detail = "Preparing AOI"
          )
          
          req(
            rv$aoi
          )
          
          processing_output_dir <- file.path(
            getwd(),
            "outputs",
            "app_processing",
            stringr::str_replace_all(
              rv$aoi_name,
              "[^A-Za-z0-9_-]",
              "_"
            ),
            input$variable_id,
            input$scenario,
            input$period
          )
          
          dir.create(
            processing_output_dir,
            recursive = TRUE,
            showWarnings = FALSE
          )
          
          setProgress(
            value = 0.75,
            detail = "Calculating statistics"
          )
          
          tryCatch(
            {
              
              message(
                "Running analysis for AOI: ",
                rv$aoi_name
              )
              
              print(
                sf::st_bbox(
                  rv$aoi
                )
              )
              
              # Temporarily save the exact active AOI passed into raster processing.
              dir.create(
                file.path(
                  "outputs",
                  "tests"
                ),
                recursive = TRUE,
                showWarnings = FALSE
              )
              
              sf::st_write(
                rv$aoi,
                file.path(
                  "outputs",
                  "tests",
                  "current_active_aoi.gpkg"
                ),
                delete_dsn = TRUE,
                quiet = TRUE
              )
              
              analysis_result <- process_continuous_raster(
                raster_file = raster_path,
                variable_id = input$variable_id,
                scenario = input$scenario,
                period = input$period,
                aoi = rv$aoi,
                output_dir = processing_output_dir
              )
              
              selected_metadata <- variable_metadata |>
                dplyr::filter(
                  variable_id == input$variable_id
                )
              
              selected_record <- raster_catalogue |>
                dplyr::filter(
                  enabled,
                  variable_id == input$variable_id,
                  scenario == input$scenario,
                  period == input$period
                )
              
              if (nrow(selected_metadata) < 1) {
                stop(
                  "No variable metadata was found for the selected variable."
                )
              }
              
              if (nrow(selected_record) != 1) {
                stop(
                  paste(
                    "No unique raster record was found for",
                    "the selected variable, scenario and period."
                  )
                )
              }
              
              variable_display_name <- selected_metadata$display_name[[1]]
              
              if (
                is.na(variable_display_name) ||
                variable_display_name == ""
              ) {
                variable_display_name <- input$variable_id
              }
              
              units_value <- selected_record$units[[1]]
              
              if (
                is.na(units_value) ||
                units_value == ""
              ) {
                units_value <- "Not specified"
              }
              
              get_analysis_value <- function(
    result_object,
    possible_names
              ) {
                
                matching_names <- possible_names[
                  possible_names %in% names(result_object)
                ]
                
                if (length(matching_names) < 1) {
                  stop(
                    paste(
                      "The analysis result is missing:",
                      paste(
                        possible_names,
                        collapse = " or "
                      )
                    )
                  )
                }
                
                value <- result_object[[matching_names[1]]]
                
                if (length(value) == 0) {
                  stop(
                    paste(
                      "The analysis result value is empty:",
                      matching_names[1]
                    )
                  )
                }
                
                value[[1]]
              }
              
              mean_value <- get_analysis_value(
                analysis_result,
                c(
                  "mean",
                  "unweighted_mean"
                )
              )
              
              minimum_value <- get_analysis_value(
                analysis_result,
                c(
                  "minimum",
                  "min"
                )
              )
              
              maximum_value <- get_analysis_value(
                analysis_result,
                c(
                  "maximum",
                  "max"
                )
              )
              
              rv$result <- list(
                aoi_name = rv$aoi_name,
                variable_id = input$variable_id,
                display_name = variable_display_name,
                scenario = input$scenario,
                period = input$period,
                mean = mean_value,
                minimum = minimum_value,
                maximum = maximum_value,
                units = units_value
              )
              
              if (
                is.list(analysis_result) &&
                "cropped_raster" %in% names(analysis_result)
              ) {
                
                rv$cropped_raster <-
                  analysis_result$cropped_raster
                
              } else if (
                is.data.frame(analysis_result) &&
                "cropped_raster" %in% names(analysis_result)
              ) {
                
                rv$cropped_raster <-
                  analysis_result$cropped_raster[[1]]
                
              } else {
                
                rv$cropped_raster <- NULL
              }
              
              setProgress(
                value = 1,
                detail = "Complete"
              )
              
              showNotification(
                paste(
                  "Analysis completed for:",
                  rv$result$aoi_name
                ),
                type = "message"
              )
            },
    
    error = function(e) {
      
      rv$result <- NULL
      rv$cropped_raster <- NULL
      
      showNotification(
        paste(
          "Analysis failed:",
          conditionMessage(e)
        ),
        type = "error",
        duration = NULL
      )
    }
          )
        }
      )
    }
  )
  
  # ----------------------------------------------------------
  # TEMPORARY AOI TEST RESULTS
  # ----------------------------------------------------------
  
  aoi_test_results <- eventReactive(
    input$run_aoi_test,
    {
      
      req(
        rv$aoi,
        rv$aoi_name,
        input$variable_id,
        input$scenario,
        input$period
      )
      
      matched_dataset <- raster_catalogue %>%
        filter(
          enabled,
          variable_id == input$variable_id,
          scenario == input$scenario,
          period == input$period
        ) %>%
        slice(1)
      
      if (nrow(matched_dataset) == 0) {
        
        showNotification(
          "No matching raster was found.",
          type = "error"
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
              input$variable_id,
              input$scenario,
              input$period,
              "No matching raster",
              "No matching raster",
              "No matching raster",
              "Not available"
            )
          )
        )
      }
      
      variable_display_name <- variable_metadata %>%
        filter(
          variable_id == input$variable_id
        ) %>%
        pull(
          display_name
        )
      
      if (
        length(variable_display_name) == 0 ||
        is.na(variable_display_name[1]) ||
        variable_display_name[1] == ""
      ) {
        variable_display_name <- input$variable_id
      }
      
      scenario_display_name <- case_when(
        input$scenario == "baseline" ~ "Baseline",
        input$scenario == "ssp126" ~ "SSP1-2.6",
        input$scenario == "ssp245" ~ "SSP2-4.5",
        input$scenario == "ssp370" ~ "SSP3-7.0",
        input$scenario == "ssp585" ~ "SSP5-8.5",
        TRUE ~ input$scenario
      )
      
      units_value <- matched_dataset$units[1]
      
      if (
        length(units_value) == 0 ||
        is.na(units_value) ||
        units_value == ""
      ) {
        units_value <- "Not specified"
      }
      
      raster_path <- file.path(
        getwd(),
        matched_dataset$file_path[1]
      )
      
      raster_path <- normalizePath(
        raster_path,
        winslash = "/",
        mustWork = FALSE
      )
      
      if (!file.exists(raster_path)) {
        
        showNotification(
          "The selected raster file could not be found.",
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
              variable_display_name[1],
              scenario_display_name,
              input$period,
              "Raster file not found",
              "Raster file not found",
              "Raster file not found",
              units_value
            )
          )
        )
      }
      
      test_output_dir <- file.path(
        getwd(),
        "outputs",
        "app_processing",
        stringr::str_replace_all(
          rv$aoi_name,
          "[^A-Za-z0-9_-]",
          "_"
        ),
        input$variable_id,
        input$scenario,
        input$period,
        "aoi_test"
      )
      
      dir.create(
        test_output_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      analysis_result <- tryCatch(
        {
          process_continuous_raster(
            raster_file = raster_path,
            variable_id = input$variable_id,
            scenario = input$scenario,
            period = input$period,
            aoi = rv$aoi,
            output_dir = test_output_dir
          )
        },
        error = function(e) {
          
          showNotification(
            paste(
              "AOI test analysis failed:",
              conditionMessage(e)
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
              variable_display_name[1],
              scenario_display_name,
              input$period,
              "Analysis failed",
              "Analysis failed",
              "Analysis failed",
              units_value
            )
          )
        )
      }
      
      get_analysis_value <- function(
    result_object,
    possible_names
      ) {
        
        matching_names <- possible_names[
          possible_names %in% names(result_object)
        ]
        
        if (length(matching_names) < 1) {
          stop(
            paste(
              "The AOI test result is missing:",
              paste(
                possible_names,
                collapse = " or "
              )
            )
          )
        }
        
        value <- result_object[[matching_names[1]]]
        
        if (length(value) == 0) {
          stop(
            paste(
              "The AOI test result value is empty:",
              matching_names[1]
            )
          )
        }
        
        value[[1]]
      }
      
      mean_value <- get_analysis_value(
        analysis_result,
        c(
          "mean",
          "unweighted_mean"
        )
      )
      
      minimum_value <- get_analysis_value(
        analysis_result,
        c(
          "minimum",
          "min"
        )
      )
      
      maximum_value <- get_analysis_value(
        analysis_result,
        c(
          "maximum",
          "max"
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
          variable_display_name[1],
          scenario_display_name,
          input$period,
          round(
            mean_value,
            2
          ),
          round(
            minimum_value,
            2
          ),
          round(
            maximum_value,
            2
          ),
          units_value
        )
      )
    }
  )
  
  output$aoi_test_results <- renderDT({
    
    req(
      aoi_test_results()
    )
    
    datatable(
      aoi_test_results(),
      rownames = FALSE,
      colnames = c(
        "Field",
        "Result"
      ),
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "35vh",
        scrollCollapse = TRUE
      )
    )
  })
}

# ------------------------------------------------------------
# 4. RUN APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
