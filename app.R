# ============================================================
# SABAH CLIMATE RISK EXPLORER
# Initial Shiny prototype
# ============================================================

library(shiny)
library(bslib)
library(leaflet)
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

  sidebar = sidebar(

    width = 360,

    h4("1. Select area"),

    radioButtons(
      inputId = "aoi_method",
      label = NULL,
      choices = c(
        "Upload polygon" = "upload",
        "Draw polygon" = "draw",
        "Select point and buffer" = "point"
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

    checkboxGroupInput(
      inputId = "variables",
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

    hr(),

    actionButton(
      inputId = "run_analysis",
      label = "Run analysis",
      class = "btn-primary",
      width = "100%"
    ),

    br(),
    br(),

    uiOutput(
      "selection_status"
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

        DTOutput(
          "results_table"
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
# 8. SERVER
# ------------------------------------------------------------

server <- function(
    input,
    output,
    session
) {

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
  # UPDATE VARIABLES FROM SELECTED THEME
  # ----------------------------------------------------------

  observeEvent(
    input$theme,
    {

      req(input$theme)

      available_variables <- theme_variables %>%
        filter(
          theme == input$theme
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
            is.na(display_name),
            variable_id,
            display_name
          )
        ) %>%
        arrange(
          display_order
        )

      variable_choices <- setNames(
        available_variables$variable_id,
        available_variables$display_name
      )

      default_variables <- available_variables %>%
        filter(
          default_selected
        ) %>%
        pull(variable_id)

      updateCheckboxGroupInput(
        session = session,
        inputId = "variables",
        choices = variable_choices,
        selected = default_variables
      )
    },
    ignoreInit = FALSE
  )

  # ----------------------------------------------------------
  # UPDATE SCENARIOS FROM SELECTED VARIABLES
  # ----------------------------------------------------------

  observeEvent(
    input$variables,
    {

      req(
        length(input$variables) > 0
      )

      available_scenarios <- raster_catalogue %>%
        filter(
          enabled,
          variable_id %in% input$variables
        ) %>%
        distinct(
          scenario
        ) %>%
        arrange(
          factor(
            scenario,
            levels = c(
              "baseline",
              "ssp126",
              "ssp245",
              "ssp370",
              "ssp585"
            )
          )
        ) %>%
        pull(
          scenario
        )

      selected_scenario <- if (
        "ssp245" %in% available_scenarios
      ) {
        "ssp245"
      } else {
        available_scenarios[1]
      }

      updateSelectInput(
        session = session,
        inputId = "scenario",
        choices = available_scenarios,
        selected = selected_scenario
      )
    },
    ignoreInit = FALSE
  )

  # ----------------------------------------------------------
  # UPDATE PERIODS FROM SELECTED VARIABLES AND SCENARIO
  # ----------------------------------------------------------

  observeEvent(
    list(
      input$variables,
      input$scenario
    ),
    {

      req(
        length(input$variables) > 0,
        input$scenario
      )

      available_periods <- raster_catalogue %>%
        filter(
          enabled,
          variable_id %in% input$variables,
          scenario == input$scenario
        ) %>%
        distinct(
          period
        ) %>%
        arrange(
          period
        ) %>%
        pull(
          period
        )

      preferred_period <- case_when(
        input$scenario == "baseline" &&
          "1981-2010" %in% available_periods ~
          "1981-2010",

        input$scenario != "baseline" &&
          "2041-2070" %in% available_periods ~
          "2041-2070",

        TRUE ~
          available_periods[1]
      )

      updateSelectInput(
        session = session,
        inputId = "period",
        choices = available_periods,
        selected = preferred_period
      )
    },
    ignoreInit = FALSE
  )

  # ----------------------------------------------------------
  # DISPLAY CURRENT SELECTION
  # ----------------------------------------------------------

  output$selection_status <- renderUI({

    selected_variables <- input$variables

    if (
      is.null(selected_variables) ||
      length(selected_variables) == 0
    ) {

      return(
        div(
          class = "text-muted",
          "Select at least one variable."
        )
      )
    }

    tagList(

      strong(
        "Current selection"
      ),

      tags$br(),

      paste0(
        "Theme: ",
        input$theme
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
      ),

      tags$br(),

      paste0(
        "Variables: ",
        paste(
          selected_variables,
          collapse = ", "
        )
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
          scrollX = TRUE
        )
      )
  })

  # ----------------------------------------------------------
  # PLACEHOLDER RESULTS TABLE
  # ----------------------------------------------------------

  analysis_results <- reactiveVal(
    tibble(
      message =
        "No analysis has been run."
    )
  )

  output$results_table <- renderDT({

    datatable(
      analysis_results(),
      rownames = FALSE,
      options = list(
        dom = "t"
      )
    )
  })

  # ----------------------------------------------------------
  # RUN ANALYSIS BUTTON
  # ----------------------------------------------------------

  observeEvent(
    input$run_analysis,
    {

      if (
        is.null(input$variables) ||
          length(input$variables) == 0
      ) {

        showNotification(
          "Select at least one variable.",
          type = "error"
        )

        return()
      }

      if (
        is.null(input$scenario) ||
          is.null(input$period)
      ) {

        showNotification(
          "Select a scenario and time period.",
          type = "error"
        )

        return()
      }

      selected_datasets <- raster_catalogue %>%
        filter(
          enabled,
          variable_id %in% input$variables,
          scenario == input$scenario,
          period == input$period
        )

      missing_variables <- setdiff(
        input$variables,
        selected_datasets$variable_id
      )

      if (length(missing_variables) > 0) {

        showNotification(
          paste0(
            "No matching raster was found for: ",
            paste(
              missing_variables,
              collapse = ", "
            )
          ),
          type = "error",
          duration = 10
        )

        return()
      }

      analysis_results(
        selected_datasets %>%
          transmute(
            variable = variable_id,
            scenario = scenario,
            period = period,
            raster = file_path,
            status =
              "Ready for AOI and raster processing"
          )
      )

      showNotification(
        paste(
          "Configuration selection validated.",
          "Raster processing will be connected next."
        ),
        type = "message"
      )
    }
  )
}

# ------------------------------------------------------------
# 9. RUN APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
