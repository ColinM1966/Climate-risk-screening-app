# ============================================================
# FIND RASTER FROM CATALOGUE
# Sabah Climate Risk Explorer
# ============================================================

library(dplyr)
library(stringr)

# ------------------------------------------------------------
# FIND ONE RASTER
# ------------------------------------------------------------

find_raster <- function(
    raster_catalogue,
    variable_id,
    scenario,
    period,
    require_file = TRUE
) {

  # ----------------------------------------------------------
  # CHECK REQUIRED INPUTS
  # ----------------------------------------------------------

  required_columns <- c(
    "dataset_id",
    "variable_id",
    "scenario",
    "period",
    "file_path",
    "enabled"
  )

  missing_columns <- setdiff(
    required_columns,
    names(raster_catalogue)
  )

  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "The raster catalogue is missing required columns:\n",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  if (
    is.null(variable_id) ||
      is.na(variable_id) ||
      variable_id == ""
  ) {
    stop(
      "A variable_id must be supplied.",
      call. = FALSE
    )
  }

  if (
    is.null(scenario) ||
      is.na(scenario) ||
      scenario == ""
  ) {
    stop(
      "A scenario must be supplied.",
      call. = FALSE
    )
  }

  if (
    is.null(period) ||
      is.na(period) ||
      period == ""
  ) {
    stop(
      "A period must be supplied.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # CLEAN SEARCH VALUES
  # ----------------------------------------------------------

  variable_id_clean <- str_squish(
    as.character(variable_id)
  )

  scenario_clean <- str_to_lower(
    str_squish(
      as.character(scenario)
    )
  )

  period_clean <- str_squish(
    as.character(period)
  )

  # ----------------------------------------------------------
  # FILTER CATALOGUE
  # ----------------------------------------------------------

  matches <- raster_catalogue %>%
    mutate(
      variable_id = str_squish(
        as.character(variable_id)
      ),

      scenario = str_to_lower(
        str_squish(
          as.character(scenario)
        )
      ),

      period = str_squish(
        as.character(period)
      )
    ) %>%
    filter(
      enabled,
      variable_id == variable_id_clean,
      scenario == scenario_clean,
      period == period_clean
    )

  # ----------------------------------------------------------
  # NO MATCH
  # ----------------------------------------------------------

  if (nrow(matches) == 0) {

    available_options <- raster_catalogue %>%
      filter(
        enabled,
        variable_id == variable_id_clean
      ) %>%
      distinct(
        scenario,
        period
      ) %>%
      arrange(
        scenario,
        period
      )

    available_text <- if (
      nrow(available_options) == 0
    ) {

      paste0(
        "No enabled raster records were found for variable: ",
        variable_id_clean
      )

    } else {

      paste(
        apply(
          available_options,
          1,
          function(x) {
            paste(
              x[["scenario"]],
              x[["period"]],
              sep = " / "
            )
          }
        ),
        collapse = "\n"
      )
    }

    stop(
      paste0(
        "No raster catalogue match was found.\n\n",
        "Variable: ",
        variable_id_clean,
        "\nScenario: ",
        scenario_clean,
        "\nPeriod: ",
        period_clean,
        "\n\nAvailable combinations:\n",
        available_text
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # MULTIPLE MATCHES
  # ----------------------------------------------------------

  if (nrow(matches) > 1) {

    stop(
      paste0(
        "More than one raster matched the selection:\n\n",
        paste(
          matches$dataset_id,
          collapse = "\n"
        ),
        "\n\nEach variable, scenario and period combination ",
        "should normally identify one enabled raster."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # CHECK FILE EXISTS
  # ----------------------------------------------------------

  raster_record <- matches[1, ]

  if (
    require_file &&
      !file.exists(
        raster_record$file_path
      )
  ) {

    stop(
      paste0(
        "The raster was found in the catalogue, ",
        "but the file does not exist locally:\n\n",
        raster_record$file_path,
        "\n\nDataset ID: ",
        raster_record$dataset_id
      ),
      call. = FALSE
    )
  }

  raster_record
}

# ------------------------------------------------------------
# FIND SEVERAL RASTERS
# ------------------------------------------------------------

find_rasters <- function(
    raster_catalogue,
    variable_ids,
    scenario,
    period,
    require_files = TRUE
) {

  if (
    is.null(variable_ids) ||
      length(variable_ids) == 0
  ) {
    stop(
      "At least one variable_id must be supplied.",
      call. = FALSE
    )
  }

  results <- lapply(
    variable_ids,
    function(variable_id) {

      find_raster(
        raster_catalogue = raster_catalogue,
        variable_id = variable_id,
        scenario = scenario,
        period = period,
        require_file = require_files
      )
    }
  )

  bind_rows(results)
}

# ------------------------------------------------------------
# LIST AVAILABLE SCENARIOS AND PERIODS FOR A VARIABLE
# ------------------------------------------------------------

available_raster_options <- function(
    raster_catalogue,
    variable_id
) {

  raster_catalogue %>%
    filter(
      enabled,
      variable_id == !!variable_id
    ) %>%
    distinct(
      scenario,
      period
    ) %>%
    arrange(
      scenario,
      period
    )
}

# ------------------------------------------------------------
# LIST VARIABLES AVAILABLE FOR A SCENARIO AND PERIOD
# ------------------------------------------------------------

available_variables_for_period <- function(
    raster_catalogue,
    scenario,
    period
) {

  raster_catalogue %>%
    filter(
      enabled,
      scenario == !!scenario,
      period == !!period
    ) %>%
    distinct(
      variable_id
    ) %>%
    arrange(
      variable_id
    )
}
