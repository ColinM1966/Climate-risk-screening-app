# ============================================================
# TEST INUNDATION RASTER PROCESSING
# ============================================================

library(sf)
library(dplyr)

# ------------------------------------------------------------
# LOAD FUNCTIONS
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
    "process_inundation_raster.R"
  )
)

# ------------------------------------------------------------
# LOAD CONFIGURATION
# ------------------------------------------------------------

app_config <- load_app_config(
  config_dir = "config"
)

raster_catalogue <-
  app_config$raster_catalogue

# ------------------------------------------------------------
# READ AND PREPARE JAMBONGAN AOI
# ------------------------------------------------------------

jambongan_raw <- read_aoi_file(
  file_path = file.path(
    "data",
    "examples",
    "Jambongan.gpkg"
  ),
  layer = "Jambongan"
)

jambongan_aoi <- prepare_aoi(
  aoi = jambongan_raw,
  aoi_name = "Jambongan",
  dissolve = TRUE
)

print_aoi_summary(
  jambongan_aoi
)

# ------------------------------------------------------------
# FIND INUNDATION RASTER
# ------------------------------------------------------------

inundation_record <- find_raster(
  raster_catalogue = raster_catalogue,
  variable_id = "INUNDATION_RP100",
  scenario = "baseline",
  period = "2021-2050"
)

print(
  inundation_record
)

# ------------------------------------------------------------
# PROCESS INUNDATION RASTER
# ------------------------------------------------------------

inundation_result <-
  process_inundation_catalogue_raster(
    raster_record = inundation_record,
    aoi_sf = jambongan_aoi,
    output_dir = file.path(
      "outputs",
      "jobs",
      "Jambongan_inundation_test"
    )
  )

print(
  inundation_result
)
