library(sf)
library(dplyr)

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

# Load configuration
app_config <- load_app_config(
  config_dir = "config"
)

raster_catalogue <-
  app_config$raster_catalogue

# Read example AOI
jambongan_raw <- read_aoi_file(
  file_path = file.path(
    "data",
    "examples",
    "Jambongan.gpkg"
  ),
  layer = "Jambongan"
)

# Prepare AOI
jambongan_aoi <- prepare_aoi(
  aoi = jambongan_raw,
  aoi_name = "Jambongan",
  dissolve = TRUE
)

# Find the selected raster
cdd_record <- find_raster(
  raster_catalogue = raster_catalogue,
  variable_id = "CDD",
  scenario = "ssp245",
  period = "2041-2070"
)

# Process raster
cdd_result <- process_catalogue_raster(
  raster_record = cdd_record,
  aoi_sf = jambongan_aoi,
  output_dir = file.path(
    "outputs",
    "jobs",
    "Jambongan_CDD_test"
  )
)

print(cdd_result)
