library(sf)

source(
  file.path(
    "R",
    "processing",
    "prepare_aoi.R"
  )
)

# Example 1: Jambongan GeoPackage
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

plot(
  st_geometry(
    jambongan_aoi
  )
)

# Example 2: Point buffer
point_aoi <- create_buffered_point_aoi(
  longitude = 116.65,
  latitude = 6.75,
  buffer_km = 10,
  aoi_name = "Selected location"
)

print_aoi_summary(
  point_aoi
)

plot(
  st_geometry(
    point_aoi
  )
)
