# Run from the Climate-risk-screening-app project root.
source(file.path("R", "utils", "register_smv_marine_layers.R"))

result <- register_smv_marine_layers(
  marine_root = file.path("rasters", "marine"),
  config_dir = "config",
  require_complete = TRUE
)

print(result$raster_rows, n = Inf)
