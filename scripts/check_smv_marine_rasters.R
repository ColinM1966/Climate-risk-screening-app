# Check the SMV marine files BEFORE changing the config tables.
source(file.path("R", "utils", "register_smv_marine_layers.R"))

scan <- build_smv_raster_catalogue_rows(
  marine_root = file.path("rasters", "marine")
)

cat("\nUsable marine raster records:", nrow(scan$rows), "\n")
print(scan$rows, n = Inf)

cat("\nMissing / ambiguous records:", nrow(scan$missing), "\n")
if (nrow(scan$missing) > 0) print(scan$missing, n = Inf)
