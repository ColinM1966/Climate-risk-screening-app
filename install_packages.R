packages <- c(
  "shiny",
  "bslib",
  "leaflet",
  "sf",
  "terra",
  "exactextractr",
  "dplyr",
  "readr",
  "tidyr",
  "purrr",
  "DT"
)

missing_packages <- packages[
  !packages %in%
    rownames(
      installed.packages()
    )
]

if (length(missing_packages) > 0) {

  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}
############
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  download.file.method = "wininet"
)

install.packages(c(
  "sys",
  "askpass",
  "pkgbuild",
  "rprojroot",
  "bit",
  "prettyunits",
  "openssl",
  "digest",
  "zoo"
))

#############
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  download.file.method = "libcurl"
)
##############
packages <- c("shiny", "bslib", "leaflet", "readr", "DT")

sapply(packages, requireNamespace, quietly = TRUE)

###############
shiny::runApp()

#########
install.packages("e1071")

#########
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  download.file.method = "wininet"
)

install.packages(c("proxy", "e1071"))

##########
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  download.file.method = "libcurl"
)

##########
library(e1071)
library(sf)

##########
pathway_file <- list.files(
  ".",
  pattern = "^pathway_themes\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

pathway_file

##########
library(readr)

pathway_themes <- read_csv(
  pathway_file[1],
  show_col_types = FALSE
)

# Remove accidental spaces from column names
names(pathway_themes) <- trimws(names(pathway_themes))

# Add the missing display-order column
if (!"display_order" %in% names(pathway_themes)) {
  pathway_themes$display_order <- seq_len(nrow(pathway_themes))
}

write_csv(pathway_themes, pathway_file[1])

names(pathway_themes)

###########
shiny::runApp()

###########
raster_files <- list.files(
  "rasters",
  pattern = "\\.(tif|tiff|asc)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

raster_files
length(raster_files)

#########
raster_catalogue <- readr::read_csv(
  "config/raster_catalogue.csv",
  show_col_types = FALSE
)

variable_metadata <- readr::read_csv(
  "config/variable_metadata.csv",
  show_col_types = FALSE
)

theme_variables <- readr::read_csv(
  "config/theme_variables.csv",
  show_col_types = FALSE
)

pathway_themes <- readr::read_csv(
  "config/pathway_themes.csv",
  show_col_types = FALSE
)

risk_thresholds <- readr::read_csv(
  "config/risk_thresholds.csv",
  show_col_types = FALSE
)

setdiff(
  unique(raster_catalogue$variable_id),
  unique(variable_metadata$variable_id)
)

setdiff(
  unique(theme_variables$variable_id),
  unique(variable_metadata$variable_id)
)

setdiff(
  unique(risk_thresholds$variable_id),
  unique(variable_metadata$variable_id)
)

setdiff(
  unique(pathway_themes$theme),
  unique(theme_variables$theme)
)

#######
risk_thresholds <- readr::read_csv(
  "config/risk_thresholds.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    !is.na(variable_id),
    variable_id != ""
  )

readr::write_csv(
  risk_thresholds,
  "config/risk_thresholds.csv"
)

risk_thresholds <- readr::read_csv(
  "config/risk_thresholds.csv",
  show_col_types = FALSE
)

setdiff(
  unique(risk_thresholds$variable_id),
  unique(variable_metadata$variable_id)
)

#########
file.exists(
  "outputs/tests/raster_lookup_test.csv"
)
