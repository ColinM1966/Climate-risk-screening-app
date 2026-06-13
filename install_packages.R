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
