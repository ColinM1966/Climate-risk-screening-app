packages <- c(
  "shiny",
  "bslib",
  "leaflet",
  "dplyr",
  "readr",
  "stringr",
  "DT",
  "sf",
  "terra",
  "exactextractr"
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
