# ============================================================
# PREPARE AREA OF INTEREST
# Sabah Climate Risk Explorer
# ============================================================

library(sf)
library(dplyr)
library(stringr)

sf_use_s2(FALSE)

# ------------------------------------------------------------
# HELPER: CREATE A SAFE ID
# ------------------------------------------------------------

make_safe_id <- function(x) {

  x %>%
    as.character() %>%
    str_squish() %>%
    str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    str_remove_all(
      "^_|_$"
    )
}

# ------------------------------------------------------------
# HELPER: READ A SPATIAL FILE
# ------------------------------------------------------------

read_aoi_file <- function(
    file_path,
    layer = NULL,
    quiet = TRUE
) {

  if (!file.exists(file_path)) {

    stop(
      paste0(
        "AOI file not found:\n",
        file_path
      ),
      call. = FALSE
    )
  }

  file_extension <- tolower(
    tools::file_ext(file_path)
  )

  supported_extensions <- c(
    "gpkg",
    "geojson",
    "json",
    "kml",
    "shp"
  )

  if (!file_extension %in% supported_extensions) {

    stop(
      paste0(
        "Unsupported AOI file type: .",
        file_extension,
        "\n\nSupported types are:\n",
        paste(
          supported_extensions,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # GeoPackage may contain several layers
  if (
    file_extension == "gpkg" &&
      is.null(layer)
  ) {

    available_layers <- st_layers(
      file_path
    )$name

    if (length(available_layers) == 0) {

      stop(
        "No spatial layers were found in the GeoPackage.",
        call. = FALSE
      )
    }

    if (length(available_layers) > 1) {

      stop(
        paste0(
          "The GeoPackage contains more than one layer.\n\n",
          "Available layers:\n",
          paste(
            available_layers,
            collapse = "\n"
          ),
          "\n\nSupply the required layer name."
        ),
        call. = FALSE
      )
    }

    layer <- available_layers[1]
  }

  if (file_extension == "gpkg") {

    aoi <- st_read(
      file_path,
      layer = layer,
      quiet = quiet
    )

  } else {

    aoi <- st_read(
      file_path,
      quiet = quiet
    )
  }

  aoi
}

# ------------------------------------------------------------
# HELPER: CREATE POLYGON FROM MAP POINT
# ------------------------------------------------------------

create_buffered_point_aoi <- function(
    longitude,
    latitude,
    buffer_km,
    aoi_name = "Point buffer",
    projected_crs = 32650
) {

  if (
    is.null(longitude) ||
      is.null(latitude) ||
      is.na(longitude) ||
      is.na(latitude)
  ) {

    stop(
      "Longitude and latitude must be provided.",
      call. = FALSE
    )
  }

  if (
    is.null(buffer_km) ||
      is.na(buffer_km) ||
      buffer_km <= 0
  ) {

    stop(
      "Buffer distance must be greater than zero.",
      call. = FALSE
    )
  }

  point_sf <- st_sf(
    AOI_NAME = aoi_name,
    geometry = st_sfc(
      st_point(
        c(
          longitude,
          latitude
        )
      ),
      crs = 4326
    )
  )

  point_projected <- st_transform(
    point_sf,
    projected_crs
  )

  point_buffer <- st_buffer(
    point_projected,
    dist = buffer_km * 1000
  )

  st_transform(
    point_buffer,
    4326
  )
}

# ------------------------------------------------------------
# HELPER: CREATE POLYGON FROM TWO POINTS
# ------------------------------------------------------------

create_two_point_buffer_aoi <- function(
    longitude_1,
    latitude_1,
    longitude_2,
    latitude_2,
    buffer_km,
    aoi_name = "Two-point buffer",
    projected_crs = 32650
) {

  coordinates <- matrix(
    c(
      longitude_1,
      latitude_1,
      longitude_2,
      latitude_2
    ),
    ncol = 2,
    byrow = TRUE
  )

  points_sf <- st_sf(
    POINT_ID = c(
      "Point_1",
      "Point_2"
    ),
    geometry = st_sfc(
      st_point(
        coordinates[1, ]
      ),
      st_point(
        coordinates[2, ]
      ),
      crs = 4326
    )
  )

  points_projected <- st_transform(
    points_sf,
    projected_crs
  )

  buffered_points <- st_buffer(
    points_projected,
    dist = buffer_km * 1000
  )

  combined_buffer <- st_union(
    st_geometry(
      buffered_points
    )
  )

  combined_sf <- st_sf(
    AOI_NAME = aoi_name,
    geometry = combined_buffer
  )

  st_transform(
    combined_sf,
    4326
  )
}

# ------------------------------------------------------------
# MAIN AOI PREPARATION FUNCTION
# ------------------------------------------------------------

prepare_aoi <- function(
    aoi,
    aoi_name = "Area of interest",
    id_field = NULL,
    dissolve = TRUE,
    point_buffer_km = NULL,
    line_buffer_km = NULL,
    projected_crs = 32650,
    output_crs = 4326,
    sabah_boundary = NULL,
    require_sabah_overlap = TRUE
) {

  # ----------------------------------------------------------
  # CHECK INPUT CLASS
  # ----------------------------------------------------------

  if (!inherits(aoi, "sf")) {

    stop(
      "The AOI input must be an sf object.",
      call. = FALSE
    )
  }

  if (nrow(aoi) == 0) {

    stop(
      "The AOI contains no features.",
      call. = FALSE
    )
  }

  if (is.na(st_crs(aoi))) {

    stop(
      "The AOI has no defined coordinate reference system.",
      call. = FALSE
    )
  }

  original_feature_count <- nrow(aoi)

  # ----------------------------------------------------------
  # REMOVE EMPTY GEOMETRIES
  # ----------------------------------------------------------

  aoi <- aoi[
    !is.na(
      st_geometry(aoi)
    ) &
      !st_is_empty(
        st_geometry(aoi)
      ),
  ]

  if (nrow(aoi) == 0) {

    stop(
      "The AOI contains only empty geometries.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # REPAIR GEOMETRIES IN PROJECTED CRS
  # ----------------------------------------------------------

  aoi_projected <- aoi %>%
    st_transform(
      projected_crs
    ) %>%
    st_make_valid()

  geometry_types <- unique(
    as.character(
      st_geometry_type(
        aoi_projected
      )
    )
  )

  # ----------------------------------------------------------
  # CONVERT POINTS TO BUFFERED POLYGONS
  # ----------------------------------------------------------

  if (
    all(
      geometry_types %in%
        c(
          "POINT",
          "MULTIPOINT"
        )
    )
  ) {

    if (
      is.null(point_buffer_km) ||
        is.na(point_buffer_km) ||
        point_buffer_km <= 0
    ) {

      stop(
        paste0(
          "The AOI contains point geometry.\n",
          "A positive point_buffer_km value is required."
        ),
        call. = FALSE
      )
    }

    aoi_projected <- st_buffer(
      aoi_projected,
      dist = point_buffer_km * 1000
    )
  }

  # ----------------------------------------------------------
  # CONVERT LINES TO BUFFERED POLYGONS
  # ----------------------------------------------------------

  if (
    all(
      geometry_types %in%
        c(
          "LINESTRING",
          "MULTILINESTRING"
        )
    )
  ) {

    if (
      is.null(line_buffer_km) ||
        is.na(line_buffer_km) ||
        line_buffer_km <= 0
    ) {

      stop(
        paste0(
          "The AOI contains line geometry.\n",
          "A positive line_buffer_km value is required."
        ),
        call. = FALSE
      )
    }

    aoi_projected <- st_buffer(
      aoi_projected,
      dist = line_buffer_km * 1000
    )
  }

  # ----------------------------------------------------------
  # EXTRACT POLYGON COMPONENTS
  # ----------------------------------------------------------

  aoi_projected <- suppressWarnings(
    st_collection_extract(
      aoi_projected,
      "POLYGON"
    )
  )

  aoi_projected <- aoi_projected[
    !st_is_empty(
      st_geometry(
        aoi_projected
      )
    ),
  ]

  if (nrow(aoi_projected) == 0) {

    stop(
      "No polygon geometry remained after AOI preparation.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # CREATE AOI NAME AND ID
  # ----------------------------------------------------------

  if (
    !is.null(id_field) &&
      id_field %in% names(aoi_projected)
  ) {

    source_names <- str_squish(
      as.character(
        aoi_projected[
          [id_field]
        ]
      )
    )

  } else {

    source_names <- rep(
      aoi_name,
      nrow(aoi_projected)
    )
  }

  aoi_projected$SOURCE_NAME <-
    source_names

  # ----------------------------------------------------------
  # DISSOLVE INTO ONE AOI
  # ----------------------------------------------------------

  if (dissolve) {

    aoi_prepared <- st_sf(
      AOI_ID = make_safe_id(
        aoi_name
      ),

      AOI_NAME = aoi_name,

      SOURCE_FEATURES =
        original_feature_count,

      geometry = st_union(
        st_geometry(
          aoi_projected
        )
      )
    )

  } else {

    aoi_prepared <- aoi_projected %>%
      mutate(
        AOI_NAME = if_else(
          is.na(SOURCE_NAME) |
            SOURCE_NAME == "",
          paste0(
            aoi_name,
            "_",
            row_number()
          ),
          SOURCE_NAME
        ),

        AOI_ID = make_safe_id(
          AOI_NAME
        ),

        SOURCE_FEATURES = 1
      ) %>%
      select(
        AOI_ID,
        AOI_NAME,
        SOURCE_FEATURES,
        everything()
      )
  }

  aoi_prepared <- st_make_valid(
    aoi_prepared
  )

  # ----------------------------------------------------------
  # CALCULATE AREA
  # ----------------------------------------------------------

  aoi_prepared$AOI_AREA_HA <-
    as.numeric(
      st_area(
        aoi_prepared
      )
    ) / 10000

  # ----------------------------------------------------------
  # CHECK SABAH OVERLAP
  # ----------------------------------------------------------

  if (!is.null(sabah_boundary)) {

    if (!inherits(sabah_boundary, "sf")) {

      stop(
        "sabah_boundary must be an sf object.",
        call. = FALSE
      )
    }

    if (is.na(st_crs(sabah_boundary))) {

      stop(
        "The Sabah boundary has no defined CRS.",
        call. = FALSE
      )
    }

    sabah_projected <- sabah_boundary %>%
      st_transform(
        projected_crs
      ) %>%
      st_make_valid()

    sabah_union <- st_union(
      st_geometry(
        sabah_projected
      )
    )

    overlap_matrix <- st_intersects(
      aoi_prepared,
      sabah_union,
      sparse = FALSE
    )

    aoi_prepared$OVERLAPS_SABAH <-
      apply(
        overlap_matrix,
        1,
        any
      )

    if (
      require_sabah_overlap &&
        !any(
          aoi_prepared$OVERLAPS_SABAH
        )
    ) {

      stop(
        "The prepared AOI does not overlap Sabah.",
        call. = FALSE
      )
    }

  } else {

    aoi_prepared$OVERLAPS_SABAH <-
      NA
  }

  # ----------------------------------------------------------
  # RETURN OUTPUT CRS
  # ----------------------------------------------------------

  aoi_prepared <- st_transform(
    aoi_prepared,
    output_crs
  )

  # ----------------------------------------------------------
  # FINAL FIELD ORDER
  # ----------------------------------------------------------

  aoi_prepared %>%
    select(
      AOI_ID,
      AOI_NAME,
      SOURCE_FEATURES,
      AOI_AREA_HA,
      OVERLAPS_SABAH,
      everything()
    )
}

# ------------------------------------------------------------
# OPTIONAL SUMMARY FUNCTION
# ------------------------------------------------------------

print_aoi_summary <- function(aoi_sf) {

  if (!inherits(aoi_sf, "sf")) {

    stop(
      "aoi_sf must be an sf object.",
      call. = FALSE
    )
  }

  cat("\n")
  cat("========================================\n")
  cat("AREA OF INTEREST SUMMARY\n")
  cat("========================================\n")

  cat(
    "\nAssessment units:",
    nrow(aoi_sf),
    "\n"
  )

  cat(
    "Total area:",
    round(
      sum(
        aoi_sf$AOI_AREA_HA,
        na.rm = TRUE
      ),
      2
    ),
    "ha\n"
  )

  if (
    "SOURCE_FEATURES" %in%
      names(aoi_sf)
  ) {

    cat(
      "Source features:",
      sum(
        aoi_sf$SOURCE_FEATURES,
        na.rm = TRUE
      ),
      "\n"
    )
  }

  if (
    "OVERLAPS_SABAH" %in%
      names(aoi_sf)
  ) {

    cat(
      "Overlaps Sabah:",
      any(
        aoi_sf$OVERLAPS_SABAH,
        na.rm = TRUE
      ),
      "\n"
    )
  }

  invisible(
    aoi_sf
  )
}
