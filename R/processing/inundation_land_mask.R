# ============================================================
# PRESENT-DAY LAND MASK FOR INUNDATION ANALYSIS
# Version 14
# Sabah Climate Risk Explorer
#
# The coastal inundation rasters may contain cells representing
# present-day sea. Inundation screening should report only:
#
#   present-day LAND that becomes inundated
#
# Therefore every inundation AOI is first clipped to the cleaned
# present-day land mask before raster processing.
# ============================================================

INUNDATION_LAND_MASK_PATH <- file.path(
  "data",
  "boundaries",
  "Borneo_land_mask.gpkg"
)

INUNDATION_LAND_MASK_LAYER <- "present_day_land"

.inundation_land_mask_cache <- new.env(
  parent = emptyenv()
)


inundation_area_ha <- function(x) {
  if (
    is.null(x) ||
    nrow(x) == 0
  ) {
    return(numeric(0))
  }

  # Sabah is principally within UTM Zone 50N. This also matches
  # the equal-area convention already used elsewhere in the app
  # for AOI area calculations.
  x_area <- sf::st_transform(
    sf::st_make_valid(x),
    32650
  )

  as.numeric(
    sf::st_area(x_area)
  ) / 10000
}


load_inundation_land_mask <- function(
    path = INUNDATION_LAND_MASK_PATH,
    layer = INUNDATION_LAND_MASK_LAYER,
    use_cache = TRUE
) {
  if (!file.exists(path)) {
    stop(
      paste(
        "Present-day land mask was not found:",
        path,
        "Run scripts/prepare_borneo_land_mask_v14.R first."
      ),
      call. = FALSE
    )
  }

  cache_key <- paste0(
    normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    ),
    "::",
    layer,
    "::",
    as.numeric(
      file.info(path)$mtime
    )
  )

  if (
    isTRUE(use_cache) &&
    exists(
      "cache_key",
      envir = .inundation_land_mask_cache,
      inherits = FALSE
    ) &&
    identical(
      get(
        "cache_key",
        envir = .inundation_land_mask_cache
      ),
      cache_key
    ) &&
    exists(
      "land",
      envir = .inundation_land_mask_cache,
      inherits = FALSE
    )
  ) {
    return(
      get(
        "land",
        envir = .inundation_land_mask_cache
      )
    )
  }

  land <- sf::st_read(
    path,
    layer = layer,
    quiet = TRUE
  )

  if (nrow(land) == 0) {
    stop(
      "The present-day land mask contains no features.",
      call. = FALSE
    )
  }

  land <- sf::st_make_valid(
    land
  )

  land <- land[
    !sf::st_is_empty(land),
    ,
    drop = FALSE
  ]

  if (nrow(land) == 0) {
    stop(
      "The present-day land mask contains no non-empty geometry.",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(land))) {
    stop(
      "The present-day land mask has no CRS.",
      call. = FALSE
    )
  }

  assign(
    "cache_key",
    cache_key,
    envir = .inundation_land_mask_cache
  )

  assign(
    "land",
    land,
    envir = .inundation_land_mask_cache
  )

  land
}


clip_inundation_aoi_to_present_day_land <- function(
    aoi_sf,
    land_mask = NULL
) {
  if (
    is.null(aoi_sf) ||
    !inherits(aoi_sf, "sf") ||
    nrow(aoi_sf) == 0
  ) {
    stop(
      "The inundation AOI must be a non-empty sf object.",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(aoi_sf))) {
    stop(
      "The inundation AOI has no CRS.",
      call. = FALSE
    )
  }

  if (is.null(land_mask)) {
    land_mask <- load_inundation_land_mask()
  }

  aoi_original <- sf::st_make_valid(
    aoi_sf
  )

  # Preserve original AOI attributes, but do the geometric
  # intersection in EPSG:32650 to avoid coastline operations in
  # geographic longitude/latitude coordinates.
  aoi_work <- sf::st_transform(
    aoi_original,
    32650
  )

  land_work <- sf::st_transform(
    sf::st_make_valid(land_mask),
    32650
  )

  # The prepared land GeoPackage is normally one multipart feature,
  # but unioning here makes the helper robust if that changes later.
  land_union <- sf::st_union(
    sf::st_geometry(land_work)
  )

  output_rows <- vector(
    "list",
    nrow(aoi_work)
  )

  for (i in seq_len(nrow(aoi_work))) {
    aoi_i <- aoi_work[
      i,
      ,
      drop = FALSE
    ]

    original_area_ha <- as.numeric(
      sf::st_area(aoi_i)
    ) / 10000

    clipped_geom <- suppressWarnings(
      sf::st_intersection(
        sf::st_geometry(aoi_i),
        land_union
      )
    )

    if (
      length(clipped_geom) == 0 ||
      all(sf::st_is_empty(clipped_geom))
    ) {
      output_rows[[i]] <- NULL
      next
    }

    clipped_geom <- sf::st_make_valid(
      clipped_geom
    )

    # Keep polygonal components only. This prevents coastline-touching
    # line/point artefacts from being treated as analysis areas.
    geom_type <- as.character(
      sf::st_geometry_type(
        clipped_geom,
        by_geometry = TRUE
      )
    )

    polygon_keep <- geom_type %in% c(
      "POLYGON",
      "MULTIPOLYGON"
    )

    clipped_geom <- clipped_geom[
      polygon_keep
    ]

    if (
      length(clipped_geom) == 0 ||
      all(sf::st_is_empty(clipped_geom))
    ) {
      output_rows[[i]] <- NULL
      next
    }

    clipped_union <- sf::st_union(
      clipped_geom
    )

    attrs <- sf::st_drop_geometry(
      aoi_i
    )

    out_i <- sf::st_sf(
      attrs,
      geometry = clipped_union,
      crs = sf::st_crs(aoi_work)
    )

    land_area_ha <- as.numeric(
      sf::st_area(out_i)
    ) / 10000

    # Retain explicit provenance fields. AOI_AREA_HA is deliberately
    # redefined as PRESENT-DAY LAND area because the downstream
    # inundation processor uses it as its exposure denominator.
    out_i$AOI_ORIGINAL_AREA_HA <-
      original_area_ha

    out_i$AOI_LAND_AREA_HA <-
      land_area_ha

    out_i$AOI_EXISTING_SEA_AREA_HA <-
      max(
        0,
        original_area_ha -
          land_area_ha
      )

    out_i$AOI_AREA_HA <-
      land_area_ha

    output_rows[[i]] <- out_i
  }

  output_rows <- Filter(
    Negate(is.null),
    output_rows
  )

  if (length(output_rows) == 0) {
    stop(
      paste(
        "The selected AOI contains no present-day land according to",
        INUNDATION_LAND_MASK_PATH,
        ". No inundation exposure can be calculated."
      ),
      call. = FALSE
    )
  }

  out <- do.call(
    rbind,
    output_rows
  )

  out <- sf::st_transform(
    out,
    sf::st_crs(aoi_original)
  )

  sf::st_make_valid(
    out
  )
}
