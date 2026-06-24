# Week 2 - Day 3

## Main objective

Add result download options for the main result table, comparison table, and cropped raster.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clear session | Done |
| Day 2 comparison controls still work | Done |
| Comparison CSV still downloads | Done |

## Download features added

| Download | Added? | Tested? |
| -------- | ------ | ------- |
| Main result CSV | Done | Done |
| Comparison CSV | Done | Done |
| Cropped raster GeoTIFF | Done | Done |

## Jambongan tests

| Output | Results |
| ------ | ------- |
| Result CSV | Done |
| Cropped raster | Done |
| Raster opens in QGIS/R | Done |

## Segama_catchment tests

| Output | Result |
| ------ | ------ |
| Result CSV | Done |
| Comparison CSV | Done |
| Cropped raster | Done |

## PPETmin test

| Check | Result |
| ----- | ------ |
| Dry-condition note appears | Done |
| CSV units correct | Done |
| Cropped raster downloads | Done |

## Issues found

1. Dynamic download buttons are not yet implemented. The Results tab will still uses fixed downloadButton() calls instead of uiOutput("download_buttons").
2. Downloads are visible before analysis. Users can see download buttons before rv$cropped_raster exist.
3. Comparison CSV buttonis always visible. The comparison CSV button appears even when no scenario/period comparison has been run.
4. Cropped raster button may appear even if no cropped raster exists. The handler depends on rv$cropped_raster, but that object is only created if process_continuous_raster() returns analysis_result$cropped_raster.
5. Draw polygon option is in the UI but not implemented. The sidebar includes "Draw polygon" = "draw", but there is no drawing tool or server logic to turn a drawn polygon into rv$aoi.
6. Point and buffer option is in the UI but not implemented. The sidebar includes "Select point and buffer" = "point", but there is no map-click observer to create a buffered AOI.
7. Comparison download wrapper class is now unnecessary. comparison-download-row was useful when the comparison download button sat near the table, but if all downloads are grouped under one Downloads section, this wrapper is probably no longer needed.
8. The raster download note always shows. The GeoTIFF note is currently shown even before a raster has been produced. It should ideally be part of the dynamic  download section, appearing after analysis.
9. No user-facing message if cropped raster is unavailable after analysis. If the summary result exists but the cropped raster was not returned, the app does not clearly explain why the raster download cannot be used.
10. Main result CSV and cropped raster handlers are mostly good. These handlers use rv$result, safe filenames, scenario ID, period ID, and raster file metadata correctly. The main problem is UI visibility, not the download logic itself.

## Next steps

Improve download layout, add comparison graph, and continue testing AOI selection methods.
