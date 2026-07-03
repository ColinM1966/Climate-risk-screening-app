# Week 3 - Day 3

## Main objective 

Add an optional cropped-raster output control and test the app with another climatology and scenario.

## Starting check

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Upload AO works | Done |
| Point-buffer works | Done |
| Comparison table works | Done |
| Graph works | Done |
| Result CSV download works | Done |
| Comparison CSV download works | Done |

## Current cropped raster behaviour

| Check | Result |
| ----- | ------ |
| Cropped raster created automatically | Done |
| Raster shown on map | Done |
| Cropped raster download available | Done |
| Output file created in outputs folder | Done |
| Processing time acceptable | Done |

## Output option added

Added a checkbox so users can choose whether to create cropped raster output.

## Map behaviour with cropped raster off

| Check | Result |
| ----- | ------ |
| AOI outline remains visible | Done |
| Result table appears | Done |
| Raster layer not shown | Done |
| App does not crash | Done |

## Cropped raster OFF test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Analysis runs | Yes | Yes | Pass |
| Result table appears | Yes | Yes | Pass |
| Result CSV downloads | Yes | Yes | Pass |
| Cropped raster button hidden | Yes | Yes | Pass |
| AOI outline remains | Yes | Yes | Pass |
| App does not crash | Yes | Yes | Pass |

## Cropped raster ON test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Analysis runs | Yes | Yes | Pass |
| Raster appears on map | Yes | Yes | Pass |
| Cropped raster button appears | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |
| Raster opens in QGIS/R | Yes | Yes | Pass |

## Comparison with cropped raster OFF

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Result table appears | Yes | Yes | Pass |
| Comparison table appears | Yes | Yes | Pass |
| Graph appears | Yes | Yes | Pass |
| Comparison CSV downloads | Yes | Yes | Pass |
| Cropped ratser button hidden | Yes | Yes | Pass |

## Comparison with cropped raster ON

| Check | Result | Actual | Pass/Fail |
| ----- | ------ | ------ | --------- |
| Comparison table appears | Yes | Yes | Pass |
| Graph appears | Yes | Yes | Pass |
| Raster appears on map | Yes | Yes | Pass |
| Cropped raster button appears | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |

## Additional climatology/scenario selected

| Field | Value |
| ----- | ----- |
| Variable | Bio05 |
| Scenario | ssp1-2.6 |
| Period/climatology | 2071-2100 |
| Raster file exists | rasters/future/ssp126/2071-2100/Bio05_AllGCMs_ssp126_2071-2100.tif |
| Enabled in catalogue | true |

## Additional scenario/climatology test - cropped raster OFF

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Scenario appears in dropdown | Yes | Yes | Pass |
| Period appears in dropdown | Yes | Yes | Pass |
| Analysis runs | Yes | Yes | Pass |
| Result table labels correct | Yes | Yes | Pass |
| CSV IDs correct | Yes | Yes | Pass |
| App does not crash | Yes | Yes | Pass |

## Additional scenario/climatology test - cropped raster ON

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Analysis runs | Yes | Yes | Pass |
| Raster appears on map | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |
| Filename scenario correct | Yes | Yes | Pass |
| Filename period correct | Yes | Yes | Pass |
| Raster opens | Yes | Yes | Pass |

## Comparison control check for additional scenario

| Check | Result |
| ----- | ------ |
| Additional scenario appears in comparison scenarios | Done |
| Additional period appears in comparison periods | Done |
| Missing combinations skipped safely | Done |
| Valid combinations processed | Done |

## Cropped raster default

Cropped raster output is set to OFF by default because most users only need table and graph outputs. Users can turn it on when they need a clipped GeoTIFF for GIS use.

## Known limitations after Day 5

Cropped raster output is optional and off by default.
If cropped raster output is off, the map may show only the AOI outline and not the raster layer.
Only scenarios and climatolgies listed in the raster catalogue can be tested.
Missing scenario-period combinations are skipped.
No combined overall-risk score is produced.
Results are screening summaries only.

## Proposed next priorities

1. Improve Data Availability tab so users can clearly see which climatologies and scenarios are available.
2. Add more prepared rasters to the raster catalogue.
3. Test SSP1-2.6, SSP3-7.0 and SSP5-8.5 where avilable.
4. Add clearer variable interpreparation notes.
5. Prepare a simple demonstration workflow for Colin.
6. Begin moving repeated analysis code into helper functions if app.R is becoming too large.
