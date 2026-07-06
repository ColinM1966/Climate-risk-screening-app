# Week 1 - Day 1

## Main objective

Implement or stabilise draw-polygon AOI selection.

## Starting checks

| Check | Result |
| ----- | ------ |
| App starts | Done |
| Jambongan AOI works | Done |
| Upload AOI works | Done |
| Point-buffer AOI works | Done |
| Draw-polygon option visible | Done |
| Analysis still works before changes | Done |

## Current draw-polygon status

| Check | Result |
| ----- | ------ |
| Draw polygon option visible | Done |
| Drawing toolbar appears | Done |
| User can draw a polygon | Done |
| Drawn polygon becomes active AOI | Done |
| Run analysis works with drawn AOI | Done |
| Clear drawn AOI button exists | Done |
| Clear drawn AOI works | Done |

## Draw-polygon status with Bio05

| Check | Expected |
| ----- | -------- |
| Drawn polygon appears | Yes |
| Active AOI updates | Drawn_AOI |
| Run analysis works | Yes |
| Result table appears | Yes |
| Result CSV downloads | Yes |
| No cropped raster button if off | Yes |

## Draw-polygon status with cropped raster ON

| Check | Expected |
| ----- | -------- |
| Raster appears on map | Yes |
| Cropped raster button appears | Yes |
| Cropped raster downloads | Yes |
| Raster clipped to drawn AOI | Yes |

## Draw-polygon status with comparison

| Check | Expected |
| ----- | -------- |
| Comparison table appears | Yes |
| Missing combinations skipped safely | Yes |
| Change from baseline appears | Yes |
| Graph appears | Yes |
| Comparison CSV downloads | Yes |

## Clearing and redrawing check

| Check | Expected |
| ----- | -------- |
| First AOI clears | Yes |
| Old result clears | Yes |
| Old raster clears | Yes |
| Second AOI becomes active | Yes |
| Analysis uses second AOI | Yes |

## Switching AOI methods

| Check | Expected |
| ----- | -------- |
| Active AOI updates correctly | Yes |
| Old map layers do not remain | Yes |
| Old result tables clear | Yes |
| Graph updates correctly | Yes |
| Downloads use current AOI | Yes |

## Draw polygon edge cases

| Test | Result | Needs fixing? |
| ---- | ------ | ------------- |
| No polygon drawn | Not active | No |
| Drawing cancelled | Not active | No |
| Very small polygon | Done | No |
| Outside raster coverage | Done | No |
| Crosses raster edge | Done | No |

## Day 1 summary

Week 4 Day 1 focused on implementing and testing the draw-polygon AOI workflow. The app was tested to confirm that drawn polygons can become the active AOI, support single analysis, comparison, graphing,downloads and optional cropped raster output. Remaining issues were recorded for Day 2.

## Additional update
SSP3-7.0 (ssp370) and SSP5-8.5 (ssp585) raster files were added to the project folders, but they still need to be connected through `config/raster_catalogue.csv` and checked in the scenario selectors before they will appear in the app.
