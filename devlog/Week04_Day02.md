# Week 4 - Day 2

## Main objective

Add monthly average maximum Wet Bulb Globe Temperature (WBGT) as a new climate variable in the app.

## Starting checks

| Check | Result |
| ----- | ------ |
| App starts | Done |
| Jambongan AOI works | Done |
| Upload AOI works | Done |
| Draw polygon works | Done |
| Point-buffer works | Done |
| Existing Bio05 analysis works | Done |
| Existing Bio017 analysis works | Done |
| Cropped raster option works | Done |
| Data Availability tab opens | Done |

## Pre-WBGT check

| Test | Result | Pass/Fail |
| ---- | ------ | --------- |
| Jambongan Bio05 SSP2-4.5 | Done | Pass |

## WBGT raster file located

| Item | Value |
| ---- | ----- |
| File name | WBGT-Sun_Max_AllGCMs_ssp245_2041_2070.tif |
| File path | rasters/future/ssp245/2041-2070/WBGT-Sun_Max_AllGCMs_ssp245_2041-2070.tif |
| Scenario | ssp245 |
| Period/climatology | 2041-2070 |
| Units assumed | degrees C |
| Monthly or annual summary? | Monthly average maximum WBGT |

## WBGT variable naming

| Field | Value |
| ----- | ----- |
| variable_id | WBGTmax |
| user-facing label | Maximum WBGT |
| description | Monthly average maximum Wet Bulb Globe Temperature |
| units | °C |

## WBGT scenario and climatology

| Field | Value |
| ----- | ----- |
| Scenario ID | ssp245 |
| Scenario label | SSP2-4.5 |
| Period ID | 2041-2070 |
| Period label | 2041-2070 |

## WBGT metadata update

| Config file | Updated? | Notes |
| ----------- | -------- | ----- |
| raster_catalogue.csv | Yes | Updated |
| variable_metadata.csv | Yes | Updated |
| theme_variable.csv | Yes | Updated |
| thresholds.csv | Yes | Updated |


## WBGT label check

| Check | Result |
| ----- | ------ |
| WBGT label appears in selector | Done |
| Label uses user-facing wording | Done |

## WBGT Data Availability check

| Check | Result |
| ----- | ------ |
