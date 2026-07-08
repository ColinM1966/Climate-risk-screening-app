# Week 4 - Day 3

## Main objective

Test and stabilise the WBGT workflow, improve Data Availability, and prepare WBGT as an optional demo variable.

## Starting check

| Check | Result |
| ----- | ------ |
| App starts | Done |
| Jambongan AOI works | Done |
| Draw polygon works | Done |
| Point-buffer works | Done |
| Existing Bio05 analysis works | Done |
| Existing Fire comparison works | Done |
| WBGT appears in Data Availability | Done |
| WBGT appears in variable selector | Done |
| Cropped raster option works | Done |

## Pre-Day-3 check

| Test | Result | Pass/Fail |
| ---- | ------ | --------- |
| Jambongan Bio05 SSP2-4.5 | Done | Pass |

## WBGT visibility check

| Check | Result |
| ----- | ------ |
| WBGT appears in Data Avaibility | Done |
| WBGT appears in variable selector | Done |
| WBGT units correct | Done |
| WBGT file exists | Done |
| WBGT enabled | Done |

## WBGT catalogue check

| Field | Value | Correct? |
| ----- | ----- | -------- |
| variable_id | WBGT | Yes |
| label | SSP_240_4170 | Yes |
| scenario | SSP245 | Yes |
| period | 2041-2070 | Yes |
| units | degrees C | Yes |
| file path | rasters/future/ssp245/2041-2070/WBGT-Sun_Max_AllGCMs_ssp245_2041-2070.tif | Yes |
| enabled | TRUE | Yes |

## WBGT raster inventory

| File name | File path | Scenario | Period/climatology | Already in catalogue? |
| --------- | --------- | -------- | ------------------ | --------------------- |
| WBGT-Sun_Max_AllGCMs_ssp245_2041-2070.tif | rasters/future/ssp245/2041-2070/WBGT-Sun_Max_AllGCMs_ssp245_2041-2070.tif | SSP245 | 2041-2070 | Yes |

## Additional WBGT rows added

| Variable | Scenario | Period | File exists? | Added? |
| -------- | -------- | ------ | ------------ | ------ |
| WBGTmax | ssp370 | 2041-2070 | Yes | Yes |
| WBGTmax | ssp585 | 2071-2100 | Yes | Yes |

## SSP label check 

| Scenario ID | Label | Present? |
| ----------- | ----- | -------- |
| ssp370 | SSP3-7.0 | Yes |
| ssp585 | SSP5-8.5 | Yes |

## App restart check

| Check | Result |
| ----- | ------ |
| Apps restarts | Done |
| No catalogue | Done |
| Variable selector works | Done |
| Scenario selector works | Done |

## WBGT cropped raster OFF test

| Check | Result |
| ----- | ------ |
| Scenario tested | Done |
| Period tested | Done |
| Analysis runs | Done |
| Units correct | Done |
| CSV downloads | Done |

