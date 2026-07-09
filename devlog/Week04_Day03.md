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

## WBGT cropped raster ON test

| Check | Result |
| ----- | ------ |
| Raster appears | Done |
| Legend appears | Done |
| Cropped raster downloads | Done |
| Raster opens | Done |

## WBGT value sanity check

| Statistic | Value | Plausible? | Notes |
| --------- | ----- | ---------- | ----- |
| Mean | 30.62 | Yes | The mean is correct. |
| Minimum | 30.01 | Yes | The minimum is correct. |
| Maximum | 31.51 | Yes | The maximum is correct. Maximum WBGT is a heat-stress indicator. Higher values indicate greater potential heat exposure and reduced outdoor workability. This layer represents monthly average maximum WBGT, not daily extreme WBGT. |

## WBGT second AOI test

| Check | Result |
| ----- | ------ |
| AOI used | Done |
| Analysis runs | Done |
| CSV downloads | Done |

## WBGT comparison test 

| Scenario | Period | Row appears? | Notes |
| -------- | ------ | ------------ | ----- |
| SSP2-4.5 | 2041-2070 | Yes | It is correct. |

## SSP3-7.0 and SSP5-8.5 selector check

| Scenario | Main selector | Comparison selector | Data Availability | Notes |
| -------- | ------------- | ------------------- | ----------------- | ----- |
| SSP3-7.0 | Done | Done | Done | It is completed. |
| SSP5-8.5 | Done | Done | Done | It is completed. |

## Data Availability update

| Check | Result |
| ----- | ------ |
| Data Availability note updated | Done |
| WBGT monthly climatology note added | Done |
| Scenario labels readable | Done |
| File exists status clear | Done |

## Result note check

| Variable | Note correct? |
| -------- | ------------- |
| WBGTmax | Maximum WBGT is a heat-stress indicator. Higher values indicate greater potential heat exposure and reduced outdoor workability. This layer represents monthly average maximum WBGT, not daily extreme WBGT. |
| Bio017 | Bio017 lower values indicate drier conditions. |
| PPETmin | PPETmin lower values indicate drier conditions. |

## Optional WBGT demo

Use this only if the main demo is working.
AOI: Jambongan
Variable: Maximum
WBGT Scenario: [available WBGT scenario]
Period: [available WBGT climatology]
Comparison: Off first
Create cropped raster output: On

Explain that this is monthly average maximum WBGT and is used as a heat-stress indicator.

## Known limitations after Day 3

- WBGT is currently added as monthly average maximum WBGT.
- WBGT should not be interpreted as daily maximum WBGT unless the source raster specifically represents daily maxima.
- WBGT comparison is only possible where multiple WBGT scenario or period rasters are connected.
- Not all variables are available for every scenario and period.
- No combined overall-risk score is produced.

## Day 3 summary

Week 4 Day 3 focused on stabilising the WBGT workflow after the initial monthly average maximum WBGT raster was added. WBGT was checked in the raster catalogue,
Data Availability tab, variable selector, result table, downloads and optional cropped raster output. Additional WBGT scenario rows were added only where
matching raster files existed. Known limitations were recorded for the friendly technical testing session.

## Proposed Day 4 priorities

1. Fix any remaining WBGT labels, unit or catalogue issues.
2. Improve the Data Availability tab if testers may use it.
3. Prepare the final friendly testing workflow and feedback sheets.
4. Rehearse the Jambongan Fire comparison demo.
5. Rehearse the optional WBGT demo.
6. Freeze the testing version if the core workflows are stable.
