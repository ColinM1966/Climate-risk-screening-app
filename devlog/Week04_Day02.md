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
| WBGT appears | Done |
| Scenario correct | Done |
| Period correct | Done |
| Units correct | Done |
| File exists | Done |
| Enabled | Done |

## WBGT selector check

| Check | Result |
| ----- | ------ |
| WBGT appears in selector | Done |
| Label clear | Done |

## WBGT single-analysis test - cropped raster OFF

| Check | Result |
| ----- | ------ |
| AOI | Jambongan |
| Scenario | SSP245 |
| Period | 2041-2070 |
| Analysis runs | Done |
| Units correct | Done |
| CSV downloads | Done |

## WBGT single-analsis test - cropped raster ON

| Check | Result |
| ----- | ------ |
| Raster appears | Done |
| Legend sensible | Done |
| Cropped raster downloads | Done |
| Raster opens | Done |

## WBGT value sanity check

| Stattistic | Value | Plausible? |
| ---------- | ----- | ---------- |
| Mean | 31.12 | Yes |
| Minimum | 25.85 | Yes |
| Maximum | 32.97 | Yes |
| Units | °C | No |

## WBGT second AOI test

| Check | Result |
| ----- | ------ |
| AOI used | Done |
| Analysis runs | Done |
| CSV downloads | Done |

## WBGT comparison test

| Scenario | Period | Row appears? | Notes |
| -------- | ------ | ------------ | ----- |
| SSP126 | 2071-2100 | Yes | Completed |

## Graph label check

| Check | Expected |
| ----- | -------- |
| Variable label says Maximum WBGT | Yes |
| Units are °C | Yes |
| Scenario labels are readable | Yes |
| Period labels are readable | Yes |
| Graph does not show internal file names| Yes |

## Known limitations after adding WBGT

- The initial WBGT layer added is monthly average maximum WBGT.
- This should not be interpreted as daily maximum WBGT unless the source raster specifically represents daily maxima.
- WBGT comparison is only possible where more than one WBGT scenario or period has been connected.
- Not all variables are available for every scenario and period.
- No combined overall-risk score is produced.

## Optional WBGT demo

WBGT can be shown as an optional heat-stress example after the main Jambongan Fire comparison demo. The recommended WBGT demo is:

AOI: Jambongan
Variable: Maximum WBGT
Scenario: SSP370
Period: 2071-2100
Cropped raster output: ON

## Day 2 summary

Week 4 Day 2 focused on adding monthly average maximum WBGT as a new climate variable. The WBGT raster was located, added to the raster catalogue, checked in the Data Availability tab, and tested using Jambongan and one additional AOI. The app was checked to confirm that WBGT results show correct labels, units, values, downloads and optional cropped raster output.

## Proposed Day 3 priorities

1. Fix any WBGT catalogue, label or unit issues.
2. Add additional WBGT scenarios or climatologies if available.
3. Add SSP3-7.0 and SSP5-8.5 WBGT rows if those rasters exist.
4. Improve the Data Availability tab so monthly climatology layers are clearer.
5. Prepare a stable demo workflow for the friendly technical testing session.
