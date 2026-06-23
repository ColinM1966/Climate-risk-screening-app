# Week 2 - Day 2

## Main objective

Improve the scenario and period comparison interface, test missing raster handling, and clean up comparison outputs.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Day 1 comparison table still works | Done |
| CSV download still works | Done |

## Interface changes

Describe changes to comparison scenario and period controls.

## Missing combination handling

| Test | Result |
| ---- | ------ |
| Unavailable baseline/future combinations skipped |  |
| App did not crash |  |
| Warning or note down |  |

## Comparison tests

| Variable | Baseline row | Future row | Change calculated | CSV works |
| -------- | ------------ | ---------- | ----------------- | --------- |
| Fire | Done | Done | Done | Done |
| Bio05 | Done | Done | Done | Done |
| PPETmin | Done | Done | Done | Done |

### Day 1 comparison report

| Test | Expected | Actual | Pass/Fail |
| ---- | -------- | ------ | --------- |
| Single result table | Appears | Done | Pass |
| Comparison table | Appears | Done | Pass |
| Baseline row | Present | Done | Pass |
| Future row | Present | Done | Pass |
| CSV download | Work | Done | Pass |

### Comparison table check

| Output location | Friendly labels used? | Pass/Fail |
| --------------- | --------------------- | --------- |
| Comparison table | Yes | Pass |
| Downloaded CSV | Yes | Pass |
| Missing-combination message | Yes | Pass |
| Result note | Yes | Pass |

### CSV table check

| CSV check | Pass/Fail |
| --------- | --------- |
| Opens clearly | Pass |
| Has baseline row | Pass |
| Has future row | Pass |
| Has Change_from_baseline column | Pass |
| Has units | Pass |
| Has raster file path | Pass |

### Fire probablity check

| Check | Result |
| ----- | ------ |
| Baseline row appears | Done |
| Future row appears | Done |
| Missing combinations skipped | Done |
| Change from baseline calculated | Done |
| CSV downloaded works | Done |

### Bio05 check

| Check | Result |
| ----- | ------ |
| Baseline row appears | Done |
| Future row appears | Done |
| Missing combinations skipped | Done |
| Change from baseline calculated | Done |
| CSV downloaded works | Done |

### PPETmin check

| Check | Result |
| ----- | ------ |
| Baseline row appears | Done |
| Future row appears | Done |
| Missing combinations skipped | Done |
| Change from baseline calculated | Done |
| CSV downloaded works | Done |

The comparison table can compare available scenario-period combinations, but map still displays one selected raster at a time.

## Issues found

1. Mean value was read from the wrong place. The newer script looked inside a summary table, but the working file reads directly from mean or nweighted_mean.
2. Minimum and maximum had the same extraction problem. The app should read minimum / min and maximum / max directly from the processing result first.
3. Main Results table could show blank or incorrect Mean. Because the exhaustion method did not match the structure returned by process_continuous_raster().
4. Comparison table could also show incorrect Mean. The comparison loop used the same weaker extraction approach.
5. Change_from_baseline could be wrong. If the baseline mean was extracted incorrectly, all change values would also show incorrect Mean values
6. Run test button was triggering Run analysis. The script used shinyjs::click("run_analysis"), so the AOI test was not independent.
7. AOI test table depended on rv$result. That means it showed the latest main analysis result, not a separate AOI test result.
8. Run analysis and Run test workflows were mixed together. They should be separate: normal analysis for the app, AOI test only for Developer Test.
9. The script mixed logic from two app versions. The newer script had expanded scenario/period controls, but the older file had the more reliable result-reading logic.
10. The AOI test did not have its own stored result object. It needed its own eventReactive(input$run_aoi_test, { ... }) so only the test button run it.

## Next

Add main results CSV download, then begin cropped raster download.
