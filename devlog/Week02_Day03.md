# Week 2 - Day 3

## Main objective

Add result download options for the main result table, comparison table, and cropped raster.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub |  |
| App starts from clear session |  |
| Day 2 comparison controls still work |  |
| Comparison CSV still downloads |  |

### Day 2 Comparison

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Comparison table appears | Yes | Done | Pass |
| Baseline row appears | Yes | Done | Pass |
| Future row appears | Yes | Done | Pass |
| Missing combinations skipped | Yes | Done | Pass |
| Change from baseline calculated | Yes | Done | Pass |
| Comparison CSV downloads | Yes | Done | Pass |

### A main result CSV download button check

| Check | Pass/Fail |
| ----- | --------- |
| Download button appears | Pass |
| CSV Downloads | Pass |
| CSV opens correctly | Pass |
| AOI name correct | Pass |
| Variable name correct | Pass |
| Mean/min/max correct | Pass |
| Units correct | Pass |

### Comparison CSV check

| CSV field | Present? |
| --------- | -------- |
| AOI | Yes |
| Variable | Yes |
| Variable_ID | Yes |
| Sceanrio | Yes |
| Scenario_ID | Yes |
| Period | Yes |
| Period_ID | Yes |
| Mean | Yes |
| Minimum | Yes |
| Maximum | Yes |
| Change_from_baseline | Yes |
| Units | Yes |
| Raster_file | Yes |

### cropped raster check

| Check | Result |
| ----- | ------ |
| Output folder created | Done |
| Cropped raster file exists | Done |
| Cropped raster path stored in rv$cropped_raster | Done |
| Raster can be opened in R/QGIS | Done |
