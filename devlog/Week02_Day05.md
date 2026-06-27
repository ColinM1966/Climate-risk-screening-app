# Week 2 - Day 5

## Main objective

Test and stabilise AOI selection workflows, confirm Week 2 comparison/download.

## Starting check

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Day 4 comparison graph still works | Done |
| Result CSV download still works | Done |
| Comparison CSV download still works | Done |
| Cropped raster download still works | Done |

### Day 4 feature

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Result table appears | Yes | Done | Pass |
| Comparison table appears | Yes | Done | Pass |
| Change from baseline appears | Yes | Done | Pass |
| Comparison graph appears | Yes | Done | Pass |
| Result CSV downloads | Yes | Done | Pass |
| Comparison CSV downloads | Yes | Done | Pass |
| Cropped raster downloads | Yes | Done | Pass |

### Uploaded AOI workflow

| Check | Result |
| ----- | ------ |
| AOI upload works | Done |
| Active AOI name correct | Done |
| Map zooms correctly | Done |
| Result table AOI correct | Done |
| Comparison table AOI correct | Done |
| Graph AOI correct | Done |
| Download filenames correct | Done |

### Jambongan test AOI workflow

| Check | Result |
| ----- | ------ |
| Jambongan loads | Done |
| Previous uploaded AOI clearly | Done |
| Result table AOI correct | Done |
| Comparison table AOI correct | Done |
| Graph AOI correct | Done |
| Downloads still work | Done |

### Segama_catchment and Jambongan test AOI workflow

| Step | Expected AOI | Variable | Old raster cleared? | Result table cleared? | Graph correct? | Pass/Fail |
| ---- | ------------ | -------- | ------------------- | --------------------- | -------------- | --------- |
| 1 | Segama_catchment | Bio05 | Yes | Yes | Yes | Yes |
| 2 | Jambongan | Bio05 | Yes | Yes | Yes | Yes |
| 3 | Segama_catchment | Fire | Yes | Yes | Yes | Yes |

### Draw-polygon mode test

| Check | Result |
| ----- | ------ |
| Draw polygon option visible | Done |
| Drawing tools appear | Draw-polygon mode is present in the interface but is not yet implemented. |
| Drawn AOI becomes active | Draw-polygon mode is present in the interfece but is not yet implemented. |
| Analysis runs on drawn AOI | Draw-polygon mode is present in the interfece but is not yet implemented. |
| Result table uses drawn AOI | Draw-polygon mode is present in the interfece but is not yet implemented. |

### Point-and-buffer mode test

| Check | Result |
| ----- | ------ |
| Point-buffer option visible | Point-and-buffer mode to present in the interface but is not yet implemented. |
| Map click captured | Point-and-buffer mode to present in the interface but is not yet implemented. |
| Buffer polygon appears | Point-and-buffer mode to present in the interface but is not yet implemented. |
| Buffer becomes active AOI | Point-and-buffer mode to present in the interface but is not yet implemented. |
| Analysis runs on buffer AOI | Point-and-buffer mode to present in the interface but is not yet implemented. |

## AOI workflow status

| AOI method | Interface visible | AOI become active | Analysis works | Status |
| ---------- | ----------------- | ----------------- | -------------- | ------ |
| Upload polygon | Yes | Done | Done | Done |
| Jambongon test AOI | Yes | Done | Done | Done |
| Draw polygon | Yes | Done | Done | Done |
| Point and buffer | Yes | Done | Done | Done |

### All outputs with one final AOI test

| Output | Works? | Notes |
| Result table | Yes | Complete |
| Comparison table | Yes | Complete |
| Comparison graph | Yes | Complete |
| Dry-condition note | Yes | Complete |
| Result CSV | Yes | Complete |
| Comparison CSV | Yes | Complete |
| Cropped raster | Yes | Complete |

### Results tab layout

| Layout check | Result |
| ------------ | ------ |
| Analysis summary visible | Done |
| Result table easy to find | Done |
| Comparison table easy to read | Done |
| Graph not too large | Done |
| Downloads easy to find | Done |
| Notes are not long | Done |

## Known limitations at end of Week 2

Only six pilot variables are currently connected.
Only baseline / 1981-2010 and SSP2-4.5 / 2041-2070 are currently available for most vaiables.
The map displays one selected raster at a time, while the comparison table compares available scenario-period combinations.
Draw-polygon and point-buffer AOI tools are not yet fully implemented if testing confirms this.
No combined overall-risk score is produced.
Results are screening summaries only.

## Week 2 progress summary

Week 2 moved the prototype from single-raster analysis to comparison and export. The app can now compare available baseline and future scenario-period combinations, calculate change from baseline, display a comparison graph, download result and comparison CSVs, and download cropped raster outputs. Testing confirmed that uploaded AOIs and the Jambongan test AOI can be used in the workflow. Remaining priorities are AOI drawing/buffer tools, layout refinement, code cleanup, and preparation for a simple demonstration version.

## Proposed Week 3 priorities

1. Implement or complete draw-polygon AOI.
2. Implement or complete point-and-buffer AOI.
3. Clean up app.R by moving repeated code into helper functions.
4. Improve user-facing labels and notes.
5. Test the app with more AOIs.
6. Prepare a short demonstration workflow.
7. Review how additional SSPs and periods will be integrated when rasters are available.
