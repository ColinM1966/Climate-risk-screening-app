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
