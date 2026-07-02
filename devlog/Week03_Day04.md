# Week 3 - Day 4

## Main objective

Improve the point-and-buffer AOI workflow and confirm that all AOI methods still work after the Day 3 AOI helper cleanup.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Upload AOI still works | Done |
| Jambongan AOI still works | Done |
| Draw-polygon AOI still works | Done |
| Point-buffer AOI still works | Done |
| Comparison table still works | Done |
| Graph still works | Done |
| Downloads still work | Done |

## Starting AOI tests

| AOI method | AOI loads | Analysis works | Map updates | Downloads work | Pass/Fail |
| ---------- | --------- | -------------- | ----------- | -------------- | --------- |
| Upload polygon | Done | Done | Done | Done | Pass |
| Jambongan | Done | Done | Done | Done | Pass |
| Draw polygon | Done | Done | Done | Done | Pass |
| Point-buffer | Done | Done | Done | Done | Pass |

## Point-buffer instruction update

Updated the point-buffer help text so it no longer says the tool is inactive if the workflow is now working.

## Point-buffer loading test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Map click creates buffer | Yes | Yes | Pass |
| Active AOI updates | Point_buffer | Yes | Pass |
| AOI outline appears | Yes | Yes | Pass |
| Run analysis enabled | Yes | Yes | Pass |
| Old results cleared | Yes | Yes | Pass |

## Point-buffer single-analysis test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Bio05 analysis run | Yes | Yes | Pass |
| Raster clipped for buffer | Yes | Yes | Pass |
| Result CSV downloads | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |

## Point-buffer comarison test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Fire comparison runs | Yes | Yes | Pass |
| Missing combinations skipped | Yes | Yes | Pass |
| Change from baseline appears | Yes | Yes | Pass |
| Graph appears | Yes | Yes | Pass |
| Comparison CSV downloads | Yes | Yes | Pass |

## Clear point-buffer test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Buffer outline removed | Yes | Yes | Pass |
| Raster removed | Yes | Yes | Pass |
| Legend removed | Yes | Yes | Pass |

## Clear point-buffer safety test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Jambongan remains alive | Yes | Yes | Pass |
| AOI not accidentally cleared | Yes | Yes | Pass |
| Message is clear | Yes | Yes | Pass |

## Buffer distance test

| Step | Expected | Actual | Pass/Fail |
| ---- | -------- | ------ | --------- |
| 5 km buffer created | Point_buffer_5km | Yes | Pass |
| Bio05 runs on 5 km buffer | Yes | Yes | Pass |
| 20 km buffer created | Point_buffer_20km | Yes | Pass |
| Bio05 runs on 20 km buffer | Yes | Yes | Pass |
| Old result cleared | Yes | Yes | Pass |

## AOI switching involving point-buffer

| Step | Expected active AOI | Result correct? | Notes |
| ---- | ------------------- | --------------- | ----- |
| Point-buffer | Point_buffer | Yes | Completed |
| Draw polygon | Drawn_AOI | Yes | Completed |
| Point-buffer | Point_buffer | Yes | Completed |
| Upload Segama | Segama_catchment | Yes | Completed |
| Point-buffer | Point_buffer | Yes | Completed |
| Jambongan | Jambongan | Yes | Completed |

## Point-buffer edge-case tests

| Test | Expected | Actual | Needs fixing? |
| ---- | -------- | ------ | ------------- |
| Point mode selected but no click | No AOI loaded | Yes | Yes |
| Buffer distance = 0 | Clear warning | Yes | Yes |
| Buffer distance = 0.1 km | Works | Yes | Yes |
| Buffer distance = 100 km | Works | Yes | Yes |
| Click outside Sabah | AOI created, analysis may fail clearly | Yes | Yes |
| Click outside raster coverage | Clear analysis error | Yes | Yes |

## AOI status message check

| AOI method | Status message clear? | Notes |
| ---------- | --------------------- | ----- |
| Upload polygon | Yes | Completed |
| Jambongan | Yes | Completed |
| Draw polygon | Yes | Completed |
| Point-buffer | Yes | Completed |

## AOI workflow status

| AOI method | Interface visible | AOI becomes active | Analysis works | Clearing/switching works | Status |
| ---------- | ----------------- | ------------------ | -------------- | ------------------------ | ------ |
| Upload polygon | Yes | Yes | Yes | Yes | Working |
| Jambongan test AOI | Yes | Yes | Yes | Yes | Working |
| Draw polygon | Yes | Yes | Yes | Yes | Working |
| Point and buffer | Yes | Yes | Yes | Yes | Working |

## Day 4 summary

Day 4 focused on improving the point-and-buffer AOI workflow. The app was tested to confirm whether clicking the map creates a buffered AOI, whether the buffer can be cleared, whether buffer distance changes work, and whether analysis, comparison, graphing and downloads still work. The point-buffer instructions were updated so users understand how to ceate and clear a buffer AOI.

## Proposed DAy 5 priporities

1. Fix any remaining point-buffer or draw-polygon issues.
2. Improve the Data Availability tab.
3. Review Results tab layout after adding graph and downloads.
4. Do a full end-to-end user test with all AOI methods.
5. Prepare a Week 3 progress summary and Week 4 priorities.
