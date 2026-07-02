# Week 3 - Day 3

## Main objective

Clean up repeated AOI workflow code by creating helper functions for loading AOIs, clearing old results, and updating map layers.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Upload AOI still works | Done |
| Jambongan AOI still works | Done |
| Point-buffer AOI still works | Done |
| Draw-polygon AOI still works | Done |
| Result CSV download still works | Done |
| Comparison CSV download still works | Done |
| Cropped raster download still works | Done |

## Pre-cleanup AOI tests

| AOI method | Active AOI updates | Analysis works | Map updates | Download work | Pass/Fail |
| ---------- | ------------------ | -------------- | ----------- | ------------- | --------- |
| Upload polygon | Done | Done | Done | Done | Pass |
| Jambongan test AOI | Done | Done | Done | Done | Pass |
| Point buffer | Done | Done | Done | Done | Pass |
| Draw polygon | Done | Done | Done | Done | Pass |

## Repeated code identified

| Repeated code block | Where found | Refacter needed? |
| ------------------- | ----------- | ---------------- |
| Clear results | Yes | No |
| Clear comparison results | Yes | No |
| Clear cropped raster | Yes | No |
| Clear map raster | Yes | No |
| Clear drawn AOI layer | Yes | No |

## AOI status improvement

Updated active AOI status to show name, feature count and geometry type.

## Post-cleanup AOI tests

| AOI method | Analysis works | Comparison works | Graph works | Downloads work | Pass/Fail |
| ---------- | -------------- | ---------------- | ----------- | -------------- | --------- |
| Upload polygon | Done | Done | Done | Done | Pass |
| Jambongan | Done | Done | Done | Done | Pass |
| Point-buffer | Done | Done | Done | Done | Pass |
| Draw polygon | Done | Done | Done | Done | Pass |

## AOI switching after cleanup

| Step | Expected active AOI | Result correct? | Notes |
| ---- | ------------------- | --------------- | ----- |
| Draw polygon | Drawn_AOI | Yes | Completed |
| Upload Segama | Segama_catchment | Yes | Completed |
| Point-buffer | Point_buffer | Yes | Completed |
| Jambongan | Jambongan | Yes | Completed |
| Draw new polygon | | Drawn_AOI | Yes | Completed |

## Download filename and AOI check

| AOI method | Download filename correct? | AOI column correct? | Pass/Fail |
| ---------- | -------------------------- | ------------------- | --------- |
| Upload polygon | Yes | Yes | Pass |
| Jambongan | Yes | Yes | Pass |
| Point-buffer | Yes | Yes | Pass |
| Draw polygon | Yes | Yes | Pass |

## Day 3 summary

Day 3 focused on cleaning up and stabilisiing the AOI workflow. Repeated AOI-loading and output-clearing code was moved into helper functions so uploaded AOIs, Jambongan, point-buffer and drawn polygons all use the same active AOI logic. Post-cleanup testing confirmed whether analysis, comparison, graphing and downloads still worked across AOI methods.

## Proposed Day 4 priorities

1. Fix any issues found after AOI refactoring.
2. Improve point-buffer user interface and instructions.
3. Add clearer AOI method status messages.
4. Begin improving the Data Availability tab.
5. Continue testing with Segama_catchment and other uploaded AOIs.
