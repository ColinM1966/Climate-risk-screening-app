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
| Map click creates buffer | Yes | Done | Pass |
| Active AOI updates | Point_buffer |Done  | Pass |
| AOI outline appears | Yes | Done | Pass |
| Run analysis enabled | Yes | Done | Pass |
| Old results cleared | Yes | Done | Pass |
