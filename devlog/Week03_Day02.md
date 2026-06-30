# Week 3 - Day 2

## Main objective

Stabilise the draw-polygon AOI workflow and test clearing, redrawing, AOI switching, analysis, comparison, graph, and downloads.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clear session | Done |
| Draw polygon tool appears | Done |
| Upload polygon still works | Done |
| Jambongan AOI still works | Done |
| Point-buffer still works | Done |
| Comparison table still works | Done |
| Graph still works | Done |
| Downloads still work | Done |

## Day 1 draw-polygon check

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Draw tool appears | Yes | Done | Pass |
| Polygon can be drawn | Yes | Done | Pass |
| Active AOI updates | Drawn_AOI | Done | Pass |
| Run analysis enabled | Yes | Done | Pass |
| Old results cleared | Yes | Done | Pass |

## Clear drawn AOI test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Drawn polygon removed | Yes | Done | Pass |
| AOI outline removed | Yes | Done | Pass |
| Raster removed | Yes | Done | Pass |
| Legend removed | Yes | Done | Pass |
| Active AOI cleared | Yes | Done | Pass |
| Run analysis disabled | Yes | Done | Pass |
| Results cleared | Yes | Done | Pass |

## Clear AOI fix

Describe any changes made to ensure drawn AOIs and old resultsare fully cleared.

## Redraw test

| Step | Expected | Actual | Pass/Fail |
| ---- | -------- | ------ | --------- |
| First polygon drawn | Drawn_AOI active | Done | Pass |
| Bio05 run on the first polygon | Results appear | Done | Pass |
| Clear drawn AOI | First polygon removed | Done | Pass |
| Second polygon drawn | New Drawn_AOI active | Done | Pass |
| Bio05 run on second polygon | Results use second polygon | Done | Pass |

