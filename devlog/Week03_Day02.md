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
| Draw tool appears | Yes | Yes | Pass |
| Polygon can be drawn | Yes | Yes | Pass |
| Active AOI updates | Drawn_AOI | Yes | Pass |
| Run analysis enabled | Yes | Yes | Pass |
| Old results cleared | Yes | Yes | Pass |

## Clear drawn AOI test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Drawn polygon removed | Yes | Yes | Pass |
| AOI outline removed | Yes | Yes | Pass |
| Raster removed | Yes | Yes | Pass |
| Legend removed | Yes | Yes | Pass |
| Active AOI cleared | Yes | Yes | Pass |
| Run analysis disabled | Yes | Yes | Pass |
| Results cleared | Yes | Yes | Pass |

## Clear AOI fix

Describe any changes made to ensure drawn AOIs and old resultsare fully cleared.

## Redraw test

| Step | Expected | Actual | Pass/Fail |
| ---- | -------- | ------ | --------- |
| First polygon drawn | Drawn_AOI active | Yes | Pass |
| Bio05 run on the first polygon | Results appear | Yes | Pass |
| Clear drawn AOI | First polygon removed | Yes | Pass |
| Second polygon drawn | New Drawn_AOI active | Yes | Pass |
| Bio05 run on second polygon | Results use second polygon | Yes | Pass |

## Draw new polygon without clearing

| Check | Result |
| ----- | ------ |
| New polygon becomes active AOI | Done |
| Old polygon remains visible? | Done |
| Old results cleared? | Done |
| Needs fixing? | Done |

## Draw-mode instruction update

Updated draw-polygon help text so users how to finish the polygon.

## Drawn AOI single-analysis test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Result table appears | Yes | Yes | Pass |
| Raster appears | Yes | Yes | Pass |
| Raster clipped to drawn AOI | Yes | Yes | Pass |
| Result CSV downloads | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |

## Drawn AOI comparison test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Comparison table appears | Yes | Yes | Pass |
| Missing combinations skipped | Yes | Yes | Pass |
| Change from baseline appears | Yes | Yes | Pass |
| Graph appears | Yes | Yes | Pass |
| Comparison CSv downloads | Yes | Yes | Pass |

## Drawn AOI PPETmin test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| PPETmin analysis runs | Yes | Yes | Pass |
| Dry-condition note appears | Yes | Yes | Pass |
| Comparison graph appears | Yes | Yes | Pass |
| Result CSV downloads | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |

## Drawn AOI to Jambongan switching test

| Step | Expected AOI | Result |
| ---- | ------------ | ------ |
| Draw polygon | Drawn_AOI | Done |
| Load Jambongan | Jambongan | Done |
| Run analysis after loading Jambongan | Results use Jambongan | Done |

## Jambongan to drawn AOI switching test

| Step | Expected AOI | Results |
| ---- | ------------ | ------- |
| Load Jambongan | Jambongan | Done |
| Draw polygon | Drawn_AOI | Done |
| Run analysis after drawing | Results use Drawn_AOI | Done |

## Drawn AOI edge-case tests

| Test | Expected | Actual | Needs fixing? |
| ---- | -------- | ------ | ------------- |
| Draw mode selected but no polygon | No AOI loaded | Yes | No |
| Drawing cancelled | App does not crash | Yes | No |
| Very small polygon | Works or clear warning | Yes | No |
| Polygon outside Sabah | Clear analysis error | Yes | No |
| Polygon outside raster coverage | Clear analysis error | Yes | No |
| Polygon utside raster edge | Works or clear warning | Yes | No |

## Map layer behaviour

| Switch | Old AOI cleared? | Old raster cleared? | Legend correct? | Notes |
| ------ | ---------------- | ------------------- | --------------- | ----- |
| Drawn -> Drawn | Yes | Yes | Yes | Completed |
| Drawn -> Upload | Yes | Yes | Yes | Completed |
| Upload -> Drawn | Yes | Yes | Yes | Completed |
| Drawn -> Jambongan | Yes | Yes | Yes | Completed |
| Jambongan -> Drawn | Yes | Yes | Yes | Completed |
| Point-buffer -> Drawn | Yes | Yes | Yes | Completed |

## AOI workflow status

| AOI method | Interface visible | AOI becomes active | Analysis works | Clearing/switching works | Status |
| ---------- | ----------------- | ------------------ | -------------- | ------------------------ | ------ |
| Upload polygon | Yes | Yes | Yes | Yes | Working |
| Jambongan test AOI | Yes | Yes | Yes | Yes | Working |
| Point and buffer | Yes | Yes | Yes | Yes | Working |
| Draw polygon | Yes | Yes | Yes | Yes | Working |

## Day 2 Summary

Day 2 focused on stbilising the draw-polygon AOI workflow. The main tests checked whether drawn AOIs could be cleared, redrawn, switched with other AOI methods, and used for single analysis, comparison, graphing and downloads. Remaining issues will be carried forward to day 3.

## Proposed Day 3 priorities

1. Fix any remaining draw-polygon clearing or switching issues.
2. Improve point-and-buffer behaviour if needed.
3. Begin cleaning repeated AOI-loading code into a helper function.
4. Improve AOI naming and user-facing status messages.
5. Continue testing with Segama_catchment and other uploaded AOIs.
