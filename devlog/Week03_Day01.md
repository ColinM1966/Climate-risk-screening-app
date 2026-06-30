# Week 3 - Day 1

## Main objective

Implement draw-polygon AOI selection and connect it to the active AOI workflow.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Upload polygon still works | Done |
| Jambongan AOI still works | Done |
| Point-buffer status checked | Done |
| Comparison table still works | Done |
| Comparison graph still works | Done |
| Downloads still work | Done |

## Starting AOI checks

| AOI method | Works before draw-polygon changes? | Notes |
| ---------- | ---------------------------------- | ----- |
| Upload polygon | Done | Completed |
| Jambongan test AOI | Done | Completed |
| Point-buffer | Done | Completed |

## Package update

Added `leaflet.extras` to to support drawing polygons on the map.

## Draw-polygon loading test

| Check | Result |
| ----- | ------ |
| Drawing tool visibile | Done |
| Polygon can be drawn | Done |
| Active AOI updates | Done |
| Run analysis enabled | Done |
| Old result cleared | Done |

## Drawn AOI single-analysis test

| Check | Result |
| ----- | ------ |
| Bio05 analysis run | Done |
| Raster clipped to drawn AOI | Done |
| Result table AOI name correct | Done |
| Result CSV downloads | Done |
| Cropped raster downloads | Done |

## Drawn AOI comparison test

| Check | Result |
| ----- | ------ |
| Fire comparison runs | Done |
| Baseline row appears | Done |
| Future row appears | Done |
| Missing combinations skipped | Done |
| Change from baseline calculated | Done |
| Graph appears | Done |
| Comparison CSV downloads | Done |

## Switching AOI test

| Step | Expected AOI | Result |
| ---- | ------------ | ------ |
| Draw polygon | Drawn_AOI | Done |
| Upload Segama | Segama_catchment | Done |
| Run analysis after upload | Results use Segama_catchment | Done |

## Uploaded-to-drawn AOI swtching test

| Step | Extected AOI | Result |
| ---- | ------------ | ------ |
| Draw polygon | Drawn_AOI | Done |
| Upload Segama | Segama_catchment | Done |
| Run analysis after upload | Results use Segama_catchment | Done |

## Draw-polygon edge-case tests

| Test | Expected | Actual | Needs fixing? |
| ---- | -------- | ------ | ------------- |
| Draw mode selected but no polygon drawn | No AOI loaded | Done | Yes |
| Drawing cancelled | App does not crash | Done | Yes |
| Very small polygon | Either works or clear error | Done | Yes |
| Polygon outside Sabah | Clear error during analysis | Done | Yes |
| Polygon outside raster coverage | Clear error during analysis | Done | Yes |

| AOI method | Interface visible | AOI becomes active | Analysis works | Status |
| ---------- | ----------------- | ------------------ | -------------- | ------ |
| Upload polygon | Yes | Yes | Yes | Working |
| Jambongan test AOI | Yes | Yes | Yes | Working |
| Point and buffer | Yes | Yes | Yes | Working |
| Draw polygon | Yes | Yes | Yes | Working |

## Known limitations

- Draw-polygon AOI now works.
- Drawn AOIs are temporary and are not saved as separate user files.
- The map displays one selected raster at a time.
- Comparison works only for scenario-period combinations present in the raster catalogue.
- No combined overall-risk score is produced.
- Results are screening summaries only.
