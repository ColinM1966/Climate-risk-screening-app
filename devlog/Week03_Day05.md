# Week 3 - Day 3

## Main objective 

Add an optional cropped-raster output control and test the app with another climatology and scenario.

## Starting check

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Upload AO works | Done |
| Point-buffer works | Done |
| Comparison table works | Done |
| Graph works | Done |
| Result CSV download works | Done |
| Comparison CSV download works | Done |

## Current cropped raster behaviour

| Check | Result |
| ----- | ------ |
| Cropped raster created automatically | Done |
| Raster shown on map | Done |
| Cropped raster download available | Done |
| Output file created in outputs folder | Done |
| Processing time acceptable | Done |

## Output option added

Added a checkbox so users can choose whether to create cropped raster output.

## Map behaviour with cropped raster off

| Check | Result |
| ----- | ------ |
| AOI outline remains visible | Done |
| Result table appears | Done |
| Raster layer not shown | Done |
| App does not crash | Done |

## Cropped raster OFF test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Analysis runs | Yes | Yes | Pass |
| Result table appears | Yes | Yes | Pass |
| Result CSV downloads | Yes | Yes | Pass |
| Cropped raster button hidden | Yes | Yes | Pass |
| AOI outline remains | Yes | Yes | Pass |
| App does not crash | Yes | Yes | Pass |

## Cropped raster ON test

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Analysis runs | Yes | Yes | Pass |
| Raster appears on map | Yes | Yes | Pass |
| Cropped raster button appears | Yes | Yes | Pass |
| Cropped raster downloads | Yes | Yes | Pass |
| Raster opens in QGIS/R | Yes | Yes | Pass |

## Comparison with cropped raster OFF

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Result table appears | Yes | Yes | Pass |
| Comparison table appears | Yes | Yes | Pass |
| Graph appears | Yes | Yes | Pass |
| Comparison CSV downloads | Yes | Yes | Pass |
| Cropped ratser button hidden | Yes | Yes | Pass |

## Comparison with cropped raster ON

| Check | Result | Actual | Pass/Fail |
| ----- | ------ | ------ | --------- |
| Comparison table appears | Yes | Yes | Pass |
| Graph appears | Yes | Yes | Pass |
| Raster appears on map | Yes | Yes | Pass |
| Cropped raster button appears | Yes | Yes | Pass |
\ Cropped raster downloads | Yes | Yes | Pass |
