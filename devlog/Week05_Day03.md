# Week 5 - Day 3

## Main objective

Make small user-facing improvements before friendly technical testing, especially clearer messages, clearer notes, and smoother demo workflow.

## Starting checks

| Check | Result |
| ----- | ------ |
| Latest GitHub version pulled | Done |
| App starts | Done |
| Main Fire demo works | Done |
| Optional WBGT demo works | Done |
| Data Availability tab opens | Done |
| Dwnloads work | Done |
| Testing files exist | Done |
| Known issues file updated | Done |

## Selection status improvement

| Check | Result |
| ----- | ------ |
| Variable label readable | Done |
| Scenario label readable | Done |
| Period label readable | Done |
| Internal IDs hidden where possible | Done |

## Active AOI status check

| Check | Result |
| ----- | ------ |
| No-AOI message clear  | Done |
| Loaded AOI name shown | Done |
| Feature count shown | Done |
| Geometry type shown | Done |

## Run button clarify

| Check | Result |
| ----- | ------ |
| User knows AOI is required before analysis | Done |
| Run button behaviour clear | Done |

## Results note improvement

| Variable | Note correct? | Notes |
| -------- | ------------- | ----- |
| Fire Probability | Yes | Higher values indicate greater modelled fire probability. |
| Bio05 | Yes | Higher values indicate hotter warm-moth conditions. |
| Bio017 | Yes | Lower values indicate drier conditions. |
| PPETmin | Yes | Lower values indicate drier conditions. |
| WBGTmax | Yes | Maximum WBGT is a heat-stress indicator. Higher values indicate greater potential heat exposure and reduced outdoor workability. This layer represents monthly average maximum WBGT, not daily extreme WBGT. |

## Comparison graph note check

| Check | Result |
| ----- | ------ |
| Mean-value explanation clear | Done |
| Min/max table explanation clear | Done |
| Bio017 and PPETmin warning retained | Done |

## Data Availability display check

| Check | Result |
| ----- | ------ |
| Variable labels readable | Done |
| Scenario labels readable | Done |
| Period labels readable | Done |
| Units visible | Done |
| File exists visible | Done |
| Enabled visible | Done |
| WBGT monthly note visible | Done |

## Scenario selector check

| Variable | Scenarios shown | Correct? |
| -------- | --------------- | -------- |
| Fire Probability | Yes | Yes |
| Bio05 | Yes | Yes |
| WBGTmax | Yes | Yes |
| PPETmin | Yes | Yes |

## Comparison selector check

| Variable | Comparison scenarios sensible? | Comparison periods sensible? | Notes |
| -------- | ------------------------------ | ---------------------------- | ----- |
| Fire Probability | Yes | Yes | The comparison scenarios and periods are sensible. |
| WBGTmax | Yes | Yes | The comparison scenarios and periods are sensible. |
| Bio05 | Yes | Yes | The comparison scenarios and periods are sensible. |

## Missing combination warning

| Check | Result |
| ----- | ------ |
| Missing combinations skipped safely | Done |
| Warning message understandable | Done |
| App does not crash | Done |

## About tab check

| Check | Result |
| ----- | ------ |
| Prototype status clear | Done |
| No risk score statement clear | Done |
| Draw/point-buffer limitation clear | Done |
| Formal boundary wording clear | Done |

## Developer Test tab decision

| Decision | Notes |
| -------- | ----- |
| Keep | Do not use Developer Test tab during friendly testing. |

## Main demo after improvements

| Check | Result |
| ----- | ------ |
| App starts | Done |
| Jambongan loads | Done |
| Fire comparison works | Done |
| Table appears | Done |
| Graph appears | Done |
| Comparison CSV downloads | Done |
| Notes are clear | Done |

## WBGT demo after improvements

| Check | Result |
| ----- | ------ |
| WBGT analysis works | Done |
| Units correct | Done |
| Note clear | Done |
| Cropped raster appears | Done |
| Cropped raster downloads | Done |

## Updated candidate testing version

Git commit:

Status:
- Ready for testing
- Ready with known issues
- Not ready yet

Notes:

## Day 3 summary

Week 5 Day 3 focused on small user-facing improvements before friendly technical testing. The selection status, AOI status, result notes, graph notes, Data Availability display, About tab wording and testing files were reviewed. The main Jambongan Fire comparison demo and optional WBGT demo were retested after improvements. No new major features were added.
