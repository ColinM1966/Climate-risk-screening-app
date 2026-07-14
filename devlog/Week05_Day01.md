# Week 5 - Day 1

## Main objective

Final pre-testing readiness check for the friendly technical testing session.

## Starting check

| Check | Result |
| ----- | ------ |
| Latest GitHub version pulled | Done |
| App starts | Done |
| Rasters available | Done |
| Jambongan demo works | Done |
| Upload AOI demo works | Done |
| Optional WBGT demo works | Done |
| Cropped raster option works | Done |
| Downloads work | Done |
| Testing files exist | Done |
| Known issues file exists | Done |
| Candidate testing commit recorded | Done |

## Raster folder check

| Check | Result |
| ----- | ------ |
| rasters folder exists | Done |
| baseline folder exists | Done |
| future folder exists | Done |
| ssp245 folder exists | Done |
| ssp370 folder exists, if used | Done |
| ssp585 folder exists, if used | Done |
| WBGT files found | Done |

## Raster catalogue testing check

| Variable | Scenario | Period | File path present? | Enabled? | File exists? |
| -------- | -------- | ------ | ----------------- | -------- | ------------ |
| Fire Probability | baseline | 1981-2010 | Yes | Yes | Yes |
| Fire Probability | ssp245 | 2041-2070 | Yes | Yes | Yes |
| Bio05 | ssp245 | 2041-2070 | Yes | Yes | Yes |
| WBGTmax | available scenario | available period | Yes | Yes | Yes |
| PPETmin | available scenario | available period | Yes | Yes | Yes |

## App startup check

| Check | Result |
| ----- | ------ |
| App opens | Done |
| Map appears | Done |
| No package error | Done |
| No catalogue error | Done |

## Main demo rehearsal

| Check | Result |
| ----- | ------ |
| Jambongan loads | Done |
| Fire comparison runs | Done |
| Baseline row appears | Done |
| SSP2-4.5 row appears | Done |
| Change from baseline appears | Done |
| Graph appears | Done |
| Comparison CSV downloads | Done |

## Main demo explanation practice

| Practice | Result |
| -------- | ------ |
| First practice | Done |
| Second practice | Done |
| What was difficult to explain | Done |

## Uploaded AOI demo rehearsal

| Check | Result |
| ----- | ------ |
| Segama upload works | Done |
| Bio05 analysis works | Done |
| Cropped raster appears | Done |
| Result CSV downloads | Done |
| Cropped raster downloads | Done |

## Optional WBGT demo rehearsal

| WBGT scenario tested | Done |
| WBGT period tested | Done |
| WBGT analysis runs | Done |
| Units correct | Done |
| Result note clear | Done |
| Cropped raster downloads | Done |

## Cropped raster ON/OFF check

| Check | Result |
| ----- | ------ |
| OFF: table appears | Done |
| OFF: cropped raster button hidden | Done |
| ON: raster appears | Done |
| ON: cropped raster downloads | Done |

## AOI method check

| AOI method | Loads? | Analysis works? | Safe for testing? | Notes |
| ---------- | ------ | --------------- | ----------------- | ----- |
| Jambongan | Yes | Yes | Yes | It work! |
| Upload polygon | Yes | Yes | Yes | It work! |
| Draw polygon | Yes | Yes | Yes | It work! |
| Point-buffer | Yes | Yes | Yes | It work! |

## Data Availability check

| Check | Result |
| ----- | ------ |
| Data Availability tab opens | Done |
| WBGT visible | Done |
| WBGT monthly note visible | Done |
| SSP3-7.0 visible where connected | Done |
| SSP5-8.5 visible where connected | Done |
| File exists column understandable | Done |
| Enabled column understandable | Done |

## Result note check

| Variable | Note correct? | Needs fixing? |
| -------- | ------------- | ------------- |
| Fire Probability | Yes | Yes |
| Bio05 | Yes | Yes |
| Bio017 | Yes | Yes |
| PPETmin | Yes | Yes |
| WBGTmax | Yes | Yes |

## Testing files check

| File | Exists? | Reviewed |
| ---- | ------- | -------- |
| friendly_testing_guide.md | Yes | Done |
| techical_feedback_sheet.md | Yes | Done |
| tester_task_list.md | Yes | Done |
| dr_corine_observation_record.md | Yes | Done |
| test_session_notes_template.md | Yes | Done |
| known_issues_for_testing.md | Yes | Done |

## Tester task list review

| Check | Result |
| ----- | ------ |
| Local-run setup clear | Done |
| Main demo included | Done |
| AOI task included | Done |
| Cropped raster task included | Done |
| Optional WBGT included | Done |

## Feedback sheet review

| Check | Result |
| ----- | ------ |
| AOI feedback included | Done |
| Result table feedback included | Done |
| Comparison feedback included | Done |
| Graph feedback included | Done |
| Cropped raster feedback included | Done |
| WBGT feedback included | Done |
| Priority improvement question included | Done |

## Dr Corine observation record review

| Check | Result |
| ----- | ------ |
| Preparation section included | Done |
| Communication section included | Done |
| Response to feedback included | Done |
| Problem solving included | Done |
| Supports included | Done |
| Summary section included | Done |

## Candidate testing version

Git commit:

Status:
- Ready for testing
- Ready with known issues
- Not ready yet

Notes:

## Freeze rules

After this point, no new features should be added before the firendly technical testing session unless they are required to fix a serious blocker.

Allowed changes:
- app does not start;
- main demo fails;
- downloads fail;
- a major label is misleading;
- a testing file is missing.

Avoid:
- new variables;
- new scenarios;
- new graph types;
- risk scoring;
- layout redesign.

## Day 1 summary

Week 5 Day 1 focused on final pre-testing readiness. The main Jambongan Fire comparison demo, uploaded AOI demo, optional WBGT demo, cropped raster ON/OFF behaviour, downloads, Data Availability tab and testing files were checked. Known issues and the candidate testing version were recorded. After this point, only serious blockers should be fixed before the friendly technical testing session.
