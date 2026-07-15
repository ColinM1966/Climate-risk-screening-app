# Week 5 - Day 2

## Main objective

Run the final pre-testing rehearsal, fix only serious blockers, check both laptops if possible, and freeze the candidate testing version.

## Starting check

| Check | Result |
| ----- | ------ |
| Latest Github version pulled | Done |
| App starts | Done |
| Main Jambongan Fire demo works | Done |
| Uploaded AOI demo works | Done |
| Optional WBGT demo works | Done |
| Cropped raster ON/OFF works | Done |
| Downloads work | Done |
| Testing files exist | Done |
| Known issues file updated | Done |
| Candidate testing commit recorded | Done |

## Full rehearsal

| Section | Completed? | Notes |
| ------- | ---------- | ----- |
| Opening explanation | Yes | It is correct. |
| Main Fire demo | Yes | It is correct. |
| Uploaded/user AOI demo | Yes | It is correct. |
| Optional WBGT demo | Yes | It is correct. |
| Downloads shown | Yes | It is correct. |
| Data Availability shown | Yes | It is correct. |
| Feedback questions explained | Yes | It is correct. |

## Opening explanation practice

| Practice | Result |
| -------- | ------ |
| First practice | Done |
| Second practice | Done |
| What sounded clear | Done |
| What was difficult | Done |

## Main demo final rehearsal

| Check | Result |
| ----- | ------ |
| Jambongan loads | Done |
| Fire comparison runs | Done |
| Graph appears | Done |
| Comparison CSV downloads | Done |
| Explanation clear | Done |

## Uploaded AOI rehearsal

| Check | Result |
| ----- | ------ |
| Segama upload works | Done |
| Bio05 analsis works | Done |
| Cropped raster appears | Done |
| Result CSV downloads | Done |
| Cropped raster downloads | Done |

## Optional WBGT rehearsal

| Check | Result |
| ----- | ------ |
| WBGT analysis works | Done |
| Units correct | Done |
| WBGT note clear | Done |
| Cropped raster works | Done |
| Explanation accurate | Done |

## Cropped raster final check

| Test | Result |
| ---- | ------ |
| Cropped raster OFF behaves correctly | Done |
| Cropped raster ON behaves correctly | Done |
| Cropped raster download works | Done |

## Final download check

| Download | Works? | Opened? | Notes |
| -------- | ------ | ------- | ----- |
| Result CSV | Yes | Yes | It works! |
| Comparison CSV | Yes | Yes | It works! |
| Cropped raster GeoTIFF | Yes | Yes | It works! |

## Marcolm laptop check

| Check | Result |
| ----- | ------ |
| App starts | Done |
| Rasters found | Done |
| Main demo works | Done |
| WBGT demo works | Done |
| Downloads work | Done |

## Testing files final check

| File | Exists? | Ready |
| ---- | ------- | ----- |
| friendly_testing_guide.md | Yes | Yes |
| technical_feedback_sheet.md | Yes | Yes |
| tester_test_list.md | Yes | Yes |
| dr_corine_observation_record.md | Yes | Yes |
| test_session_notes_template.md | Yes | Yes |
| known_issues_for_testing.md | Yes | Yes |

## Backup workflow confirmed

Fallback workflow:

Jambongan → Fire Probability → comparison → graph → comparison CSV.

Use this if upload AOI, draw polygon, point-buffer, WBGT or cropped raster output fails.

## Known Issues for Friendly Testing

| Issue | Severity | Workaround |
| ----- | -------- | ---------- |
| Not all variables are available for every scenario and period | Medium | Check Data Availability tab |
| WBGT is monthly average maximum, not daily extreme WBGT | Medium | Explain before showing WBGT |
| Cropped raster output can be slower | Low | Leave cropped raster output off unless needed |
| Large AOIs may take longer to process | Medium | Use Jambongan for demo |
| Draw polygon may be experimental | Low/Medium | Use upload AOI or Jambongan if it fails |
| Point-buffer may be experimental | Low/Medium | Use upload AOI or Jambongan if it fails |

## Frozen testing version

Git commit:

Date:

Status:
- Ready for testing
- Read with known issues
- Not ready yet

Notes:

If the app is ready, mark: "Ready with known issues" That is usually more honest than "perfect".

## Freeze rule

After this point, no new features should be added before the friendly technical testing session unless they are required to fix a serious blocker.

Allowed changes:
- app does not start;
- main demo fails;
- downloads fail;
- a major label is misleading;
- a testing file is missing;
- raster catalogue row needed for the demo is broken.

Avoid:
- new variables;
- new scenarios;
- new graph types;
- risk scoring;
- layout redesign;
- major refactoring.

