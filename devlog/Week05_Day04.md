# Week 5 - Day 4

## Main objective

Prepare the final friendly testing pack, run a full dry run, confirm the backup workflow, and record the testing version.

## Starting check

| Check | Result |
| ----- | ------ |
| Latest GitHub version pulled | Done |
| App starts | Done |
| Main Fire demo works | Done |
| Optional WBGT demo works | Done |
| Downloads work | Done |
| Testing files exist | Done |
| Known issues file exists | Done |
| Demo prompt card exists | Done |
| Backup workflow recorded | Done |
| Candidate testing commit recorded | Done |

## Testing folder check

| File | Exists? | Ready? |
| ---- | ------- | ------ |
| friendly_testing_guide.md | Yes | Yes |
| technical_feedback_sheet.md | Yes | Yes |
| tester_task_list.md | Yes | Yes |
| dr_corine_observation_record.md | Yes | Yes |
| test_session_notes_template.md | Yes | Yes |
| known_issues_for_testing.md | Yes | Yes |
| marcolm_demo_prompt_card.md | Yes | Yes |

## Tester task list final review

| Check | Result |
| ----- | ------ |
| Local-run setup clear | Done |
| Main demo included | Done |
| AOI task included | Done |
| Cropped raster task included | Done |
| Optional WBGT task included | Done |
| Feedback focus clear | Done |

## Technical feedback sheet final review

| Check | Result |
| ----- | ------ |
| AOI questions included | Done |
| Table questions included | Done |
| Graph questions included | Done |
| Download questions included | Done |
| WBGT question included | Done |
| Priority improvement question included | Done |

## Dr Corine observation record final review

| Check | Result |
| ----- | ------ |
| Preparation section ready | Done |
| Communication section ready | Done |
| Feedback response section ready | Done |
| Problem-solving section ready | Done |
| Supports section ready | Done |
| Summary section ready | Done |

## Known Issues for Friendly Testing

| Issue | Severity | Workaround |
| ----- | -------- | ---------- |
| Not all variables are available for every scenario and period | Medium | Check Data Availability tab |
| WBGT is monthly average maximum, not daily extreme WBGT | Medium | Explain before showing WBGT |
| Cropped raster output can be slower | Low | Leave cropped raster output off unless needed |
| Large AOIs may take longer to process | Medium | Use Jambongan for demo |
| Draw polygon is for exploratory testing | Low | Use uploaded AOI for formal boundaries |
| Point-buffer is for exploratory testing | Low | Use uploaded AOI for formal boundaries |

## Testing materials prepared

| Material | Paper copy? | Digital copy? | Ready? |
| -------- | ----------- | ------------- | ------ |
| Tester task list | Yes | Yes | Yes |
| Technical feedback sheet | Yes | Yes | Yes |
| Friendly testing guide | Yes | Yes | Yes |
| Session notes template | Yes | Yes | Yes |
| Known issues | Yes | Yes | Yes |
| Dr Corine observation record | Yes | Yes | Yes |
| Demo prompt card | Yes | Yes | Yes |

## Timed dry run

| Section | Target time | Actual time | Notes |
| ------- | ----------: | ----------: | ----- |
| Opening explanation | 3 min | 4 min | It's 1 minute late |
| Main Fire demo | 10 min | 8 min | It's 2 minutes early |
| AOI task | 8 min | 6min | It's 2 minutes early |
| Optional WBGT demo | 5 min | 4 min | It's 1 minutes early |
| Downloads and Data Availability | 5 min | 6 min | It's 1 minute late |
| Feedback questions | 10 min | 6 min | It's 4 minutes early |
| Wrap-up | 2 min | 3 min | It's 1 minute late |

## Main demo dry run

| Check | Result |
| ----- | ------ |
| Jambongan loads | Done |
| Fire comparison works | Done |
| Result table appears | Done |
| Comparison table appears | Done |
| Graph appears | Done |
| Comparison CSV download | Done |
| Explanation clear | Done |

## AOI task dry run

| Check | Result |
| ----- | ------ |
| AOI method used | Done |
| AOI loads | Done |
| Active AOI status clear | Done |
| Bio05 analysis works | Done |
| Result table appears | Done |
| Result CSV downloads | Done |

## Optional WBGT dry run

| Check | Result |
| ----- | ------ |
| WBGT analysis works |  |
| Units correct |  |
| WBGT note clear |  |
| Cropped raster appears |  |
| Cropped raster downloads |  |
| Explanation accurate |  |

## Downloads and Data Availability dry run

| Check | Result |
| ----- | ------ |
| Result CSV downloads | Done |
| Comparison CSV downloads | Done |
| Cropped raster downloads | Done |
| Data Availability opens | Done |
| Data Availability explanation clear | Done |

## Marcolm laptop readiness

| Check | Result |
| ----- | ------ |
| App starts | Done |
| Rasters found | Done |
| Main demo works | Done |
| WBGT demo works | Done |
| Downloads work | Done |

## Backup plan for testing session

If upload AOI, draw polygon, point-buffer, WBGT or cropped raster output fails, use the reliable fallback:

Jambongan → Fire Probability → comparison → graph → comparison CSV.

If the app freezes:
1. stop the app;
2. restart RStudio if needed;
3. run the app again;
4. use Jambongan Fire comparison only.

If a tester asks for a feature that is not ready:
"I will record that as a suggestion for later development."

## Testing version

Git commit: Done

Status: Ready with known issues

Notes: It is ready.

## Final freeze rule

No new features should be added before the friendly technical testing session.

Allowed changes only:
- app does not start;
- main demo fails;
- downloads fail;
- wrong or misleading WBGT wording;
- missing testing file;
- broken raster catalogue row needed for the demo.

Avoid:
- new variables;
- new scenarios;
- new graphs;
- risk scoring;
- layout redesign;
- major refactoring.

## Day 4 summary

Week 5 Day 4 focused on preparing the final friendly testing pack and running a full dry run. Testing files were checked, printed or prepared digitally, the main Jambongan Fire comparison demo was rehearsed, the AOI task and optional WBGT demo were tested, downloads and Data Availability were checked, the backup plan was confirmed, and the testing version was recorded.

