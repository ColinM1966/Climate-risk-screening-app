Week 1 - Day 5:

Main objective:

Stabilise the GitHub version of the prototype, test active AOI switching, test active AOI switching, test all six variables, and check baseline/future raster behaviour.

GitHub sync check:

| Check | Result |
| Local branch up to date with origin/main | Done |
| App runs from clean session | Done |
| Latest active AOI code present | Done |

Active AOI tests:

| Step | Expected AOI | Actual AOI | Pass/Fail |
| Startup | None | Yes | Pass |
| Jambongan selected | Jambongan | Yes | Pass |
| Papar Buayan uploaded | Papar Buayan | Yes | Pass |
| Jambongan selected again | Jambongan | Yes | Pass |

AOI comparison:

| AOI | Variable | Mean | Minimum | Maximum |
| Jambongan | Bio05 | 30.97 | 30.56 | 31.65 |
| Papar_Buayan | Bio05 | 28.26 | 24.4 | 30.69 |

Six-variable Jambongan tests:

| Variable | Future passed | Baseline passed | Units correct | Legend OK |
| CDD | Yes | Yes | Yes | Yes |
| Bio05 | Yes | Yes | Yes | Yes |
| Bio017 | Yes | Yes | Yes | Yes |
| Fire | Yes | Yes | Yes | Yes |
| PPETConDryMth | Yes | Yes | Yes | Yes |
| PPETmin | Yes | Yes | Yes | Yes |

Uploaded AOI tests:

| Variable | Papar Buayan passed | Notes |
| Bio05 | Yes | It passed. |
| CDD | Yes | It passed. |
| PPETmin | Yes | It passed. |

Error handling:

| Test | Result |
| No AOI | Done |
| Invalid upload | Done |
| Unsupported raster combination | Done |

Interction test:

Person: Colin
Date: 20 June 2026
Duration: 2 minutes
Task attempted: Both tasks on new AoI
Where they hesitated: None
Question they asked: None
Suggestion received: able to show/selct multipe scenarios and time periods. Able to download results, graphs and maps (user defined what tehy want)
Change made or planned: More scenarios and more time periods.

Screenshots:

- https://github.com/ColinM1966/Climate-risk-screening-app/commit/776cd18c9656f31e5acc77057caf565374348c75
- https://github.com/ColinM1966/Climate-risk-screening-app/commit/f5c8c64b3ff07d863a50f7464a054417e1aff753
- https://github.com/ColinM1966/Climate-risk-screening-app/commit/4579d0283d049df050a568eacbafd18f1f41752b
- https://github.com/ColinM1966/Climate-risk-screening-app/commit/90c648e963c778a70de12dccaed0f2311b56c959
- <img width="1467" height="756" alt="image" src="https://github.com/user-attachments/assets/77a6e8cd-6fa2-4e61-a96c-d0dad6d0b34a" />

Next steps:

Prepare Week 2 work on draw polygon, point-and-buffer AOI, result downloads, usability testing, and code cleanup.
