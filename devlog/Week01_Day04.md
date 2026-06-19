AOI | Variable | Mean | Minimum | Maximum

Jambongan | Bio05 | 30.97 | 30.56 | 31.65

Papar Buayan | Bio05 | 28.26 | 24.4 | 30.69

Main changes completed:
- Updated the app so uploaded AOIs, such as Papar Buayan, and the built-in Jambongan test AOI use the same workflow.
- Removed separate AOI Test variable, scenario, and period selectors so the AOI Test tab now uses the main app selections.
- Confirmed that Jambongan and Papar Buayan should have the same available variables, scenario options, period options, and result fields.
- Updated the AOI Test table so Mean, Minimum,and Maximum are no longer placeholder values. These are now calculated from the selected raster using the active AOI.
- Added checks so the active AOI name is stored with each result. this helps prevent results from one AOI being displayed as if they belong to another AOI.
- Added temporary diagnostic outputs to compare the AOI shown on the Leaflet map with the AOI passed into raster processing.
- Added a temporary export of the active AOI before raster processing: outputs/tests/current_active_aoi.gpkg.This allows the exact AOI used in the analysis to be opened and checked in QGIS.

Run analysis button update:
- Added shinyjs support to disable the Run analysis button when no AOI is active.
- The button is enabled only after an AOI has been uploaded or the Jambongan test AOI has been locked.
- Added a fallback validation message: Select or upload an AOI before running the analysis.

Testing workflow:

The test workflow is now:
1. Upload or load an AOI.
2. Confirm the sidebar shows the correct active AOI.
3. Confirm the AOI appears correctly on the map.
4. Select the same variable, scenario, and period for all AOIs.
5. Run the analysis.
6. Check the Results tab.
7. Check the temporary exported AOI in QGIS.

The current comparison test is:
- AOI: Papar Buayan.
- Variable: Bio05.
- Scenario: SSP2-4.5.
- Period: 2041-2070.

The same test should also be run for Jambongan. The output fields and selected variables should be the same for boths AOIs. Only the Mean, Minimum, and Maximum values should differ because the AOI polygons cover different areas.

Issues found:
- The app was previously still showing or using Jambongan in places where the active uploaded AOI should have been used.
- The AOI Test tab previously had its own selectors, which could lead to testing Jambongan and Papar Buayan with different variables or periods.
- Mean, Minimum, and Maximum in the AOI Test table were placeholder text rather than real extracted raster values.
- The shinyjs package was initially missing and caused the app to fail at startup, but it was later installed successfully.

Current status:
The app now has a clearer active AOI workflow. Uploaded AOIs and the Jambongan test AOI should use the same variable selection, raster lookup, result table structure, and analysis process. Additional testing is still needed to confirm that Papar Buayan and jambongan produce different spatial statistics but use the same selected raster user.
