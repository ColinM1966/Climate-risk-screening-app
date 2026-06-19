App starts: Yes
Configuration controls load: Yes
Map loads: Yes
Run analysis fully working: Not yet / Partly
AOI upload working: Not yet / Partly

Variable | CDD | PPETmin | PPETConDryMth | Fire | Bio05 | Bio017
Analysis passed | Yes | Yes | Yes | Yes | Yes | Yes
Table displayed | Yes | Yes | Yes | Yes | Yes | Yes
Raster displayed | Yes | Yes | Yes | Yes | Yes | Yes
Notes | Unit correct | Unit correct | Unit correct | Unit correct | Unit correct | Unit correct

Selected or load AOI.
Select a variable.
Select one variable.
Add raster path.
AOI must be in raster path.
Invalid AOI.

Work completed:
- Added a temporary Jambongan AOI test option using data/examples/Jambongan.gpkg.
- Loaded and prepared the AOI with prepare_AOI().
- Displayed the selected AOI on the Leaflet map.
- Added the variable selector that includes Bio05, Bio017, CDD, and other enabled continuous variables.
- Kept the workflow to process one selected variable at a time.
- Added named scenario choices:
  - Baseline
  - SSP2-4.5
- Added named period choices:
  - 1981-2010.
  - 2041-2070.
- Added checks to ensure exactly one raster record is returned.
- Added raster file-path and file-existence validation.
- Connected the selected raster and AOI to process_continuous_raster().
- Stored the analysis output in rv$result.
- Stored the cropped raster separately in rv$cropped_raster.
- Added a loading progress bar showing:
  - Finding raster.
  - Preparing AOI.
  - Calculating statistics.
  - Complete.
- Added a simple result table showing:
  - AOI.
  - Variable.
  - Scenario.
  - Period.
  - Mean.
  - Minimum.
  - Maximum.
  - Units.
- Added analysis status text confirming the completed selection.
- Added the processed raster to the Leaflet map.
- Added a basic continuous colour legend.
- Used variable metadata for the legend title and units.

Problems encountered:
- Older helper functions remained in the script and caused repeated argument errors.
- Duplicate raster observers caused the processed raster to be drawn more than once.
- the UI and server used different result-table output IDs.
- An attempted multi-variable version introduced sourcing and parser errors.
- The workflow was simplified back to one variable at a time for stability.
- Named labels had to be separated from the internal catalogue values.

Current status:
- Jambongan can be loaded and shown on the map.
- A continuous raster can be selected, proceed, summarised, and displayed.
- The results table and status text update after a successful analysis.
_ Loading and error feedback are now visible to the user.
