<img width="1917" height="1135" alt="image" src="https://github.com/user-attachments/assets/71890133-75d8-4e9a-bf55-57adee6072f7" />

> getwd()
[1] "C:/Users/User/Documents/Climate-risk-screening-app"

<img width="1121" height="1012" alt="image" src="https://github.com/user-attachments/assets/53ae49ba-4fc6-4342-a35a-e869d8e3332d" />

The app is launched
The map did appear.
Sabah is visible.

A meesage appears, saying "Select at least one variable."

raster_catalogue.csv exists.
variable_metadata.csv exists.
theme_variables.csv exists.
pathway_themes.csv exists.
risk_threshold.csv exists.

Config file
raster_catalogue.csv
variable_metadata.csv
theme_variables.csv
pathway_themes.csv
risk_thresholds.csv

Opens correctly
Yes
Yes
Yes
Yes
Yes

Main issue found
Only three starter records
None
Check IDs
None
None

Action needed
Add more later
None
Review
None
None

nrow(raster_catalogue)
nrow(variable_metadata)
nrow(theme_variables)
nrow(pathway_themes)
nrow(risk_thresholds)

# Week 1:

## Work complete
- Updated R and configured the project to run with R 4.6.0.
- Cloned the Climate Risk Screening App repository onto the local computer.
- Installed the packages required to run the Shiny application.
- Fixed a syntax error in R/processing/prepare_aoi.R.
- Replaced and reorganised the raster data used by the prototype.
- Added six baseline rasters and six SSP245 2041-2070 rasters.
- Updated raster_catalogue.csv from 3 records to 12 records.
- Successfully launched the Shiny application and displayed the Sabah Map.

## Main problems encountered
- Some R packages initially failed to download because of SSL connection errors.
- A formatting error in prepare_aoi.R prevented the app from starting.
- The original raster catalogue contained only three records and did not match the revised raster dataset.
- The raster catalogue had to be expanded and checked against the actual local files.
- Theme and variable configuration is still incomplete.

## Current raster variables
- Each variable has a baseline raster for 1981-2010, and an SSP245 raster for 2041-2070.

## Current status
- The app launches successfully.
- The Sabah map display correctly.
- All 12 raster files are recognised.
- The main remaining issue is linking the available variables to the correct themes and pathways.

## Remaining configuration issue
- Human Heat is listed as a theme but currently has no matching variables in theme_variables.csv.
- Coastal Exposure also no link variables.
- Because the selected theme has no linked variables, the variable, scenario, and period controls may remain empty.
