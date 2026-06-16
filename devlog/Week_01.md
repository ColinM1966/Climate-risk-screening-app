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

Work complete
- Updated R and configured the project to run with R 4.6.0.
- Cloned the Climate Risk Screening App repository onto the local computer.
- Installed the packages required to run the Shiny application.
- Fixed a syntax error in R/processing/prepare_aoi.R.
- Replaced and reorganised the raster data used by the prototype.
- Added six baseline rasters and six SSP245 2041-2070 rasters.
- Updated raster_catalogue.csv from 3 records to 12 records.
- Successfully launched the Shiny application and displayed the Sabah Map.

Main problems encountered
- Some R packages initially failed to download because of SSL connection errors.
- A formatting error in prepare_aoi.R prevented the app from starting.
- The original raster catalogue contained only three records and did not match the revised raster dataset.
- The raster catalogue had to be expanded and checked against the actual local files.
- Theme and variable configuration is still incomplete.

Current raster variables
- Each variable has a baseline raster for 1981-2010, and an SSP245 raster for 2041-2070.

Current status
- The app launches successfully.
- The Sabah map display correctly.
- All 12 raster files are recognised.
- The main remaining issue is linking the available variables to the correct themes and pathways.

Remaining configuration issue
- Human Heat is listed as a theme but currently has no matching variables in theme_variables.csv.
- Coastal Exposure also no link variables.
- Because the selected theme has no linked variables, the variable, scenario, and period controls may remain empty.

Simple feature collections with 56 features and 0 field (with 51 geometries empty).
Geometry type: POLYGON.
Coordinate Reference System:
  User input: WGS 84 
  wkt:
GEOGCRS["WGS 84",
    ENSEMBLE["World Geodetic System 1984 ensemble",
        MEMBER["World Geodetic System 1984 (Transit)"],
        MEMBER["World Geodetic System 1984 (G730)"],
        MEMBER["World Geodetic System 1984 (G873)"],
        MEMBER["World Geodetic System 1984 (G1150)"],
        MEMBER["World Geodetic System 1984 (G1674)"],
        MEMBER["World Geodetic System 1984 (G1762)"],
        MEMBER["World Geodetic System 1984 (G2139)"],
        MEMBER["World Geodetic System 1984 (G2296)"],
        ELLIPSOID["WGS 84",6378137,298.257223563,
            LENGTHUNIT["metre",1]],
        ENSEMBLEACCURACY[2.0]],
    PRIMEM["Greenwich",0,
        ANGLEUNIT["degree",0.0174532925199433]],
    CS[ellipsoidal,2],
        AXIS["geodetic latitude (Lat)",north,
            ORDER[1],
            ANGLEUNIT["degree",0.0174532925199433]],
        AXIS["geodetic longitude (Lon)",east,
            ORDER[2],
            ANGLEUNIT["degree",0.0174532925199433]],
    USAGE[
        SCOPE["Horizontal component of 3D system."],
        AREA["World."],
        BBOX[-90,-180,90,180]],
    ID["EPSG",4326]]

First 10 features:
        geometry
1  POLYGON EMPTY
2  POLYGON EMPTY
3  POLYGON EMPTY
4  POLYGON EMPTY
5  POLYGON EMPTY
6  POLYGON EMPTY
7  POLYGON EMPTY
8  POLYGON EMPTY
9  POLYGON EMPTY
10 POLYGON EMPTY

Bounding box: xmin: 117.3253 ymin: 6.618561 xmax: 117.5114 ymax: 6.758163
