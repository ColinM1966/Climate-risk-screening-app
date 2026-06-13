# Marcolm Development Brief  
## Sabah Climate Risk Explorer – First Prototype with Future-Ready Setup

## 1. Purpose

The first prototype will test a simple Shiny workflow for selecting an area of interest, choosing climate variables, running raster extraction and producing maps and downloadable outputs.

The prototype will initially use:

- Baseline: 1981–2010
- Future scenario: SSP245
- Future period: 2041–2070
- A limited set of drought, fire and coastal variables

However, the project structure and code must be designed from the start to support:

- SSP126, SSP245, SSP370 and SSP585
- 2011–2040, 2041–2070 and 2071–2100
- Baseline climate layers
- Drought and fire
- Human humid heat
- Livestock heat stress
- Agriculture impacts
- Coastal inundation and storm surge
- Restoration, conservation, protected-area management and community-focused analysis

The prototype must not hard-code SSP245 or 2041–2070 into the processing functions.

---

## 2. Core design rule

Each raster is stored only once.

A raster can be used in several themes. For example, Consecutive Dry Days may be used for:

- drought and fire;
- restoration planning;
- agriculture;
- protected-area management;
- community climate-risk discussions.

The raster should not be copied into several theme folders.

The physical file structure records:

- scenario;
- time period;
- raster file.

Configuration tables record:

- which themes use each variable;
- which user pathways use each theme;
- how each variable is processed;
- how results are classified and displayed.

---

## 3. Required project structure

```text
SabahClimateRiskApp/
│
├── app.R
├── SabahClimateRiskApp.Rproj
│
├── R/
│   ├── modules/
│   ├── processing/
│   ├── themes/
│   └── utils/
│
├── config/
│   ├── raster_catalogue.csv
│   ├── variable_metadata.csv
│   ├── theme_variables.csv
│   ├── pathway_themes.csv
│   └── risk_thresholds.csv
│
├── data/
│   ├── boundaries/
│   ├── examples/
│   └── reference/
│
├── rasters/
│   ├── baseline/
│   │   └── 1981-2010/
│   │
│   ├── future/
│   │   ├── ssp126/
│   │   │   ├── 2011-2040/
│   │   │   ├── 2041-2070/
│   │   │   └── 2071-2100/
│   │   ├── ssp245/
│   │   │   ├── 2011-2040/
│   │   │   ├── 2041-2070/
│   │   │   └── 2071-2100/
│   │   ├── ssp370/
│   │   │   ├── 2011-2040/
│   │   │   ├── 2041-2070/
│   │   │   └── 2071-2100/
│   │   └── ssp585/
│   │       ├── 2011-2040/
│   │       ├── 2041-2070/
│   │       └── 2071-2100/
│   │
│   ├── coastal/
│   │   ├── sea_level_rise/
│   │   └── storm_surge/
│   │
│   └── static/
│       ├── topography/
│       ├── soils/
│       ├── land_cover/
│       ├── hydrology/
│       └── protected_areas/
│
├── outputs/
│   ├── jobs/
│   ├── cache/
│   └── logs/
│
├── tests/
└── www/
```

The approximately 30 climate rasters should be placed directly inside the relevant scenario and period folder.

Example:

```text
rasters/future/ssp245/2041-2070/
├── Bio05_AllGCMs_ssp245_2041-2070.tif
├── Bio17_AllGCMs_ssp245_2041-2070.tif
├── CDD_AllGCMs_ssp245_2041-2070.tif
├── Fire_AllGCMs_ssp245_2041-2070.tif
├── PPETmin_AllGCMs_ssp245_2041-2070.tif
├── HI41days_AllGCMs_ssp245_2041-2070.tif
├── PoultryHeat_AllGCMs_ssp245_2041-2070.tif
└── OilPalmYieldChange_AllGCMs_ssp245_2041-2070.tif
```

Do not create separate copies of these files for restoration, agriculture, protected-area management or community analysis.

---

## 4. Configuration files

### 4.1 raster_catalogue.csv

This table contains one row for each physical raster file.

Required fields:

```text
dataset_id
variable_id
scenario
period
file_path
file_format
units
dataset_type
resolution
nodata_value
enabled
```

Example:

```csv
dataset_id,variable_id,scenario,period,file_path,file_format,units,dataset_type,resolution,nodata_value,enabled
CDD_BASE,CDD,baseline,1981-2010,rasters/baseline/1981-2010/CDD_AllGCMs_historical_1981-2010.tif,tif,days,continuous,1km,-9999,TRUE
CDD_245_4170,CDD,ssp245,2041-2070,rasters/future/ssp245/2041-2070/CDD_AllGCMs_ssp245_2041-2070.tif,tif,days,continuous,1km,-9999,TRUE
HI41_245_4170,HI41_DAYS,ssp245,2041-2070,rasters/future/ssp245/2041-2070/HI41days_AllGCMs_ssp245_2041-2070.tif,tif,days/year,continuous,1km,-9999,TRUE
```

### 4.2 variable_metadata.csv

This table contains one row per variable.

Required fields:

```text
variable_id
display_name
description
units
summary_method
risk_direction
baseline_variable_id
classification_method
interpretation
limitations
```

### 4.3 theme_variables.csv

This table allows one variable to appear in several themes.

Required fields:

```text
theme
variable_id
display_order
default_selected
```

Example:

```csv
theme,variable_id,display_order,default_selected
Drought and Fire,CDD,1,TRUE
Restoration Planning,CDD,1,TRUE
Protected Area Management,CDD,1,TRUE
Agriculture,CDD,3,FALSE
Community Climate Concerns,CDD,4,FALSE
```

### 4.4 pathway_themes.csv

This table controls which themes appear under different user pathways.

Example pathways:

- Restoration and conservation planning
- Protected-area management
- Community climate-risk discussion

Required fields:

```text
pathway
theme
display_order
default_enabled
```

### 4.5 risk_thresholds.csv

This table stores agreed classification thresholds.

Thresholds must not be buried inside the Shiny server code.

---

## 5. Prototype scope

The first working prototype will include:

### Area selection

- upload one polygon;
- draw one polygon;
- click one point and create a buffer.

### Themes

- drought and fire;
- coastal inundation.

### Variables

Initial drought and fire variables:

- fire probability;
- Consecutive Dry Days;
- minimum monthly P:PET.

Initial coastal variables:

- selected storm-surge or inundation return periods.

### Scenario and period

- baseline, 1981–2010;
- SSP245, 2041–2070.

### Outputs

- AOI map;
- raster result map;
- summary table;
- CSV download;
- GeoPackage download;
- cropped GeoTIFF download.

---

## 6. Future-ready coding requirements

Marcolm must follow these rules even though the prototype uses only one future scenario and period.

### Rule 1: Never hard-code the selected scenario

Do not write processing functions that assume:

```r
scenario <- "ssp245"
```

The scenario must be passed into the function:

```r
process_analysis(
  aoi_sf,
  variable_id,
  scenario,
  period
)
```

### Rule 2: Never hard-code the selected period

The period must come from the user interface or test settings.

### Rule 3: Find rasters through the catalogue

Do not construct raster paths throughout `app.R`.

Use a function such as:

```r
find_raster <- function(
    catalogue,
    variable_id,
    scenario,
    period
) {

  catalogue |>
    dplyr::filter(
      .data$variable_id == variable_id,
      .data$scenario == scenario,
      .data$period == period,
      .data$enabled
    )
}
```

### Rule 4: Keep themes separate from physical raster storage

Theme selection should use `theme_variables.csv`.

### Rule 5: Keep analysis functions independent of Shiny

Raster-processing functions must also work from a normal R script.

### Rule 6: Use standard output fields

Every analysis function should return fields such as:

```text
AOI_ID
AOI_NAME
variable_id
scenario
period
baseline_value
future_value
change
units
risk_score
risk_class
interpretation
cropped_raster
```

### Rule 7: Use -9999 for raster NoData outputs

### Rule 8: Record the analysis settings

Each run should save:

- AOI source;
- variables;
- scenario;
- period;
- processing date;
- raster files used;
- output files created.

---

## 7. Prototype development sequence

### Step 1 – Confirm project setup

- Open `SabahClimateRiskApp.Rproj`.
- Confirm the required folders exist.
- Confirm the configuration CSV files can be read.
- Confirm the app launches.

Completion test:

```text
The app opens and displays a Sabah map.
```

### Step 2 – Load the raster catalogue

Create a function to read and validate `raster_catalogue.csv`.

Completion test:

```text
The app can list available variables, scenarios and periods from the catalogue.
```

### Step 3 – Build scenario and period controls

For the prototype, only show:

- baseline / 1981–2010;
- SSP245 / 2041–2070.

The options must still be generated from the catalogue.

Completion test:

```text
No scenario or period is written directly into the processing function.
```

### Step 4 – Build theme-variable controls

Use `theme_variables.csv`.

Completion test:

```text
Selecting Drought and Fire displays Fire, CDD and P:PET.
Selecting Coastal Inundation displays only coastal variables.
```

### Step 5 – Add AOI upload, drawing and point buffering

Completion test:

```text
The AOI is displayed and its area is calculated.
```

### Step 6 – Connect standalone processing functions

Functions should:

- identify the selected raster;
- transform the AOI;
- crop and mask the raster;
- use exactextractr;
- create summary statistics;
- write the cropped raster.

Completion test:

```text
Jambongan results match the existing standalone scripts.
```

### Step 7 – Add results and downloads

Completion test:

```text
The user can download a CSV, GeoPackage and cropped raster.
```

### Step 8 – Test the future structure

Before completing the prototype, temporarily add one catalogue row for another scenario or period and confirm the app detects it without code changes.

Suggested test:

```text
SSP126, 2011–2040, CDD
```

The raster itself can be a small test raster if the full layer is not yet available.

Completion test:

```text
A new scenario-period combination becomes available by editing the catalogue only.
```

---

## 8. What Marcolm should not do

Do not:

- copy rasters into multiple thematic folders;
- hard-code the full path to each raster in `app.R`;
- create separate processing code for SSP126, SSP245, SSP370 and SSP585;
- create separate processing code for each time period;
- put risk thresholds directly into interface code;
- combine all hazards into one overall-risk score without an agreed method;
- redesign the scientific calculations without discussion;
- begin advanced styling before the processing and downloads work.

---

## 9. What should be prepared before Marcolm begins

The supervisor should provide:

- the clean RStudio project;
- the approximately 30 source rasters;
- the completed or partly completed raster catalogue;
- variable names and definitions;
- agreed units;
- agreed summary methods;
- agreed risk thresholds where available;
- Sabah boundary;
- Jambongan example AOI;
- existing drought/fire and coastal scripts;
- expected Jambongan outputs for checking;
- a list of variables intended for each theme and pathway.

---

## 10. Prototype completion criteria

The prototype is complete when:

1. the user can upload, draw or buffer one AOI;
2. the app reads available datasets from configuration files;
3. the user can select drought/fire or coastal analysis;
4. the user can run baseline or SSP245, 2041–2070;
5. the app crops and masks the selected rasters;
6. exact extraction results match the standalone scripts;
7. outputs include CSV, GeoPackage and cropped rasters;
8. no raster path, scenario or period is hard-coded in the processing functions;
9. adding another scenario or period requires catalogue changes, not rewriting the app;
10. the project is ready to expand to humid heat, livestock heat, agriculture and other user pathways.
