# Sabah Climate Risk Explorer

The **Sabah Climate Risk Explorer** is an R Shiny application for screening climate hazards and sector impacts for user-defined areas of interest in Sabah, Malaysia.

Users will be able to upload or draw an area, select climate variables, choose a scenario and time period, run raster-based analysis, ~~view results on an interactive map~~, and download summary outputs.

## Prototype scope

The first prototype will focus on:

- one uploaded, drawn, or buffered area of interest;
- drought and fire indicators;
- coastal inundation;
- baseline climate for 1981–2010;
- SSP245 for 2041–2070;
- interactive maps;
- summary tables;
- downloadable CSV, GeoPackage, and cropped raster outputs.

The project is structured from the beginning to support:

- SSP126, SSP245, SSP370, and SSP585;
- 2011–2040, 2041–2070, and 2071–2100;
- humid heat and human heat stress;
- livestock heat stress;
- agricultural impacts and yield decline;
- sea-level rise and storm surge;
- restoration planning;
- conservation and protected-area management;
- community-focused climate-risk discussions.

## Project structure

```text
Climate-risk-screening-app/
├── README.md
├── Marcolm_Sabah_Climate_Risk_Prototype_Brief.md
├── app.R
├── SabahClimateRiskApp.Rproj
├── install_packages.R
├── config/
│   ├── raster_catalogue.csv
│   ├── variable_metadata.csv
│   ├── theme_variables.csv
│   ├── pathway_themes.csv
│   └── risk_thresholds.csv
├── R/
│   ├── modules/
│   ├── processing/
│   ├── themes/
│   └── utils/
├── data/
│   ├── boundaries/
│   ├── examples/
│   └── reference/
├── rasters/
│   ├── baseline/
│   ├── future/
│   ├── coastal/
│   └── static/
├── outputs/
└── www/
```

## Data organisation

Each raster should be stored only once.

Climate rasters are organised by scenario and time period:

```text
rasters/baseline/1981-2010/
rasters/future/ssp126/2011-2040/
rasters/future/ssp126/2041-2070/
rasters/future/ssp126/2071-2100/
rasters/future/ssp245/2011-2040/
rasters/future/ssp245/2041-2070/
rasters/future/ssp245/2071-2100/
rasters/future/ssp370/
rasters/future/ssp585/
```

Coastal and static layers are stored separately:

```text
rasters/coastal/
rasters/static/
```

Variables used in several themes are not duplicated. Their use is defined in the configuration tables.

## Configuration files

### `raster_catalogue.csv`

Contains one row for each physical raster file, including:

- dataset ID;
- variable ID;
- scenario;
- period;
- relative file path;
- units;
- data type;
- NoData value;
- enabled status.

### `variable_metadata.csv`

Stores the definition, units, summary method, risk direction, interpretation, and limitations for each variable.

### `theme_variables.csv`

Links variables to one or more analysis themes.

For example, Consecutive Dry Days may be used in:

- drought and fire;
- restoration planning;
- agriculture;
- protected-area management;
- community climate-risk discussions.

### `pathway_themes.csv`

Links themes to intended user pathways, such as:

- restoration and conservation planning;
- protected-area management;
- community climate-risk analysis.

### `risk_thresholds.csv`

Stores agreed fixed thresholds used to classify numerical results as Low, Moderate, High, or Very High.

## Core technical approach

The application will use:

- `sf` for vector data;
- `terra` for raster cropping and masking;
- `exactextractr` for fractional-cell extraction;
- `leaflet` for interactive maps;
- `shiny` and `bslib` for the application interface.

Cropped rasters should use:

```text
-9999 = NoData
```

## Development rules

- Do not hard-code scenario or period values inside processing functions.
- Do not hard-code raster paths in `app.R`.
- Use `raster_catalogue.csv` to locate datasets.
- Keep raster-processing functions independent of Shiny.
- Store each raster only once.
- Keep scientific thresholds and metadata in configuration files.
- Do not create a combined overall-risk score unless the method has been agreed and documented.
- Do not commit large raster files to the normal GitHub repository.

## Running the prototype

1. Clone or download the repository.
2. Open `SabahClimateRiskApp.Rproj` in RStudio.
3. Run:

```r
source("install_packages.R")
```

4. Add the required local raster and example data files.
5. Check that file paths in `config/raster_catalogue.csv` match the local project structure.
6. Run:

```r
shiny::runApp()
```

## Initial development milestones

1. Launch the app and display a Sabah basemap.
2. Read and validate all configuration files.
3. Upload, draw, or buffer an area of interest.
4. Select a theme and variables.
5. Select scenario and time period.
6. Crop rasters and run exact extraction.
7. Display maps and summary tables.
8. Download CSV, GeoPackage, and cropped raster outputs.
9. Confirm results match the existing standalone R scripts.

## Large data files

Large GeoTIFF, ASC, NetCDF, and generated output files should not be committed directly to GitHub.

Use one of the following:

- local project storage;
- institutional shared storage;
- cloud storage;
- Git LFS where appropriate.

The repository should contain the application code, configuration files, documentation, and small example datasets only.

## Current status

The repository now contains:

- the starter Shiny application;
- the future-ready configuration system;
- AOI preparation functions;
- continuous-raster processing;
- inundation-raster processing;
- raster catalogue lookup functions;
- the Jambongan example AOI;
- the planned directory structure.

The next stage is to:

1. complete the raster catalogue;
2. add local test rasters;
3. run the standalone example scripts;
4. connect AOI selection and raster processing to the Shiny interface;
5. validate outputs against the existing standalone analyses.
