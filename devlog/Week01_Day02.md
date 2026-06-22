The GeoPackage opens successfully.
The CRS is defined.
The geometry is valid.
The AOI is located in Sabah.

the output remains an sf object;
the geometry is valid;
the CRS is retained or correctly transformed;
no features disappear;
no unexpected buffer is added;
the AOI remains in the correct location.

# Week 1 - Day 2

## AOI Text Panel Added
- Added a new basic AOI Test tab to app.R.
- Added a fixed AOI name field using Jambongan.
- Added a variable selector linked to the enabled variables in the raster catalogue.
- Added scenario and period selectors that update according to the selected variables.
- Added a simple result table with the table:
  - AOI
  - Variable
  - Scenario
  - Period
  - Mean
  - Minimum
  - Maximum
  - Units

## Configuration Integration
- Connected the variable selector to raster_catalogue.
- Used variable_metadata to display readable variable names.
- Converted scenario codes such as ssp245 into readable labels such as SSP2-4.5.
- Set preferred default:
  - ssp245 for future scenarios.
  - 2041-2070 for future periods.
  - 1981-2010 for baseline.

## Current Status
- The panel successfully tests whether the selected variable, scenario and period exist in the catalogue.
- The result table structure is working.
- Mean, minimum and maximum are currently placeholder values.
- Jambongan is currently used as a test AOI rather than uploaded or selected interactively.
