# Soil input
-----
Scripts in this directory create soil input files.

## Input
1) a grid file
2) HWSD source data

## Files included in this directory
  - aggregate_soils.R: script which aggregates HWSD source data to target
    resolution and creates soil input files
  - aggregate_soils_SLURM.sh: example batch script to run aggregate_soils.R as a
    parallel job on PIK high-performance cluster
  - extract_attribute_table.sh: bash shell script to extract attribute tables
    from HWSD MS Access database on Linux
  - README.md: this file
  - r_modules.sh: shell script to load necessary modules to run R script on PIK
    high-performance cluster

## Software requirements
- `R`
- R packages `foreach`, `geosphere`, `raster`, to use MPI backend for
  parallelization: `doMPI`, `Rmpi`; or use `doParallel`, `parallel` to run on
  multiple CPUs of a local machine.
- optional: `bash` and `mdb-export` to extract data tables from HWSD.mdb on
  Linux systems

## How to use
- Download HWSD data:
  - Source: https://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/HWSD_Data.html?sb=4
  - Download HWSD_RASTER.zip and unzip in working directory
  - Download HWSD.mdb
- Extract data tables from HWSD.mdb
  - You may use `extract_attribute_table.sh` on Linux systems, which requires
    the software `mdb-export`
  or:
  - Use other tools to extract table "HWSD_DATA" and save as "HWSD_DATA.csv" in
    working directory
  - Use other tools to extract table "D_USDA_TEX_CLASS" and save as
    "D_USDA_TEX_CLASS.csv" in working directory
 - Run aggregate_soils.R
  
## Minimum setup for aggregate_soils.R
  - `cluster`: whether the script runs in parallel mode on several CPUs
  - `soil_dir`: working directory where outputs of the script will be saved
    (also default location of HWSD source data)
  - `gridname`: set path to LPJmL grid input file

### Additional settings in aggregate_soils.R
  - `hwsd_raster`: file name of HWSD raster map (unzipped from HWSD_RASTER.zip)
  - `hwsd_attribute_file`: CSV file containing table "HWSD_DATA"
  - `hwsd_usda_class_file`: CSV file containing table "D_USDA_TEX_CLASS"
  - `version_string`: optional version string to use in names of files created
    by this script (resolution string is added automatically)
  - `rock_ice_shared`: whether rock and ice will be treated like any other soil
    (TRUE) or assigned only if they have the largest area share in a cell
    (FALSE) (Default: TRUE)
  - `allow_skip`: whether to allow missing values in soil data (will be encoded
    as soil code `0` to be skipped in LPJmL) (Default: FALSE; try to gap-fill
    all missing values)
  - `max_search`: maximum search radius for gap-filling missing values
    (Default: 100 degree)
  - `idw_power_par`: power parameter used for inverse distance weighting in
    gap-filling (Default: 1; set to 0 for no inverse distance weighting)
  - `quicksearch`: whether to use faster search method in gap-filling
    (Default: TRUE)
  - `lpjml_soil_format`: Format for soil texture file created by this script
    (Default: LPJmL input format "BIN")
  - `lpjml_soil_version`: Header version for LPJmL input format (Default: 3)
  - `lpjml_soil_headername`: Header name of soil code input files (must match
    value defined in LPJmL source code)
  - `lpjml_soilph_format`: Format for soil pH file created by this script;
    comment this variable to skip generating soil pH (Default: LPJmL input
    format "BIN")
  - `lpjml_soilph_version`: Header version for LPJmL input format (Default: 3)
  - `lpjml_soilph_headername`: Header name of soil pH input files (must match
    value defined in LPJmL source code)
  - `lpjml_soiltypes`: soil types used in LPJmL (must match order defined in
    LPJmL code)
  - `lpjml_soilcodes`: corresponds to the "ID" column in the LPJmL soil
    parameter file
  - `usda_to_lpjml`: mapping of USDA texture names used in HWSD data to LPJmL
    soil types


  
