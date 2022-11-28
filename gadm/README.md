# GADM administrative units
-----
This directory contains scripts to derive gridded land masks and 
country/region/district datasets from GADM shapes.

Unless you specify a predefined list of coordinates the scripts in this
directory will create a list of coordinates (the grid) which forms the basis
of all other input datasets.

## Input
GADM data needs to be downloaded from https://gadm.org/. 
  - Download the entire world as six separate layers (one for each level of 
    subdivision/aggregation). The scripts support either GeoPackage format or
    ESRI Shapefiles. 
  - File names: `gadm36_levels_gpkg.zip` or `gadm36_levels_shp.zip`.
  - Unzip in place.

If you use a different GADM version check file names in `gadm_load()` in
gadm_helper.R.

Optional: A CSV file providing the coordinates of a precribed grid.

## Software requirements
- `R`
- R packages `foreach`, `lwgeom`, `raster`, `rgdal`, `stringi`, `sf`, `units`,
  to use MPI backend for parallelization: `doMPI`, `Rmpi`, 
  or use `doParallel`, `parallel` to run on multiple CPUs of a local machine.


## Files included in this directory
  - 1_map_countries_to_grid_preparation.R: Script to prepare GADM (level 0
    and 1) data for grid intersection.
  - 2_map_countries_to_grid_intersection.R: Script to perform grid intersection
    between GADM data and grid.
  - 2_map_countries_to_grid_intersection_SLURM.sh: Example batch script for
    SLURM to run step 2 on the PIK cluster.
  - 3_map_countries_to_grid_collection.R: Script to collect data from grid
    intersection and create model input files.
  - 4_map_districts_to_grid_preparation.R: Script to prepare GADM (level 0-2)
    data for grid intersection.
  - 5_map_districts_to_grid_intersection.R: Script to perform grid intersection
    between GADM level 0-2 data and grid.
  - 5_map_districts_to_grid_intersection_SLURM.sh: Example batch script for
    SLURM to run step 5 on the PIK cluster.
  - 6_map_districts_to_grid_collection.R: Script to collect data from grid
    intersection and create files for further processing.
  - gadm_helper.R: Script defines a number of utility functions.
  - gadm_setup.R: Main setup script.
  - gridlist_CRU.csv: Example list of coordinates at 0.5° spatial resolution,
    corresponds to land grid used by the Climate Research Unit's time-series
    datasets of variations in climate with variations in other phenomena.
  - README.md: This file.
  - r_with_spatial_libs.sh: Bash script that loads all required modules to run
    R scripts on the PIK cluster.

## How to use
This is the minimum setup to start processing. This will create a global
gridded dataset including all countries except Antarctica.
  - gadm_setup.R: 
    - Set `gadm_dir`, the base directory where scripts and unzipped GADM data
      are stored.
    - Set `lpj_res`, the spatial resolution of gridded data to be produced.
    - Set `gadm_format` to either GeoPackage or ESRI Shapefile. Support may
      depend on your location installation.
    - Install all R packages listed under required packages.
  - gadm_helper.R:
    - Check if file names used in function `gadm_load()` match your downloaded
      files.

By default the following files are created by step 3:
  - Grid: list of center coordinates of all cells that contain land area (in
    LPJmL input format).
  - Country code: largest country/region in each cell, list of two columns: 1)
    country index, 2) region index for some large countries defined in setting
    `include_regions` (in LPJmL input format).
  - Tables with meta information mapping indices used in country code file to
    country names and ISO codes (CSV tables).
  - Raster files of information contained in country code file.
  - Number of countries in each cell: Information used by land use data 
    processing scripts in this toolbox (LPJmL input format and raster file).
  - Land fraction: fraction of each cell covered by land polygons according to
    GADM (LPJmL input format and raster file).

Results of scripts for step 4-6 are required by land use data processing
scripts in this toolbox. By default the following files are created by step 6:
  - GADM level 0-2 code: largest country/region/district in each cell, list
    of three columns: 1) country index, 2) region index for all countries,
    not just countries defined in setting `include_regions`, 3) district index
    (LPJmL input format and raster file).
  - Table with meta information mapping indices used in GADM level 0-2 code
    file to names and ISO codes (CSV table).

### Additional options/settings:
  - gadm_setup.R:
    - Setting `force_grid`: This setting allows to use a predefined grid
      (list of coordinates). If FALSE, all grid cells with land according to
      GADM polygons will be included in grid. If TRUE, supply a matrix with
      coordinates as variable `griddata`. Default: FALSE.
    - Setting `threshold_grid`: If set to a non-zero value, all grid cells with
      a total land area below the threshold (in m2) will not be included in
      grid. Default: 1000.
    - Setting `include_regions`: Provides a vector of country codes for which
      regions/states (GADM level 1) will be derived in country code file.
      By default, LPJmL distinguishes regions in 6 large countries: AUS, BRA,
      CAN, CHN, IND, RUS, USA. This setting must be consistent with regions
      defined in the LPJmL model source code.
    - Setting `skip_countries`: Provides a vector of countries that are not
      processed. Default: ATA (Antarctica).
    - Setting `water_bodies`: GADM data defines the Caspian Sea as a separate
      level-0 administrative area (i.e. country). All country codes included in
      variable `water_bodies` will only be assigned to grid cells if no other
      countries are present in the cell. Default: XCA.
    - Setting `gadm_no_land`: If setting `force_grid` to TRUE, any cells in the
      predefined coordinates list that don't have any land according to GADM
      will be assigned this country code.
    - Setting `split_directory`: working directory created by step 1 script.
      Normally a subdirectory of `gadm_dir`.
    - Setting `intersect_directory`: working directory created by step 2 
      script. Normally a subdirectory of `gadm_dir`.
    - Setting `split_directory_districts`: working directory created by step 4
      script. Normally a subdirectory of `gadm_dir`.
    - Setting `intersect_directory_districts`: working directory created by
      step 5 script. Normally a subdirectory of `gadm_dir`.
    - Default file names of files created by step 3 and step 6: `gridname`,
      `cowname`, `cowmetaname`, `regmetaname`, `cowraster`, `regraster`,
      `ncountryname`, `ncountryraster`, `landfracname`, `landfracraster`,
      `gadmname`, `gadmmetaname`, `gadmraster`. Comment any of the file names
      to skip creation of the respective file.
    - Default formats for files created by step 3 and step 6: `gridformat`,
      `cowformat`, `ncountryformat`, `landfracformat`, `gadmformat`; can be
      either `BIN` for LPJmL input format or `CSV`.
    - Setting `bintype`: version of LPJmL input format used (default: 3).
    - Header names used in LPJmL input format: `grid_headername`,
      `cow_headername`, `ncountry_headername`, `landfrac_headername`,
      `gadm_headername`; these need to match header names declared in LPJmL
      model code and usually don't need to be changed.
    - Setting `gridcell_shapefile`: File name for a shapefile created by
      step 1 or step 4 containing polygons. File name depends on spatial
      resolution `lpj_res` and setting `force_grid`. Usually saved in
      `gadm_dir`.
    - Setting `cell_list_RData`, `cell_list_districts_RData`: File names for
      RData files containing intermediate results created by step 3 and step 6,
      respectively. File names depend on spatial resolution `lpj_res` and
      setting `force_grid`. Usually saved in `gadm_dir`.
