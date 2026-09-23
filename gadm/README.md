# GADM administrative units
-----
This directory contains scripts to derive gridded land masks and 
country/region/district datasets from GADM shapes.

Unless you specify a predefined list of coordinates the scripts in this
directory will create a list of coordinates (the grid) which forms the basis
of all other input datasets.

## Input

GADM data needs to be downloaded from https://gadm.org/. LandInG includes a
`download_gadm()` function that can be used to automatically download a specific
GADM version. If LandInG is run on a system without internet access GADM data
need to be downloaded manually:

- Download the entire world as separate layers (one for each level of 
  subdivision/aggregation). Level 0-2 are required. The scripts support either
  GeoPackage format or ESRI Shapefiles. 
- Unzip in working directory.

The GADM website provides versions 2.8, 3.6, 4.0 and 4.1 at the time of this
LandInG release. However, version 2.8 uses incompatible attribute names and the
copy of version 4.0 appears to be missing the layer for GADM level 0. GADM
version 4.1 can be loaded but its attribute data contain multiple missing values
or values that do not follow GADM naming schemes, which lead to errors in
LandInG processing. These attribute values could be fixed manually by the user.
Without such fixes, only version 3.6 is currently expected to work with LandInG
unless processing of GADM 4.1 is restricted to countries without attribute
errors.

Optional: A CSV file providing the coordinates of a precribed grid.

## Software requirements
- `R`
- R packages `foreach`, `lpjmlkit`, `sf`, `stringi`, `terra`, `units`,
- To use MPI backend for parallelization: `doMPI`, `Rmpi`
- Or use `doParallel`, `parallel` to run on multiple CPUs of a local machine.
- Depending on the version of `sf` and some settings the additional `lwgeom`
  package may be required.

**Note regarding "sf" package**: Starting with version 1.0, the "sf" package by
default uses spherical geometry from the "s2" package for spatial objects in a
geographical coordinate reference system. However, testing shows that this
causes errors with some GADM versions. By default, LandInG switches off use of
"s2" functionality in "sf". It can be re-enabled by the user in `gadm_setup.R`
at their own risk. If changing the setting, any previous results including
intermediate files should be deleted and all steps should be run again.

## Files included in this directory

- 1_map_admin_to_grid_preparation.R: Script to prepare GADM (level 0-2) data
  for grid intersection.
- 1_map_admin_to_grid_preparation_SLURM.sh: Example batch script for SLURM to
  run step 1 on the PIK cluster.
- 2_map_admin_to_grid_intersection.R: Script to perform grid intersection
  between GADM data and grid.
- 2_map_admin_to_grid_intersection_SLURM.sh: Example batch script for SLURM to
  run step 2 on the PIK cluster.
- 3_map_admin_to_grid_collection.R: Script to collect data from grid
  intersection and create model input files.
- 3_map_admin_to_grid_collection_SLURM.sh: Example batch script for SLURM to run
  step 3 on the PIK cluster.
- gadm_helper.R: Script defines a number of utility functions.
- gadm_setup.R: Main setup script.
- gridlist_CRU.csv: Example list of coordinates at 0.5° spatial resolution,
  corresponds to land grid used by the Climate Research Unit's time-series
  datasets of variations in climate with variations in other phenomena.
- README.md: This file.

## How to use
This is the minimum setup to start processing. This will create a global
gridded dataset including all countries except Antarctica.

- gadm_setup.R: 
  - Set `gadm_dir`, the base directory where scripts and unzipped GADM data
    are stored.
  - Set `lpj_res`, the spatial resolution of gridded data to be produced.
  - Set `gadm_format` to either GeoPackage or ESRI Shapefile. Support may
    depend on your location installation.
  - Set `gadm_data_version` to "3.6" or "4.1".
  - Install all R packages listed under required packages.
- gadm_helper.R:
  - Check if file names used in function `load_gadm()` and `download_gadm()` 
    match your GADM files, especially if using a version other than 3.6 or 4.1
    or if applying any user-specific processing to GADM data before use in
    LandInG.

By default the following files are created by step 3:

- Grid: list of center coordinates of all cells that contain land area (in
  LPJmL input format).
- Country code: largest country/region in each cell, list of one or two columns:
  1) country index, 2) region index for some large countries defined in setting
  `include_regions` (in LPJmL input format).
- Tables with meta information mapping indices used in country code file to
  country names and ISO codes (CSV tables).
- Raster files of information contained in country code file (switch off if
  not needed)
- Land fraction: fraction of each cell covered by land polygons according to
  GADM (LPJmL input format and raster file).

The script for step 3 can also produce datasets required by land use and
fertilizer data processing scripts in LandInG. By default the following files
are created:

- GADM level 0-2 code: largest country/region/district in each cell, list of
  three columns: 1) country index, 2) region index for all countries, not just
  countries defined in setting `include_regions`, 3) district index
  (LPJmL input format and raster file).
- Table with meta information mapping indices used in GADM level 0-2 code file
  to names and ISO codes (CSV table).
- Number of countries in each cell: Information used for gap-filling in other 
  processing scripts in LandInG (LPJmL input format and raster file).

Land use data and fertilizer data processing scripts in LandInG require these
datasets in raster format at the spatial resolution used for the respective data
processing.

### Additional options/settings:

- gadm_setup.R:
  - Setting `force_grid`: This setting allows to use a predefined grid
    (list of coordinates). If FALSE, all grid cells with land according to GADM
    polygons will be included in grid. If TRUE, supply a matrix with coordinates
    as variable `griddata`. Default: FALSE.
  - Setting `threshold_grid`: If set to a non-zero value, all grid cells with
    a total land area below the threshold (in m2) will not be included in grid.
    Default: 1000.
  - Setting `include_regions`: Provides a vector of country codes for which
    regions/states (GADM level 1) will be derived in country code file.
    By default, LPJmL versions up until 5.9.3 distinguish regions in 7 large
    countries: AUS, BRA, CAN, CHN, IND, RUS, USA. This setting must be
    consistent with regions defined in the LPJmL model source code. Starting
    with version 5.9.4, regions are no longer used in LPJmL but can still be
    included in the input. Default set for version with regions.
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
  - Setting `intersect_directory`: working directory created by step 2 script.
    Normally a subdirectory of `gadm_dir`.
  - Default file names of files created by step 3: `gridname`, `cowname`,
    `cowmetaname`, `regmetaname`, `cowraster`, `regraster`, `ncountryname`,
    `ncountryraster`, `landfracname`, `landfracraster`, `gadmname`,
    `gadmmetaname`, `gadmraster`. Comment any of the file names to skip
    creation of the respective file.
  - Default formats for files created by step 3: `gridformat`, `cowformat`,
    `ncountryformat`, `landfracformat`, `gadmformat`; can be either "BIN" for
    LPJmL input format or "CSV".
  - Setting `bintype`: version of LPJmL input format used (default: 3).
  - Header names used in LPJmL input format: `grid_headername`,
    `cow_headername`, `ncountry_headername`, `landfrac_headername`,
    `gadm_headername`; these need to match header names declared in LPJmL model
    code and usually do not need to be changed.
  - Setting `gridcell_shapefile`: File name for a shapefile created by step 1
    containing polygons. File name depends on spatial resolution `lpj_res` and
    setting `force_grid`. Usually saved in `gadm_dir`.
  - Setting `cell_list_RData`: File name for RData file containing intermediate
    results created by step 3. File name depends on spatial resolution
    `lpj_res`, GADM version `gadm_data_version` and setting `force_grid`.
    Usually saved in `gadm_dir`.
