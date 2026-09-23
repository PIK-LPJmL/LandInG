# HYDE data

This sub-directory contains scripts to process HYDE data.

## Input

This README assumes using HYDE version 3.2.1, which was used in the original
sample application in [Ostberg et al. (2023)](https://doi.org/10.5194/gmd-16-3375-2023).
Newer HYDE versions may require changes to parameters `LASTYEAR` and
`SPLIT_YEAR_ANNUAL` below.

HYDE website: https://landuse.sites.uu.nl/hyde-project/

Data portal: https://landuse.sites.uu.nl/datasets/

## Software requirements

- `bash`
- The scripts `step2_hyde_convert.sh` and `step3_hyde_interpol_merge.sh`
  require Climate Data Operators (`CDO`) and NetCDF Operators (`NCO`):
  * CDO: https://code.mpimet.mpg.de/projects/cdo
  * NCO: http://nco.sourceforge.net/

## Files included in this directory

- grid.txt: Provides a grid description for a global NetCDF file at 5 arcmin
  spatial resolution, used to convert ASCII grids to NetCDF
- README.md: This file
- step1_hyde_unzip.sh: Script to unzip individual variable files from zipped
  HYDE data files per year
- step2_hyde_convert.sh: Script to convert ASCII grids into NetCDF files
- step2_hyde_convert_SLURM.sh: Example batch script for SLURM to run step 2 on
  PIK high performance cluster
- step3_hyde_interpol_merge.sh: Script to interpolate centennial/decadal HYDE
  data to annual timeseries and merge years into one file
- step3_hyde_interpol_merge_SLURM.sh: Example batch script for SLURM to run
  step 3 on PIK high performance cluster

## How to use

1. Create sub-directories `baseline` and `general_files`
2. Download `baseline` and `general_files` of the latest HYDE version from the
   HYDE data portal and put into sub-directories. For `baseline` files use `zip`
   version and put zipped individual files into `baseline/zip` sub-directory
3. Use step1_hyde_unzip.sh
4. Use step2_hyde_convert.sh
5. Use step3_hyde_interpol_merge.sh

## Settings in bash scripts:

- `TYPE`: HYDE groups variables into landuse (lu) and population (pop); select
  which group to process. Default: lu
- `STARTYEAR`: HYDE v. 3.2.1 data cover 10000 BC to 2017; select first year to
  process. This should match the `output_period` in `../landuse_setup.R`
- `LASTYEAR`: Select last year to process. This should match the `output_period`
  in `../landuse_setup.R`. Default: 2017
- `ASCIIDIR`: Directory where individual variables files are unzipped in step 1.
  Default: unzipped
- `NCDIR`: Directory where NetCDF files are created in step 2. Default: ncdf_tmp
- `VARS`: Vector of HYDE variables that should be interpolated and merged in
  step 3.
- `ODIR`: Directory where merged, interpolated timeseries is saved by step 3.
- `HYDE_VERSION_STRING`: optional string added to final NetCDF timeseries
  filenames to denote different HYDE versions (compare `hyde_version_string`
  setting in `../landuse_setup.R`)
- `SPLIT_YEAR_ANNUAL`: last year where HYDE source data are not annual (used for
  interpolation target, Default: 2000 for HYDE 3.2.1)
