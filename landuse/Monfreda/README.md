# Monfreda et al. data

This file describes the "Monfreda" data used in the landuse toolbox.

"Monfreda" refers to:
Monfreda, C., N. Ramankutty, and J. A. Foley (2008), Farming the planet: 2. 
Geographic distribution of crop areas, yields, physiological types, and net
primary production in the year 2000, Global Biogeochem. Cycles, 22, GB1022, 
[doi: 10.1029/2007GB002947](https://www.doi.org/10.1029/2007GB002947)

## Input
The data can be downloaded from
http://www.earthstat.org/harvested-area-yield-175-crops/

At the time of release of the toolbox Monfreda data are offered as a zipped
collection of Geotiffs. However, the data have previously also been available as
a zipped collection of NetCDF files.

## Files included in this directory
- monfreda_names.txt: This file should contain the names of all crops included
  in the Monfreda dataset. Confirm with your data download.
- README.md: This file

## How to use
1. Download the full dataset and decompress.
2. Set `monfreda_format` in `../landuse_setup.R` to the file format of your
   download (either "Geotiff" or "NetCDF").
3. Check that `monfreda_datadir` in `../landuse_setup.R` points to the correct
   subdirectory.

The gridded Monfreda data are used by the following R scripts:
- `../country_level_data.R`
- `../harvested_fraction.R`
