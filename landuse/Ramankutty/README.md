# Ramankutty data

This file describes the "Ramankutty" data used in the landuse toolbox.

"Ramankutty" refers to:
Ramankutty, N., A.T. Evan, C. Monfreda, and J.A. Foley (2008), Farming the 
planet: 1. Geographic distribution of global agricultural lands in the 
year 2000. Global Biogeochemical Cycles 22, GB1003, 
[doi:10.1029/2007GB002952](https://doi.org/10.1029/2007GB002952)

## Input
The data can be downloaded from
http://www.earthstat.org/cropland-pasture-area-2000/

## How to use
1. Download the dataset "Cropland and Pasture Area in 2000" and decompress.
2. Set variable `ramankutty_cropland_file` in `../landuse_setup.R` to point to
   the cropland file, not the pasture file. At the time of release of the
   toolbox this file was called `Cropland2000_5m.tif`.

