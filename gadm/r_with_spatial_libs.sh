#!/bin/bash
################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

## This script can be used to load R and all required modules to run the R
## scripts in this directory
## Note: This bash script has been put together specifically for the PIK
## high-performance cluster and will probably not work in any other software
## environment.
module load R/3.6.2
module load intel/2018.1
module load netcdf-c/4.2.1.1/serial
module load udunits/2.2.19
module load geos/3.6.1
module load proj4/5.2.0
module load gdal/2.4.0

export R_LIBS=/p/projects/lpjml/R.3.6.2/library 
## This must match with R module selected above

R --interactive
