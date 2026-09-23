#!/bin/bash

################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

## Source this script before running any R scripts included in LandInG on the
## PIK high-performance cluster "Foote". Loads required modules and sets a
## library path with pre-installed R packages. This is not expected to work
## outside the PIK system.
module purge
module load R/4.3.2
module load openmpi/5.0.8
module load udunits/2.2.28
module load proj/9.5.1
module load gdal/3.13.0
module load geos/3.12.1
module load netcdf-c/4.9.2
module load sqlite3/3.44.2

# Local library of R packages for use in LandInG
export R_LIBS=/p/projects/lpjml/scripts/LandInG/1.1.0/R.4.3.2
export LC_ALL=C.UTF-8 # Set UTF-8 character set in R
# Local installation of mdbtools
export PATH=$PATH:/p/projects/lpjml/scripts/mdbtools-1.0.1/bin
