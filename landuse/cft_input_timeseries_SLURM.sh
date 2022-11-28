#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --partition=standard
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="write CFT input files"
#SBATCH --output=cft_input_timeseries_%j.out
#SBATCH --error=cft_input_timeseries_%j.err
#SBATCH --mail-type=END
#SBATCH --job-name="write CFT input files"

################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

ulimit -c unlimited

module load R/3.6.2
module load intel/2018.1
module load netcdf-c/4.2.1.1/serial
module load udunits/2.2.19
module load geos/3.6.1
module load proj4/5.2.0
module load gdal/2.4.0

export R_LIBS=/p/projects/lpjml/R.3.6.2/library # Must match with R module selected above

R --no-save --file=cft_input_timeseries.R --silent --slave
