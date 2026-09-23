#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="aggregate harvested area time series"
#SBATCH --output=aggregate_cft_timeseries_%j.out
#SBATCH --error=aggregate_cft_timeseries_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name="aggregate harvested area time series"

################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

ulimit -c unlimited

start_year=1900
end_year=2017

if [ -d /p/system/lenovo/ctt ]; then
  # Load modules for PIK 2024 high-performance computer
  source ../R_env_PIK.sh
fi

Rscript --vanilla aggregate_cft_timeseries.R --args start_year=$start_year end_year=$end_year
