#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="split global harvested areas"
#SBATCH --output=split_global_harvested_areas_into_rainfed_irrigated_%j.out
#SBATCH --error=split_global_harvested_areas_into_rainfed_irrigated_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name="split global harvested areas"

################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

ulimit -c unlimited

if [ -d /p/system/lenovo/ctt ]; then
  # Load modules for PIK 2024 high-performance computer
  source ../R_env_PIK.sh
  module load cdo/2.4.4
  # cdo module changes the netcdf-c module so reload module set in
  # ../R_env_PIK.sh.
  module load netcdf-c/4.9.2
fi

Rscript --vanilla split_global_harvested_areas_into_rainfed_irrigated.R
