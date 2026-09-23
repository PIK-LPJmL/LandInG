#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --qos=short
#SBATCH --mem=10G # larger memory requirement if existing grid cell polygon needs to be checked
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="grid_preparation"
#SBATCH --output=1_map_admin_to_grid_preparation_%j.out
#SBATCH --error=1_map_admin_to_grid_preparation_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=grid_preparation

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
fi

Rscript --vanilla 1_map_admin_to_grid_preparation.R
