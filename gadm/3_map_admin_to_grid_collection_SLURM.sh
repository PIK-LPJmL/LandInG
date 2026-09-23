#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="grid_collection"
#SBATCH --output=3_map_admin_to_grid_collection_%j.out
#SBATCH --error=3_map_admin_to_grid_collection_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=grid_collection

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

Rscript --vanilla 3_map_admin_to_grid_collection.R
