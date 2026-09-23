#!/bin/bash
#SBATCH --ntasks=10
#SBATCH --mem=320G # ~4 GB per task at 0.5° (which is below the default assignment), 32 GB per task at 5 arcmin
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="grid_intersection"
#SBATCH --output=2_map_admin_to_grid_intersection_%j.out
#SBATCH --error=2_map_admin_to_grid_intersection_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=grid_intersection

################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
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

mpirun Rscript --vanilla 2_map_admin_to_grid_intersection.R
