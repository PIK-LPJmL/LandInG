#!/bin/bash
#SBATCH --ntasks=8 # e.g. up to 8 for 0.5°, 64 for 5min
#SBATCH --mem-per-cpu=15G # tests failed with 10G per task
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="aggregate soils"
#SBATCH --output=aggregate_soils_%j.out
#SBATCH --error=aggregate_soils_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=aggregate_soils

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

mpirun Rscript --vanilla aggregate_soils.R
