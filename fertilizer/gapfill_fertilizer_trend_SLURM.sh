#!/bin/bash
#SBATCH --ntasks=6 # no need to set higher than number of crops + 1
#SBATCH --cpus-per-task=1
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="gapfill_fertilizer_trend"
#SBATCH --output=gapfill_fertilizer_trend_%j.out
#SBATCH --error=gapfill_fertilizer_trend_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=gapfill_fertilizer_trend

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

mpirun Rscript --vanilla gapfill_fertilizer_trend.R
