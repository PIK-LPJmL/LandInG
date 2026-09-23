#!/bin/bash
#SBATCH --ntasks=32
#SBATCH --cpus-per-task=1
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="combine_fertilizer_pattern_trend_national"
#SBATCH --output=combine_fertilizer_pattern_trend_national_%j.out
#SBATCH --error=combine_fertilizer_pattern_trend_national_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=combine_fertilizer_pattern_trend_national

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

mpirun Rscript --vanilla combine_fertilizer_pattern_trend_national.R
