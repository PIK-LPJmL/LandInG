#!/bin/bash
#SBATCH --ntasks=8 # scales well with 64 CPUs at 5 arcmin, less for 0.5°
#SBATCH --cpus-per-task=1
###SBATCH --mem-per-cpu=10G # 5arcmin requires up to 10GB RAM per CPU, less for 0.5°
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="neighbour irrigation"
#SBATCH --output=neighbour_irrigation_%j.out
#SBATCH --error=neighbour_irrigation_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --job-name=neighbour_irrigation

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

mpirun Rscript --vanilla neighbour_irrigation.R
