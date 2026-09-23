#!/bin/bash
#SBATCH --qos=short
#SBATCH --account=lpjml
#SBATCH --job-name=HYDE_conversion
#SBATCH --output=ascii2ncdf.%j.out
#SBATCH --error=ascii2ncdf.%j.err

################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

if [ -d /p/system/lenovo/ctt ]; then
  # Load modules for PIK 2024 high-performance computer
  module load cdo/2.4.4
fi

./step2_hyde_convert.sh
