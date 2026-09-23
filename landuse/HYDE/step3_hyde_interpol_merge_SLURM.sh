#!/bin/bash
#SBATCH --qos=short
#SBATCH --account=lpjml
#SBATCH --job-name=HYDE_conversion
#SBATCH --output=inttime_merge.%j.out
#SBATCH --error=inttime_merge.%j.err

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
  module load nco/5.1.9
fi

./step3_hyde_interpol_merge.sh
