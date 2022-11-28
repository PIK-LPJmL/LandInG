#!/bin/bash
#SBATCH --qos=short
#SBATCH --partition=standard
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

module load cdo/1.7.1

./step2_hyde_convert.sh
