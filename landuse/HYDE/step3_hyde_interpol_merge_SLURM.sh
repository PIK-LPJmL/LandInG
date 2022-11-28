#!/bin/bash
#SBATCH --qos=short
#SBATCH --partition=standard
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

module load cdo/1.7.1
module load nco/4.6.8 

./step3_hyde_interpol_merge.sh
