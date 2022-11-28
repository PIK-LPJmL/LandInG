#!/bin/bash
#SBATCH --ntasks=16
#SBATCH --cpus-per-task=1
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="lakes and rivers"
#SBATCH --output=lakes_rivers_polygonbased%j.out
#SBATCH --error=lakes_rivers_polygonbased%j.err
#SBATCH --mail-type=END
#SBATCH --job-name=lakes_rivers_fraction

################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

ulimit -c unlimited
export I_MPI_PMI_LIBRARY=/p/system/slurm/lib/libpmi.so

module load R/3.6.2
module load intel/2018.1
module load geos/3.6.1
module load udunits/2.2.28
module load gdal/2.4.0
module load proj4/5.2.0

export R_LIBS=/p/projects/lpjml/R.3.6.2/library # Must match with R module selected above

srun --propagate R --no-save --file=lakes_rivers_polygonbased.R --silent --slave
