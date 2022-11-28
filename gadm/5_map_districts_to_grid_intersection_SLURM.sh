#!/bin/bash
#SBATCH --ntasks=10
#SBATCH --cpus-per-task=2 # One cpu/task on largemem should be enough at 0.5°, 2 cpus/task on largemem for 5 arcmin
#SBATCH --partition=largemem # Needs to run on largemem due to large memory requirement
#SBATCH --qos=medium
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="grid_intersection"
#SBATCH --output=5_map_districts_to_grid_intersection_%j.out
#SBATCH --error=5_map_districts_to_grid_intersection_%j.err
#SBATCH --mail-type=END
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
export I_MPI_PMI_LIBRARY=/p/system/slurm/lib/libpmi.so

module load R/3.6.2
module load intel/2018.1
module load geos/3.6.1
module load proj4/5.2.0
module load gdal/2.4.0
module load udunits/2.2.19

export R_LIBS=/p/projects/lpjml/R.3.6.2/library # Must match with R module selected above

srun --propagate R --no-save --file=5_map_districts_to_grid_intersection.R --silent --slave
