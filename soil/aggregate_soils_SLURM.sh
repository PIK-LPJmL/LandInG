#!/bin/bash
#SBATCH --ntasks=16 # e.g. up to 8 for 0.5°, 64 for 5min
#SBATCH --cpus-per-task=4 # requires close to 14 GB of RAM
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="aggregate soils"
#SBATCH --output=aggregate_soils_%j.out
#SBATCH --error=aggregate_soils_%j.err
#SBATCH --mail-type=END
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
export I_MPI_PMI_LIBRARY=/p/system/slurm/lib/libpmi.so

module load R/3.6.2
module load intel/2018.1

export R_LIBS=/p/projects/lpjml/R.3.6.2/library # Must match with R module selected above

srun --propagate R --no-save --file=aggregate_soils.R --silent --slave
