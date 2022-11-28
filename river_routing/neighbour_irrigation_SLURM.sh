#!/bin/bash
#SBATCH --ntasks=48
#SBATCH --cpus-per-task=2 # 5arcmin requires up to 7GB RAM per CPU
#SBATCH --qos=short
#SBATCH --export=ALL
#SBATCH --account=lpjml
#SBATCH --comment="neighbour irrigation"
#SBATCH --output=neighbour_irrigation_%j.out
#SBATCH --error=neighbour_irrigation_%j.err
#SBATCH --mail-type=END
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
export I_MPI_PMI_LIBRARY=/p/system/slurm/lib/libpmi.so

module load R/3.6.2
module load intel/2018.1

export R_LIBS=/p/projects/lpjml/R.3.6.2/library # must match with R module selected above

srun --propagate R --no-save --file=neighbour_irrigation.R --silent --slave
