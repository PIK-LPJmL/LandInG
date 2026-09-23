################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################


################################################################################
## This file sets up an environment for the whole LandInG package and some    ##
## basic settings valid across the sub-projects found in sub-directories.     ##
LandInG_setup <- new.env()
LandInG_setup$LandInG_version <- readLines("VERSION", n = 1)
# Earth radius used across multiple scripts
LandInG_setup$earthradius <- 6371000.785
# Single precision floating point epsilon
LandInG_setup$single.eps <- 2^(-23)
################################################################################


################################################################################
## The lpjmlkit package provides a number of utility functions to work with   ##
## the LPJmL input format.                                                    ##
if (!"lpjmlkit" %in% .packages(all.available = TRUE)) {
  stop(
    "LandInG requires the lpjmlkit package. Information how to install it ",
    "can be found here: https://github.com/PIK-LPJmL/lpjmlkit"
  )
}
################################################################################
