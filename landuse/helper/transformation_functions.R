################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Logit and logistic transformation functions.                               ##
## These are slightly faster than functions logit and inv.logit included in   ##
## the "boot" package.                                                        ##
logit_trans <- function(p) {
  return(log(p / (1 - p)))
}
logistic_trans <- function(x) {
  return(1 / (1 + exp(-x)))
}
