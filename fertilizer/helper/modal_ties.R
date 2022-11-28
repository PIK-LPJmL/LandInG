################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Utility functions to use modal() within aggregate() with different "ties"  ##
## parameters. By default, aggregate() does not pass "ties" to modal().       ##
################################################################################
modal_ties_lowest <- function(x, ..., na.rm = TRUE, freq = FALSE) {
  return(raster::modal(x, ..., ties = "lowest", na.rm = na.rm, freq = freq))
}
modal_ties_highest <- function(x, ..., na.rm = TRUE, freq = FALSE) {
  return(raster::modal(x, ..., ties = "highest", na.rm = na.rm, freq = freq))
}
modal_ties_first <- function(x, ..., na.rm = TRUE, freq = FALSE) {
  return(raster::modal(x, ..., ties = "first", na.rm = na.rm, freq = freq))
}
modal_ties_random <- function(x, ..., na.rm = TRUE, freq = FALSE) {
  return(raster::modal(x, ..., ties = "random", na.rm = na.rm, freq = freq))
}
