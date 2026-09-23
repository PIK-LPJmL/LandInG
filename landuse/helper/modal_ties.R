################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Utility functions to calculate mode within aggregate() with different      ##
## "ties" parameters. This allows for easier replacement of the underlying    ##
## function used to calculate the mode and removes the need to pass the "ties"##
## parameter separately.                                                      ##
################################################################################
modal_ties_lowest <- function(x, ..., na.rm = TRUE) {
  collapse::fmode(x, ..., ties = "min", na.rm = na.rm, nthreads = 1)
}
modal_ties_highest <- function(x, ..., na.rm = TRUE) {
  collapse::fmode(x, ..., ties = "max", na.rm = na.rm, nthreads = 1)
}
modal_ties_first <- function(x, ..., na.rm = TRUE) {
  collapse::fmode(x, ..., ties = "first", na.rm = na.rm, nthreads = 1)
}
modal_ties_random <- function(x, ..., na.rm = TRUE) {
  # fmode does not provide ties = "random" so shuffle x instead.
  x <- sample(rep(x, 2), size = length(x) * 2)
  collapse::fmode(x, ..., ties = "first", na.rm = na.rm, nthreads = 1)
}
