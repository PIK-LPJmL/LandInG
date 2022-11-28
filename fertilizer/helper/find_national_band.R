################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This utility function can be used on multi-band raster objects to return   ##
## the band with the smallest number of unique values. For admin unit datasets##
## it is assumed that the band with the smallest number of unique values      ##
## represents national admin units.                                           ##
## Parameters:                                                                ##
## unit_raster: RasterBrick or RasterLayer object                             ##
##                                                                            ##
## Returns band index.                                                        ##
################################################################################
find_national_band <- function(unit_raster) {
  if (nlayers(unit_raster) == 1)
    return(1)
  # Find number of unique values in each band
  band_nunits <- sapply(
    cellStats(unit_raster, unique),
    length
  )
  # Check if bands have same number of unique values.
  if (any(table(band_nunits) > 1) &&
    band_nunits[which.min(band_nunits)] %in%
    band_nunits[which(table(band_nunits) > 1)]
  ) {
    # Several bands have same number of admin units. Cannot detect country layer
    stop("Cannot reliably detect country layer in unit_raster")
  }
  return(which.min(band_nunits))
}
