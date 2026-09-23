################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Defines global extent object and function to compare the extent of         ##
## different raster objects.                                                  ##
################################################################################
if ("terra" %in% .packages(all.available = TRUE)) {
  # Global extent object to check datasets against
  global_extent <- terra::ext(-180, 180, -90, 90)

  # Function to check if two extents are sufficiently similar, as they may not
  # be completely identical due to numeric inaccuracies.
  # The difference must be smaller than "tolerance" fraction of the resolution,
  # e.g. smaller than 1% of 1/12 degree
  matching_extent <- function(ext1, ext2, resx, resy = resx, tolerance = 0.01) {
    if (!is(ext1, "SpatExtent")) {
      stop(
        "ext1 must be of class SpatExtent but has class ", class(ext1), ".\n",
        "If you want to provide a Spat*object use ext(obj)"
      )
    }
    if (!is(ext2, "SpatExtent")) {
      stop(
        "ext2 must be of class SpatExtent but has class ", class(ext2), ".\n",
        "If you want to provide a Spat*object use ext(obj)"
      )
    }
    check <- c(
      abs(terra::xmin(ext1) - terra::xmin(ext2)) < abs(resx) * tolerance,
      abs(terra::xmax(ext1) - terra::xmax(ext2)) < abs(resx) * tolerance,
      abs(terra::ymin(ext1) - terra::ymin(ext2)) < abs(resy) * tolerance,
      abs(terra::ymax(ext1) - terra::ymax(ext2)) < abs(resy) * tolerance
    )
    all(check, na.rm = TRUE)
  }
}

################################################################################
## Function to unlist data.frame of returned values of a SpatRaster           ##
################################################################################
ul <- function(df) {
  if (ncol(df) > 1) {
    as.matrix(df)
  } else {
    c(unlist(df, recursive = FALSE, use.names = FALSE))
  }
}
