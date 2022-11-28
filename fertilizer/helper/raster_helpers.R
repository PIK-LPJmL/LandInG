################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
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
if (require(raster)) {
  # Global extent object to check datasets against
  global_extent <- extent(-180, 180, -90, 90)

  # Function to check if two extents are sufficiently similar, as they may not
  # be completely identical due to numeric inaccuracies.
  # The difference must be smaller than "tolerance" fraction of the resolution,
  # e.g. smaller than 1% of 1/12 degree
  matching_extent <- function(ext1, ext2, resx, resy = resx, tolerance = 0.01) {
    if (class(ext1) != "Extent") {
      stop(
        "ext1 must be of class Extent but has class ", class(ext1), ".\n",
        "If you want to provide a Raster*object use extent(obj)"
      )
    }
    if (class(ext2) != "Extent") {
      stop(
        "ext2 must be of class Extent but has class ", class(ext2), ".\n",
        "If you want to provide a Raster*object use extent(obj)"
      )
    }
    check <- c(
      abs(xmin(ext1) - xmin(ext2)) < abs(resx) * tolerance,
      abs(xmax(ext1) - xmax(ext2)) < abs(resx) * tolerance,
      abs(ymin(ext1) - ymin(ext2)) < abs(resy) * tolerance,
      abs(ymax(ext1) - ymax(ext2)) < abs(resy) * tolerance
    )
    all(check, na.rm = TRUE)
  }
}
