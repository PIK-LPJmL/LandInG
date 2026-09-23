################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to load harvested fraction pattern for one crop. If pattern has   ##
## multiple years returns matching year, interpolating if necessary.          ##
## Used in ../harvested_area_timeseries.R                                     ##
## Parameters:                                                                ##
## nc: Object of class 'ncdf4' (opened NetCDF file)                           ##
## crop: index of crop in file                                                ##
## year: Target year for files containing multiple years                      ##
## harvested_fraction_hastime: Whether nc has time axis                       ##
## harvested_fraction_flip: Whether data need to be flipped vertically        ##
## LandInG_setup: List containing LandInG settings                            ##
##                                                                            ##
## Important note: Code to interpolate patterns between years is currently    ##
## missing. Function will fail for any year that needs interpolation.         ##
################################################################################
load_ha_fraction <- function(nc,
                             crop_index,
                             year,
                             harvested_fraction_hastime,
                             harvested_fraction_flip,
                             LandInG_setup
                            ) {
  cyear <- as.character(year)
  if (harvested_fraction_hastime) {
    harvested_fraction <- ncdf4::ncvar_get(
      nc,
      "harvested_fraction",
      start = c(1, 1, crop_index, 1),
      count = c(-1, -1, 1, -1),
      collapse_degen = FALSE
    )

    if (harvested_fraction_flip) {
      harvested_fraction <- harvested_fraction[, seq(nc$dim$lat$len, 1), , ]
    }
    harvested_fraction <- array(
      harvested_fraction,
      dim = c(
        cell = nc$dim$lat$len * nc$dim$lon$len,
        time = length(LandInG_setup$landuse$mon_refyear)
      ),
      dimnames = list(cell = NULL, time = LandInG_setup$landuse$mon_refyear)
    )
    if (year %in% LandInG_setup$landuse$mon_refyear) {
      harvested_fraction <- harvested_fraction[, cyear]
    } else if (
      year < min(LandInG_setup$landuse$mon_refyear) ||
        year > max(LandInG_setup$landuse$mon_refyear)
    ) {
      retyear <- ifelse(
        year < min(LandInG_setup$landuse$mon_refyear),
        as.character(min(LandInG_setup$landuse$mon_refyear)),
        as.character(max(LandInG_setup$landuse$mon_refyear))
      )
      harvested_fraction <- harvested_fraction[, retyear]
    } else {
      # TODO: Add code to interpolate patterns for multiple values of
      # mon_refyear.
      stop("Missing code for pattern interpolation")
    }
  } else {
    harvested_fraction <- ncdf4::ncvar_get(
      nc,
      "harvested_fraction",
      start = c(1, 1, crop_index),
      count = c(-1, -1, 1)
    )
    if (harvested_fraction_flip) {
      harvested_fraction <- harvested_fraction[, seq(nc$dim$lat$len, 1)]
    }
  }
  if (length(LandInG_setup$landuse$mon_refyear) > 1) {
  }
  c(harvested_fraction)
}
