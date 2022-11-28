################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to derive file years in a NetCDF file.                            ##
## This function currently only supports yearly time axis, but works for both ##
## relative and absolute time axes.                                           ##
## Parameters:                                                                ##
## nc: NetCDF ID returned by nc_open or nc_create                             ##
## Return value:                                                              ##
## Vector of years included in file or NULL if file years cannot be           ##
## determined.                                                                ##
################################################################################
nc_file_years <- function(nc) {
  file_timeunit <- nc$dim$time$units
  file_time_relative <- grepl("since", file_timeunit)
  if (file_time_relative && !grepl("years since", file_timeunit)) {
    # Only support relative time axis with yearly increment for now.
    stop(
      "Time unit ", sQuote(file_timeunit),
      " in file ", sQuote(nc$filename), " not supported"
    )
  } else if (!grepl("year", file_timeunit)) {
    # Absolute time axis with unit other than year not supported for now.
    stop(
      "Time unit ", sQuote(file_timeunit),
      " in file ", sQuote(nc$filename), " not supported"
    )
  }
  if (file_time_relative) {
    file_refyear <- as.integer(
      format.Date(sub("^[a-z ]*", "", file_timeunit), format = "%Y")
    )
    if (!is.finite(file_refyear)) {
      stop(
        "Error determining file_refyear from time unit ",
        sQuote(file_timeunit), " in file ", sQuote(nc$filename)
      )
    }
    file_years <- nc$dim$time$vals + file_refyear
  } else {
    file_years <- nc$dim$time$vals
  }
  file_years
}
