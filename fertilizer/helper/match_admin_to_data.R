################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to aggregate admin data to match resolution of coarser file data. ##
## Parameters:                                                                ##
## filedata: Target resolution to match admin data to                         ##
## unit_raster: Admin data to aggregate to target resolution.                 ##
## fun: Function to use for aggregation.                                      ##
## na.rm: Whether to remove missing values in aggregation.                    ##
## verbose: Whether to provide diagnostics messages.                          ##
##                                                                            ##
## Returns aggregated version of unit_raster.                                 ##
################################################################################
match_admin_to_data <- function(filedata,
                                unit_raster,
                                fun,
                                na.rm = TRUE,
                                verbose = TRUE
                               ) {
  if (!is(filedata, "SpatRaster")) {
    stop(
      "file data must be a SpatRaster.",
      "\nProvided: ", class(filedata)
    )
  }
  if (!is(unit_raster, "SpatRaster")) {
    stop(
      "unit_raster must be a SpatRaster.",
      "\nProvided: ", class(unit_raster)
    )
  }
  # Check if filedata is global
  if (exists("global_extent") && matching_extent(
    terra::ext(filedata),
    global_extent,
    terra::xres(filedata),
    terra::yres(filedata)
  )
  ) {
    terra::ext(filedata) <- global_extent
  }
  # Check if unit_raster is global
  if (exists("global_extent") && matching_extent(
    terra::ext(unit_raster),
    global_extent,
    terra::xres(unit_raster),
    terra::yres(unit_raster)
  )
  ) {
    terra::ext(unit_raster) <- global_extent
  }
  # Check if cell boundaries of filedata and unit_raster are aligned
  x <- min(terra::xres(filedata), terra::xres(unit_raster))
  y <- min(terra::yres(filedata), terra::yres(unit_raster))
  if (
    ((abs(terra::xmin(unit_raster) - terra::xmin(filedata)) / x) %% 1 > 0.01 &&
        (abs(terra::xmin(unit_raster) - terra::xmin(filedata)) / x) %% 1 < 0.99
    ) ||
    ((abs(terra::ymin(unit_raster) - terra::ymin(filedata)) / y) %% 1 > 0.01 &&
       (abs(terra::ymin(unit_raster) - terra::ymin(filedata)) / y) %% 1 < 0.99)
  ) {
    stop("Cell boundaries of filedata and unit_raster are mis-aligned")
  }
  # Check if spatial resolutions match
  file2unit <- terra::res(filedata) / terra::res(unit_raster)
  if (any(file2unit > 1.001 & file2unit %% 1 > 0.001)) {
    stop("Resolution of filedata is not an integer multiple of unit_raster")
  }
  if (any(file2unit < 0.999 & (1 / file2unit) %% 1 > 0.001)) {
    stop("Resolution of unit_raster is not an integer multiple of filedata")
  }
  if (any(file2unit > 1.001)) {
    if (verbose) {
      warning(
        "filedata has coarser resolution ",
        "than unit_raster. You should normally provide unit_raster and ",
        "filedata at the same resolution.\nTrying to aggregate unit_raster.",
        immediate. = TRUE
      )
    }
    # Load unit_raster into memory to speed up aggregation.
    if (!terra::inMemory(unit_raster)) {
      unit_raster <- terra::toMemory(unit_raster)
    }
    scale_unit <- ifelse(file2unit > 1, file2unit, 1)
    scale_unit <- round(scale_unit)
    # Aggregate using "fun" parameter. Note: Should not use "random" to solve
    # ties in mode calculation to allow for reproducibility.
    unit_raster <- terra::aggregate(
      unit_raster,
      rev(scale_unit), # res() returns lon/lat, fact is lat/lon
      fun = fun,
      na.rm = na.rm
    )
    # Update spatial scaling factor
    file2unit <- terra::res(filedata) / terra::res(unit_raster)
    if (any(file2unit > 1.001 & file2unit %% 1 > 0.001)) {
      stop("Resolution of filedata is not an integer multiple of unit_raster")
    }
    if (any(file2unit < 0.999 & (1 / file2unit) %% 1 > 0.001)) {
      stop("Resolution of unit_raster is not an integer multiple of filedata")
    }
  }
  return(unit_raster)
}
