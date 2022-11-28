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
  if (!class(filedata) %in% c("RasterLayer", "RasterBrick")) {
    stop(
      "file data must be a RasterLayer or RasterBrick.",
      "\nProvided: ", class(filedata)
    )
  }
  if (!class(unit_raster) %in% c("RasterLayer", "RasterBrick")) {
    stop(
      "unit_raster must be a RasterLayer or RasterBrick.",
      "\nProvided: ", class(unit_raster)
    )
  }
  # Check if filedata is global
  if (matching_extent(
    extent(filedata),
    global_extent,
    xres(filedata),
    yres(filedata)
  )) {
    filedata <- setExtent(filedata, global_extent)
  }
  # Check if unit_raster is global
  if (matching_extent(
    extent(unit_raster),
    global_extent,
    xres(unit_raster),
    yres(unit_raster)
  )) {
    unit_raster <- setExtent(unit_raster, global_extent)
  }
  # Check if cell boundaries of filedata and unit_raster are aligned
  x <- min(xres(filedata), xres(unit_raster))
  y <- min(yres(filedata), yres(unit_raster))
  if (((abs(xmin(unit_raster) - xmin(filedata)) / x) %% 1 > 0.01 &&
    (abs(xmin(unit_raster) - xmin(filedata)) / x) %% 1 < 0.99) ||
    ((abs(ymin(unit_raster) - ymin(filedata)) / y) %% 1 > 0.01 &&
    (abs(ymin(unit_raster) - ymin(filedata)) / y) %% 1 < 0.99)
  ) {
    stop("Cell boundaries of filedata and unit_raster are mis-aligned")
  }
  # Check if spatial resolutions match
  file2unit <- res(filedata) / res(unit_raster)
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
    if (!inMemory(unit_raster)) {
      readAll(unit_raster)
    }
    scale_unit <- ifelse(file2unit > 1, file2unit, 1)
    scale_unit <- round(scale_unit)
    # Aggregate using most frequent value. Do not use "random" to solve ties to
    # allow for reproducibility.
    unit_raster <- aggregate(
      unit_raster,
      scale_unit,
      fun = fun,
      na.rm = na.rm
    )
    # Update spatial scaling factor
    file2unit <- res(filedata) / res(unit_raster)
    if (any(file2unit > 1.001 & file2unit %% 1 > 0.001)) {
      stop("Resolution of filedata is not an integer multiple of unit_raster")
    }
    if (any(file2unit < 0.999 & (1 / file2unit) %% 1 > 0.001)) {
      stop("Resolution of unit_raster is not an integer multiple of filedata")
    }
  }
  return(unit_raster)
}
