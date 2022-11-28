################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to load one year of gridded harvested area time series data,      ##
## aggregate to target resolution and crop to target extent.                  ##
## Used in ../cft_input_timeseries.R                                          ##
## Parameters:                                                                ##
## year: Year for which to load data                                          ##
## file_startyear: Vector giving start years of NetCDF input files (in case   ##
##                 time series is spread across several files)                ##
## file_endyear: Vector giving end years of NetCDF input files (in case time  ##
##               series is spread across several files)                       ##
## file_list: List of opened NetCDF input files                               ##
## file_raster: Raster object representing NetCDF input (same resolution and  ##
##              extent                                                        ##
## vars_req: NetCDF variable(s) to be loaded from NetCDF input file           ##
## area_raster: Raster object providing cell area corresponding to NetCDF     ##
##              input data                                                    ##
## target_unit: unit of returned HYDE data (area_raster must be in this unit) ##
## target_raster: Raster object representing requested output resolution and  ##
##                extent                                                      ##
## target_varname: Name of variable that this data is returned to. Used for   ##
##                 print messages.                                            ##
## fact: Aggregation factor to use to get from HYDE source data to target     ##
##       resolution.                                                          ##
################################################################################
load_ha_yeardata <- function(year,
                             file_startyears,
                             file_endyears,
                             file_list,
                             file_raster,
                             vars_req,
                             area_raster,
                             target_unit = fao_area_units,
                             target_raster = cft_raster,
                             target_varname,
                             fact
                            ) {
  file_index <- which(
    file_startyears <= year &
    file_endyears >= year
  )
  # Load names of crops available in NetCDF source file
  nc <- file_list[[file_index]]
  file_bandnames <- ncvar_get(nc = nc, varid = "crop")
  ## Find year in source file
  refyear <- as.integer(
    format.Date(gsub("days|years since ", "", nc$dim$time$units), "%Y")
  )
  years <- refyear + nc$dim$time$vals
  file_yearindex <- which(years == year)
  # Load data
  file_yeardata <- array(
    dim = c(
      nc$dim$lon$len,
      nc$dim$lat$len,
      length(file_bandnames),
      length(vars_req)
    ),
    dimnames = list(NULL, NULL, file_bandnames, vars_req)
  )
  for (v in vars_req) {
    file_yeardata[, , , v] <- ncvar_get(
      nc = nc,
      varid = v,
      start = c(1, 1, 1, file_yearindex),
      count = c(-1, -1, -1, 1)
    )
  }
  # Check whether latitude dimension has correct orientation
  lats <- nc$dim$lat$vals
  raster_lats <- yFromRow(file_raster)
  file_flip <- (
    (lats[1] < lats[2]) != (raster_lats[1] < raster_lats[2])
  )
  if (file_flip) {
    index <- seq.int(ncol(file_yeardata), 1)
    file_yeardata <- file_yeardata[, index, , ]
    if (length(dim(file_yeardata)) >= 2 && length(dim(file_yeardata)) < 4) {
      dim(file_yeardata) <- c(
        dim(file_yeardata)[1:2],
        length(file_bandnames),
        length(vars_req)
      )
      dimnames(file_yeardata) <- list(NULL, NULL, file_bandnames, vars_req)
    }
  }
  # Convert source unit to output unit
  for (v in vars_req) {
    unit <- nc$var[[v]]$units
    if (!ud.are.convertible(unit, "m2")) {
      # Source unit is fractional, use area raster to convert to absolute area
      cat(
        "Convert", sQuote(v), "in", sQuote(target_varname), "from",
        sQuote(unit), "to", sQuote(target_unit), "\n"
      )
      file_yeardata[, , , v] <- file_yeardata[, , , v] *
      ud.convert(1, unit, "1") * values(area_raster)
    } else {
      # Source has absolute area, convert to output unit
      if (ud.convert(1, unit, target_unit) != 1) {
        cat(
          "Convert", sQuote(v), "in", sQuote(target_varname), "from",
          sQuote(unit), "to", sQuote(target_unit), "\n"
        )
        file_yeardata[, , , v] <- file_yeardata[, , , v] *
        ud.convert(1, unit, target_unit)
      }
    }
  }
  # Crop to CFT spatial extent
  crop_x <- which(
    xFromCol(file_raster) > xmin(target_raster) &
    xFromCol(file_raster) < xmax(target_raster)
  )
  crop_y <- which(
    yFromRow(file_raster) > ymin(target_raster) &
    yFromRow(file_raster) < ymax(target_raster)
  )
  file_yeardata <- file_yeardata[crop_x, crop_y, , ]
  if (any(dim(file_yeardata)[1:2] / fact != dim(target_raster)[2:1])) {
    stop(
      "Mismatch between ", sQuote(target_varname), " from ",
      sQuote(nc$filename),
      " and target_raster"
    )
  }
  if (length(dim(file_yeardata)) >= 2 && length(dim(file_yeardata)) < 4) {
    dim(file_yeardata) <- c(
      dim(file_yeardata)[1:2],
      length(file_bandnames),
      length(vars_req)
    )
    dimnames(file_yeardata) <- list(NULL, NULL, file_bandnames, vars_req)
  }
  # Aggregate to output resolution if necessary
  if (max(fact) > 1) {
    cat(
      "Aggregating ", sQuote(target_varname), " from [",
      toString(round(res(file_raster), 8)),
      "] to output resolution [",
      toString(round(res(target_raster), 8)),
      "]\n",
      sep = ""
    )
    if (any((dim(file_yeardata)[1:2] / fact) %% 1 != 0)) {
      stop(
        "Spatial dimension of ", sQuote(target_varname), " [",
        toString(dim(file_yeardata)[1:2]),
        "] not an integer multiple of fact [",
        toString(fact),
        "]"
      )
    }
    tmpdata <- array(
      dim = c(
        dim(file_yeardata)[1:2] / fact,
        dim(file_yeardata)[-c(1:2)]
      )
    )
    if (length(dim(tmpdata)) < 4) {
      dim(tmpdata) <- c(dim(tmpdata), length(vars_req))
      dim(file_yeardata) <- c(dim(file_yeardata), length(vars_req))
    }
    for (v in seq_along(vars_req)) {
      tmpdata[, , , v] <- aggregate_array(
        file_yeardata[, , , v],
        fact = fact,
        fun = "sum",
        FALSE
      )
    }
    tmpsum <- apply(tmpdata, c(3, 4), sum, na.rm = TRUE)
    tmpsum2 <- apply(
      file_yeardata,
      c(3, 4),
      sum,
      na.rm = TRUE
    )
    if (any(abs(tmpsum - tmpsum2) > 1e-6)) {
      stop(
        "Error aggregating ", sQuote(target_varname), ". ",
        "Sum of aggregated data different from sum of original data."
      )
    }
    file_yeardata <- tmpdata
    dimnames(file_yeardata) <- list(NULL, NULL, file_bandnames, vars_req)
    rm(tmpdata)
  }
  # Dissolve longitude and latitude dimension to use cft_raster_gridindex
  indexed_dim <- c(
    prod(dim(file_yeardata)[1:2]),
    dim(file_yeardata)[-c(1:2)]
  )
  indexed_dimnames <- dimnames(file_yeardata)[-1]
  dim(file_yeardata) <- indexed_dim
  dimnames(file_yeardata) <- indexed_dimnames
  file_yeardata
}
