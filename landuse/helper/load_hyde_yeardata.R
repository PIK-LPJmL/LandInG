################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to load one year of HYDE data (rainfed or irrigated cropland,     ##
## grazing land), aggregate to target resolution and crop to target extent.   ##
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
## enforce_grid_max: Whether to enforce that HYDE area cannot exceed grid area##
################################################################################
load_hyde_yeardata <- function(year,
                               file_startyears,
                               file_endyears,
                               file_list,
                               file_raster,
                               vars_req,
                               area_raster,
                               target_unit = fao_area_units,
                               target_raster = cft_raster,
                               target_varname,
                               fact,
                               enforce_grid_max = FALSE
                               ) {
  # Find file to use (if input timeseries is spread across multiple files)
  file_index <-  which(
    file_startyears <= year & file_endyears >= year
  )
  # Find year in source file
  unit <- file_list[[file_index]]$dim$time$units
  refyear <- as.integer(
    format.Date(gsub("days|years since ", "", unit), "%Y")
  )
  years <- refyear + file_list[[file_index]]$dim$time$vals
  file_yearindex <- which(years == year)
  # Check whether latitude dimension has correct orientation
  lats <- file_list[[file_index]]$dim$lat$vals
  raster_lats <- yFromRow(file_raster)
  file_flip <- (lats[1] < lats[2]) != (raster_lats[1] < raster_lats[2])
  # Load data
  file_yeardata <- array(
    dim = c(
      file_list[[file_index]]$dim$lon$len,
      file_list[[file_index]]$dim$lat$len,
      length(vars_req)
    ),
    dimnames = list(NULL, NULL, vars_req)
  )
  for (v in vars_req) {
    file_yeardata[, , v] <- ncvar_get(
      nc = file_list[[file_index]],
      varid = v,
      start = c(1, 1, file_yearindex),
      count = c(-1, -1, 1)
    )
  }
  # Check latitudinal direction and correct if necessary
  if (file_flip) {
    index <- seq.int(ncol(file_yeardata), 1)
    file_yeardata <- file_yeardata[, index, ]
    if (length(dim(file_yeardata)) == 2) {
      dim(file_yeardata) <- c(dim(file_yeardata), length(vars_req))
      dimnames(file_yeardata) <- list(NULL, NULL, vars_req)
    }
  }
  # Convert source data to target_unit
  for (v in vars_req) {
    unit <- file_list[[file_index]]$var[[v]]$units
    if (!ud.are.convertible(unit, "m2")) {
      # Source data has fractional unit, convert to absolute area using
      # area_raster
      cat(
        "Convert", sQuote(v), "in", sQuote(target_varname), "from",
        sQuote(unit),
        "to", sQuote(target_unit), "\n"
      )
      file_yeardata[, , v] <- file_yeardata[, , v] *
        ud.convert(1, unit, "1") * values(area_raster)
    } else {
      # Source data has absolute unit, convert to target_unit
      if (ud.convert(1,  unit, target_unit) != 1) {
        cat(
          "Convert", sQuote(v), "in", sQuote(target_varname), "from",
          sQuote(unit),
          "to", sQuote(target_unit), "\n"
        )
        file_yeardata[, , v] <- file_yeardata[, , v] *
          ud.convert(1, unit, target_unit)
      }
    }
    # Check for any values exceeding values in area_raster (HYDE area > grid
    # cell area)
    if (enforce_grid_max) {
      if (
        any(c(file_yeardata[, , v]) > (values(area_raster) * 1.0001),
            na.rm = TRUE)
      ) {
        warning(
          "Values in ",
          length(which(c(file_yeardata[, , v]) > (values(area_raster) * 1.0001))),
          " cell(s) of ", sQuote(v),
          " exceed grid cell area by up to ",
          max(c(file_yeardata[, , v]) - values(area_raster), na.rm = TRUE),
          " ", target_unit, " during year ", year,
          ". Reducing to grid cell area.",
          immediate. = TRUE
        )
      }
      file_yeardata[, , v] <- pmin(
        file_yeardata[, , v],
        matrix(values(area_raster), nrow = ncol(area_raster))
      )
    }
  }
  # Crop to CFT spatial extent
  crop_x <- which(xFromCol(file_raster) > xmin(target_raster) &
    xFromCol(file_raster) < xmax(target_raster)
  )
  crop_y <- which(yFromRow(file_raster) > ymin(target_raster) &
    yFromRow(file_raster) < ymax(target_raster)
  )
  if (length(dim(file_yeardata)) == 2) {
    file_yeardata <- file_yeardata[crop_x, crop_y]
  } else if (length(dim(file_yeardata)) == 3) {
    file_yeardata <- file_yeardata[crop_x, crop_y, ]
    if (length(vars_req) == 1) {
      dim(file_yeardata) <- c(dim(file_yeardata), length(vars_req))
      dimnames(file_yeardata) <- list(NULL, NULL, vars_req)
    }
  } else {
    stop(
      length(dim(file_yeardata)), "-dimensional array not supported for ",
      target_varname
    )
  }
  if (any(dim(file_yeardata)[1:2] / fact != dim(target_raster)[2:1])) {
    stop(
      "Mismatch between ", sQuote(varname), " from ",
      sQuote(file_list[[file_index]]$filename),
      " and target_raster."
    )
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
    if (length(dim(file_yeardata)) == 2) {
      dim(file_yeardata) <- c(dim(file_yeardata), length(vars_req))
      dimnames(file_yeardata) <- list(NULL, NULL, vars_req)
    }
    tmpdata <- aggregate_array(file_yeardata, fact = fact, fun = "sum", FALSE)
    if (length(dim(tmpdata)) == 2) {
      dim(tmpdata) <- c(dim(tmpdata), length(vars_req))
    }
    tmpsum <- apply(tmpdata, c(3), sum, na.rm = TRUE)
    tmpsum2 <- apply(file_yeardata, c(3), sum, na.rm = TRUE)
    if (any(abs(tmpsum - tmpsum2) > 1e-6)) {
      stop(
        "Error aggregating", sQuote(target_varname), ". ",
        "Sum of aggregated data different from sum of original data."
      )
    }
    file_yeardata <- tmpdata
    dimnames(file_yeardata) <- list(NULL, NULL, vars_req)
    rm(tmpdata)
  }
  # Dissolve longitude and latitude dimension to use cft_raster_gridindex
  indexed_dim <- c(prod(dim(file_yeardata)[1:2]), dim(file_yeardata)[-c(1:2)])
  indexed_dimnames <- dimnames(file_yeardata)[-1]
  dim(file_yeardata) <- indexed_dim
  dimnames(file_yeardata) <- indexed_dimnames
  file_yeardata
}
