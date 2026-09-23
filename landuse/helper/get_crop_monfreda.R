################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This function reads crop-specific harvested area from Monfreda et al. and  ##
## aggregates data to spatial resolution of unit_raster if necessary.         ##
## Parameters:                                                                ##
## filename: name of file containing harvested areas (either Geotiff or       ##
##           NetCDF depending on file_format)                                 ##
## file_format: either NetCDF or Geotiff                                      ##
## file_area_layer: NetCDF layer which contains harvested areas               ##
## file_quality_layer: NetCDF layer which contains quality flag               ##
## file_is_fraction: whether harvested area unit is a fraction                ##
## file_var: Variable-specific file names used in case of Geotiff format      ##
## file_units: unit of harvested area in filename                             ##
## file_years: year(s) read from filename; must be all years in file          ##
## crop: crop name                                                            ##
## unit_raster: raster to match, normally use GADM raster                     ##
## fact: aggregation factor (if harvested area needs to be aggregated)        ##
## global_cover: extent object used for comparison, normally use global_extent##
## area_raster: Raster object providing cell areas, must be same resolution   ##
##              as harvested area, used if file_is_fraction==TRUE             ##
## crop_names: character vector specifying names of all Monfreda crops        ##
################################################################################
get_crop_monfreda <- function(filename, file_format, file_area_layer,
                              file_quality_layer, file_is_fraction, file_var,
                              file_units, file_years,
                              crop, unit_raster, fact, global_cover,
                              area_raster, crop_names
                             ) {
  if (file_format == "NetCDF") {
    # This expects the NetCDF file to contain a variable paste0(crop, "Data"),
    # which has both harvested area and quality flag as different layers
    mon_file <- ncdf4::nc_open(filename)
    if (
      mon_file[["var"]][[paste0(crop, "Data")]][["size"]][4] !=
        length(file_years)
    ) {
      stop(
        "Number of years in ", sQuote(filename), " (",
        mon_file[["var"]][[paste0(crop, "Data")]][["size"]][4],
        ") does not match supplied file_years ", toString(file_years)
      )
    }
    mon_flip <- ifelse(
      mon_file$dim$lat$vals[2] < mon_file$dim$lat$vals[1],
      FALSE,
      TRUE
    )
    mon_filedata <- ncdf4::ncvar_get(
      mon_file,
      varid = paste0(crop, "Data"),
      start = c(1, 1, file_area_layer, 1),
      count = c(
        mon_file$dim$lon$len,
        mon_file$dim$lat$len,
        1,
        length(file_years)
      )
    )
    mon_qualitydata <- ncdf4::ncvar_get(
      mon_file,
      varid = paste0(crop, "Data"),
      start = c(1, 1, file_quality_layer, 1),
      count = c(
        mon_file$dim$lon$len,
        mon_file$dim$lat$len,
        1,
        length(file_years)
      )
    )
    dim(mon_filedata) <- dim(mon_qualitydata) <- c(
      lon = mon_file$dim$lon$len,
      lat = mon_file$dim$lat$len,
      time = length(file_years)
    )
    if (mon_flip) {
      index <- seq(mon_file$dim$lat$len, 1)
      mon_filedata <- mon_filedata[, index, ]
      mon_qualitydata <- mon_qualitydata[, index, ]
    }
    resx <- abs(mon_file$dim$lon$vals[2] - mon_file$dim$lon$vals[1])
    resy <- abs(mon_file$dim$lat$vals[2] - mon_file$dim$lat$vals[1])
    mon_extent <- terra::ext(
      c(min(mon_file$dim$lon$vals) - resx / 2,
        max(mon_file$dim$lon$vals) + resx / 2,
        min(mon_file$dim$lat$vals) - resy / 2,
        max(mon_file$dim$lat$vals) + resy / 2
      )
    )
    # Function matching_extent defined in terra_helpers.R
    if (matching_extent(mon_extent, global_cover, resx, resy)) {
      mon_extent <- global_cover
    }
  } else {
    # Geotiff format
    mon_raster <- terra::rast(filename)
    if (terra::nlyr(mon_raster) != length(file_years)) {
      stop(
        "Number of years in ", sQuote(filename), " (",
        terra::nlyr(mon_raster),
        ") does not match supplied file_years ", toString(file_years)
      )
    }
    mon_filedata <- array(
      ul(terra::values(mon_raster)),
      dim = c(
        lon = terra::ncol(mon_raster),
        lat = terra::nrow(mon_raster),
        time = terra::nlyr(mon_raster)
      )
    )
    quality_filename <- file.path(
      dirname(filename),
      paste0(
        gsub(
          file_var[[file_format]][1],
          file_var[[file_format]][2],
          basename(filename)
        )
      )
    )
    mon_qualityraster <- terra::rast(quality_filename)
    if (terra::nlyr(mon_qualityraster) != length(file_years)) {
      stop(
        "Number of years in ", sQuote(quality_filename), " (",
        terra::nlyr(mon_qualityraster),
        ") does not match supplied file_years ", toString(file_years)
      )
    }
    mon_qualitydata <- array(
      ul(terra::values(mon_qualityraster)),
      dim = c(
        lon = terra::ncol(mon_qualityraster),
        lat = terra::nrow(mon_qualityraster),
        time = terra::nlyr(mon_qualityraster)
      )
    )
    if (matching_extent(
      global_cover,
      terra::ext(mon_raster),
      terra::xres(mon_raster),
      terra::yres(mon_raster)
    )) {
      terra::ext(mon_raster) <- global_cover
    }
    mon_extent <- terra::ext(mon_raster)
    resx <- terra::xres(mon_raster)
    resy <- terra::yres(mon_raster)
  }
  if (!matching_extent(mon_extent, terra::ext(unit_raster), resx, resy)) {
    stop(
      "Cannot aggregate Monfreda harvested area to spatial units because ",
      "their spatial extents differ"
    )
  }
  if (file_is_fraction) {
    mon_filedata <- mon_filedata *
      units::ud_convert(1, file_units, "1") * ul(terra::values(area_raster))
  }
  if (max(fact) > 1) {
    if (which(crop_names == crop) %% 10 == 0) {
      cat(
        "Monfreda data needs to be aggregated from",
        toString(round(terra::res(area_raster), 5)),
        "to",
        toString(round(terra::res(unit_raster), 5)),
        "first.\n"
      )
    }
    # Function aggregate_array defined in aggregate_array.R
    mon_filedata <- aggregate_array(mon_filedata, fact, "sum", FALSE)
    if (file_format == "NetCDF") {
      mon_qualityraster <- terra::rast(area_raster, nlyrs = length(file_years))
      terra::set.values(
        mon_qualityraster,
        cells = seq_len(terra::ncell(mon_qualityraster)),
        values = c(mon_qualitydata)
      )
    }
    # Quality flag is categorical, aggregate by selecting most frequent
    mon_qualityraster <- terra::aggregate(
      mon_qualityraster,
      fact = rev(fact), # res() returns lon/lat, fact is lat/lon
      fun = modal_ties_lowest,
      na.rm = TRUE
    )
    mon_qualitydata <- array(
      terra::values(mon_qualityraster),
      dim = c(
        lon = terra::ncol(mon_qualityraster),
        lat = terra::nrow(mon_qualityraster),
        time = terra::nlyr(mon_qualityraster)
      )
    )
  }
  if (file_format == "NetCDF") {
    ncdf4::nc_close(mon_file)
  } else {
    rm(mon_raster)
    rm(mon_qualityraster)
  }
  dimnames(mon_filedata) <- dimnames(mon_qualitydata) <-
    list(lon = NULL, lat = NULL, time = file_years)
  list(filedata = mon_filedata, qualitydata = mon_qualitydata)
}
