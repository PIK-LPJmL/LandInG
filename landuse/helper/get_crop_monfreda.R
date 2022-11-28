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
## data to spatial resolution of unit_raster if necessary.                    ##
## Parameters:                                                                ##
## filename: name of file containing harvested areas (either Geotiff or       ##
##           NetCDF depending on file_format)                                 ##
## crop: crop name                                                            ##
## unit_raster: raster to match, normally use GADM raster                     ##
## fact: aggregation factor (if harvested area needs to be aggregated)        ##
## file_format: either NetCDF or Geotiff                                      ##
## file_area_layer: NetCDF layer which contains harvested areas               ##
## file_quality_layer: NetCDF layer which contains quality flag               ##
## global_cover: extent object used for comparison, normally use global_extent##
## file_is_fraction: whether harvested area unit is a fraction                ##
## file_var: Variable-specific file names used in case of Geotiff format      ##
## area_raster: Raster object providing cell areas, must be same resolution   ##
##              as harvested area, used if file_is_fraction==TRUE             ##
## area_units: unit of harvested area in filename                             ##
## crop_names: character vector specifying names of all Monfreda crops        ##
################################################################################
get_crop_monfreda <- function(filename, crop, unit_raster, fact,
                              file_format = monfreda_format,
                              # monfreda_format set in landuse_setup.R
                              file_area_layer = monfreda_area_layer,
                              file_quality_layer = monfreda_area_quality_layer,
                              # monfreda_area_layer, monfreda_area_quality_layer
                              # set in landuse_setup.R
                              global_cover = global_extent,
                              file_is_fraction = monfreda_is_fraction,
                              file_var = monfreda_file_var,
                              # monfreda_file_var set in landuse_setup.R
                              area_raster = monfreda_area,
                              # monfreda_area must exist in calling script
                              area_units = monfreda_area_units,
                              # monfreda_area_units set in landuse_setup.R
                              crop_names = monfreda_names
                             ) {
  if (file_format == "NetCDF") {
    # This expects the NetCDF file to contain a variable paste0(crop, "Data"),
    # which has both harvested area and quality flag as different layers
    monfreda_file <- nc_open(filename)
    monfreda_flip <- ifelse(
      monfreda_file$dim$lat$vals[2] < monfreda_file$dim$lat$vals[1],
      FALSE,
      TRUE
    )
    monfreda_filedata <- ncvar_get(
      monfreda_file,
      varid = paste0(crop, "Data"),
      start = c(1, 1, file_area_layer, 1),
      count = c(monfreda_file$dim$lon$len, monfreda_file$dim$lat$len, 1, 1)
    )
    monfreda_qualitydata <- ncvar_get(
      monfreda_file,
      varid = paste0(crop, "Data"),
      start = c(1, 1, file_quality_layer, 1),
      count = c(monfreda_file$dim$lon$len, monfreda_file$dim$lat$len, 1, 1)
    )
    if (monfreda_flip) {
      index <- seq(monfreda_file$dim$lat$len, 1)
      monfreda_filedata <- monfreda_filedata[, index]
      monfreda_qualitydata <- monfreda_qualitydata[, index]
    }
    resx <- abs(monfreda_file$dim$lon$vals[2] - monfreda_file$dim$lon$vals[1])
    resy <- abs(monfreda_file$dim$lat$vals[2] - monfreda_file$dim$lat$vals[1])
    monfreda_extent <- extent(
      c(min(monfreda_file$dim$lon$vals) - resx / 2,
        max(monfreda_file$dim$lon$vals) + resx / 2,
        min(monfreda_file$dim$lat$vals) - resy / 2,
        max(monfreda_file$dim$lat$vals) + resy / 2
      )
    )
    # Function matching_extent defined in raster_helpers.R
    if (matching_extent(monfreda_extent, global_cover, resx, resy)) {
      monfreda_extent <- global_cover
    }
  } else {
    # Geotiff format
    monfreda_raster <- raster(filename)
    monfreda_filedata <- array(
      values(monfreda_raster),
      dim = c(ncol(monfreda_raster), nrow(monfreda_raster))
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
    monfreda_qualityraster <- raster(quality_filename)
    monfreda_qualitydata <- array(
      values(monfreda_qualityraster),
      dim = c(ncol(monfreda_qualityraster), nrow(monfreda_qualityraster))
    )
    if (matching_extent(
      global_cover,
      extent(monfreda_raster),
      xres(monfreda_raster),
      yres(monfreda_raster)
    )) {
      monfreda_raster <- setExtent(monfreda_raster, global_cover)
    }
    monfreda_extent <- extent(monfreda_raster)
    resx <- xres(monfreda_raster)
    resy <- yres(monfreda_raster)
  }
  if (!matching_extent(monfreda_extent, extent(unit_raster), resx, resy)) {
    stop(
      "Cannot aggregate Monfreda harvested area to spatial units because ",
      "their spatial extents differ"
    )
  }
  if (file_is_fraction) {
    monfreda_filedata <- monfreda_filedata * ud.convert(1, area_units, "") *
      values(area_raster)
  }
  if (max(fact) > 1) {
    if (which(crop_names == crop) %% 10 == 0) {
      cat(
        "Monfreda data needs to be aggregated from",
        toString(round(res(area_raster), 5)),
        "to",
        toString(round(res(unit_raster), 5)),
        "first.\n"
      )
    }
    # Function aggregate_array defined in aggregate_array.R
    monfreda_filedata <- aggregate_array(monfreda_filedata, fact, "sum", FALSE)
    if (monfreda_format == "NetCDF") {
      monfreda_qualityraster <- raster(area_raster)
      values(monfreda_qualityraster) <- c(monfreda_qualitydata)
    }
    # Quality flag is categorical, aggregate by selecting most frequent
    monfreda_qualityraster <- aggregate(
      monfreda_qualityraster,
      fact = fact,
      fun = modal_ties_lowest
    )
    monfreda_qualitydata <- array(
      values(monfreda_qualityraster),
      dim = c(ncol(monfreda_qualityraster), nrow(monfreda_qualityraster))
    )
  }
  if (file_format == "NetCDF") {
    nc_close(monfreda_file)
  } else {
    rm(monfreda_raster)
    rm(monfreda_qualityraster)
  }
  list(filedata = monfreda_filedata, qualitydata = monfreda_qualitydata)
}
