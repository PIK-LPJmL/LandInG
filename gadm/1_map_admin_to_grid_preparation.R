################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script takes the level 2 GADM layer and splits it into countries (or  ##
## several individual files per country for countries with a large number of  ##
## districts).                                                                ##
################################################################################

# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Load variables and helper functions used across several scripts.           ##
## You should always call gadm_setup.R first and make sure it is in the same  ##
## directory.                                                                 ##
source("gadm_setup.R")
source("gadm_helper.R")
################################################################################

cat("*** Script run in", getwd(), "***\n")
cat("Spatial resolution:", LandInG_setup$gadm$lpj_res_string, "\n")
if (LandInG_setup$gadm$force_grid) {
  cat(
    "Info: Using predefined grid with", nrow(LandInG_setup$gadm$griddata),
    "cells and a spatial extent:", toString(LandInG_setup$gadm$lpjgrid_extent),
    "\n"
  )
}

################################################################################
## Load country, state/region, and county/district layer.                     ##
## load_gadm() is defined in gadm_helper.R                                    ##
## gadm_dir is defined in gadm_setup.R                                        ##
## Parameter levels: 0 = country, 1 = region/state, 2 = county/district       ##
cat(
  "Loading GADM shapes from",
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  "\n"
)
load_gadm(
  LandInG_setup$gadm$gadm_dir,
  version = LandInG_setup$gadm$gadm_data_version,
  levels = c(0, 1, 2),
  format = LandInG_setup$gadm$gadm_format,
  return_env = LandInG_setup
)
################################################################################


################################################################################
## Split would map into countries/regions for grid intersection.              ##
## split_directory, skip_countries defined in gadm_setup.R                    ##
if (!file.exists(LandInG_setup$gadm$split_directory)) {
  dir.create(LandInG_setup$gadm$split_directory, recursive = TRUE)
} else if(length(list.files(LandInG_setup$gadm$split_directory, ".shp$")) > 0) {
  files <- list.files(
    LandInG_setup$gadm$split_directory,
    ".shp$",
    full.names = TRUE
  )
  message(
    "Deleting ", length(files), " existing files from split_directory ",
    LandInG_setup$gadm$split_directory,
    " to avoid inconsistencies."
  )
  file.remove(files)
}
cat(
  "Splitting GADM shapes into individual countries or group of regions and",
  "saving to:", LandInG_setup$gadm$split_directory, "\n"
)
for (country in unique(as.character(LandInG_setup$gadm$gadm_countries$GID_0))) {
  if (country %in% LandInG_setup$gadm$skip_countries) {
    next
  }
  # GADM level 2 is not available for all countries, fall back on higher GADM
  # levels if necessary
  if (country %in% as.character(LandInG_setup$gadm$gadm_districts$GID_0)) {
    index <- which(LandInG_setup$gadm$gadm_districts$GID_0 == country)
    country_shape <- LandInG_setup$gadm$gadm_districts[index, ]
    rm(index)
  } else if (country %in% as.character(LandInG_setup$gadm$gadm_regions$GID_0)) {
    message("Using administrative level 1 instead of 2 for ", country)
    index <- which(LandInG_setup$gadm$gadm_regions$GID_0 == country)
    country_shape <- LandInG_setup$gadm$gadm_regions[index, ]
    rm(index)
  } else {
    message("Using administrative level 0 instead of 2 for ", country)
    index <- which(LandInG_setup$gadm$gadm_countries$GID_0 == country)
    country_shape <- LandInG_setup$gadm$gadm_countries[index, ]
    rm(index)
    if (country %in% LandInG_setup$gadm$include_regions) {
      warning(
        "No region data available for ", country,
        " included in include_regions",
        call. = FALSE, immediate. = TRUE
      )
    }
  }
  if (nrow(country_shape) > 0) {
    threshold <- ifelse("GID_2" %in% names(country_shape), 100, 5)
    if (nrow(country_shape) > threshold) {
      # Countries with a lot of districts are split into shape collections to
      # better distribute intersection in next script between parallel tasks
      for (r in seq(1, nrow(country_shape), by = threshold)) {
        rows <- seq(r, min(r + threshold - 1, nrow(country_shape)))
        outfilename <- file.path(
          LandInG_setup$gadm$split_directory,
          paste0(country, r %/% threshold + 1, ".shp")
        )
        sf::st_write(
          country_shape[rows, ],
          dsn = outfilename,
          delete_dsn = TRUE,
          quiet = TRUE,
          layer_options = "ENCODING=UTF-8"
        )
      }
    } else {
      outfilename <- file.path(
        LandInG_setup$gadm$split_directory,
        paste0(country, ".shp")
      )
      sf::st_write(
        country_shape,
        dsn = outfilename,
        delete_dsn = TRUE,
        quiet = TRUE,
        layer_options = "ENCODING=UTF-8"
      )
    }
  } else {
    warning(
      "No entries for country ", sQuote(country, q = FALSE),
      call. = FALSE, immediate. = TRUE
    )
  }
}
################################################################################


################################################################################
## Create shapefile with polygons for each gridcell.                          ##
## Filename gridcell_shapefile is defined in gadm_setup.R                     ##
## lpjgrid_raster and lpjgrid_extent also set up in gadm_setup.R              ##
redo <- FALSE
if (file.exists(LandInG_setup$gadm$gridcell_shapefile)) {
  # Get information on existing file
  cat(
    "Check existing grid cell shapefile",
    LandInG_setup$gadm$gridcell_shapefile, "\n"
  )
  lpjgrid_shape_info <-
    terra::ext(terra::vect(LandInG_setup$gadm$gridcell_shapefile))
  res_check <- c(
    x = unname(terra::xres(LandInG_setup$gadm$lpjgrid_raster) / 100),
    y = unname(terra::yres(LandInG_setup$gadm$lpjgrid_raster) / 100)
  )
  if (
    terra::xmin(lpjgrid_shape_info) -
      terra::xmin(LandInG_setup$gadm$lpjgrid_extent) > res_check["x"] ||
      terra::ymin(lpjgrid_shape_info) -
        terra::ymin(LandInG_setup$gadm$lpjgrid_extent) > res_check["y"] ||
      terra::xmax(LandInG_setup$gadm$lpjgrid_extent) -
        terra::xmax(lpjgrid_shape_info) >  res_check["x"] ||
      terra::ymax(LandInG_setup$gadm$lpjgrid_extent) -
        terra::ymax(lpjgrid_shape_info) > res_check["y"]
  ) {
    print(lpjgrid_shape_info)
    message(
      "Existing grid cell shapefile ", LandInG_setup$gadm$gridcell_shapefile,
      " does not cover the full extent of the grid: ",
      toString(LandInG_setup$gadm$lpjgrid_extent),
      ". It will be generated again."
    )
    redo <- TRUE
  }
  rm(lpjgrid_shape_info)
  gc(reset = TRUE)
}
if (!file.exists(LandInG_setup$gadm$gridcell_shapefile) || redo) {
  cat(
    "Converting grid cell raster to shapefile",
    LandInG_setup$gadm$gridcell_shapefile, "\n"
  )
  # Split into parts to reduce memory requirements
  lonsteps <- seq(
    terra::xmin(LandInG_setup$gadm$lpjgrid_extent),
    terra::xmax(LandInG_setup$gadm$lpjgrid_extent) -
      LandInG_setup$gadm$lpj_res["lon"],
    by = 30
  )
  for (lonstart in lonsteps) {
    cat(
      lonstart, "to",
      min(lonstart + 30, terra::xmax(LandInG_setup$gadm$lpjgrid_extent)), "\n"
    )
    subset <- terra::crop(
      LandInG_setup$gadm$lpjgrid_raster,
      terra::ext(
        lonstart,
        min(lonstart + 30, terra::xmax(LandInG_setup$gadm$lpjgrid_extent)),
        terra::ymin(LandInG_setup$gadm$lpjgrid_extent),
        terra::ymax(LandInG_setup$gadm$lpjgrid_extent)
      )
    )
    # Convert raster into polygon shape
    subset_shape <- terra::as.polygons(subset)
    rm(subset)
    gc(reset = TRUE)
    subset_shape <- sf::st_as_sf(subset_shape)
    # Add column with cell area in square meters
    subset_shape <- cbind(subset_shape, Gridarea = sf::st_area(subset_shape))
    # Name column with cell IDs extracted from lpjgrid_raster
    colnames(subset_shape)[grep("layer|lyr", colnames(subset_shape))] <- "GridID"
    # Note: older versions of the sf package do not support the "append" flag
    # in st_write. If the following code fails try the commented version below.
    if (lonstart == lonsteps[1]) {
      sf::st_write(
        subset_shape,
        dsn = LandInG_setup$gadm$gridcell_shapefile,
        delete_dsn = TRUE,
        quiet = TRUE
      )
    } else {
      sf::st_write(
        subset_shape,
        dsn = LandInG_setup$gadm$gridcell_shapefile,
        append = TRUE, # comment this line if append does not work
        # update = TRUE, # uncomment this line if append does not work
        quiet = TRUE
      )
    }
    rm(subset_shape)
    gc()
  }
} else {
  cat(
    "Skipping creation of grid cell shapefile",
    LandInG_setup$gadm$gridcell_shapefile,
    "because it exists already. Delete file to force recreation.\n"
  )
}
################################################################################
