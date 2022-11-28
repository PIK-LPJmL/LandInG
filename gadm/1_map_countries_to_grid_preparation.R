################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script has two tasks in preparation of grid intersection:             ##
## - split global shape into countries (or collection of states for countries ##
##   with regional distinction)                                               ##
## - create a shapefile with polygons for each grid cell (which will be used  ##
##   for grid intersection in next script)                                    ##
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
cat("Spatial resolution:", lpj_res_string, "\n")
if (force_grid) {
  cat(
    "Info: Using predefined grid with", nrow(griddata),
    "cells and a spatial extent:", toString(lpjgrid_extent), "\n"
  )
}

################################################################################
## Load country and state/region layer.                                       ##
## gadm_load() is defined in gadm_helper.R                                    ##
## gadm_dir is defined in gadm_setup.R                                        ##
## Parameter levels: 0 = country, 1 = region/state, 2 = county/district       ##
cat(
  "Loading GADM shapes from",
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()), "\n"
)
load_gadm(gadm_dir, levels = c(0, 1))
################################################################################

################################################################################
## Split would map into countries/regions for grid intersection.              ##
## split_directory, skip_countries, include_regions defined in gadm_setup.R   ##
if (!file.exists(split_directory)) {
  dir.create(split_directory, recursive = TRUE)
}
cat(
  "Splitting GADM shapes into individual countries and saving to:",
  split_directory, "\n"
)
for (country in unique(as.character(gadm_countries$GID_0))) {
  if (country %in% skip_countries) {
    # Do not process countries from skip_countries
    next
  }
  if (country %in% include_regions) {
    # Use GADM level 1 shapes for countries with regions
    country_shape <- gadm_regions[which(gadm_regions$GID_0 == country), ]
    if (nrow(country_shape) == 0) {
      # Not all GADM levels are available for all countries. Try level 0 if
      # level 1 is not available.
      warning(
        "GADM region level not available for country ",
        sQuote(country),
        " included in include_regions",
        call. = FALSE,
        immediate. = TRUE
      )
      country_shape <- gadm_countries[which(gadm_countries$GID_0 == country), ]
    }
  } else {
    # Use GADM level 0 shapes for countries without regions
    country_shape <- gadm_countries[which(gadm_countries$GID_0 == country), ]
  }
  if (nrow(country_shape) > 0) {
    if (nrow(country_shape) > 5) {
      # Split countries with large number of shapes into several parts
      for (r in seq(1, nrow(country_shape), by = 5)) {
        rows <- seq(r, min(r + 4, nrow(country_shape)))
        outfilename <- file.path(
          split_directory,
          paste0(country, r %/% 5 + 1, ".shp")
        )
        st_write(
          country_shape[rows, ],
          dsn = outfilename,
          delete_dsn = TRUE,
          quiet = TRUE
        )
      }
    } else {
      outfilename <- file.path(split_directory, paste0(country, ".shp"))
      st_write(
        country_shape,
        dsn = outfilename,
        delete_dsn = TRUE,
        quiet = TRUE
      )
    }
  } else {
    warning(
      "No entries for country ", country,
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
if (file.exists(gridcell_shapefile)) {
  # Get information on existing file
  cat("Check existing grid cell shapefile", gridcell_shapefile, "\n")
  lpjgrid_shape_info <- ogrInfo(
    gridcell_shapefile,
    sub("\\.[a-z]+$", "", basename(gridcell_shapefile))
  )
  res_check <- c(
    x = unname(xres(lpjgrid_raster) / 100),
    y = unname(yres(lpjgrid_raster) / 100)
  )
  if (lpjgrid_shape_info$extent[1] - xmin(lpjgrid_extent) > res_check["x"] ||
      lpjgrid_shape_info$extent[2] - ymin(lpjgrid_extent) > res_check["y"] ||
      xmax(lpjgrid_extent) - lpjgrid_shape_info$extent[3] >  res_check["x"] ||
      ymax(lpjgrid_extent) - lpjgrid_shape_info$extent[4]  > res_check["y"]) {
    print(lpjgrid_shape_info)
    message(
      "Existing grid cell shapefile ", gridcell_shapefile,
      " does not cover the full extent of the grid: ",
      toString(lpjgrid_extent),
      ". It will be generated again."
    )
    redo <- TRUE
  }
}
if (!file.exists(gridcell_shapefile) || redo) {
  cat("Converting grid cell raster to shapefile", gridcell_shapefile, "\n")
  # Split into parts to reduce memory requirements
  lonsteps <- seq(
    xmin(lpjgrid_extent),
    xmax(lpjgrid_extent) - lpj_res["lon"],
    by = 30
  )
  for (lonstart in lonsteps) {
    cat(lonstart, "to", min(lonstart + 30, xmax(lpjgrid_extent)), "\n")
    subset <- crop(
      lpjgrid_raster,
      extent(
        lonstart,
        min(lonstart + 30, xmax(lpjgrid_extent)),
        ymin(lpjgrid_extent),
        ymax(lpjgrid_extent)
      )
    )
    # Convert raster into polygon shape
    subset_shape <- rasterToPolygons(subset)
    rm(subset)
    gc(reset = TRUE)
    subset_shape <- st_as_sf(subset_shape)
    # Add column with cell area in square meters
    subset_shape <- cbind(subset_shape, Gridarea = st_area(subset_shape))
    # Name column with cell IDs extracted from lpjgrid_raster
    colnames(subset_shape)[which(colnames(subset_shape) == "layer")] <- "GridID"
    # Note: older versions of the sf package do not support the "append" flag
    # in st_write. If the following code fails try the commented version
    # below.
    if (lonstart == lonsteps[1]) {
      st_write(
        subset_shape,
        dsn = gridcell_shapefile,
        delete_dsn = TRUE,
        quiet = TRUE
      )
    } else {
      st_write(
        subset_shape,
        dsn = gridcell_shapefile,
        # append = TRUE, # comment this line if append does not work
        update = TRUE, # uncomment this line if append does not work
        quiet = TRUE
      )
    }
    rm(subset_shape)
    gc()
  }
  rm(lpjgrid_raster)
} else {
  cat(
    "Skipping creation of grid cell shapefile", gridcell_shapefile,
    "because it exists already. Delete file to force recreation.\n"
  )
}
################################################################################
