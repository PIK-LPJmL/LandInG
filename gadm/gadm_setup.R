################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Basic setup script for GADM processing                                     ##
## This is used by the scripts for the individual processing steps.           ##
################################################################################

################################################################################
## Set up base directory gadm_dir for data processing.                        ##
## This is where scripts and downloaded GADM data are located.                ##
################################################################################
gadm_dir <- ""
if (nchar(gadm_dir) > 0) {
  # Set working directory to gadm_dir
  setwd(gadm_dir)
}

################################################################################
## Set up spatial resolution that you want to process GADM data at.           ##
## The scripts only support regular lon/lat grids.                            ##
## 2 values:                                                                  ##
##    1) longitude resolution                                                 ##
##    2) latitude resolution                                                  ##
## unit: degree                                                               ##
lpj_res <- c(lon = 1 / 2, lat = 1 / 2)
##                                                                            ##
# Determine resolution string to use in filenames
tmp_res <- unique(
  ifelse(lpj_res[c("lon", "lat")] < 1 / 60, 3600, 60) * lpj_res[c("lon", "lat")]
)
lpj_res_string <- paste(
  round(tmp_res),
  unique(ifelse(lpj_res[c("lon", "lat")] < 1 / 60, "arcsec", "arcmin")),
  sep = "", collapse = "_by_"
)
rm(tmp_res)
################################################################################


################################################################################
## Data format of GADM source data                                            ##
## gadm_helper.R has predefined functionality for GeoPackage and ESRI         ##
## Shapefile format, however support may depend on your local system          ##
## installation.                                                              ##
# gadm_format <- "GPKG"
gadm_format <- "ESRI Shapefile"
################################################################################


################################################################################
## You may either use a predefined list of coordinates or let the script      ##
## determine the grid cells based on all cells that have land according to    ##
## GADM.                                                                      ##
## force_grid: set to TRUE to use predefined grid                             ##
force_grid <- FALSE
##                                                                            ##
## If force_grid == TRUE you need to provide a matrix with two columns "lon"  ##
## and "lat" giving longitude and latitude of grid cell center points. The    ##
## spatial resolution needs to fit with lpj_res.                              ##
## This could be read in, e.g., from a CSV file. Default: empty list          ##
griddata <- matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("lon", "lat")))
## To use the CRU land grid you may load gridlist_CRU.csv included in the     ##
## toolbox:                                                                   ##
# griddata <- read.csv("gridlist_CRU.csv")
################################################################################

################################################################################
## Minimum grid cell area to be included in grid.                             ##
## Unless force_grid == TRUE, grid is derived from all cells containing land. ##
## This threshold allows to skip cells with very small land fractions. Set to ##
## 0 to preserve all cells.                                                   ##
threshold_grid <- 1000
## unit: square meter                                                         ##
################################################################################

################################################################################
## Load required packages. These may need to be installed first.              ##
library(sf)
library(raster)
library(lwgeom)
library(rgdal)
library(foreach)
library(stringi)
library(units)
# Note: Starting with version 1.0 package "sf" uses the new package "s2" for
# spherical geometry, i.e. when doing calculations on spatial objects in a
# geographical coordinate reference system. Because of this, results differ from
# earlier versions of "sf". If you need to reproduce the behaviour of earlier
# "sf" versions you can switch off "s2" functionality with:
# sf_use_s2(FALSE)
################################################################################

################################################################################
## The country code input for LPJmL includes regions for some large           ##
## countries. Set up which countries (3-letter ISO code):                     ##
include_regions <- c("AUS", "BRA", "CAN", "CHN", "IND", "RUS", "USA")
##                                                                            ##
## The default for global LPJmL setups is to exclude Antarctica (ATA).        ##
## You may add more countries to exclude by adding their ISO codes.           ##
## Set to NULL if you do not want to skip any countries.                      ##
skip_countries <- c("ATA")
##                                                                            ##
## GADM version 3.6 includes the Caspian Sea (XCA) as a separate country.     ##
## If defined here, the algorithm will assign Caspian Sea only to cells fully ##
## covered by Caspian Sea. All other cells will be assigned to the second     ##
## largest country. You may add other GADM-0 units to be treated in the same  ##
## way. Use ISO code.                                                         ##
water_bodies <- c("XCA")
## Country code to be used for cells without GADM coverage (if                ##
## force_grid == TRUE). Set variable value to country name to be used, and    ##
## name attribute to ISO code. The ISO code should consist of three capital   ##
## letters and start with an "X" to denote it is not an official ISO code.    ##
gadm_no_land <- c(XNL = "No land")
################################################################################

################################################################################
## Set up further directories.                                                ##
## - directory where country-specific shapes are saved                        ##
split_directory <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  "by_country"
)
##                                                                            ##
## - directory where shape intersections between country shapes and grid      ##
##   cells are saved (name depends on resolution)                             ##
intersect_directory <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "grid_intersection_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", "")
  )
)
##                                                                            ##
## - directory where for country-specific shapes for GADM level 2             ##
##   intersection are saved                                                   ##
split_directory_districts <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  "by_country_districts"
)
##                                                                            ##
## - directory where shape intersections between level 2 shapes and grid      ##
##   cells are saved (name depends on resolution)                             ##
intersect_directory_districts <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "grid_intersection_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_districts"
  )
)
################################################################################


################################################################################
## Main output files of GADM processing                                       ##
##                                                                            ##
## Created by step 3 script:                                                  ##
##                                                                            ##
## Grid (list of coordinates):                                                ##
## This corresponds to griddata read above if force_grid == TRUE.             ##
##   - format: either "BIN" for LPJmL input format or "CSV".                  ##
gridformat <- "BIN"
##   - filename (comment to this line skip writing grid file)                 ##
gridname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "grid_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".", tolower(gridformat)
  )
)
##                                                                            ##
## Country code:                                                              ##
##   This is a list with two columns:                                         ##
##   1st column: dominant country code per cell (as vector of increasing      ##
##   integer values).                                                         ##
##   2nd column: dominant state/region code for countries included in         ##
##   include_regions, otherwise same as first column.                         ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
cowformat <- "BIN"
##   - filename (comment this line to skip writing country code file)         ##
cowname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "cow_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".", tolower(cowformat)
  )
)
##   - meta table linking codes in country code file to country names and ISO ##
##     codes (comment to this line skip writing file)                         ##
cowmetaname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "cow_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_countries.csv"
  )
)
##   - meta table linking region codes in country code file to region names   ##
##     and ISO codes (comment this line to skip writing file)                 ##
regmetaname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "cow_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_regions.csv"
  )
)
##   - raster of country code (format can be any format supported by          ##
##     writeRaster() (comment to skip writing country raster)                 ##
cowraster <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "cow_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_countries.nc" # file extension determines format
  )
)
##   - raster of region code (format can be any format supported by           ##
##     writeRaster() (comment this line to skip writing region raster)        ##
regraster <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "cow_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_regions.nc" # file extension determines format
  )
)
##                                                                            ##
## Number of countries in each cell:                                          ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
ncountryformat <- "BIN"
##   - filename (comment this line to skip writing file)                      ##
ncountryname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "ncountry_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".", tolower(ncountryformat)
  )
)
##   - raster of number of countries (format can be any format supported      ##
##     by writeRaster() (comment this line to skip writing raster)            ##
ncountryraster <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "ncountry_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".nc" # file extension determines format
  )
)
##                                                                            ##
## Land fraction in each cell                                                 ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
landfracformat <- "BIN"
##   - filename (comment this line to skip writing file)                      ##
landfracname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "landfrac_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".", tolower(landfracformat)))
##   - raster of land fraction (format can be any format supported by         ##
##     writeRaster() (comment this line to skip writing raster)               ##
landfracraster <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "landfrac_gadm_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".nc" # file extension determines format
  )
)
##                                                                            ##
##                                                                            ##
## Created by step 6 script:                                                  ##
##                                                                            ##
## Dominant country, region/state and district/county in each cell.           ##
##   This is a list with 3 columns:                                           ##
##   1st column: dominant country code per cell (as vector of increasing      ##
##   integer values)                                                          ##
##   2nd column: dominant region/state code per cell (as vector of increasing ##
##   integer values)                                                          ##
##   3rd column: dominant district/county code per cell (as vector of         ##
##   increasing integer values)                                               ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
gadmformat <- "BIN"
##   - filename (comment this line to skip writing file)                      ##
gadmname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "gadm_level0_1_2_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".", tolower(gadmformat)
  )
)
##   - meta table linking codes in country code file to country names and ISO ##
##     codes (comment to this line skip writing file)                         ##
gadmmetaname <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "gadm_level0_1_2_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_indices.csv"
  )
)
##   - raster with all three dominant GADM levels (format can be any format   ##
##     supported by writeRaster() with multiband support (comment this line   ##
##     to skip writing raster)                                                ##
gadmraster <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "gadm_level0_1_2_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".nc" # file extension determines format
  )
)
##                                                                            ##
## LPJmL input format includes a file header that describes the structure of  ##
## the input file to the model. If selecting "BIN" as format for any of the   ##
## files above you need to set this as well.                                  ##
## - header type: must be 2 or 3. Type 3 allows values to be saved as float   ##
##   whereas type 2 only supports integer values. If selecting type 2 any     ##
##   floating point values will be multiplied by a scaling factor and rounded ##
##   to integer values, leading to reduced precision. This is mostly relevant ##
##   for the grid and landfrac files. Only bintype 3 allows for different     ##
##   longitude and latitude resolutions.                                      ##
bintype <- 3
## - header name: Headers of LPJmL input files include a name. Header names   ##
##   are defined in /include/header.h of the LPJmL source code and do not     ##
##   usually need to be changed.                                              ##
##   Not all headers below exist in LPJmL source code.                        ##
grid_headername <- "LPJGRID"
cow_headername <- "LPJ_COW"
ncountry_headername <- "LPJNCOW" # currently not in LPJmL
landfrac_headername <- "LPJLFRC" # currently not in LPJmL
gadm_headername <- "LPJGADM"     # currently not in LPJmL
################################################################################

################################################################################
## Create raster layer based on                                               ##
## - spatial resolution lpj_res                                               ##
## - predefined coordinate list (if force_grid == TRUE) or                    ##
## - global extent (if force_grid == FALSE)                                   ##
if (force_grid) {
  if (nrow(griddata) < 1) {
    stop(
      "You have set force_grid to TRUE but have not provided ",
      "a list of grid coordinates in gadm_setup.R"
    )
  }
  # Extent defined by minimum/maximum coordinates in griddata
  lpjgrid_extent <- extent(
    min(griddata[, "lon"]) - lpj_res["lon"] / 2,
    xmax = max(griddata[, "lon"]) + lpj_res["lon"] / 2,
    ymin = min(griddata[, "lat"]) - lpj_res["lat"] / 2,
    ymax = max(griddata[, "lat"]) + lpj_res["lat"] / 2
  )
} else {
  # Global extent
  # You may reduce this if you only intend to process a subset of countries
  lpjgrid_extent <- extent(-180, xmax = 180, ymin = -90, ymax = 90)
}
lpjgrid_raster <- raster(lpjgrid_extent, res = lpj_res[c("lon", "lat")])
# Set projection
crs(lpjgrid_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
# Assign IDs to raster cells
if (force_grid) {
  # Coordinates from griddata are assigned consecutive numbers
  lpjgrid_raster[cellFromXY(lpjgrid_raster, griddata)] <-
    seq_len(nrow(griddata))
  # Coordinates not included in griddata list follow
  lpjgrid_raster[which(is.na(lpjgrid_raster[]))] <- seq(
    nrow(griddata) + 1,
    ncell(lpjgrid_raster)
  )
} else {
  # All cells get consecutive IDs
  values(lpjgrid_raster) <- seq_len(ncell(lpjgrid_raster))
}
################################################################################


################################################################################
## A shapefile containing polygons for each raster grid cell is created as    ##
## part of processing. The filename depends on spatial resolution lpj_res.    ##
## By default file is saved to gadm_dir.                                      ##
gridcell_shapefile <- file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "gridcell_polygons_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".shp"
  )
)
################################################################################

################################################################################
## Step 3 and 6 collect information from shape intersection and save it to    ##
## RData files to speed up in case of repeated processing. Filenames depend   ##
## on spatial resolution lpj_res. By default file is saved to gadm_dir.       ##
cell_list_RData <-  file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "celllist_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    ".RData"
  )
)
cell_list_districts_RData <-  file.path(
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
  paste0(
    "celllist_",
    lpj_res_string,
    ifelse(force_grid, "_predefined_grid", ""),
    "_districts.RData"
  )
)
################################################################################



################################################################################
## Prevent recoding of attribute tables when writing shapefiles with ESRI     ##
## driver. This may avoid problems with shapefiles using UTF8 encoding when R ##
## is not running on a UTF8 locale.                                           ##
setCPLConfigOption("SHAPE_ENCODING", "")
################################################################################

################################################################################
## Make sure that character strings are not converted to factors              ##
options("stringsAsFactors" = FALSE)
################################################################################

################################################################################
## Helper functions for LPJmL input format                                    ##
## The script lpjml_format_helper_functions.R is saved in the parent          ##
## directory by default.                                                      ##
if (file.exists("../lpjml_format_helper_functions.R")) {
  source("../lpjml_format_helper_functions.R")
} else {
  stop("Please update path to script with LPJmL input format helper function")
}
################################################################################


################################################################################
## Plausibility checks                                                        ##
if (length(gadm_no_land) != 1 || any(nchar(names(gadm_no_land)) != 3)) {
  stop(
    "You have defined ", length(gadm_no_land),
    ifelse(length(gadm_no_land) == 1, " entry", " entries"),
    " for gadm_no_land of which ",
    length(which(nchar(names(gadm_no_land)) != 3)),
    ifelse(
      length(which(nchar(names(gadm_no_land)) != 3)) == 1,
      " has",
      " have"
    ),
    " an invalid ISO code.\n",
    "You need to define one country name with a 3-letter ISO code for ",
    "gadm_no_land in gadm_setup.R"
  )
}
if (threshold_grid < 0) {
  stop(
    "Negative threshold_grid: ",
    threshold_grid, " m2\n",
    "Threshold must be positive or zero."
  )
}
if (bintype < 3 && lpj_res["lon"] != lpj_res["lat"]) {
  stop(
    "You have selected bintype ", bintype, " and different values for",
    "longitude and latitude resolution.\n",
    "Use bintype 3 if longitude and latitude resolutions differ."
  )
}
################################################################################
