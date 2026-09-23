################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Helper functions for LPJmL input format and basic LandInG setup.           ##
## The script landing_setup.R is saved in the parent directory by default.    ##
################################################################################
if (file.exists("../landing_setup.R")) {
  source("../landing_setup.R", chdir = TRUE)
  LandInG_setup$gadm <- list()
} else if (!exists("LandInG_setup") || !is.environment(LandInG_setup)) {
  stop("Please update path to script with LandInG setup script")
}
################################################################################

################################################################################
## Basic setup script for GADM processing                                     ##
## This is used by the scripts for the individual processing steps.           ##
################################################################################

################################################################################
## Set up base directory gadm_dir for data processing.                        ##
## This is where scripts and downloaded GADM data are located.                ##
################################################################################
LandInG_setup$gadm$gadm_dir <- ""
if (nchar(LandInG_setup$gadm$gadm_dir) > 0) {
  # Set working directory to gadm_dir
  setwd(LandInG_setup$gadm$gadm_dir)
}

################################################################################
## Set up spatial resolution that you want to process GADM data at.           ##
## The scripts only support regular lon/lat grids.                            ##
## 2 values:                                                                  ##
##    1) longitude resolution                                                 ##
##    2) latitude resolution                                                  ##
## unit: degree                                                               ##
LandInG_setup$gadm$lpj_res <- c(lon = 1 / 2, lat = 1 / 2)
##                                                                            ##
# Automatically determine resolution string to use in filenames.
tmp_res <- unique(
  ifelse(LandInG_setup$gadm$lpj_res[c("lon", "lat")] < 1 / 60, 3600, 60) *
    LandInG_setup$gadm$lpj_res[c("lon", "lat")]
)
LandInG_setup$gadm$lpj_res_string <- paste(
  round(tmp_res),
  unique(
    ifelse(
      LandInG_setup$gadm$lpj_res[c("lon", "lat")] < 1 / 60,
      "arcsec",
      "arcmin"
    )
  ),
  sep = "", collapse = "_by_"
)
rm(tmp_res)
################################################################################


################################################################################
## Data format and version of GADM source data                                ##
## gadm_helper.R has predefined functionality for GeoPackage and ESRI         ##
## Shapefile format, however support may depend on your local system          ##
## installation.                                                              ##
# LandInG_setup$gadm$gadm_format <- "GPKG"
LandInG_setup$gadm$gadm_format <- "ESRI Shapefile"
## Technically, version 2.8, 3.6, 4.0 and 4.1 can loaded but version 2.8 has  ##
## incompatible column names and the version 4.0 currently available on the   ##
## GADM server lacks level 0 and can therefore not be used.                   ##
## Version 3.6 is available as GPKG and ESRI Shapefile format. Version 4.1 is ##
## only available as GPKG format.                                             ##
LandInG_setup$gadm$gadm_data_version <- "3.6"
################################################################################


################################################################################
## You may either use a predefined list of coordinates or let the script      ##
## determine the grid cells based on all cells that have land according to    ##
## GADM.                                                                      ##
## force_grid: set to TRUE to use predefined grid                             ##
LandInG_setup$gadm$force_grid <- FALSE
##                                                                            ##
## If force_grid == TRUE you need to provide a matrix with two columns "lon"  ##
## and "lat" giving longitude and latitude of grid cell center points. The    ##
## spatial resolution needs to fit with lpj_res.                              ##
## This could be read in, e.g., from a CSV file. Default: empty list          ##
LandInG_setup$gadm$griddata <-
  matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("lon", "lat")))
## To use the CRU land grid you may load gridlist_CRU.csv included in         ##
## LandInG:                                                                   ##
# LandInG_setup$gadm$griddata <- read.csv("gridlist_CRU.csv")
################################################################################


################################################################################
## Minimum grid cell area to be included in grid.                             ##
## Unless force_grid == TRUE, grid is derived from all cells containing land. ##
## This threshold allows to skip cells with very small land fractions. Set to ##
## 0 to preserve all cells.                                                   ##
LandInG_setup$gadm$threshold_grid <- 1000
## unit: square meter                                                         ##
################################################################################


################################################################################
## The country code input for LPJmL versions up to 5.9.3 includes regions for ##
## some large countries. Set up which countries (3-letter ISO code):          ##
LandInG_setup$gadm$include_regions <-
  c("AUS", "BRA", "CAN", "CHN", "IND", "RUS", "USA")
## Leave empty to skip region band for LPJmL versions starting with 5.9.4:    ##
# LandInG_setup$gadm$include_regions <- character(0)
##                                                                            ##
## The default for global LPJmL setups is to exclude Antarctica (ATA).        ##
## You may add more countries to exclude by adding their ISO codes.           ##
## Set to NULL if you do not want to skip any countries.                      ##
LandInG_setup$gadm$skip_countries <- c("ATA")
##                                                                            ##
## GADM version 3.6 includes the Caspian Sea (XCA) as a separate country.     ##
## If defined here, the algorithm will assign Caspian Sea only to cells fully ##
## covered by Caspian Sea. All other cells will be assigned to the second     ##
## largest country. You may add other GADM-0 units to be treated in the same  ##
## way. Use ISO code.                                                         ##
LandInG_setup$gadm$water_bodies <- c("XCA")
## Country code to be used for cells without GADM coverage (if                ##
## force_grid == TRUE). Set variable value to country name to be used, and    ##
## name attribute to ISO code. The ISO code should consist of three capital   ##
## letters and start with an "X" to denote it is not an official ISO code.    ##
LandInG_setup$gadm$gadm_no_land <- c(XNL = "No land")
################################################################################

################################################################################
## Set up further directories.                                                ##
## - directory where country-specific shapes for GADM level 0-2 intersection  ##
##   are saved                                                                ##
LandInG_setup$gadm$split_directory <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "by_country_districts_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE)
  )
)
##                                                                            ##
## - directory where shape intersections between level 0-2 shapes and grid    ##
##   cells are saved (name depends on resolution)                             ##
LandInG_setup$gadm$intersect_directory <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "grid_intersection_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_districts_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE)
  )
)
################################################################################


################################################################################
## Main output files of GADM processing                                       ##
##                                                                            ##
## Grid (list of coordinates):                                                ##
## This corresponds to griddata read above if force_grid == TRUE.             ##
##   - format: either "BIN" for LPJmL input format or "CSV".                  ##
LandInG_setup$gadm$gridformat <- "BIN"
##   - filename (comment to this line skip writing grid file)                 ##
LandInG_setup$gadm$gridname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "grid_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".", tolower(LandInG_setup$gadm$gridformat)
  )
)
##                                                                            ##
## Country code:                                                              ##
##   This is a list with one or two columns:                                  ##
##   1st column: dominant country code per cell (as vector of increasing      ##
##   integer values).                                                         ##
##   2nd column: dominant state/region code for countries included in         ##
##   include_regions, otherwise same as first column.                         ##
##   The 2nd column is ommitted if include_regions is empty.                  ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
LandInG_setup$gadm$cowformat <- "BIN"
##   - filename (comment this line to skip writing country code file)         ##
LandInG_setup$gadm$cowname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "cow_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".", tolower(LandInG_setup$gadm$cowformat)
  )
)
##   - meta table linking codes in country code file to country names and ISO ##
##     codes (comment to this line skip writing file)                         ##
LandInG_setup$gadm$cowmetaname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "cow_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_countries.csv"
  )
)
##   - meta table linking region codes in country code file to region names   ##
##     and ISO codes (comment this line to skip writing file)                 ##
## Also skipped if no include_regions are defined.                            ##
LandInG_setup$gadm$regmetaname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir, getwd()
  ),
  paste0(
    "cow_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_regions.csv"
  )
)
##   - raster of country code (format can be any format supported by          ##
##     writeRaster() (comment to skip writing country raster)                 ##
LandInG_setup$gadm$cowraster <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "cow_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_countries.nc" # file extension determines format
  )
)
##   - raster of region code (format can be any format supported by           ##
##     writeRaster() (comment this line to skip writing region raster)        ##
## Also skipped if no include_regions are defined.                            ##
LandInG_setup$gadm$regraster <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "cow_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_regions.nc" # file extension determines format
  )
)
##                                                                            ##
## Number of countries in each cell:                                          ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
LandInG_setup$gadm$ncountryformat <- "BIN"
##   - filename (comment this line to skip writing file)                      ##
LandInG_setup$gadm$ncountryname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "ncountry_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".", tolower(LandInG_setup$gadm$ncountryformat)
  )
)
##   - raster of number of countries (format can be any format supported      ##
##     by writeRaster() (comment this line to skip writing raster)            ##
##     Note: This file is required by scripts in ../landuse and ../fertilizer.##
LandInG_setup$gadm$ncountryraster <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "ncountry_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".nc" # file extension determines format
  )
)
##                                                                            ##
## Land fraction in each cell                                                 ##
##   - format: either "BIN" for LPJmL input format or "CSV"                   ##
LandInG_setup$gadm$landfracformat <- "BIN"
##   - filename (comment this line to skip writing file)                      ##
LandInG_setup$gadm$landfracname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "landfrac_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".", tolower(LandInG_setup$gadm$landfracformat)
  )
)
##   - raster of land fraction (format can be any format supported by         ##
##     writeRaster() (comment this line to skip writing raster)               ##
LandInG_setup$gadm$landfracraster <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "landfrac_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".nc" # file extension determines format
  )
)
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
LandInG_setup$gadm$gadmformat <- "BIN"
##   - filename (comment this line to skip writing file)                      ##
LandInG_setup$gadm$gadmname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_level0_1_2_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".", tolower(LandInG_setup$gadm$gadmformat)
  )
)
##   - meta table linking codes in country code file to country names and ISO ##
##     codes (comment to this line skip writing file)                         ##
LandInG_setup$gadm$gadmmetaname <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_level0_1_2_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_indices.csv"
  )
)
##   - raster with all three dominant GADM levels (format can be any format   ##
##     supported by writeRaster() with multiband support (comment this line   ##
##     to skip writing raster)                                                ##
##     Note: This file is required by scripts in ../landuse and ../fertilizer.##
LandInG_setup$gadm$gadmraster <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_level0_1_2_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
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
LandInG_setup$gadm$bintype <- 3
## - header name: Headers of LPJmL input files include a name. Header names   ##
##   are defined in /include/header.h of the LPJmL source code and do not     ##
##   usually need to be changed.                                              ##
##   Not all headers below exist in LPJmL source code.                        ##
LandInG_setup$gadm$grid_headername <- "LPJGRID"
LandInG_setup$gadm$cow_headername <- "LPJ_COW"
LandInG_setup$gadm$ncountry_headername <- "LPJNCOW" # currently not in LPJmL
LandInG_setup$gadm$landfrac_headername <- "LPJLFRC" # currently not in LPJmL
LandInG_setup$gadm$gadm_headername <- "LPJGADM"     # currently not in LPJmL
################################################################################


################################################################################
## A shapefile containing polygons for each raster grid cell is created as    ##
## part of processing. The filename depends on spatial resolution lpj_res.    ##
## By default file is saved to gadm_dir.                                      ##
LandInG_setup$gadm$gridcell_shapefile <- file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "gridcell_polygons_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    ".shp"
  )
)
################################################################################


################################################################################
## Step 3 collects information from the shape intersection and saves it to    ##
## RData files to speed up in case of repeated processing. Filenames depend   ##
## on spatial resolution lpj_res. By default file is saved to gadm_dir.       ##
LandInG_setup$gadm$cell_list_RData <-  file.path(
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  paste0(
    "celllist_gadm",
    gsub(".", "", LandInG_setup$gadm$gadm_data_version, fixed = TRUE),
    "_",
    LandInG_setup$gadm$lpj_res_string,
    ifelse(LandInG_setup$gadm$force_grid, "_predefined_grid", ""),
    "_districts.RData"
  )
)
################################################################################


################################################################################
## No further user settings below this point.                                 ##
################################################################################


################################################################################
## Check for availability of required packages. These may need to be          ##
## installed first.                                                           ##
required_packages <- c("sf", "terra", "foreach", "stringi", "units")
if (!all(required_packages %in% .packages(all.available = TRUE))) {
  stop(
    "Please install missing package(s): ",
    toString(
      sQuote(
        setdiff(required_packages, .packages(all.available = TRUE)),
        q = FALSE
      )
    )
  )
}
# Note: Starting with version 1.0 package "sf" by default uses the new package
# "s2" for spherical geometry, i.e. when doing calculations on spatial objects
# in a geographical coordinate reference system. Because of this, results differ
# from earlier versions of "sf". However, shapefiles are not necessarily created
# in a way that they work correctly with spherical geometry leading to polygon
# errors. For example, tests have shown that some polygons in GADM 4.1 cannot be
# processed with spherical geometry. Therefore, LandInG switches off use of "s2"
# in "sf" by default. This may also help with comparability to data processed
# with "sf" version < 1.0. Re-enable at your own risk.
if ("sf_use_s2" %in% getNamespaceExports("sf")) {
  sf::sf_use_s2(FALSE)
}

# Check if function st_make_valid is available in package sf, otherwise try
# lwgeom package. Package lwgeom is also required if sf_use_s2() is FALSE.
if (
  !"st_make_valid" %in% getNamespaceExports("sf") ||
    ("sf_use_s2" %in% getNamespaceExports("sf") && !sf::sf_use_s2())
) {
  if (!"lwgeom" %in% .packages(all.available = TRUE)) {
    stop("Please install missing 'lwgeom' package")
  }
}
################################################################################


################################################################################
## Create raster layer based on                                               ##
## - spatial resolution lpj_res                                               ##
## - predefined coordinate list (if force_grid == TRUE) or                    ##
## - global extent (if force_grid == FALSE)                                   ##
if (LandInG_setup$gadm$force_grid) {
  if (nrow(LandInG_setup$gadm$griddata) < 1) {
    stop(
      "You have set force_grid to TRUE but have not provided ",
      "a list of grid coordinates in gadm_setup.R"
    )
  }
  # Extent defined by minimum/maximum coordinates in griddata
  LandInG_setup$gadm$lpjgrid_extent <- terra::ext(
    min(LandInG_setup$gadm$griddata[, "lon"]) -
      LandInG_setup$gadm$lpj_res["lon"] / 2,
    xmax = max(LandInG_setup$gadm$griddata[, "lon"]) +
      LandInG_setup$gadm$lpj_res["lon"] / 2,
    ymin = min(LandInG_setup$gadm$griddata[, "lat"]) -
      LandInG_setup$gadm$lpj_res["lat"] / 2,
    ymax = max(LandInG_setup$gadm$griddata[, "lat"]) +
      LandInG_setup$gadm$lpj_res["lat"] / 2
  )
} else {
  # Global extent
  # You may reduce this if you only intend to process a subset of countries
  LandInG_setup$gadm$lpjgrid_extent <-
    terra::ext(-180, xmax = 180, ymin = -90, ymax = 90)
}
LandInG_setup$gadm$lpjgrid_raster <- terra::rast(
  LandInG_setup$gadm$lpjgrid_extent,
  res = LandInG_setup$gadm$lpj_res[c("lon", "lat")]
)
# Set projection
terra::crs(LandInG_setup$gadm$lpjgrid_raster) <-
  "+proj=longlat +datum=WGS84 +no_defs"
# Assign IDs to raster cells
if (LandInG_setup$gadm$force_grid) {
  # Coordinates from griddata are assigned consecutive numbers
  index <- terra::cellFromXY(
    LandInG_setup$gadm$lpjgrid_raster,
    LandInG_setup$gadm$griddata
  )
  LandInG_setup$gadm$lpjgrid_raster[index] <-
    seq_len(nrow(LandInG_setup$gadm$griddata))
  index <- which(is.na(LandInG_setup$gadm$lpjgrid_raster[]))
  # Coordinates not included in griddata list follow
  LandInG_setup$gadm$lpjgrid_raster[index] <- seq(
    nrow(LandInG_setup$gadm$griddata) + 1,
    terra::ncell(LandInG_setup$gadm$lpjgrid_raster)
  )
  rm(index)
} else {
  # All cells get consecutive IDs
  terra::values(LandInG_setup$gadm$lpjgrid_raster) <-
    seq_len(terra::ncell(LandInG_setup$gadm$lpjgrid_raster))
}
################################################################################


################################################################################
## Prevent recoding of attribute tables when writing shapefiles with ESRI     ##
## driver. This may avoid problems with shapefiles using UTF8 encoding when R ##
## is not running on a UTF8 locale.                                           ##
terra::setGDALconfig("SHAPE_ENCODING", "")
################################################################################


################################################################################
## Make sure that character strings are not converted to factors              ##
options("stringsAsFactors" = FALSE)
################################################################################


################################################################################
## Plausibility checks                                                        ##
if (length(LandInG_setup$gadm$gadm_no_land) != 1 ||
    any(nchar(names(LandInG_setup$gadm$gadm_no_land)) != 3)
) {
  stop(
    "You have defined ", length(LandInG_setup$gadm$gadm_no_land),
    ifelse(length(LandInG_setup$gadm$gadm_no_land) == 1, " entry", " entries"),
    " for gadm_no_land of which ",
    length(which(nchar(names(LandInG_setup$gadm$gadm_no_land)) != 3)),
    ifelse(
      length(which(nchar(names(LandInG_setup$gadm$gadm_no_land)) != 3)) == 1,
      " has",
      " have"
    ),
    " an invalid ISO code.\n",
    "You need to define one country name with a 3-letter ISO code for ",
    "gadm_no_land in gadm_setup.R"
  )
}
if (LandInG_setup$gadm$threshold_grid < 0) {
  stop(
    "Negative threshold_grid: ",
    LandInG_setup$gadm$threshold_grid, " m2\n",
    "Threshold must be positive or zero."
  )
}
if (LandInG_setup$gadm$bintype < 3 &&
    LandInG_setup$gadm$lpj_res["lon"] != LandInG_setup$gadm$lpj_res["lat"]
) {
  stop(
    "You have selected bintype ", LandInG_setup$gadm$bintype,
    " and different values for longitude and latitude resolution.\n",
    "Use bintype 3 if longitude and latitude resolutions differ."
  )
}
if (length(LandInG_setup$gadm$include_regions) == 0 &&
    !is.null(LandInG_setup$gadm$regmetaname)
) {
  warning(
    "Unsetting regmetaname ", LandInG_setup$gadm$regmetaname,
    " in gadm_setup.R because include_regions is empty",
    call. = FALSE, immediate. = TRUE
  )
  LandInG_setup$gadm$regmetaname <- NULL
}
if (length(LandInG_setup$gadm$include_regions) == 0 &&
    !is.null(LandInG_setup$gadm$regraster)
) {
  warning(
    "Unsetting regraster ", LandInG_setup$gadm$regraster,
    " in gadm_setup.R because include_regions is empty",
    call. = FALSE, immediate. = TRUE
  )
  LandInG_setup$gadm$regraster <- NULL
}
################################################################################
