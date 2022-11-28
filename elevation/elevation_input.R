################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script creates an LPJmL input file containing median elevation above  ##
## above sea level.                                                           ##
## Technical note: elevation is implemented as an integer in LPJmL so values  ##
## will be rounded to integer values before saving to file.                   ##
################################################################################

# clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Set up base directory for data processing                                  ##
## This is where scripts and downloaded elevation data are located.           ##
################################################################################
elevationdir <- ""
if (nchar(elevationdir) > 0) {
  # set working directory to elevationdir
  setwd(elevationdir)
}

################################################################################
## LPJmL grid file for which elevation file is to be produced                 ##
##
## Can be either LPJmL input format "BIN" or a CSV file (gridformat <- "CSV") ##
## containing a matrix with two columns "lon" and "lat" giving longitude and  ##
## latitude of grid cell. In case of "CSV" you must also provide spatial      ##
## resolution lpj_res.
gridformat <- "BIN"
gridname <- "ADD_GRIDFILE_NAME_HERE"
lpj_res <- c("lon" = 1 / 2, "lat" = 1 / 2)
# lpj_res determined automatically if gridformat == "BIN"
################################################################################

################################################################################
## Source file of gridded elevation data. This must be a raster file in a     ##
## format supported by the raster package in R.                               ##
## Spatial resolution must be the same as the resolution of LPJmL grid. If    ##
## using ETOPO1 elevation data use resample_etopo.sh to resample source data  ##
## to target resolution.                                                      ##
elevation_source_name <- "ADD_ELEVATION_SOURCE_FILE_HERE"
################################################################################

################################################################################
## File format for elevation file created by this script. Either LPJmL input  ##
## format "BIN" or "CSV".                                                     ##
elevationformat <- "BIN"
## LPJmL format version: must be 2 or 3. Only bintype 3 allows for different  ##
## longitude and latitude resolutions.                                        ##
bintype <- 2
## LPJmL header name: Headers of LPJmL input files include a name. Header     ##
## names are defined in /include/header.h of the LPJmL source code and do not ##
## usually need to be changed.                                                ##
elevation_headername <- "LPJELEV"                                             ##
################################################################################

################################################################################
## Helper functions for LPJmL input format                                    ##
## The script lpjml_format_helper_functions.R is saved in the parent          ##
## directory by default.                                                      ##
################################################################################
if (file.exists("../lpjml_format_helper_functions.R")) {
  source("../lpjml_format_helper_functions.R")
} else {
  stop("Please update path to script with LPJmL input format helper function")
}

################################################################################
## Load required packages. These may need to be installed first.              ##
################################################################################
library(raster)

################################################################################
## Read LPJmL grid.                                                           ##
if (gridformat == "BIN") {
  # Read file header
  # Function read_header() defined in lpjml_format_helper_functions.R
  gridheader <- read_header(gridname)
  # Open file for reading
  gridfile <- file(gridname, "rb")
  # Skip over file header
  # Function get_headersize() defined in lpjml_format_helper_functions.R
  seek(gridfile, get_headersize(gridheader))
  # Read data
  # Function get_datatype() defined in lpjml_format_helper_functions.R
  griddata <- matrix(
    readBin(
      gridfile,
      what = get_datatype(gridheader)$type,
      size = get_datatype(gridheader)$size,
      n = gridheader$header["nbands"] * gridheader$header["ncell"],
      endian = gridheader$endian
    ) * gridheader$header["scalar"],
    ncol = gridheader$header["nbands"],
    byrow = TRUE,
    dimnames = list(NULL, c("lon", "lat"))
  )
  close(gridfile)
  # Derive resolution from file header
  lpj_res <- gridheader$header[c("cellsize_lon", "cellsize_lat")]
  names(lpj_res) <- colnames(griddata)
} else if (gridformat == "CSV") {
  griddata <- read.csv(gridname)
  if (ncol(griddata) != 2 || any(!colnames(griddata) %in% c("lon", "lat"))) {
    stop("gridname must contain a CSV table with two colums 'lon' and 'lat'")
  }
} else {
  stop(paste("Invalid gridformat", sQuote(gridformat)))
}
cat(
  "LPJmL grid file: ", gridname, " (", nrow(griddata),
  " cells, resolution: ",
  "[", toString(lpj_res[c("lon", "lat")]), "])\n",
  sep = ""
)
# Derive grid extent
gridextent <- extent(
  min(griddata[, "lon"]) - lpj_res["lon"] / 2,
  max(griddata[, "lon"]) + lpj_res["lon"] / 2,
  min(griddata[, "lat"]) - lpj_res["lat"] / 2,
  max(griddata[, "lat"]) + lpj_res["lat"] / 2
)
gridraster <- raster(gridextent, res = lpj_res[c("lon", "lat")])
################################################################################

################################################################################
## Determine elevation output file name based on grid resolution.             ##
tmp_res <- unique(
  ifelse(lpj_res[c("lon", "lat")] < 1/60, 3600, 60) * lpj_res[c("lon", "lat")]
)
lpj_res_string <- paste(
  round(tmp_res),
  unique(ifelse(lpj_res[c("lon", "lat")] < 1/60, "arcsec", "arcmin")),
  sep = "", collapse = "_by_"
)
rm(tmp_res)
elevationname <- file.path(
  getwd(),
  paste0(
    "elevation_",
    ifelse(grepl("gadm", basename(gridname)), "gadm_", ""),
    lpj_res_string,
    ".",
    tolower(elevationformat)
  )
)
if (file.exists(elevationname)) {
  stop(
    paste(
      "Elevation file", elevationname,
      "exists already. Delete to create again."
    )
  )
}
################################################################################

################################################################################
## Load elevation source raster.                                              ##
elevation_raster <- raster(elevation_source_name)
cat(
  "Elevation source file: ", elevation_source_name,
  " (resolution: [", toString(res(elevation_raster)), "])\n",
  sep = ""
)
# Check that resolution is valid.
elevation_to_lpj <- round(lpj_res[c("lon", "lat")] / res(elevation_raster), 4)
if (any(elevation_to_lpj != 1)) {
  stop(
    paste0(
      "LPJmL resolution [", toString(lpj_res[c("lon", "lat")]), "] ",
      "does not match elevation raster [",  toString(res(elevation_raster)), "]"
    )
  )
}
# Check that elevation raster and LPJml grid are aligned correctly
diff_x <- abs(xmin(elevation_raster) - xmin(gridextent)) / xres(elevation_raster)
diff_y <- abs(ymin(elevation_raster) - ymin(gridextent)) / yres(elevation_raster)
if ((diff_x %% 1) > 1e-2 || (diff_y %% 1) > 1e-2) {
  stop(
    paste(
      "Cell borders are misaligned between LPJmL grid", gridname,
      "and elevation source raster", elevation_source_name
    )
  )
}
################################################################################


################################################################################
## Extract cells included in grid from elevation_raster.                      ##
# First check that all grid cells are covered by source data.
raster_to_grid <- cellFromXY(elevation_raster, griddata)
if (anyNA(raster_to_grid)) {
  stop(
    paste(
      length(which(is.na(raster_to_grid))),
      "LPJmL cells are outside of range of",
      elevation_source_name
    )
  )
}
# Now extract data only for grid cells.
elevation_data <- elevation_raster[raster_to_grid]
if (anyNA(elevation_data)) {
  cat(
    "Setting", length(which(is.na(elevation_data))),
    "NA values in elevation_data to zero\n"
  )
  elevation_data[which(is.na(elevation_data))] <- 0
}
# Write data to file.
# Note: Internally, LPJmL uses integer to represent elevation so saving input
# files with floating point precision does not make sense.
if (elevationformat == "BIN") {
  # Create file header
  if (bintype < 3 && lpj_res["lon"] != lpj_res["lat"]) {
    stop("Only bintype 3 allows for longitude and latitude resolution to differ.")
  }
  elevation_header <- create_header(
    name = elevation_headername,
    version = bintype,
    order = 1,
    nyear = 1,
    ncell = length(elevation_data),
    nbands = 1,
    cellsize_lon = lpj_res["lon"],
    scalar = 1,
    cellsize_lat = lpj_res["lat"],
    datatype = 1
  )
  # Write header to file
  write_header(elevationname, elevation_header)
  # Add data
  zz <- file(elevationname, "ab")
  if (typeof(get_datatype(elevation_header)$type) == "integer") {
    writeBin(
      as.integer(round(elevation_data / elevation_header$header["scalar"])), zz,
      size = get_datatype(elevation_header)$size,
      endian = elevation_header$endian
    )
  } else if (typeof(get_datatype(elevation_header)$type) == "double") {
    writeBin(
      # Remove round() here if floating point values are supported
      as.double(round(elevation_data / elevation_header$header["scalar"])), zz,
      size = get_datatype(elevation_header)$size,
      endian = elevation_header$endian
    )
  } else {
    stop(
      paste(
        "Invalid datatype", elevation_header$header["datatype"],
        "in elevation_header"
      )
    )
  }
  close(zz)
  cat("File", elevationname, "created\n")
} else if (elevationformat == "CSV") {
  write.csv(
    matrix(round(elevation_data),
    ncol = 1,
    dimnames = list(NULL, "elevation")),
    file = elevationname,
    row.names = FALSE
  )
} else {
  stop(paste("Invalid elevationformat", sQuote(elevationformat)))
}
################################################################################
