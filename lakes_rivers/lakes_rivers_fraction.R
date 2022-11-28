################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script derives cell fractions covered by different types of water     ##
## bodies from the Global Lakes and Wetlands Database (GLWD).                 ##
## By default, LPJmL uses the combined fraction of lakes and rivers as input. ##
################################################################################
# Clean up memory
rm(list = ls(all = TRUE))
################################################################################
## Basic setup:                                                               ##
## Working directory:                                                         ##
## This is where outputs from this script will be saved.                      ##
glwd_dir <- ""
if (nchar(glwd_dir) > 0) {
  setwd(glwd_dir)
}
## Grid file:                                                                 ##
## Must be in LPJmL input format.                                             ##
gridname <- "ENTER_GRID_FILE_HERE"
##                                                                            ##
## GLWD Level 3:                                                              ##
## Grid in ArcView/ArcInfo coverage format at 30 x 30 second resolution       ##
glwd3_name <- "glwd_3/hdr.adf"
## GLWD Level 3 classes to extract (see GLWD Documentation for full list)     ##
glwd3_classes <- c(lakes = 1, rivers = 3)
##                                                                            ##
## Version string (optional)
## Added to names of created files (resolution string added automatically)    ##
version_string <- ""
## Output format: Either "BIN" (native LPJmL input format with header), "RAW" ##
## (LPJmL input format without header) or any raster format supported by the  ##
## raster package, such as "NC" or "ASC". Note: Formats "RAW" and "BIN" with  ##
## with version < 3 will round fractions to full percent.                     ##
output_format <- "BIN"
## Version of "BIN" format used. Only version 3 allows for longitude and      ##
## latitude resolution to differ. Also, version 1 and 2 will round water body ##
## fractions to full percent.                                                 ##
bintype <- 3
## Header name of "BIN" format used. Must match header name defined in LPJmL  ##
## code.                                                                      ##
headername <- "LPJLAKE"
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
## Load required R packages. These may need to be installed first.            ##
library(raster)
library(rgdal)
################################################################################

################################################################################
## Load GLWD3 data.                                                           ##
cat("Load GLWD Level 3 data from", sQuote(glwd3_name), "\n")
glwd3_raster <- raster(glwd3_name)
# longlat projection string
ps <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
if (all(
  abs(xmin(glwd3_raster) + 180) / xres(glwd3_raster) < 0.01,
  abs(xmax(glwd3_raster) - 180) / xres(glwd3_raster) < 0.01,
  abs(ymin(glwd3_raster) + 90) / yres(glwd3_raster) < 0.01,
  abs(ymax(glwd3_raster) - 90) / yres(glwd3_raster) < 0.01
)) {
  # Raster is roughly global, set to exact global extent
  extent(glwd3_raster) <- extent(-180, 180, -90, 90)
  # Set longlat projection
  proj4string(glwd3_raster) <- ps
}
################################################################################

################################################################################
## Load grid file.                                                            ##
## Functions read_header(), get_headersize(), get_datatype() defined in       ##
## lpjml_format_helper_functions.R                                            ##
cat("Load grid from", sQuote(gridname), "\n")
gridheader <- read_header(gridname)
gridfile <- file(gridname, "rb")
seek(gridfile, get_headersize(gridheader))
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
# Create grid raster from coordinates.
gridextent <- extent(
  min(griddata[, "lon"]) - gridheader[["header"]]["cellsize_lon"] / 2,
  max(griddata[, "lon"]) + gridheader[["header"]]["cellsize_lon"] / 2,
  min(griddata[, "lat"]) - gridheader[["header"]]["cellsize_lat"] / 2,
  max(griddata[, "lat"]) + gridheader[["header"]]["cellsize_lat"] / 2
)
gridraster <- raster(
  gridextent,
  resolution = gridheader$header[c("cellsize_lon", "cellsize_lat")]
)
# Calculate grid area. Function cellarea() defined in
# lpjml_format_helper_functions.R.
gridarea <- cellarea(
  griddata[, "lat"],
  gridheader$header["cellsize_lon"],
  gridheader$header["cellsize_lat"]
)
# Set projection.
proj4string(gridraster) <- ps
# Determine resolution string to be used in files created by this script.
tmp_res <- unique(
  ifelse(
    gridheader$header[c("cellsize_lon", "cellsize_lat")] < 1 / 60,
    3600,
    60
  ) * gridheader$header[c("cellsize_lon", "cellsize_lat")]
)
lpj_res_string <- paste(
  round(tmp_res),
  unique(
    ifelse(
      gridheader$header[c("cellsize_lon", "cellsize_lat")] < 1 / 60,
      "arcsec",
      "arcmin"
    )
  ),
  sep = "", collapse = "_by_"
)
rm(tmp_res)
# Check if selected output format is compatible with grid file.
if (output_format == "RAW" || (output_format == "BIN" && bintype < 3)) {
  if (gridheader$header["cellsize_lon"] != gridheader$header["cellsize_lat"]) {
    ## Only bintype 3 supports different lon/latr resolution
    stop(
      "Different longitude and latitude resolutions are not supported by ",
      "output_format ", sQuote(output_format),
      ifelse(output_format == "BIN", paste(" with bintype", bintype), "")
    )
  }
}
# Check if files to be created by this script exist already.
if (length(grep("lake|river", names(glwd3_classes), ignore.case = TRUE)) == 2) {
  outputname <- paste0(
    "glwd_lakes_and_rivers_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(output_format)
  )
  if (file.exists(outputname)) {
    if (output_format == "RAW") {
      if (file.info(outputname)$size != gridheader$header["ncell"]) {
        stop(
          "Output file ", sQuote(outputname),
          " exists already but its number of cells does not match grid file ",
          sQuote(gridname), ".\n",
          "Delete or rename existing file."
        )
      } else {
        stop(
          "Output file ", sQuote(outputname), " exists already.\n",
          "Delete or rename existing file to force this script to run again."
        )
      }
    } else if (output_format == "BIN") {
      outputheader <- read_header(outputname)
      if (outputheader$header["ncell"] != gridheader$header["ncell"] ||
        any(outputheader$header[c("cellsize_lon", "cellsize_lat")] !=
        gridheader$header[c("cellsize_lon", "cellsize_lat")])
      ) {
        stop(
          "Output file ", sQuote(outputname),
          " exists already but does not match grid file ", sQuote(gridname),
          ".\n Delete or rename existing file."
        )
      } else {
        stop(
          "Output file ", sQuote(outputname), " exists already.\n",
          "Delete or rename existing file to force this script to run again."
        )
      }
    } else {
      stop(
        "Output file ", sQuote(outputname), " exists already.\n",
        "Delete or rename existing file to force this script to run again."
      )
    }
  }
}
for (type in names(glwd3_classes)) {
  outputname <- paste0(
    "glwd_",
    type,
    "_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(output_format)
  )
  if (file.exists(outputname)) {
    if (output_format == "RAW") {
      if (file.info(outputname)$size != gridheader$header["ncell"]) {
        stop(
          "Output file ", sQuote(outputname), " exists already ",
          "but its number of cells does not match grid file ",
          sQuote(gridname), ".\n",
          "Delete or rename existing file."
        )
      } else {
        stop(
          "Output file ", sQuote(outputname), " exists already.\n",
          "Delete or rename existing file to force this script to run again."
        )
      }
    } else if (output_format == "BIN") {
      outputheader <- read_header(outputname)
      if (outputheader$header["ncell"] != gridheader$header["ncell"] ||
        any(outputheader$header[c("cellsize_lon", "cellsize_lat")] !=
        gridheader$header[c("cellsize_lon", "cellsize_lat")])
      ) {
        stop(
          "Output file ", sQuote(outputname), " exists already ",
          "but does not match grid file ", sQuote(gridname), ".\n",
          "Delete or rename existing file."
        )
      } else {
        stop(
          "Output file ", sQuote(outputname), " exists already.\n",
          "Delete or rename existing file to force this script to run again."
        )
      }
    } else {
      stop(
        "Output file ", sQuote(outputname), " exists already.\n",
        "Delete or rename existing file to force this script to run again."
      )
    }
  }
}
  
################################################################################

################################################################################
## Check compatibility between gridraster and glwd3_raster.                   ##
cat("Check compatibility\n")
if (any(res(gridraster) / res(glwd3_raster) < 0.9999)) {
  ## source is too coarse
  stop(
    "Source resolution ", paste(res(glwd3_raster), collapse = " by "),
    " in ", sQuote(glwd3_name),
    " is coarser than target resolution ",
    paste(
      gridheader$header[c("cellsize_lon", "cellsize_lat")],
      collapse = " by "
    ),
    " of grid file ", sQuote(gridname)
  )
}
if (any((res(gridraster) / res(glwd3_raster)) %% 1 > 1e-4) &&
  any((res(gridraster) / res(glwd3_raster)) %% 1 < (1 - 1e-4))
) {
  # Target is not integer multiple of source resolution.
  stop(
    "Target resolution ",
    paste(
      gridheader$header[c("cellsize_lon", "cellsize_lat")],
      collapse = " by "
    ),
    " of grid file ", sQuote(gridname),
    " is not an integer multiple of source resolution ",
    paste(res(glwd3_raster), collapse = " by "),
    " in ", sQuote(glwd3_name)
  )
}
# Check spatial extent. Allow for small tolerance.
if (xmin(gridraster) - xmin(glwd3_raster) < (-xres(glwd3_raster) / 100) ||
  xmax(gridraster) - xmax(glwd3_raster) > xres(glwd3_raster) / 100 ||
  ymin(gridraster) - ymin(glwd3_raster) < (-yres(glwd3_raster) / 100) ||
  ymax(gridraster) - ymax(glwd3_raster) > yres(glwd3_raster) / 100
) {
  stop(
    "Spatial extent of source data ", toString(extent(glwd3_raster)),
    " does not cover the full spatial extent ",
    toString(extent(gridraster)),
    " of grid file ", sQuote(gridname)
  )
}
# Check cell boundary alignment.
if ((
    (abs(xmin(gridraster) - xmin(glwd3_raster)) / xres(glwd3_raster)) %% 1 >
    1e-3 &&
    (abs(xmin(gridraster) - xmin(glwd3_raster)) / xres(glwd3_raster)) %% 1 <
    0.999
  ) || (
    (abs(ymin(gridraster) - ymin(glwd3_raster)) / yres(glwd3_raster)) %% 1 >
    1e-3 &&
    (abs(ymin(gridraster) - ymin(glwd3_raster)) / yres(glwd3_raster)) %% 1 <
    0.999
  )
) {
  stop(
    "Spatial extent of source data ", toString(extent(glwd3_raster)),
    " and spatial extent of grid file ", sQuote(gridname), " ",
    toString(extent(gridraster)),
    " are mis-aligned."
  )
} else {
  # Align extent to take care of numerical inaccuracies.
  extent(gridraster) <- alignExtent(extent(gridraster), glwd3_raster)
}
aggregation_factor <- round(res(gridraster) / res(glwd3_raster), 4)
if (any(aggregation_factor %% 1 != 0)) {
  stop(
    "Target resolution of grid file ", sQuote(gridname), " ",
    paste(
      gridheader$header[c("cellsize_lon", "cellsize_lat")],
      collapse = " by "
    ),
    " is not an integer multiple of source resolution ",
    paste(res(glwd3_raster), collapse = " by "),
    " in ", sQuote(glwd3_name)
  )
}
# Crop glwd3_raster if it has larger extent than gridraster.
glwd3_raster <- crop(glwd3_raster, gridraster)
################################################################################


################################################################################
## Derive matching raster of cell areas.                                      ##
glwd_area <- raster(extent(glwd3_raster), resolution = res(glwd3_raster))
values(glwd_area) <- rep(
  cellarea(yFromRow(glwd3_raster), xres(glwd3_raster), yres(glwd3_raster)),
  each = ncol(glwd3_raster)
)
################################################################################

################################################################################
## Aggregate areas covered by selected glwd3_classes in each cell at target   ##
## resolution.                                                                ##
cat(
  "Aggregating", length(glwd3_classes),
  "GLWD", ifelse(length(glwd3_classes) > 1, "types", "type"),
  toString(sQuote(names(glwd3_classes))),
  "from source resolution",
  paste(format(res(glwd3_raster), digits = 4), collapse = " by "), "degree",
  "to target resolution",
  paste(format(res(gridraster), digits = 4), collapse = " by "),
  "degree.\n"
)
for (type in names(glwd3_classes)) {
  cat("Processing", type, "\n")
  # Get area of cells that have the respective type
  typearea <- mask(
    mask(glwd_area, glwd3_raster),
    glwd3_raster,
    maskvalue = glwd3_classes[type],
    inverse = TRUE
  )
  # Global sum before aggregation
  typesum <- cellStats(typearea, sum)
  # Aggregate to target resolution
  typearea <- aggregate(typearea, fact = aggregation_factor, fun = sum)
  # Check global sum after aggregation
  if (abs(cellStats(typearea, sum) / typesum - 1) > 1e-8) {
    stop(
      "Global sum after aggregation differs from global sum before aggregation."
    )
  }
  assign(paste0("lpj_", type, "_area"), typearea)
  rm(typearea)
}
# Convert absolute areas into cell fractions and extract values for all cells
# in grid.
grid_index <- cellFromXY(gridraster, griddata)
for (type in names(glwd3_classes)) {
  lpj_type_frac <- get(paste0("lpj_", type, "_area"))[grid_index] / gridarea
  lpj_type_frac[which(is.na(lpj_type_frac))] <- 0 # Cells without water bodies
  assign(paste0("lpj_", type, "_frac"), lpj_type_frac)
  rm(lpj_type_frac)
}
# Aggregate lake and river fractions if both have been extracted.
if (length(grep("lake|river", names(glwd3_classes), ignore.case = TRUE)) == 2) {
  lpj_lakes_and_river_frac <- get(
    paste0(
      "lpj_",
      grep("lake", names(glwd3_classes), ignore.case = TRUE, value = TRUE),
      "_frac"
    )
  ) + get(
    paste0(
      "lpj_",
      grep("river", names(glwd3_classes), ignore.case = TRUE, value = TRUE),
      "_frac"
    )
  )
  # Global sum
  if (output_format == "RAW" || (output_format == "BIN" && bintype < 3)) {
    global_lakes_and_rivers <- sum(lpj_lakes_and_river_frac * gridarea)
    global_lakes_and_rivers_round <- sum(
      round(lpj_lakes_and_river_frac, 2) * gridarea
    )
    cat(
      "Rounding lakes and rivers fraction to integer percent values ",
      ifelse(
        global_lakes_and_rivers_round > global_lakes_and_rivers,
        "adds ",
        "removes "
      ),
      abs(global_lakes_and_rivers_round - global_lakes_and_rivers) * 1e-6,
      " km2 or ",
      abs(global_lakes_and_rivers_round / global_lakes_and_rivers - 1) * 100,
      "% ",
      ifelse(
        global_lakes_and_rivers_round > global_lakes_and_rivers,
        "to ",
        "from "
      ),
      "global water bodies\n",
      sep = ""
    )
  }
  outputname <- paste0(
    "glwd_lakes_and_rivers_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(output_format)
  )
  cat("Saving lake and river fraction to", sQuote(outputname), "\n")
  if (output_format == "RAW" || output_format == "BIN") {
    if (output_format == "RAW" || bintype < 3) {
      datatype <- 0
      scalar <- 0.01
    } else {
      datatype <- 3
      scalar <- 1
    }
    outputheader <- create_header(
      name = headername,
      version = ifelse(output_format == "RAW", 0, bintype),
      nyear = 1,
      ncell = gridheader$header["ncell"],
      nbands = 1,
      cellsize_lon = gridheader$header["cellsize_lon"],
      scalar = scalar,
      cellsize_lat = gridheader$header["cellsize_lon"],
      datatype = datatype
    )
    if (output_format == "BIN") {
      write_header(outputname, outputheader)
      outputfile <- file(outputname, "ab")
    } else {
      outputfile <- file(outputname, "wb")
    }
    if (typeof(get_datatype(outputheader)$type) == "raw") {
      writeBin(as.raw(round(lpj_lakes_and_river_frac / scalar)), outputfile)
    } else if (typeof(get_datatype(outputheader)$type) == "integer") {
      writeBin(
        as.integer(round(lpj_lakes_and_river_frac / scalar)),
        outputfile,
        size = get_datatype(outputheader)$size,
        endian = outputheader$endian
      )
    } else if (typeof(get_datatype(outputheader)$type) == "double") {
      writeBin(
        as.double(lpj_lakes_and_river_frac / scalar),
        outputfile,
        size = get_datatype(outputheader)$size,
        endian = outputheader$endian
      )
    } else {
      stop(
        "Invalid get_datatype ", outputheader$header["datatype"],
        " for file ", sQuote(outputname)
      )
    }
    close(outputfile)
  } else {
    # Assume output_format is a raster format supported by raster package.
    outputraster <- raster(gridraster)
    outputraster[cellFromXY(outputraster, griddata)] <- lpj_lakes_and_river_frac
    # The following will fail if output_format is not supported by the raster
    # package.
    writeRaster(
      outputraster,
      filename = outputname,
      varname = "lakes_and_rivers_frac",
      longname = "cell fraction covered by lakes and rivers"
    )
  }
}
# Save individual fractions covered by each GLWD3_class.
for (type in names(glwd3_classes)) {
  lpj_type_frac <- get(paste0("lpj_", type, "_frac"))
  if (output_format == "RAW" || (output_format == "BIN" && bintype < 3)) {
    global_sum <- sum(lpj_type_frac * gridarea)
    global_sum_percent <- sum(round(lpj_type_frac, 2) * gridarea)
    cat(
      "Rounding ", type, " fraction to integer percent values ",
      ifelse(global_sum_percent > global_sum, "adds ", "removes "),
      abs(global_sum_percent - global_sum) * 1e-6, " km2 or ",
      abs(global_sum_percent / global_sum - 1) * 100, "% ",
      ifelse(global_sum_percent > global_sum, "to ", "from "),
      "global water bodies\n",
      sep = ""
    )
  }
  outputname <- paste0(
    "glwd_", type, "_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(output_format)
  )
  cat("Saving", type, "fraction to", sQuote(outputname), "\n")
  if (output_format == "RAW" || output_format == "BIN") {
    if (output_format == "RAW" || bintype < 3) {
      datatype <- 0
      scalar <- 0.01
    } else {
      datatype <- 3
      scalar <- 1
    }
    outputheader <- create_header(
      name = headername,
      version = ifelse(output_format == "RAW", 0, bintype),
      nyear = 1,
      ncell = gridheader$header["ncell"],
      nbands = 1,
      cellsize_lon = gridheader$header["cellsize_lon"],
      scalar = scalar,
      cellsize_lat = gridheader$header["cellsize_lon"],
      datatype = datatype
    )
    if (output_format == "BIN") {
      write_header(outputname, outputheader)
      outputfile <- file(outputname, "ab")
    } else {
      outputfile <- file(outputname, "wb")
    }
    if (typeof(get_datatype(outputheader)$type) == "raw") {
      writeBin(as.raw(round(lpj_type_frac / scalar)), outputfile)
    } else if (typeof(get_datatype(outputheader)$type) == "integer") {
      writeBin(
        as.integer(round(lpj_type_frac / scalar)),
        outputfile,
        size = get_datatype(outputheader)$size,
        endian = outputheader$endian
      )
    } else if (typeof(get_datatype(outputheader)$type) == "double") {
      writeBin(
        as.double(lpj_type_frac / scalar),
        outputfile,
        size = get_datatype(outputheader)$size,
        endian = outputheader$endian
      )
    } else {
      stop(
        "Invalid get_datatype ", outputheader$header["datatype"],
        " for file ", sQuote(outputname)
      )
    }
    close(outputfile)
  } else {
    # Assume output_format is a raster format supported by raster package.
    outputraster <- raster(gridraster)
    outputraster[cellFromXY(outputraster, griddata)] <- lpj_type_frac
    # The following will fail if output_format is not supported by the raster
    # package.
    writeRaster(
      outputraster,
      filename = outputname,
      varname = paste0(type, "_frac"),
      longname = paste("cell fraction covered by", type)
    )
  }
}
################################################################################
