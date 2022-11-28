################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Script to read HWSD data and aggregate to target resolution.               ##
## This script selects in each grid cell of the target resolution the texture ##
## class that has the largest area share in the underlying source grid cells. ##
## The assigned pH value is taken from the soil profile with the largest area ##
## belonging to the selected texture class.                                   ##
## To speed up processing at fine resolutions the script allows               ##
## parallelization. The parallelization mechanism is implemented through      ##
## foreach which has backends for several parallelization mechanisms. This    ##
## script has options for the doMPI backend (using MPI) and the doParallel    ##
## backend. Further backends are possible. The example below may not work on  ##
## your system. Try to adjust parameters or set "cluster <- FALSE" to switch  ##
## off parallelization and run in sequential mode.                            ##
################################################################################
# Clean up memory before starting
rm(list = ls(all = TRUE))
################################################################################
## Basic set up:                                                              ##
##                                                                            ##
## Try parallelization                                                        ##
cluster <- FALSE
## Directory where outputs are to be saved                                    ##
soil_dir <- ""
##                                                                            ##
## HWSD raster soil map file (default: "hwsd.bil")                            ##
hwsd_raster <- "hwsd.bil"
##                                                                            ##
## HWSD main soil attribute database (default: "HWSD_DATA.csv")               ##
## Note: This needs to be extracted from the original MS Access soil          ##
## attribute database provided by HWSD. See extract_attribute_table.sh        ##
hwsd_attribute_file <- "HWSD_DATA.csv"
##                                                                            ##
## HWSD soil texture class definitions.                                       ##
## These are usually found in a table named "D_USDA_TEX_CLASS" in the soil    ##
## attribute database provided by HWSD. Use extract_attribute_table.sh or any ##
## other tool to extract the table into a separate CSV file (default if using ##
## extract_attribute_table.sh: "D_USDA_TEX_CLASS.csv")                        ##
hwsd_usda_class_file <- "D_USDA_TEX_CLASS.csv"
##                                                                            ##
## LPJml grid file. This will be used to determine the target resolution and  ##
## for which cells to extract data from HWSD. Must be in LPJmL input format.  ##
gridname <- "ENTER_PATH_TO_GRIDFILE"
##                                                                            ##
## Version string for files created by this script. This is optional but can  ##
## be used to distinguish different versions. The file names will always      ##
## contain a resolution string that is generated automatically.               ##
version_string <- ""
##                                                                            ##
## How to treat rock and ice: If set to FALSE, rock and ice will only be      ##
## assigned to cells where no other soil types are present. If set to TRUE,   ##
## rock and ice will be treated like any other soil and assigned if they have ##
## the largest area share in a cell. (Default: TRUE)                          ##
rock_ice_shared <- TRUE
##                                                                            ##
## Gap filling settings:                                                      ##
## The script can gap-fill cells in the target resolution if no source data   ##
## is available in HWSD.
##                                                                            ##
## Should the script accept missing values in soil texture and soil pH data?  ##
## If set to TRUE, script assigns soil code 0 to missing cells which makes    ##
## LPJmL skip them during simulations. If set to FALSE, any missing cells     ##
## will cause an error. (Default: FALSE)                                      ##
allow_skip <- FALSE
## Maximum search radius (in degree) for gap-filling cells with missing soil  ##
## information. Search starts with directly adjacent cells and expands        ##
## outward until any cell with soil information is found or maximum search    ##
## radius is reached. Set to 0 to do no gap-filling at all. (Default: 100)    ##
max_search <- 100
## Inverse distance weighting is used when determining dominant soil type in  ##
## search window. idw_power_par controls the influence of distance on weights.##
## Set idw_power_par to 0 to use no inverse distance weighting. (Default: 1)  ##
idw_power_par <- 1
## Quick search: When searching for available soil data for gap-filling use   ##
## aggregated soil information assigned to cells in gridname. Set this  ##
## to FALSE if you only have a partial grid and want to make sure that all    ##
## HWSD source data is checked when searching for data for gap-filling.       ##
quicksearch <- TRUE
################################################################################


################################################################################
## Advanced set up:                                                           ##
##                                                                            ##
## Settings for the files created by this script:                             ##
##                                                                            ##
## Soil type file that is created:                                            ##
##                                                                            ##
## Format: either "BIN" (LPJmL input format with file header), "RAW" (LPJmL   ##
## input format without file header), or "CSV" (CSV table, cannot be used     ##
## LPJmL). Optionally: Provide any format supported by the raster package to  ##
## create a raster map, e.g. "ASC" for ascii grid or "NC" for NetCDF.         ##
lpjml_soil_format <- "BIN"
##                                                                            ##
## Header version to use in case that format is "BIN". Only version 3 allows  ##
## for longitude and latitude resolutions to differ.                          ##
lpjml_soil_version <- 3
##                                                                            ##
## Header name to use in case that format is "BIN". This is defined in the    ##
## LPJmL source code in /include/header.h
lpjml_soil_headername <- "LPJSOIL"
##                                                                            ##
## Soil pH file that is created:                                              ##
##                                                                            ##
## Format: either "BIN" or "CSV" (CSV cannot be used with LPJmL).             ##
## Optionally: Provide any format supported by the raster package to create a ##
## raster map, e.g. "ASC" for ascii grid or "NC" for NetCDF.                  ##
## Comment out "lpjml_soilph_format" to skip creating a soil pH file, i.e. if ##
## using LPJmL version 4 or below.                                            ##
lpjml_soilph_format <- "BIN"
##                                                                            ##
## Header version to use. Must be 2 or 3. Only version 3 allows for longitude ##
## and latitude resolutions to differ.                                        ##
lpjml_soilph_version <- 3
##                                                                            ##
## Header name. This is defined in the LPJmL source code in /include/header.h ##
lpjml_soilph_headername <- "LPJ_SPH"
##                                                                            ##
## LPJmL soil types                                                           ##
##                                                                            ##
## Depending on the model version, these are usually defined in the LPJmL     ##
## source in /par/soil.par or /par/soil.js. Make sure to use the same types   ##
## in the correct order.                                                      ##
## "lpjml_soiltypes" corresponds to the name column, "lpjml_soilcodes" to the ##
## ID column in the LPJmL soil parameter file.                                ##
## These should normally correspond to the 12 official USDA texture classes   ##
## plus one or two classes for rock and/or ice.                               ##
lpjml_soiltypes <- c(
  "clay", "silty clay", "sandy clay", "clay loam", "silty clay loam",
  "sandy clay loam", "loam", "silt loam", "sandy loam", "silt", "loamy sand",
  "sand", "rock and ice"
)
lpjml_soilcodes <- c(
  "Cl", "SiCl", "SaCl", "ClLo", "SiClLo", "SaClLo", "Lo", "SiLo", "SaLo", "Si",
  "LoSa", "Sa", "ROCK"
)
##                                                                            ##
## Mapping of USDA soil texture classes to LPJmL soil types.                  ##
## USDA types need to match types in hwsd_usda_class_file. LPJmL types need   ##
## to match lpjml_soiltypes.                                                  ##
usda_to_lpjml <- data.frame(
  rbind(
    cbind(USDA = "clay(heavy)", LPJmL = "clay"),
    cbind(USDA = "silty clay", LPJmL = "silty clay"),
    cbind(USDA = "clay (light)", LPJmL = "clay"),
    cbind(USDA = "silty clay loam", LPJmL = "silty clay loam"),
    cbind(USDA = "clay loam", LPJmL = "clay loam"),
    cbind(USDA = "silt", LPJmL = "silt"),
    cbind(USDA = "silt loam", LPJmL = "silt loam"),
    cbind(USDA = "sandy clay", LPJmL = "sandy clay"),
    cbind(USDA = "loam", LPJmL = "loam"),
    cbind(USDA = "sandy clay loam", LPJmL = "sandy clay loam"),
    cbind(USDA = "sandy loam", LPJmL = "sandy loam"),
    cbind(USDA = "loamy sand", LPJmL = "loamy sand"),
    cbind(USDA = "sand", LPJmL = "sand")
  ),
  stringsAsFactors = FALSE
)
################################################################################


################################################################################
## R packages required for this script. These may need to be installed first. ##
library(raster)
library(geosphere)
################################################################################


################################################################################
## Trying to set up cluster using MPI interface or doParallel.                ##
## The implementation using the MPI interface has been developed for a high   ##
## performance cluster. If Rmpi and doMPI are installed and cluster == TRUE   ##
## the script tries to use this combination of packages.                      ##
## If Rmpi is not available but cluster == TRUE the script attempts to use    ##
## parallelization through the "parallel" package and doParallel.             ##
##                                                                            ##
## This part may need to be tweaked for your system set up.                   ##
parallel_mpi <- parallel_local <- FALSE # Not to be set by user
if (cluster) {
  # Try parallelization
  if (require(Rmpi)) {
    # Rmpi = R implementation of MPI interface
    # This is intended for parallelization on high-performance cluster.
    if (require(doMPI)) {
      # doMPI = interface for foreach construct to run in MPI parallel mode
       # Start MPI cluster (link R instances together)
      cl <- doMPI::startMPIcluster()
      # Number of R instances linked together
      num_cluster <- doMPI::clusterSize(cl)
      if (num_cluster > 1) {
        # Script is using more than 1 CPU, so really run in parallel mode
        # Tell foreach to use MPI backend for parallelization
        doMPI::registerDoMPI(cl)
        cat("Running in parallel mode on", num_cluster, "worker nodes.\n")
        parallel_mpi <- TRUE
      } else {
        # Only one task
        # Tell foreach to use sequential mode
        registerDoSEQ()
        cat("Running in sequential mode because only one node is available.\n")
        num_cluster <- 1
      }
    } else {
      # doMPI package is missing
      warning(
        "It seems that you have Rmpi installed. This script requires ",
        "both Rmpi and doMPI package to run in parallel mode.",
        call. = FALSE,
        immediate. = TRUE
      )
      registerDoSEQ() # Tell foreach to use sequential mode
      cat("Falling back to running in sequential mode.\n")
      num_cluster <- 1
    }
  } else if (require(doParallel)) {
    # Try parallelization through parallel package.
    # This is probably more suitable to run in parallel on a local machine
    # Get number of CPU cores
    ncores <- parallel::detectCores()
    # It is probably a good idea not to use all CPUs on your computer.
    # The number of usable CPUs may also be limited by the memory requirement
    # of each task. Test thoroughly in order not to cripple your system.
    # Tests have shown that parallel tasks mostly share memory during the first
    # parallel loop, therefore more tasks do not increase total memory
    # requirement substantially.
    # The second parallel loop is more memory-intensive. Therefore, the number
    # of tasks num_cluster is automatically reduced before the second loop.
    if (is.finite(ncores)) {
      num_cluster <- ceiling(ncores / 2)
    } else {
      # parallel::detectCores() could not detect number of CPUs. Fall back to 1.
      num_cluster <- 1
    }
    if (num_cluster > 1) {
      # Start cluster on local machine
      cl <- parallel::makeCluster(num_cluster)
      # Tell foreach to use this cluster
      registerDoParallel(num_cluster)
      parallel_local <- TRUE
      cat("Running in parallel mode on", num_cluster, "CPUs\n")
    } else {
      # Only one task
      registerDoSEQ() # Tell foreach to use sequential mode
      cat("Running in sequential mode because only one CPU is available.\n")
    }
  } else {
    # Rmpi and doParallel package are missing
    warning(
      "This script requires Rmpi and doMPI or ",
      "parallel and doParallel to run in parallel mode.\n",
      "Please install missing packages or set cluster to FALSE.",
      call. = FALSE,
      immediate. = TRUE
    )
    registerDoSEQ() # Tells foreach to use sequential mode
    cat("Falling back to running in sequential mode.\n")
    num_cluster <- 1
  }
} else {
  # Do not try parallelization
  library(foreach)
  registerDoSEQ() # Tells foreach to use sequential mode
  cat("Running in sequential mode.\n")
  num_cluster <- 1
}
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

## Set working dir to soil_dir, if soil_dir is set
if (nchar(soil_dir) > 0) {
  setwd(soil_dir)
}

################################################################################
## Read grid file                                                             ##
## Functions read_header(), get_headersize(), and get_datatype() defined in   ##
## ../lpjml_format_helper_functions.R                                         ##
cat("Reading LPJmL grid from", sQuote(gridname), "\n")
gridheader <- read_header(gridname)
gridfile <- file(gridname, "rb")
seek(gridfile, get_headersize(gridheader))
griddata <- matrix(
  readBin(
    gridfile,
    what = get_datatype(gridheader)$type,
    size = get_datatype(gridheader)$size,
    n = gridheader$header["ncell"] * gridheader$header["nbands"],
    endian = gridheader$endian
  ) * gridheader$header["scalar"],
  ncol = gridheader$header["nbands"],
  byrow = TRUE,
  dimnames = list(NULL, c("lon", "lat"))
)
close(gridfile)
# Determine resolution string to use in filenames.
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
  sep = "",
  collapse = "_by_"
)
rm(tmp_res)
# Create raster fitting griddata.
gridextent <- extent(
  min(griddata[, "lon"]) - gridheader$header["cellsize_lon"] / 2,
  max(griddata[, "lon"]) + gridheader$header["cellsize_lon"] / 2,
  min(griddata[, "lat"]) - gridheader$header["cellsize_lat"] / 2,
  max(griddata[, "lat"]) + gridheader$header["cellsize_lat"] / 2
)
gridraster <- raster(
  gridextent,
  res = gridheader$header[c("cellsize_lon", "cellsize_lat")]
)
# Set projection.
proj4string(gridraster) <-
  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# Assign grid indices.
gridraster[cellFromXY(gridraster, griddata)] <-
  seq_len(gridheader$header["ncell"])
################################################################################

################################################################################
## Set up file names for files created by this script and check if they exist ##
## already                                                                    ##
lpjml_soiltexture_name <- paste0(
  "soil_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string, "_",
  length(lpjml_soiltypes), "_types.",
  tolower(lpjml_soil_format)
)
if (file.exists(lpjml_soiltexture_name)) {
  if (lpjml_soil_format == "BIN") {
    soiltexture_header <- read_header(lpjml_soiltexture_name)
    if (soiltexture_header$header["ncell"] != gridheader$header["ncell"] ||
      any(
        soiltexture_header$header[c("cellsize_lon", "cellsize_lat")] /
        gridheader$header[c("cellsize_lon", "cellsize_lat")] < 0.9999
      ) || any(
        soiltexture_header$header[c("cellsize_lon", "cellsize_lat")] /
        gridheader$header[c("cellsize_lon", "cellsize_lat")] > 1.0001
      )
    ) {
      stop(
        "Existing file ", sQuote(lpjml_soiltexture_name),
        " does not match with grid file ", sQuote(gridname)
      )
    } else {
      stop(
        "Soil texture file ", sQuote(lpjml_soiltexture_name),
        " exists already.",
        "\nRename or delete existing file before running this script."
      )
    }
  } else if (lpjml_soil_format == "RAW") {
    if (file.size(lpjml_soiltexture_name) != gridheader$header["ncell"]) {
      stop(
        "Existing file ", sQuote(lpjml_soiltexture_name),
        " does not match with grid file ", sQuote(gridname)
      )
    } else {
      stop(
        "Soil texture file ", sQuote(lpjml_soiltexture_name),
        " exists already.",
        "\nRename or delete existing file before running this script."
      )
    }
  } else {
    stop(
      "Soil texture file ", sQuote(lpjml_soiltexture_name),
      " exists already.",
      "\nRename or delete existing file before running this script."
    )
  }
}
# Creation of soil pH file can be skipped by commenting lpjml_soilph_format.
# Only check if file exists if format exists.
if (exists("lpjml_soilph_format")) {
  lpjml_soilph_name <- paste0(
    "soil_pH_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(lpjml_soilph_format)
  )
  if (file.exists(lpjml_soilph_name)) {
    if (lpjml_soilph_format == "BIN") {
      soilph_header <- read_header(lpjml_soilph_name)
      if (soilph_header$header["ncell"] != gridheader$header["ncell"] ||
        any(
          soilph_header$header[c("cellsize_lon", "cellsize_lat")] /
          gridheader$header[c("cellsize_lon", "cellsize_lat")] < 0.9999
        ) || any(
          soilph_header$header[c("cellsize_lon", "cellsize_lat")] /
          gridheader$header[c("cellsize_lon", "cellsize_lat")] > 1.0001
        )
      ) {
        stop(
          "Existing file ", sQuote(lpjml_soilph_name),
          " does not match with grid file ", sQuote(gridname)
        )
      } else {
        stop(
          "Soil pH file ", sQuote(lpjml_soilph_name),
          " exists already.",
          "\nRename or delete existing file before running this script."
        )
      }
    } else {
      stop(
        "Soil pH file ", sQuote(lpjml_soilph_name),
        " exists already.",
        "\nRename or delete existing file before running this script."
      )
    }
  }
}
# Temporary file for saving preliminary results
tmp_RData <- paste0(
  "soil_aggregation_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string, "_",
  length(lpjml_soiltypes), "_types.RData"
)
# Variables to be saved in tmp_RData
savevar <- c(
  "griddata",
  "hwsd_raster",
  "hwsd_attribute_file",
  "rock_ice_shared",
  "max_search",
  "idw_power_par",
  "quicksearch",
  "lpjml_soiltexture",
  "lpjml_soiltexture_gapfilling",
  "lpjml_soilph",
  "lpjml_soilph_gapfilling",
  "lpjml_soilph_source",
  "lpjml_soil_ds",
  "allow_skip"
)
################################################################################

################################################################################
## Read HWSD soil attribute database                                          ##
cat("Reading HWSD soil attribute table from", sQuote(hwsd_attribute_file), "\n")
hwsd_attribute_table <- read.csv(hwsd_attribute_file, stringsAsFactors = FALSE)
# Read USDA texture class definitions.
hwsd_usda_classes <- read.csv(hwsd_usda_class_file, stringsAsFactors = FALSE)
# Check that texture classes match classes defined in this script.
if (any(!hwsd_usda_classes$VALUE %in% usda_to_lpjml$USDA)) {
  # HWSD has class that cannot be assigned. Stop.
  stop(
    "Soil texture class(es) ",
    toString(sQuote(setdiff(hwsd_usda_classes$VALUE, usda_to_lpjml$USDA))),
    " from HWSD soil attribute database not defined in usda_to_lpjml.",
    "\nPlease check soil type mapping."
  )
}
if (any(!usda_to_lpjml$USDA %in% hwsd_usda_classes$VALUE)) {
  # Our mapping has class not present in HWSD data -> class will never be
  # assigned.
  warning(
    "Soil texture class(es) ",
    toString(sQuote(setdiff(usda_to_lpjml$USDA, hwsd_usda_classes$VALUE))),
    " defined in usda_to_lpjml not found in HWSD soil attribute database.",
    "\nPlease check soil type mapping.",
    call. = FALSE,
    immediate. = TRUE
  )
}
# Check compatibility between usda_to_lpjml and lpjml_soiltypes.
if (any(!usda_to_lpjml$LPJmL %in% lpjml_soiltypes)) {
  stop(
    "Soil type(s) ",
    toString(sQuote(setdiff(usda_to_lpjml$LPJmL, lpjml_soiltypes))),
    " defined in usda_to_lpjml not part of lpjml_soiltypes"
  )
}
if (any(!lpjml_soiltypes %in% usda_to_lpjml$LPJmL)) {
  message(
    "Info: LPJmL soil type(s) ",
    toString(sQuote(setdiff(lpjml_soiltypes, usda_to_lpjml$LPJmL))),
    " not part of usda_to_lpjml mapping."
  )
}
# Compatibility between hwsd_attribute_table and hwsd_usda_classes.
if (any(
  !na.omit(hwsd_attribute_table$T_USDA_TEX_CLASS) %in% hwsd_usda_classes$CODE
)) {
  stop(
    "Soil texture index/indices ",
    toString(
      setdiff(
        na.omit(hwsd_attribute_table$T_USDA_TEX_CLASS),
        hwsd_usda_classes$CODE
      )
    ),
    " found in ", sQuote(hwsd_attribute_file),
    " missing in ", sQuote(hwsd_usda_class_file)
  )
}
# Some attribute table preparations.
# Fibric Histosols
rindex <- which(
  hwsd_attribute_table[, "SU_SYM90"] == "HSf" &
  is.na(hwsd_attribute_table[, "T_SAND"])
)
hwsd_attribute_table[rindex, c("T_SAND", "T_SILT", "T_CLAY")] <- 0
# Use SU_SYM74 value for missing SU_SYM90 values.
rindex <- which(is.na(hwsd_attribute_table[, "SU_CODE90"]))
hwsd_attribute_table[rindex, "SU_SYM90"] <-
  hwsd_attribute_table[rindex, "SU_SYM74"]
# Convert share from percent to fraction.
hwsd_attribute_table[, "SHARE"] <- hwsd_attribute_table[, "SHARE"] / 100
################################################################################

################################################################################
## Open HWSD raster map                                                       ##
cat("Reading HWSD soil map raster from", sQuote(hwsd_raster), "\n")
hwsd <- raster(hwsd_raster)
# Set projection.
proj4string(hwsd) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
################################################################################

################################################################################
## Compare resolution for compatibility                                       ##
if (any(res(gridraster) / res(hwsd) < 0.9999)) {
  # Source is too coarse
  stop(
    "Source resolution ", paste(res(hwsd), collapse = " by "),
    " in ", sQuote(hwsd_raster),
    " is coarser than target resolution ",
    paste(
      gridheader$header[c("cellsize_lon", "cellsize_lat")],
      collapse = " by "
    ),
    " of grid file ", sQuote(gridname)
  )
}
if (any((res(gridraster) / res(hwsd)) %% 1 > 1e-4)) {
  # Target is not integer multiple of source resolution
  stop(
    "Target resolution ",
    paste(
      gridheader$header[c("cellsize_lon", "cellsize_lat")],
      collapse = " by "
    ),
    " of grid file ", sQuote(gridname),
    " is not an integer multiple of source resolution ",
    paste(res(hwsd), collapse = " by "),
    " in ", sQuote(hwsd_raster)
  )
}
# Check spatial extent.
# Allow for small tolerance.
if (xmin(gridraster) - xmin(hwsd) < (-xres(hwsd) / 100) ||
  xmax(gridraster) - xmax(hwsd) > xres(hwsd) / 100 ||
  ymin(gridraster) - ymin(hwsd) < (-yres(hwsd) / 100) ||
  ymax(gridraster) - ymax(hwsd) > yres(hwsd) / 100
) {
  stop(
    "Spatial extent of source data ",
    toString(extent(hwsd)),
    " does not cover the full spatial extent ",
    toString(extent(gridraster)),
    " of grid file ", sQuote(gridname), "."
  )
}
# Check cell boundary alignment.
if (
  (
    (abs(xmin(gridraster) - xmin(hwsd)) / xres(hwsd)) %% 1 > 1e-3 &&
    (abs(xmin(gridraster) - xmin(hwsd)) / xres(hwsd)) %% 1 < 0.999
  ) || (
    (abs(ymin(gridraster) - ymin(hwsd)) / yres(hwsd)) %% 1 > 1e-3 &&
    (abs(ymin(gridraster) - ymin(hwsd)) / yres(hwsd)) %% 1 < 0.999
  )
) {
  stop(
    "Spatial extent of source data ", toString(extent(hwsd)),
    " and spatial extent of grid file ", sQuote(gridname),
    toString(extent(gridraster)),
    " are mis-aligned."
  )
} else {
  # Align extent to take care of numerical inaccuracies.
  extent(gridraster) <- alignExtent(extent(gridraster), hwsd)
}
aggregation_factor <- round(res(gridraster) / res(hwsd))
if (xres(gridraster) != yres(gridraster)) {
  if (lpjml_soil_format == "BIN" && lpjml_soil_version < 3) {
    stop(
      "Only header version 3 supports longitude and latitude resolutions ",
      "to differ.",
      "\nPlease check lpjml_soil_version"
    )
  }
  if (exists("lpjml_soilph_format") && lpjml_soilph_format == "BIN" &&
    lpjml_soilph_version < 3) {
    stop(
      "Only header version 3 supports longitude and latitude resolutions ",
      "to differ.",
      "\nPlease check lpjml_soilph_version"
    )
  }
}
################################################################################

################################################################################
## Read HWSD raster map into RAM                                              ##
## This is memory intensive but speeds up further processing compared to      ##
## working from file. The full HWSD raster map alone requires roughly 3.5 GB  ##
## (more during read operation). Skip this part if your computer does not     ##
## have enough RAM.                                                           ##
## We are also setting a higher limit to how much data the raster package can ##
## keep in memory before writing to temporary files. Adjust this to your      ##
## local machine. Temporary files are slower than rasters in memory.          ##
# Set memory limit for raster package
rasterOptions(maxmemory = 16e9, memfrac = 0.8)
# Load data
if (!inMemory(hwsd)) {
  hwsd <- readAll(hwsd)
  gc(reset = TRUE)
}
################################################################################

################################################################################
## Find unique mapping units in HWSD raster map and detect missing values.    ##
# This is split into chunks to reduce memory requirements.
map_mu <- integer(0)
for (i in seq(1, ncell(hwsd), by = ncell(hwsd) %/% 20)) {
  chunk <- seq(i, min(i + ncell(hwsd) %/% 20 - 1, ncell(hwsd)))
  chunk_data <- hwsd[chunk]
  chunk_unique <- unique(chunk_data, na.last = TRUE)
  map_mu <- union(map_mu, chunk_unique)
  rm(chunk, chunk_data, chunk_unique)
  gc(full = FALSE)
}
att_mu <- unique(hwsd_attribute_table$MU_GLOBAL)
if (!anyNA(map_mu) && length(setdiff(map_mu, att_mu)) == 1) {
  message(
    "Assuming that value ", sQuote(setdiff(map_mu, att_mu)),
    " in HWSD soil map raster represents missing value"
  )
} else if (length(setdiff(map_mu, att_mu)) != 1) {
  stop(
    "The following mapping unit(s) from HWSD soil map raster is/are missing ",
    "in HWSD soil attribute table: ",
    toString(sQuote(setdiff(map_mu, att_mu)))
  )
}
################################################################################


################################################################################
## Extract for each LPJmL cell all source data cells and find dominant soil   ##
## type. The dominant soil type is the one with the largest area share.       ##
## Each cell in source map data has 1 associated mapping unit in attribute    ##
## table. Each mapping unit can be assigned to several cells. Each mapping    ##
## may contain one or several soil entries.                                   ##
## The algorithm first looks for the USDA texture class with the largest      ##
## area share (across all mapping units). To derive soil pH, the algorithm    ##
## uses with descending priority:                                             ##
## 1) the soil with the largest area share that belongs to the dominant soil  ##
##    texture and mapping unit, or                                            ##
## 2) the soil with the largest area share that belongs to the dominant soil  ##
##    texture, or                                                             ##
## 3) the soil with the largest area share that belongs to the dominant       ##
##    mapping unit                                                            ##
##                                                                            ##
cat(
  "Aggregating soil data from", sQuote(hwsd_raster),
  "with a spatial resolution of",
  paste(format(res(hwsd), digits = 4), collapse = " by "),
  "to", gridheader$header["ncell"], "cells from grid file", sQuote(gridname),
  "with a spatial resolution of",
  paste(format(res(gridraster), digits = 4), collapse = " by "), ".\n"
)
if (!rock_ice_shared) {
  cat(
    "Soiltype(s)",
    toString(
      sQuote(
        grep("rock|ice", lpjml_soiltypes, ignore.case = TRUE, value = TRUE)
      )
    ),
    "only assigned to cells if no other soil types are present.\n"
  )
}
# Names of rock and/or ice soil types (may be the same or separate)
lpjml_rock <- grep("rock", lpjml_soiltypes, ignore.case = TRUE, value = TRUE)
lpjml_ice <- grep("ice", lpjml_soiltypes, ignore.case = TRUE, value = TRUE)
# Name of "sand" soil type (expects "sand" in either lower or upper case)
lpjml_sand <- grep("^sand$", lpjml_soiltypes, value = TRUE, ignore.case = TRUE)
# Soil types except rock and/or ice
lpjml_no_rock_ice <- grep(
  "rock|ice",
  lpjml_soiltypes,
  ignore.case = TRUE,
  value = TRUE,
  invert = TRUE
)
# Index of LPJmL grid cells in gridraster.
gridraster_index <- cellFromXY(gridraster, griddata)
# Initialize result vectors.
lpjml_soiltexture <- lpjml_soilph <- lpjml_soilph_source <-
  rep(NA, gridheader$header["ncell"])
lpjml_soil_ds <- rep(0, gridheader$header["ncell"])

lpjml_soiltexture_gapfilling <- lpjml_soilph_gapfilling <- data.frame(
  cell = integer(0),
  nvalid = integer(0),
  mindist = double(0),
  maxdist = double(0),
  window = double(0)
)

# Parallelized loop using foreach. Packages of 1000 cells each are sent to each
# worker. Workers return results for their package and get the next package
# automatically until all cells have been processed.
# Check if results already exist. If so, check if they are compatible with
# current settings.
if (file.exists(tmp_RData)) {
  cat(
    "Loading data from previous script run from", sQuote(tmp_RData),
    "and checking validity.\n"
  )
  load(tmp_RData)
  for (testvar in c(
    "griddata",
    "hwsd_raster",
    "hwsd_attribute_file",
    "rock_ice_shared",
    "max_search",
    "idw_power_par",
    "quicksearch",
    "allow_skip"
  )) {
    if (!identical(get(paste0("tmp_", testvar)), get(testvar))) {
      stop(
        "Variable ", sQuote(testvar),
        " from previous script run does not match current script run.",
        "\nFile", sQuote(tmp_RData), "is incompatible"
      )
    }
  }
  cat(
    "Data appears to have been generated using the same settings.",
    "Skipping unnecessary aggregation steps.\n"
  )
  for (takeover in c(
    "lpjml_soiltexture",
    "lpjml_soiltexture_gapfilling",
    "lpjml_soilph",
    "lpjml_soilph_gapfilling",
    "lpjml_soilph_source",
    "lpjml_soil_ds"
  )) {
    assign(takeover, get(paste0("tmp_", takeover)))
  }
  rm(list = paste0("tmp_", savevar))
} else {
  if (num_cluster > 1) {
    cat("Parallel execution of aggregation on", num_cluster, "tasks\n")
  }
  start_time <- proc.time()["elapsed"]
  if (parallel_mpi) {
    # Reduce amount of data that needs to be transferred over the network to
    # parallel tasks by not exporting high-resolution HWSD source data
    noexport <- "hwsd"
    # Delete data from control task since it is not needed anymore
    rm(hwsd)
  } else {
    noexport <- NULL
  }
  # Make progress updates dependent on number of cells to process.
  # Set a lower progress_step for fewer updates.
  progress_step <- ifelse(gridheader$header["ncell"] < 100000, 50, 500)
  # In case of MPI-based parallelization .verbose is set to TRUE to give some
  # progress report in the control task. You may switch this off (FALSE) if you
  # do not need this.
  aggregation_loop <- foreach(
    cell = seq(1, gridheader$header["ncell"], by = 1000),
    .inorder = FALSE,
    .combine = rbind,
    .verbose = (parallel_mpi && num_cluster > 1),
    .packages = c("raster", "geosphere"),
    .noexport = noexport
  ) %dopar% {
#  for (cell in seq(1, gridheader$header["ncell"], by = 1000)) {
    if (!exists("hwsd")) {
      # If hwsd does not exist (MPI parallelization) reload it.
      # Each worker only needs to do this once, not for every loop iteration.
      hwsd <- raster(hwsd_raster)
      # Set projection.
      proj4string(hwsd) <-
        "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      # Load hwsd into RAM, skip this part if your setup does not have enough
      # RAM. If hwsd is not loaded into memory processing will be slowed down
      # considerably.
      if (!inMemory(hwsd)) {
        hwsd <- readAll(hwsd)
      }
      # Expand the amount of memory the raster package can use before offloading
      # to temporary files. Adjust this parameter to your system setup.
      rasterOptions(maxmemory = 16e9, memfrac = 0.8)
    } else if (!inMemory(hwsd)) {
      # Load hwsd into RAM, skip this part if your setup does not have enough
      # RAM.
      hwsd <- readAll(hwsd)
    }
    # Table which will collect results of all cells in one package.
    results_table <- cbind(
      cell = integer(0),
      soiltexture = integer(0),
      soilph = double(0),
      soil_ds = double(0),
      soilph_source = integer(0)
    )
    for (c in seq(cell, min(cell + 999, gridheader$header["ncell"]))) {
      if (c %% round(gridheader$header["ncell"] / progress_step) == 0 &&
        !parallel_mpi
      ) {
        # doParallel and registerDoSEQ send print output from workers to control
        # task whereas doMPI does not. So do not print progress reports for MPI
        # parallelization.
        cat("Finished", round(c / gridheader$header["ncell"] * 100, 1), "%\n")
      }
      # To avoid a bias of the position in a vector when selecting the maximum
      # value vectors are randomly shuffled. set.seed still allows for
      # reproducibility. Called for each cell to allow partial processing of
      # grid list.
      set.seed(c)
      # Raster package may create temporary files that can take up substantial
      # disk space. Keep track to delete them.
      raster_names <- character(0)
      # Crop global map to cell extent
      cell_hwsd <- crop(hwsd, extentFromCells(gridraster, gridraster_index[c]))
      if (fromDisk(cell_hwsd)) {
        raster_names <- c(raster_names, cell_hwsd@file@name)
      }
      # Check to cropping returned expected number of source cells
      if (ncell(cell_hwsd) != prod(aggregation_factor)) {
        # Error cropping source cells to extent of target cell.
        # stop() does not always work correctly inside dopar.
        message(
          "Error: Wrong number of source cells ", ncell(cell_hwsd),
          " in cell ", c, ". Expecting: ", prod(aggregation_factor)
        )
        return(NULL)
      }

      # Mapping units in cell subset
      cell_mu <- unique(cell_hwsd)
      if (length(intersect(cell_mu, att_mu)) == 0) {
        # HWSD has no data for LPJmL cell, skip further processing
        results_table <- rbind(
          results_table,
          cbind(
            cell = c,
            soiltexture = NA,
            soilph = NA,
            soil_ds = 0,
            soilph_source = NA
          ),
          deparse.level = 0
        )
        rm(cell_hwsd, cell_mu)
        # Clean up temporary raster files.
        if (length(raster_names) > 0) {
          file.remove(raster_names)
          raster_names <- sub(".grd$", ".gri", raster_names)
          file.remove(raster_names)
          rm(raster_names)
        }
        next
      }
      # Filter possible missing values.
      cell_mu <- intersect(cell_mu, att_mu)
      # Subset of attribute table
      index <- which(hwsd_attribute_table$MU_GLOBAL %in% cell_mu)
      cell_attribute_table <- hwsd_attribute_table[index, ]
      rm(index)
      # USDA texture classes in cells
      cell_usda <- unique(cell_attribute_table$T_USDA_TEX_CLASS)
      # Areas per USDA texture class plus rock and ice
      cell_usda_area <- list()
      for (t in lpjml_soiltypes) {
        cell_usda_area[[t]] <- double(0)
      }
      # Area of HWSD source cells
      cell_hwsd_area <- cellarea(
        yFromCell(cell_hwsd, seq_len(ncell(cell_hwsd))),
        xres(cell_hwsd),
        yres(cell_hwsd)
      )
      # Area of each USDA texture class in LPJmL cell
      mu_area <- double(length(cell_mu))
      names(mu_area) <- cell_mu
      rm(cell_mu)
      for (usda in na.omit(cell_usda)) {
        # Translate texture class index from column "T_USDA_TEX_CLASS" to
        # texture class name in HWSD and then to texture class name in LPJmL
        codeindex <- match(usda, hwsd_usda_classes$CODE)
        nameindex <- match(
          hwsd_usda_classes$VALUE[codeindex],
          usda_to_lpjml$USDA
        )
        lpj_usda <- usda_to_lpjml$LPJmL[nameindex]
        # Attribute rows with texture usda
        subset_usda <- which(cell_attribute_table$T_USDA_TEX_CLASS == usda)
        rm(codeindex, nameindex)
        for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
          mucells <- which(cell_hwsd[] == mu)
          # Attribute rows with texture usda and mapping unit mu
          subset_usda_mu <- intersect(
            which(cell_attribute_table$MU_GLOBAL == mu),
            subset_usda
          )
          # Using sum() because there may be more than one soil with the same
          # texture class within a mapping unit
          usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
          tmp_area <- sum(cell_hwsd_area[mucells] * usda_mu_share)
          mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
          if (mu %in% names(cell_usda_area[[lpj_usda]])) {
            cell_usda_area[[lpj_usda]][as.character(mu)] <-
              cell_usda_area[[lpj_usda]][as.character(mu)] + tmp_area
          } else {
            cell_usda_area[[lpj_usda]] <- c(
              cell_usda_area[[lpj_usda]],
              tmp_area
            )
            last <- length(cell_usda_area[[lpj_usda]])
            names(cell_usda_area[[lpj_usda]])[last] <- mu
            rm(last)
          }
          rm(tmp_area, mucells, subset_usda_mu, usda_mu_share)
        }
        rm(subset_usda)
      }

      # Rock (classified as SU_SYM90 == "RK" in HWSD)
      # Attribute rows with "RK"
      subset_usda <- which(cell_attribute_table$SU_SYM90 == "RK")
      for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
        mucells <- which(cell_hwsd[] == mu)
        subset_usda_mu <- intersect(
          which(cell_attribute_table$MU_GLOBAL == mu),
          subset_usda
        )
        usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
        tmp_area <- sum(cell_hwsd_area[mucells] * usda_mu_share)
        mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
        if (mu %in% names(cell_usda_area[[lpjml_rock]])) {
          cell_usda_area[[lpjml_rock]][as.character(mu)] <-
            cell_usda_area[[lpjml_rock]][as.character(mu)] + tmp_area
        } else {
          cell_usda_area[[lpjml_rock]] <- c(
            cell_usda_area[[lpjml_rock]],
            tmp_area
          )
          last <- length(cell_usda_area[[lpjml_rock]])
          names(cell_usda_area[[lpjml_rock]])[last] <- mu
          rm(last)
        }
        rm(tmp_area, mucells, subset_usda_mu, usda_mu_share)
      }
      rm(subset_usda)

      # Ice (classified as SU_SYM90 == "GG" in HWSD)
      # Attribute rows with "GG"
      subset_usda <- which(cell_attribute_table$SU_SYM90 == "GG")
      for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
        mucells <- which(cell_hwsd[] == mu)
        subset_usda_mu <- intersect(
          which(cell_attribute_table$MU_GLOBAL == mu),
          subset_usda
        )
        usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
        tmp_area <- sum(cell_hwsd_area[mucells] * usda_mu_share)
        mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
        if (mu %in% names(cell_usda_area[[lpjml_ice]])) {
          cell_usda_area[[lpjml_ice]][as.character(mu)] <-
            cell_usda_area[[lpjml_ice]][as.character(mu)] + tmp_area
        } else {
          cell_usda_area[[lpjml_ice]] <- c(
            cell_usda_area[[lpjml_ice]],
            tmp_area
          )
          last <- length(cell_usda_area[[lpjml_ice]])
          names(cell_usda_area[[lpjml_ice]])[last] <- mu
          rm(last)
        }
        rm(tmp_area, mucells, subset_usda_mu, usda_mu_share)
      }
      rm(subset_usda)

      # Include dunes/shifting sand in sand
      # Attribute rows with "DS"
      subset_usda <- which(cell_attribute_table$SU_SYM90 == "DS")
      for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
        mucells <- which(cell_hwsd[] == mu)
        subset_usda_mu <- intersect(
          which(cell_attribute_table$MU_GLOBAL == mu),
          subset_usda
        )
        usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
        tmp_area <- sum(cell_hwsd_area[mucells] * usda_mu_share)
        mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
        lpjml_soil_ds[c] <- lpjml_soil_ds[c] + tmp_area
        if (mu %in% names(cell_usda_area[[lpjml_sand]])) {
          cell_usda_area[[lpjml_sand]][as.character(mu)] <-
            cell_usda_area[[lpjml_sand]][as.character(mu)] + tmp_area
        } else {
          cell_usda_area[[lpjml_sand]] <- c(
            cell_usda_area[[lpjml_sand]],
            tmp_area
          )
          last <- length(cell_usda_area[[lpjml_sand]])
          names(cell_usda_area[[lpjml_sand]])[last] <- mu
          rm(last)
        }
        rm(tmp_area, mucells, subset_usda_mu, usda_mu_share)
      }
      rm(subset_usda)

      # Determine dominant USDA soil texture class based on area covered by
      # classes in cell.
      # Since several classes may have the same area share shuffle (sample())
      # cell_usda_area to avoid bias from the sequence of soil types
      if (any(sapply(cell_usda_area, sum) > 0)) {
        if (rock_ice_shared) {
          # Treat rock and ice like any other soil
          dominant_usda <- names(which.max(sample(sapply(cell_usda_area, sum))))
        } else {
          # Only assign rock and ice if no other soil type is present
          if (any(sapply(cell_usda_area[lpjml_no_rock_ice], sum) > 0)) {
            # Soil types without rock and ice
            dominant_usda <- names(
              which.max(sample(sapply(cell_usda_area[lpjml_no_rock_ice], sum)))
            )
          } else {
            # Try soil types with rock and ice. Rock and ice may either be
            # individual soil types or combined into one depending on setup.
            dominant_usda <- names(
              which.max(sample(sapply(cell_usda_area, sum)))
            )
          }
        }
      } else {
        # No soil texture class has any areas, keep cell as NA and skip further
        # processing
        results_table <- rbind(
          results_table,
          cbind(
            cell = c,
            soiltexture = NA,
            soilph = NA,
            soil_ds = 0,
            soilph_source = NA
          ),
          deparse.level = 0
        )
        rm(cell_hwsd, cell_hwsd_area, cell_attribute_table, cell_usda,
           cell_usda_area, mu_area)
        # Clean up temporary raster files.
        if (length(raster_names) > 0) {
          file.remove(raster_names)
          raster_names <- sub(".grd$", ".gri", raster_names)
          file.remove(raster_names)
          rm(raster_names)
        }
        next
      }
      # Set dominant texture class for cell c
      lpjml_soiltexture[c] <- match(dominant_usda, lpjml_soiltypes)
      # Determine dominant mu in dominant USDA texture class, again use sampling
      # to avoid bias from the sequence of soil types
      if (length(cell_usda_area[[dominant_usda]]) > 1) {
        dominant_mu_in_class <- names(
          which.max(sample(cell_usda_area[[dominant_usda]]))
        )
      } else {
        dominant_mu_in_class <- names(cell_usda_area[[dominant_usda]])
      }
      # Determine share of dunes/shifting sand in cell
      lpjml_soil_ds[c] <- lpjml_soil_ds[c] / sum(mu_area)
      # Only determine pH if lpjml_soilph_format is set, i.e. user has requested
      # soil pH file to be created
      if (exists("lpjml_soilph_format")) {
        # Determine pH value for the assigned soil texture
        # Translate LPJmL texture class name back to texture index from column
        # T_USDA_TEX_CLASS, may be more than one USDA code
        nameindex <- which(usda_to_lpjml$LPJmL == dominant_usda)
        codeindex <- match(
          usda_to_lpjml$USDA[nameindex],
          hwsd_usda_classes$VALUE
        )
        dominant_usda_codes <- hwsd_usda_classes$CODE[codeindex]
        rm(nameindex, codeindex)
        # Attribute rows with matching dominant_usda_codes
        subset_usda <- which(cell_attribute_table$T_USDA_TEX_CLASS %in%
          dominant_usda_codes
        )
        # Attribute rows with matching mapping unit mu
        subset_mu <- which(cell_attribute_table$MU_GLOBAL ==
          as.integer(dominant_mu_in_class)
        )
        # Combination of texture class and mapping unit
        subset_usda_mu <- intersect(subset_usda, subset_mu)
        # Attribute rows with pH value
        subset_ph <- which(!is.na(cell_attribute_table$T_PH_H2O))
        # Columns to copy from attribute table
        cols <- c("ID", "SHARE", "T_PH_H2O", "MU_GLOBAL")
        if (any(!is.na(cell_attribute_table$T_PH_H2O[subset_usda_mu]))) {
          # Use pH from same texture class and same mapping unit.
          selected <- intersect(subset_ph, subset_usda_mu)
          tmptable <- cell_attribute_table[selected, cols]
          # Sort by SHARE and use random value to break ties.
          r <- order(
            tmptable[, "SHARE"],
            sample(nrow(tmptable)),
            decreasing = TRUE
          )[1]
          lpjml_soilph[c] <- tmptable[r, "T_PH_H2O"]
          lpjml_soilph_source[c] <- 1
          rm(selected, tmptable, r)
        } else if (any(!is.na(cell_attribute_table$T_PH_H2O[subset_usda]))) {
          # Use pH from same texture class.
          selected <- intersect(subset_ph, subset_usda)
          tmptable <- cell_attribute_table[selected, cols]
          # Append area (since different mapping units may be combined).
          muindex <- match(tmptable$MU_GLOBAL, names(mu_area))
          tmptable <- cbind(tmptable, area = mu_area[muindex] * tmptable$SHARE)
          # Sort by area and use random value to break ties.
          r <- order(
            tmptable[, "area"],
            sample(nrow(tmptable)),
            decreasing = TRUE
          )[1]
          lpjml_soilph[c] <- tmptable[r, "T_PH_H2O"]
          lpjml_soilph_source[c] <- 2
          rm(selected, munindex, tmptable, r)
        } else if (any(!is.na(cell_attribute_table$T_PH_H2O[subset_mu]))) {
          # Use pH from same mapping unit.
          selected <- intersect(subset_ph, subset_mu)
          tmptable <- cell_attribute_table[selected, cols]
          # Sort by SHARE and use random value to break ties.
          r <- order(
            tmptable[, "SHARE"],
            sample(nrow(tmptable)),
            decreasing = TRUE
          )[1]
          lpjml_soilph[c] <- tmptable[r, "T_PH_H2O"]
          lpjml_soilph_source[c] <- 3
          rm(selected, tmptable, r)
        } else if (lpjml_soiltexture[c] %in%
            grep("rock|ice", lpjml_soiltypes, ignore.case = TRUE)
        ) {
          # Set pH value for rock and ice to 7.
          lpjml_soilph[c] <- 7
          lpjml_soilph_source[c] <- 4
        } else if (any(cell_attribute_table[subset_mu, "SU_SYM90"] == "DS")) {
          # Set pH value for dunes/shifting sand.
          lpjml_soilph[c] <- 7
          lpjml_soilph_source[c] <- 4
        } else {
          message(
            "No pH value for cell ", c,
            " with dominant soil texture ", dominant_usda
          )
        }
        rm(subset_usda, subset_mu, subset_usda_mu, subset_ph, cols,
           dominant_usda_codes)
      }
      rm(cell_hwsd, cell_hwsd_area, dominant_mu_in_class, dominant_usda,
         cell_attribute_table, cell_usda, cell_usda_area, mu_area)
      # Clean up temporary raster files.
      if (length(raster_names) > 0) {
        file.remove(raster_names)
        raster_names <- sub(".grd$", ".gri", raster_names)
        file.remove(raster_names)
        rm(raster_names)
      }
      results_table <- rbind(
        results_table,
        cbind(
          cell = c,
          soiltexture = lpjml_soiltexture[c],
          soilph = lpjml_soilph[c],
          soil_ds = lpjml_soil_ds[c],
          soilph_source = lpjml_soilph_source[c]
        )
      )
    }
    # Return results_table to master task.
    results_table
  } # End of foreach loop

  run_time <- proc.time()["elapsed"] - start_time
  cat(
    "Aggregation took ",
    run_time %/% 3600, ":",
    formatC((run_time %% 3600) %/% 60, format = "d", flag = "0", width = 2),
    ":",
    formatC(run_time %% 60, format = "d", flag = "0", width = 2),
    ifelse(num_cluster > 1, paste(" on", num_cluster, "tasks\n"), "\n"),
    sep = ""
  )

  # Consistency checks.
  # Have all cells been processed in foreach loop?
  if (nrow(aggregation_loop) != gridheader$header["ncell"]) {
    stop(
      "Unexpected length of aggregation_loop: ",
      nrow(aggregation_loop), " != ", gridheader$header["ncell"],
      ifelse(
        num_cluster > 1,
        "\nThis may indicate that some parallel tasks have failed.",
        ""
      )
    )
  }
  if (any(aggregation_loop[, "cell"] < 1) ||
    any(aggregation_loop[, "cell"] > gridheader$header["ncell"])
  ) {
    stop("Invalid cell ID in aggregation_loop")
  }
  # Results in results_table may be out of order, assign to result vectors.
  lpjml_soiltexture[aggregation_loop[, "cell"]] <-
    aggregation_loop[, "soiltexture"]
  lpjml_soilph[aggregation_loop[, "cell"]] <- aggregation_loop[, "soilph"]
  lpjml_soil_ds[aggregation_loop[, "cell"]] <- aggregation_loop[, "soil_ds"]
  lpjml_soilph_source[aggregation_loop[, "cell"]] <-
    aggregation_loop[, "soilph_source"]
  rm(aggregation_loop)

  # Make copies of variables and save to tmp_RData.
  for (copyvar in savevar) {
    assign(paste0("tmp_", copyvar), get(copyvar))
  }
  save(list = paste0("tmp_", savevar), file = tmp_RData)
  cat("Preliminary results of aggregation saved to", sQuote(tmp_RData), "\n")
  rm(list = paste0("tmp_", savevar))
  gc(reset = TRUE)
}
################################################################################


################################################################################
## Check if there are any cells with missing values and do gap-filling if     ##
## necessary. Gap-filling uses all cells from HWSD within a search window to  ##
## determine the dominant soil within that window. The search window starts   ##
## with cells directly adjacent to the cell with missing data. The search     ##
## is expanded until any cells with values are found in HWSD or until maximum ##
## search radius max_search is reached.                                       ##
## If idw_power_par is non-zero cell areas of cells in the search window are  ##
## weighted based on their distance to the center cell.                       ##
## If quicksearch is TRUE, lpjml_soiltexture is used to search for cells with ##
## values instead of the original HWSD raster (coarser resolution of          ##
## lpjml_soiltexture makes for faster search). Set quicksearch to FALSE if    ##
## your grid is incomplete, i.e. if HWSD is guaranteed to have data in cells  ##
## not included in lpjml_soiltexture.                                         ##
if (
  (anyNA(lpjml_soilph) && exists("lpjml_soilph_format")) ||
  anyNA(lpjml_soiltexture)
) {
  cat(
    "Derived soil texture has missing value in",
    length(which(is.na(lpjml_soiltexture))), "cell(s).\n"
  )
  if (exists("lpjml_soilph_format")) {
    cat(
      "Derived soil pH has missing value in",
      length(which(is.na(lpjml_soilph))), "cell(s).\n"
    )
  }
  if (max_search > 0) {
    cat("Trying to gap-fill\n")
  } else {
    warning(
      "Gap-filling is prevented because max_search is set to zero",
      call. = FALSE, immediate. = TRUE
    )
  }
  if (quicksearch) {
    # Create raster for faster search
    soilraster <- soilphraster <- gridraster
    soilraster[] <- NA
    soilraster[cellFromXY(soilraster, griddata)] <- lpjml_soiltexture
    soilphraster[] <- NA
    soilphraster[cellFromXY(soilraster, griddata)] <- lpjml_soilph
  }
  lpjml_soiltexture_gapfilled <- lpjml_soiltexture
  lpjml_soilph_gapfilled <- lpjml_soilph
  if (exists("lpjml_soilph_format")) {
    nacells <- which(is.na(lpjml_soiltexture_gapfilled) |
      is.na(lpjml_soilph_gapfilled)
    )
  } else {
    nacells <- which(is.na(lpjml_soiltexture_gapfilled))
  }
  if (max_search <= 0) {
    nacells <- integer(0)
  }
} else {
  nacells <- integer(0)
}
start_time <- proc.time()["elapsed"]
if (num_cluster > 1 && length(nacells) > 0) {
  # Randomly shuffle cells in nacells to avoid that cell clusters in far-off
  # locations are assigned to the same task
  set.seed(length(nacells))
  nacells <- sample(nacells, length(nacells))
  if (parallel_local) {
    # Reduce number of CPUs used because this part has higher memory requirement
    num_cluster <- ceiling(num_cluster / 2)
    registerDoParallel(num_cluster)
  }
  # Split nacells into packages for parallel execution. Each package has between
  # 10 and 250 cells depending on number of missing cells and number of parallel
  # tasks
  tmplist <- list()
  for (c in seq(
    1,
    length(nacells),
    by = min(max(10, length(nacells) %/% num_cluster), 150)
  )) {
    index <- seq(
      c,
      min(
        c + min(max(10, length(nacells) %/% num_cluster), 150) - 1,
        length(nacells)
      )
    )
    tmplist[[length(tmplist) + 1]] <- nacells[index]
  }
  nacells <- tmplist
  cat(
    "Parallel execution of gap-filling on", num_cluster, "tasks.\n",
    "Status updates may be out of sequence.\n"
  )
} else {
  nacells <- list(nacells)
}
if (parallel_mpi) {
  # Reduce amount of data that needs to be transferred over the network to
  # parallel tasks by not exporting high-resolution HWSD source data
  noexport <- "hwsd"
} else {
  noexport <- NULL
}
gapfill_loop <- foreach(
  cellgroup = seq_len(length(nacells)),
  .inorder = FALSE,
  .combine = rbind,
  .verbose = (parallel_mpi && num_cluster > 1),
  .packages = c("raster", "geosphere"),
  .noexport = noexport
) %dopar% {
  if (!exists("hwsd")) {
    # Reload hwsd if not available on task.
    hwsd <- raster(hwsd_raster)
    # Set projection.
    proj4string(hwsd) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    # Load hwsd into RAM, skip this part if your setup does not have enough RAM
    if (!inMemory(hwsd)) {
      hwsd <- readAll(hwsd)
    }
    rasterOptions(maxmemory = 16e9, memfrac = 0.8)
  } else if (!inMemory(hwsd)) {
    # Load hwsd into RAM, skip this part if your setup does not have enough RAM
    hwsd <- readAll(hwsd)
  }
  # Table which will collect results of all cells in one package
  results_table <- data.frame(
    cell = integer(0),
    soiltexture_gapfilled = integer(0),
    soilph_gapfilled = double(0),
    soil_ds = double(0),
    soilph_source = integer(0),
    soiltexture_nvalid = integer(0),
    soiltexture_mindist = double(0),
    soiltexture_maxdist = double(0),
    soiltexture_window = double(0),
    soilph_nvalid = integer(0),
    soilph_mindist = double(0),
    soilph_maxdist = double(0),
    soilph_window = double(0),
    stringsAsFactors = FALSE
  )
  cell <- -1
  if (length(nacells[[cellgroup]]) > 0) {
    for (cell in nacells[[cellgroup]]) {
      cell_table <- data.frame(
        cell = cell,
        soiltexture_gapfilled = NA,
        soilph_gapfilled = NA,
        soil_ds = NA,
        soilph_source = NA,
        soiltexture_nvalid = NA,
        soiltexture_mindist = NA,
        soiltexture_maxdist = NA,
        soiltexture_window = NA,
        soilph_nvalid = NA,
        soilph_mindist = NA,
        soilph_maxdist = NA,
        soilph_window = NA,
        stringsAsFactors = FALSE
      )
      # Search window
      dist <- c(lon = 0, lat = 0)
      # Search window increment is set to resolution of gridraster. This could
      # be set to res(hwsd) instead but would slow down search substantially
      search_incr <- res(gridraster)
      while (all(res(gridraster) / 2 + dist + search_incr < max_search) &&
        (is.na(lpjml_soiltexture_gapfilled[cell]) ||
        (is.na(lpjml_soilph_gapfilled[cell]) && exists("lpjml_soilph_format")))
      ) {
        # Need to gap-fill soil texture class or soil pH
        # Expand search window
        dist <- dist + search_incr
        # Find cells in box with size of LPJmL grid resolution + dist
        boxextent <- extentFromCells(gridraster, gridraster_index[cell]) +
          dist * 2
        if (quicksearch) {
          # To speed up search use lpjml_soiltexture/lpjml_soilph instead of
          # HWSD raster to search for cells with values.
          # This may expand the search window too much in some cases if HWSD has
          # data outside of cells included in the LPJmL grid.
          # Do not use quicksearch if you expand window by res(hwsd) instead of
          # res(gridraster).
          if (is.na(lpjml_soiltexture_gapfilled[cell])) {
            # If soil texture needs gap-filling check soil texture data in
            # window
            cellsearch <- crop(soilraster, boxextent)
          } else {
            # If only soil pH needs gap-filling check soil pH data in window
            cellsearch <- crop(soilphraster, boxextent)
          }
          if (xmin(boxextent) < -180) {
            boxextent2 <- extent(
              xmin(boxextent) + 360,
              180,
              ymin(boxextent),
              ymax(boxextent)
            )
            if (xmax(boxextent2) - xmin(boxextent2) > xres(hwsd) / 2) {
              # Make sure boxextent2 is at least one cell wide and not just
              # numerical inaccuracies
              if (is.na(lpjml_soiltexture_gapfilled[cell])) {
                cellsearch2 <- crop(soilraster, boxextent2)
              } else {
                cellsearch2 <- crop(soilphraster, boxextent2)
              }
            } else {
              cellsearch2 <- raster(matrix(c(NA, NA)))
            }
            rm(boxextent2)
          } else if (xmax(boxextent) > 180) {
            boxextent2 <- extent(
              -180,
              xmax(boxextent) - 360,
              ymin(boxextent),
              ymax(boxextent)
            )
            if (xmax(boxextent2) - xmin(boxextent2) > xres(hwsd) / 2) {
              # Make sure boxextent2 is at least one cell wide and not just
              # numerical inaccuracies.
              if (is.na(lpjml_soiltexture_gapfilled[cell])) {
                cellsearch2 <- crop(soilraster, boxextent2)
              } else {
                cellsearch2 <- crop(soilphraster, boxextent2)
              }
            } else {
              cellsearch2 <- raster(matrix(c(NA, NA)))
            }
            rm(boxextent2)
          } else {
            cellsearch2 <- raster(matrix(c(NA, NA)))
          }
          if (all(is.na(values(cellsearch))) &&
            all(is.na(values(cellsearch2)))
          ) {
            # No cells in soilraster/soilphraster have soil code, assume that
            # HWSD has no data either and skip to next iteration with larger
            # search window
            rm(cellsearch, cellsearch2, boxextent)
            next
          }
          rm(cellsearch, cellsearch2)
        }
        # Raster package may create temporary files that can take up substantial
        # disk space. Keep track to delete them.
        raster_names <- character(0)
        # Now check actual HWSD data.
        cell_hwsd <- crop(hwsd, boxextent)
        if (fromDisk(cell_hwsd)) {
          raster_names <- c(raster_names, cell_hwsd@file@name)
        }
        # If box crosses 180° E/W line, find pixels across the line
        if (xmin(boxextent) < -180) {
          boxextent2 <- extent(
            xmin(boxextent) + 360,
            180,
            ymin(boxextent),
            ymax(boxextent)
          )
          cell_hwsd2 <- crop(hwsd, boxextent2)
          if (fromDisk(cell_hwsd2)) {
            raster_names <- c(raster_names, cell_hwsd2@file@name)
          }
          rm(boxextent2)
        } else if (xmax(boxextent) > 180) {
          boxextent2 <- extent(
            -180,
            xmax(boxextent) - 360,
            ymin(boxextent),
            ymax(boxextent)
          )
          cell_hwsd2 <- crop(hwsd, boxextent2)
          if (fromDisk(cell_hwsd2)) {
            raster_names <- c(raster_names, cell_hwsd2@file@name)
          }
          rm(boxextent2)
        } else {
          cell_hwsd2 <- integer(0)
        }
        cell_mu <- unique(unique(cell_hwsd), unique(cell_hwsd2))
        if (length(intersect(cell_mu, att_mu)) == 0) {
          # HWSD has no data for LPJmL cell, skip to next iteration with larger
          # search window.
          rm(cell_hwsd, cell_hwsd2, boxextent, cell_mu)
          # Clean up temporary raster files.
          if (length(raster_names) > 0) {
            file.remove(raster_names)
            raster_names <- sub(".grd$", ".gri", raster_names)
            file.remove(raster_names)
            rm(raster_names)
          }
          next
        }
        cell_mu <- intersect(cell_mu, att_mu)
        # Extract only cells with valid soil information from search window.
        cell_hwsd_valid <-  which(cell_hwsd[] %in% att_mu)
        cell_hwsd_valid_area <- cellarea(
          yFromCell(cell_hwsd, cell_hwsd_valid),
          xres(cell_hwsd),
          yres(cell_hwsd)
        )
        cell_hwsd_valid_coords <- xyFromCell(cell_hwsd, cell_hwsd_valid)
        cell_hwsd_valid <- cell_hwsd[cell_hwsd_valid]
        rm(cell_hwsd)
        # Append values across the 180° E/W line.
        if (xmin(boxextent) < -180 || xmax(boxextent) > 180) {
          cell_hwsd2_valid <-  which(cell_hwsd2[] %in% att_mu)
          cell_hwsd2_valid_area <- cellarea(
            yFromCell(cell_hwsd2, cell_hwsd2_valid),
            xres(cell_hwsd2),
            yres(cell_hwsd2)
          )
          cell_hwsd2_valid_coords <- xyFromCell(cell_hwsd2, cell_hwsd2_valid)
          cell_hwsd2_valid <- cell_hwsd2[cell_hwsd2_valid]
          # Append to main data
          cell_hwsd_valid <- c(cell_hwsd_valid, cell_hwsd2_valid)
          cell_hwsd_valid_area <- c(cell_hwsd_valid_area, cell_hwsd2_valid_area)
          cell_hwsd_valid_coords <- rbind(
            cell_hwsd_valid_coords,
            cell_hwsd2_valid_coords,
            deparse.level = 0
          )
          rm(cell_hwsd2, cell_hwsd2_valid, cell_hwsd2_valid_area,
             cell_hwsd2_valid_coords)
        }
        # Clean up temporary raster files.
        if (length(raster_names) > 0) {
          file.remove(raster_names)
          raster_names <- sub(".grd$", ".gri", raster_names)
          file.remove(raster_names)
          rm(raster_names)
        }
        # Inverse distance weighting of cells in search window. This reduces the
        # cell area of cells that are farther away
        cell_hwsd_valid_dist <- distHaversine(
          cell_hwsd_valid_coords,
          griddata[cell, ],
          r = earthradius
        )
        rm(cell_hwsd_valid_coords)
        if (idw_power_par > 0) {
          cell_hwsd_valid_area <- cell_hwsd_valid_area /
            (cell_hwsd_valid_dist ^ idw_power_par)
        }
        # Attribute table for cells in window
        index <- which(hwsd_attribute_table$MU_GLOBAL %in% cell_mu)
        cell_attribute_table <- hwsd_attribute_table[index, ]
        rm(index)
        # USDA texture classes in cells in window
        cell_usda <- unique(cell_attribute_table$T_USDA_TEX_CLASS)
        # Areas per USDA texture class plus rock and ice
        cell_usda_area <- list()
        for (t in lpjml_soiltypes) {
          cell_usda_area[[t]] <- double(0)
        }
        mu_area <- double(length(cell_mu))
        names(mu_area) <- cell_mu
        for (usda in na.omit(cell_usda)) {
          # Translate texture class index from column "T_USDA_TEX_CLASS" to
          # texture class name in HWSD and then to texture class name in LPJmL
          codeindex <- match(usda, hwsd_usda_classes$CODE)
          nameindex <- match(
            hwsd_usda_classes$VALUE[codeindex],
            usda_to_lpjml$USDA
          )
          lpj_usda <- usda_to_lpjml$LPJmL[nameindex]
          rm(codeindex, nameindex)
          # Sum up all areas with "usda" texture class, group by mapping unit
          subset_usda <- which(cell_attribute_table$T_USDA_TEX_CLASS == usda)
          for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
            subset_usda_mu <- intersect(
              which(cell_attribute_table$MU_GLOBAL == mu),
              subset_usda
            )
            # Using sum(SHARE) because there may be more than one soil with the
            # same texture class within a mapping unit
            usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
            mucells <- which(cell_hwsd_valid == mu)
            tmp_area <- sum(cell_hwsd_valid_area[mucells] * usda_mu_share)
            mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
            if (mu %in% names(cell_usda_area[[lpj_usda]])) {
              cell_usda_area[[lpj_usda]][as.character(mu)] <-
                cell_usda_area[[lpj_usda]][as.character(mu)] + tmp_area
            } else {
              cell_usda_area[[lpj_usda]] <- c(
                cell_usda_area[[lpj_usda]],
                tmp_area
              )
              last <- length(cell_usda_area[[lpj_usda]])
              names(cell_usda_area[[lpj_usda]])[last] <- mu
              rm(last)
            }
            rm(subset_usda_mu, usda_mu_share, mucells, tmp_area)
          }
        }
        rm(subset_usda)

        # Rock (classified as SU_SYM90 == "RK" in HWSD)
        subset_usda <- which(cell_attribute_table$SU_SYM90 == "RK")
        for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
          subset_usda_mu <- intersect(
            which(cell_attribute_table$MU_GLOBAL == mu),
            subset_usda
          )
          usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
          mucells <- which(cell_hwsd_valid == mu)
          tmp_area <- sum(cell_hwsd_valid_area[mucells] * usda_mu_share)
          mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
          if (mu %in% names(cell_usda_area[[lpjml_rock]])) {
            cell_usda_area[[lpjml_rock]][as.character(mu)] <-
              cell_usda_area[[lpjml_rock]][as.character(mu)] + tmp_area
          } else {
            cell_usda_area[[lpjml_rock]] <- c(
              cell_usda_area[[lpjml_rock]],
              tmp_area
            )
            last <- length(cell_usda_area[[lpjml_rock]])
            names(cell_usda_area[[lpjml_rock]])[last] <- mu
            rm(last)
          }
          rm(subset_usda_mu, usda_mu_share, mucells, tmp_area)
        }
        rm(subset_usda)

        # Ice (classified as SU_SYM90 == "GG" in HWSD)
        subset_usda <- which(cell_attribute_table$SU_SYM90 == "GG")
        for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
          subset_usda_mu <- intersect(
            which(cell_attribute_table$MU_GLOBAL == mu),
            subset_usda
          )
          usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
          mucells <- which(cell_hwsd_valid == mu)
          tmp_area <- sum(cell_hwsd_valid_area[mucells] * usda_mu_share)
          mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
          if (mu %in% names(cell_usda_area[[lpjml_ice]])) {
            cell_usda_area[[lpjml_ice]][as.character(mu)] <-
              cell_usda_area[[lpjml_ice]][as.character(mu)] + tmp_area
          } else {
            cell_usda_area[[lpjml_ice]] <- c(
              cell_usda_area[[lpjml_ice]],
              tmp_area
            )
            last <- length(cell_usda_area[[lpjml_ice]])
            names(cell_usda_area[[lpjml_ice]])[last] <- mu
            rm(last)
          }
          rm(subset_usda_mu, usda_mu_share, mucells, tmp_area)
        }
        rm(subset_usda)

        # Include dunes/shifting sand in sand
        subset_usda <- which(cell_attribute_table$SU_SYM90 == "DS")
        for (mu in unique(cell_attribute_table$MU_GLOBAL[subset_usda])) {
          subset_usda_mu <- intersect(
            which(cell_attribute_table$MU_GLOBAL == mu),
            subset_usda
          )
          usda_mu_share <- sum(cell_attribute_table$SHARE[subset_usda_mu])
          mucells <- which(cell_hwsd_valid == mu)
          tmp_area <- sum(cell_hwsd_valid_area[mucells] * usda_mu_share)
          mu_area[as.character(mu)] <- mu_area[as.character(mu)] + tmp_area
          lpjml_soil_ds[cell] <- lpjml_soil_ds[cell] + tmp_area
          if (mu %in% names(cell_usda_area[[lpjml_sand]])) {
            cell_usda_area[[lpjml_sand]][as.character(mu)] <-
              cell_usda_area[[lpjml_sand]][as.character(mu)] + tmp_area
          } else {
            cell_usda_area[[lpjml_sand]] <- c(
              cell_usda_area[[lpjml_sand]],
              tmp_area
            )
            last <- length(cell_usda_area[[lpjml_sand]])
            names(cell_usda_area[[lpjml_sand]])[last] <- mu
            rm(last)
          }
          rm(subset_usda_mu, usda_mu_share, mucells, tmp_area)
        }
        rm(subset_usda)

        # Determine dominant USDA soil texture class based on area covered by
        # classes in cell.
        # Since several classes may have the same area share shuffle (sample())
        # cell_usda_area to avoid bias from the sequence of soil types
        # set.seed should allow for reproducibility despite random shuffling of
        # values.
        set.seed(cell)
        if (any(sapply(cell_usda_area, sum) > 0)) {
          if (rock_ice_shared) {
            # Treat rock and ice like any other soil
            dominant_usda <- names(
              which.max(sample(sapply(cell_usda_area, sum)))
            )
          } else {
            # Only assign rock and ice if no other soil type is present
            if (any(sapply(cell_usda_area[lpjml_no_rock_ice], sum) > 0)) {
              # First try without rock and ice
              dominant_usda <- names(
                which.max(
                  sample(sapply(cell_usda_area[lpjml_no_rock_ice], sum))
                )
              )
            } else {
              # Try with rock and ice
              dominant_usda <- names(
                which.max(sample(sapply(cell_usda_area, sum)))
              )
            }
          }
        } else {
          # No soil texture class has any areas, go to next iteration.
          rm(cell_hwsd_valid, cell_hwsd_valid_area, cell_hwsd_valid_dist,
             cell_attribute_table, cell_usda, cell_usda_area, mu_area)
          next
        }
        if (is.na(lpjml_soiltexture_gapfilled[cell])) {
          # Only set soil texture if missing.
          lpjml_soiltexture_gapfilled[cell] <- match(
            dominant_usda,
            lpjml_soiltypes
          )
          # Determine share of dunes/shifting sand in cell
          lpjml_soil_ds[cell] <- lpjml_soil_ds[cell] / sum(mu_area)
          # Collect data for return to control task
          cell_table$cell <- cell
          cell_table$soiltexture_gapfilled <- lpjml_soiltexture_gapfilled[cell]
          cell_table$soil_ds <- lpjml_soil_ds[cell]
          cell_table$soiltexture_nvalid <- length(cell_hwsd_valid)
          cell_table$soiltexture_mindist <- min(cell_hwsd_valid_dist)
          cell_table$soiltexture_maxdist <- max(cell_hwsd_valid_dist)
          cell_table$soiltexture_window <- max(res(gridraster) + dist * 2)
        } else {
          # Otherwise only soil pH is missing, read texture class from
          # lpjml_soiltexture_gapfilled[cell]
          dominant_usda <- lpjml_soiltypes[lpjml_soiltexture_gapfilled[cell]]
        }
        # Dominant mu in dominant USDA. Use sampling to avoid bias from sequence
        # of soil types.
        if (length(cell_usda_area[[dominant_usda]]) > 1) {
          dominant_mu_in_class <- names(
            which.max(sample(cell_usda_area[[dominant_usda]]))
          )
        } else {
          dominant_mu_in_class <- names(cell_usda_area[[dominant_usda]])
        }
        # Now determine pH value for the assigned soil texture. This is only
        # done if lpjml_soilph_format exists, i.e. if file creation has not been
        # skipped.
        if (exists("lpjml_soilph_format") &&
          is.na(lpjml_soilph_gapfilled[cell])
        ) {
          # Translate LPJmL texture class name back to texture index from column
          # T_USDA_TEX_CLASS, may be more than one USDA class
          nameindex <- which(usda_to_lpjml$LPJmL == dominant_usda)
          codeindex <- match(
            usda_to_lpjml$USDA[nameindex],
            hwsd_usda_classes$VALUE
          )
          dominant_usda_codes <- hwsd_usda_classes$CODE[codeindex]
          rm(codeindex, nameindex)
          # Attribute rows with matching dominant_usda_codes
          subset_usda <- which(cell_attribute_table$T_USDA_TEX_CLASS %in%
            dominant_usda_codes
          )
          # Attribute rows with matching mapping unit mu
          subset_mu <- which(cell_attribute_table$MU_GLOBAL ==
            as.integer(dominant_mu_in_class)
          )
          # Combination of texture class and mapping unit
          subset_usda_mu <- intersect(subset_usda, subset_mu)
          # Attribute rows with pH value
          subset_ph <- which(!is.na(cell_attribute_table$T_PH_H2O))
          # Columns to copy from attribute table
          cols <- c("ID", "SHARE", "T_PH_H2O", "MU_GLOBAL")
          if (any(!is.na(cell_attribute_table$T_PH_H2O[subset_usda_mu]))) {
            # Use pH from same texture class and same mapping unit.
            selected <- intersect(subset_ph, subset_usda_mu)
            tmptable <- cell_attribute_table[selected, cols]
            # Sort by SHARE and use random value to break ties.
            r <- order(
              tmptable[, "SHARE"],
              sample(nrow(tmptable)),
              decreasing = TRUE
            )[1]
            lpjml_soilph_gapfilled[cell] <- tmptable[r, "T_PH_H2O"]
            lpjml_soilph_source[cell] <- 1
            rm(selected, tmptable, r)
          } else if (any(!is.na(cell_attribute_table$T_PH_H2O[subset_usda]))) {
            # Use pH from same texture class
            selected <- intersect(subset_ph, subset_usda)
            tmptable <- cell_attribute_table[selected, cols]
            # Append area (since different mapping units may be combined)
            muindex <- match(tmptable$MU_GLOBAL, names(mu_area))
            tmptable <- cbind(
              tmptable,
              area = mu_area[muindex] * tmptable$SHARE
            )
            # Sort by area and use random value to break ties.
            r <- order(
              tmptable[, "area"],
              sample(nrow(tmptable)),
              decreasing = TRUE
            )[1]
            lpjml_soilph_gapfilled[cell] <- tmptable[r, "T_PH_H2O"]
            lpjml_soilph_source[cell] <- 2
            rm(selected, tmptable, muindex, r)
          } else if (any(!is.na(cell_attribute_table$T_PH_H2O[subset_mu]))) {
            # Use pH from same mapping unit.
            selected <- intersect(subset_ph, subset_mu)
            tmptable <- cell_attribute_table[selected, cols]
            # Sort by SHARE and use random value to break ties.
            r <- order(
              tmptable[, "SHARE"],
              sample(nrow(tmptable)),
              decreasing = TRUE
            )[1]
            lpjml_soilph_gapfilled[cell] <- tmptable[r, "T_PH_H2O"]
            lpjml_soilph_source[cell] <- 3
            rm(selected, tmptable, r)
          } else if (lpjml_soiltexture_gapfilled[cell] %in%
            grep("rock|ice", lpjml_soiltypes, ignore.case = TRUE)
          ) {
            # Set pH value for rock and ice to 7.
            lpjml_soilph_gapfilled[cell] <- 7
            lpjml_soilph_source[cell] <- 4
          } else if (cell_attribute_table[subset_mu, "SU_SYM90"] == "DS") {
            # Set pH value for dunes/shifting sand.
            lpjml_soilph_gapfilled[cell] <- 7
            lpjml_soilph_source[cell] <- 4
          } else {
            message(
              "No pH value for cell ", cell,
              " with dominant soil texture ", dominant_usda
            )
          }
          if (!is.na(lpjml_soilph_gapfilled[cell])) {
            # Collect data for return to control task
            cell_table$cell <- cell
            cell_table$soilph_gapfilled <- lpjml_soilph_gapfilled[cell]
            cell_table$soilph_source <- lpjml_soilph_source[cell]
            cell_table$soilph_nvalid <- length(cell_hwsd_valid)
            cell_table$soilph_mindist <- min(cell_hwsd_valid_dist)
            cell_table$soilph_maxdist <- max(cell_hwsd_valid_dist)
            cell_table$soilph_window <- max(res(gridraster) + dist * 2)
          }
        } # End of soil pH processing
      } # End of while loop (window search)

      if (!allow_skip && (is.na(lpjml_soiltexture_gapfilled[cell]) ||
        (exists("lpjml_soilph_format") && is.na(lpjml_soilph_gapfilled[cell])))
      ) {
        warning(
          toString(
            c(
              ifelse(
                is.na(lpjml_soiltexture_gapfilled[cell]),
                "soil texture",
                ""
              ),
              ifelse(
                exists("lpjml_soilph_format") &&
                  is.na(lpjml_soilph_gapfilled[cell]),
                "soil pH value",
                ""
              )
            )
          ),
          " still NA after gap-filling in cell ", cell, " (",
          toString(
            paste0(
              format(griddata[cell, ], nsmall = 3, trim = TRUE),
              c("E", "N")
            )
          ),
          ")",
          call. = FALSE,
          immediate. = TRUE
        )
        # If there are still NA values in any cells at this point you have two
        # options:
        # 1) Increase max_search
        # 2) Define some default value to use for cells for which no soil
        # texture / soil pH can be determined from HWSD
      }
      rm(cell_hwsd_valid, cell_hwsd_valid_area, cell_hwsd_valid_dist,
         dominant_mu_in_class, dominant_usda, cell_attribute_table, cell_usda,
         cell_usda_area, mu_area)
      gc(full = FALSE)
      if (!is.na(cell_table$cell)) {
        results_table <- rbind(results_table, cell_table, deparse.level = 0)
      }
    } # End of loop over cells in group
    if (!parallel_mpi) {
      cat(
        "Finished",
        round(
          which(
            sapply(
              nacells,
              function(indata, search) search %in% indata, search = cell
            )
          ) / length(nacells) * 100
        ),
        "% of cells requiring gap-filling.\n"
      )
    }
  }
  # Return results_table to master task
  results_table
}
  
if ((exists("lpjml_soilph_format") && anyNA(lpjml_soilph)) ||
  anyNA(lpjml_soiltexture)
) {
  missing_texture <- which(is.na(lpjml_soiltexture))
  # Check validity of gapfill_loop
  if (exists("lpjml_soilph_format")) {
    missing_ph <- which(is.na(lpjml_soilph) | is.na(lpjml_soiltexture))
    if (max_search > 0 && nrow(gapfill_loop) !=  length(missing_ph)) {
      stop(
        "Unexpected length of gapfill_loop: ", nrow(gapfill_loop),
        " != ", length(missing_ph),
        ifelse(
          num_cluster > 1,
          "\nThis may indicate that some parallel tasks have failed.",
          ""
        )
      )
    }
    if (max_search > 0 && !all(missing_ph %in% gapfill_loop[, "cell"])) {
      stop(
        "Mismatch between cells included in gapfill_loop and cells missing ",
        "in lpjml_soilph/lpjml_soiltexture"
      )
    }
    rm(missing_ph)
  } else {
    if (max_search > 0 && nrow(gapfill_loop) != length(missing_texture)) {
      stop(
        "Unexpected length of gapfill_loop: ", nrow(gapfill_loop),
        " != ",
        length(missing_texture),
        ifelse(
          num_cluster > 1,
          "\nThis may indicate that some parallel tasks have failed.",
          ""
        )
      )
    }
    if (max_search > 0 &&
      !all(missing_texture %in% gapfill_loop[, "cell"])
    ) {
      stop(
        "Mismatch between cells included in gapfill_loop and cells missing ",
        "in lpjml_soiltexture"
      )
    }
  }
  if (max_search > 0) {
    index <- match(missing_texture, gapfill_loop[, "cell"])
    lpjml_soiltexture_gapfilled[missing_texture] <-
      gapfill_loop[index, "soiltexture_gapfilled"]
    rm(index)
    if (exists("lpjml_soilph_format")) {
      missing_ph <- which(is.na(lpjml_soilph))
      index <- match(missing_ph, gapfill_loop[, "cell"])
      lpjml_soilph_gapfilled[missing_ph] <-
        gapfill_loop[index, "soilph_gapfilled"]
      rm(missing_ph, index)
    }
    lpjml_soil_ds[gapfill_loop[, "cell"]] <- gapfill_loop[, "soil_ds"]
    lpjml_soilph_source[gapfill_loop[, "cell"]] <-
      gapfill_loop[, "soilph_source"]
  }
  lpjml_soiltexture <- lpjml_soiltexture_gapfilled
  # Update gap-filling log
  update_vals <- intersect(
    lpjml_soiltexture_gapfilling[, "cell"],
    gapfill_loop[, "cell"]
  )
  cols_texture <- c(
    "cell",
    "soiltexture_nvalid",
    "soiltexture_mindist",
    "soiltexture_maxdist",
    "soiltexture_window"
  )
  if (length(update_vals) > 0) {
    index_dst <- match(update_vals, lpjml_soiltexture_gapfilling[, "cell"])
    index_src <- match(update_vals, gapfill_loop[, "cell"])
    lpjml_soiltexture_gapfilling[index_dst, ] <-
      gapfill_loop[index_src, cols_texture]
    rm(update_vals, index_dst, index_src)
  }
  # Append gap-filling log
  append_vals <- setdiff(
    gapfill_loop[, "cell"],
    lpjml_soiltexture_gapfilling[, "cell"]
  )
  if (length(append_vals) > 0) {
    index_src <- match(append_vals, gapfill_loop[, "cell"])
    lpjml_soiltexture_gapfilling <- rbind(
      lpjml_soiltexture_gapfilling,
      gapfill_loop[index_src, cols_texture],
      deparse.level = 0
    )
    rm(append_vals, index_src)
  }
  if (exists("lpjml_soilph_format")) {
    lpjml_soilph <- lpjml_soilph_gapfilled
    # Update gap-filling log
    update_vals <- intersect(
      gapfill_loop[, "cell"],
      lpjml_soilph_gapfilling[, "cell"]
    )
    cols_ph <- c(
      "cell",
      "soilph_nvalid",
      "soilph_mindist",
      "soilph_maxdist",
      "soilph_window"
    )
    if (length(update_vals) > 0) {
      index_dst <- match(update_vals, lpjml_soilph_gapfilling[, "cell"])
      index_src <- match(update_vals, gapfill_loop[, "cell"])
      lpjml_soilph_gapfilling[index_dst, ] <- gapfill_loop[index_src, cols_ph]
      rm(update_vals, index_dst, index_src)
    }
    # Append gap-filling log
    append_vals <- setdiff(
      gapfill_loop[, "cell"],
      lpjml_soilph_gapfilling[, "cell"]
    )
    if (length(append_vals) > 0) {
      index_src <- match(append_vals, gapfill_loop[, "cell"])
      lpjml_soilph_gapfilling <- rbind(
        lpjml_soilph_gapfilling,
        gapfill_loop[index_src, cols_ph],
        deparse.level = 0
      )
      rm(append_vals, index_src)
    }
  }
  run_time <- proc.time()["elapsed"] - start_time
  cat(
    "Gap-filling took ",
    run_time %/% 3600, ":",
    formatC((run_time %% 3600) %/% 60, format = "d", flag = "0", width = 2),
    ":", formatC(run_time %% 60, format = "d", flag = "0", width = 2), "\n",
    sep = ""
  )
  rm(gapfill_loop, missing_texture)
}

################################################################################
## Write aggregated soil data to files.                                       ##
# Check for NA values first.
if (anyNA(lpjml_soiltexture)) {
  if (allow_skip) {
    cat(
      "Setting", length(which(is.na(lpjml_soiltexture))),
      "NA values in lpjml_soiltexture to", sQuote(0),
      "which will skip them in LPJmL simulations.\n"
    )
    lpjml_soiltexture[which(is.na(lpjml_soiltexture))] <- 0
  } else {
    stop(
      "lpjml_soiltexture has ", length(which(is.na(lpjml_soiltexture))),
      " NA values. No skipping allowed."
    )
  }
}
if (exists("lpjml_soilph_format") && anyNA(lpjml_soilph)) {
  if (allow_skip) {
    cat(
      "Setting", length(which(is.na(lpjml_soilph))),
      "cells in lpjml_soiltexture to", sQuote(0),
      "due to missing soil pH value.",
      "This will skip them in LPJmL simulations.\n"
    )
    lpjml_soiltexture[which(is.na(lpjml_soilph))] <- 0
    # Set pH to dummy value 7 to avoid read error in LPJmL.
    lpjml_soilph[which(is.na(lpjml_soilph))] <- 7
  } else {
    stop(
      "lpjml_soilph has ", length(which(is.na(lpjml_soilph))),
      " NA values. No skipping allowed."
    )
  }
}
# Make copies of variables and save to tmp_RData
for (copyvar in savevar) {
  assign(paste0("tmp_", copyvar), get(copyvar))
}
save(list = paste0("tmp_", savevar), file = tmp_RData)
rm(list = paste0("tmp_", savevar))
cat("Results after gap-filling saved to", sQuote(tmp_RData), "\n")

if (lpjml_soil_format == "BIN") {
  soiltexture_header <- create_header(
    name = lpjml_soil_headername,
    version = lpjml_soil_version,
    nyear = 1,
    ncell = length(lpjml_soiltexture),
    nbands = 1,
    cellsize_lon = gridheader$header["cellsize_lon"],
    scalar = 1,
    cellsize_lat = gridheader$header["cellsize_lat"],
    datatype = ifelse(lpjml_soil_version > 2, 0, 1)
  )
  write_header(lpjml_soiltexture_name, soiltexture_header)
  soiltexture_file <- file(lpjml_soiltexture_name, "ab")
  if (typeof(get_datatype(soiltexture_header)$type) == "raw") {
    writeBin(as.raw(lpjml_soiltexture), soiltexture_file, size = 1)
  } else if (typeof(get_datatype(soiltexture_header)$type) == "integer") {
    writeBin(
      as.integer(lpjml_soiltexture),
      soiltexture_file,
      size = get_datatype(soiltexture_header)$size
    )
  } else {
    writeBin(
      as.double(lpjml_soiltexture),
      soiltexture_file,
      size = get_datatype(soiltexture_header)$size
    )
  }
  close(soiltexture_file)
} else if (lpjml_soil_format == "RAW") {
  writeBin(as.raw(lpjml_soiltexture), lpjml_soiltexture_name, size = 1)
} else if (lpjml_soil_format == "CSV") {
  write.csv(
    matrix(lpjml_soiltexture, ncol = 1, dimnames = list(NULL, c("soilcode"))),
    file = lpjml_soiltexture_name,
    row.names = FALSE
  )
} else {
  # Assume raster format.
  soilraster <- gridraster
  soilraster[] <- NA
  soilraster[cellFromXY(soilraster, griddata)] <- lpjml_soiltexture
  # This will fail if lpjml_soil_format is not a valid format supported by the
  # raster package.
  writeRaster(
    soilraster,
    filename = lpjml_soiltexture_name,
    varname = "soilcode",
    datatype = "INT2S",
    NAflag = -999
  )
}
cat("Soil texture saved to", sQuote(lpjml_soiltexture_name), "\n")

if (exists("lpjml_soilph_format")) {
  if (lpjml_soilph_format == "BIN") {
    if (lpjml_soilph_version > 2) {
      scalar <- 1
    } else {
      scalar <- 0.01
    }
    soilph_header <- create_header(
      name = lpjml_soilph_headername,
      version = lpjml_soilph_version,
      nyear = 1,
      ncell = length(lpjml_soiltexture),
      nbands = 1,
      cellsize_lon = gridheader$header["cellsize_lon"],
      scalar = scalar,
      cellsize_lat = gridheader$header["cellsize_lat"],
      datatype = ifelse(lpjml_soil_version > 2, 3, 1)
    )
    write_header(lpjml_soilph_name, soilph_header)
    soilph_file <- file(lpjml_soilph_name, "ab")
    if (typeof(get_datatype(soilph_header)$type) == "raw") {
      writeBin(as.raw(round(lpjml_soilph / scalar)), soilph_file, size = 1)
    } else if (typeof(get_datatype(soilph_header)$type) == "integer") {
      writeBin(
        as.integer(round(lpjml_soilph / scalar)),
        soilph_file,
        size = get_datatype(soilph_header)$size
      )
    } else {
      writeBin(
        as.double(lpjml_soilph / scalar),
        soilph_file,
        size = get_datatype(soilph_header)$size
      )
    }
    close(soilph_file)
  } else if (lpjml_soilph_format == "CSV") {
    write.csv(
      matrix(lpjml_soilph, ncol = 1, dimnames = list(NULL, c("soilph"))),
      file = lpjml_soilph_name,
      row.names = FALSE
    )
  } else {
    # Assume raster format.
    soilraster <- gridraster
    soilraster[] <- NA
    soilraster[cellFromXY(soilraster, griddata)] <- lpjml_soilph
    # This will fail if lpjml_soilph_format is not a valid format supported by
    # the raster package.
    writeRaster(
      soilraster,
      filename = lpjml_soilph_name,
      varname = "soilph",
      datatype = "FLT4S",
      NAflag = -9999.99
    )
  }
  cat("Soil pH saved to", sQuote(lpjml_soilph_name), "\n")
}
################################################################################


################################################################################
## If running in parallel mode do some clean-up.                              ##
if (parallel_mpi) {
  # Release linked R instances and close MPI cluster.
  doMPI::closeCluster(cl)
  Rmpi::mpi.quit()
}
if (parallel_local) {
  # Release parallel cluster
  parallel::stopCluster(cl)
}
################################################################################
