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
## Polygon intersection can take very long, especially at high spatial        ##
## resolutions. Because of this the script is parallelized.                   ##
##                                                                            ##
## The parallelization mechanism is implemented through foreach which has     ##
## backends for several parallelization mechanisms. This script has options   ##
## for the doMPI backend (using MPI) and the doParallel backend. Further      ##
## are possible. The example below may not work on your system. Try to adjust ##
## parameters or set "cluster <- FALSE" to switch off parallelization.        ##
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
## GLWD Level 1 and 2                                                         ##
## Shape files per level                                                      ##
glwd1_name <- "glwd_1.shp"
glwd2_name <- "glwd_2.shp"
## GLWD classes to extract (see GLWD Documentation for full list)             ##
glwd_classes <- c(lakes = "Lake", rivers = "River")
## Version string (optional)
## Added to names of created files (resolution string added automatically)    ##
version_string <- "polygon-based"
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
library(sf)
library(lwgeom)
################################################################################

################################################################################
## Whether R is running on a cluster with multiple CPUs (used to determine    ##
## whether to try parallelization).                                           ##
## Set to FALSE to run in sequential mode and not try to parallelize.         ##
cluster <- TRUE
##                                                                            ##
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
      parallel_mpi <- TRUE
      if (num_cluster > 1) {
        # Script is using more than 1 CPU, so really run in parallel mode
        # Tell foreach to use MPI backend for parallelization
        doMPI::registerDoMPI(cl)
        cat("Running in parallel mode on", num_cluster, "worker nodes.\n")
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
## Load GLWD data.                                                           ##
# Projection string
ps <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
load_glwd <- function(glwd1_name, glwd2_name, ps, att_only = FALSE) {
  cat("Load GLWD Level 1 data from", sQuote(glwd1_name), "\n")
  glwd1_shape <- st_read(glwd1_name, quiet = TRUE)
  if (is.na(crs(glwd1_shape))) {
    # Assume longlat WGS84
    st_crs(glwd1_shape) <- ps
  }
  cat("Load GLWD Level 2 data from", sQuote(glwd2_name), "\n")
  glwd2_shape <- st_read(glwd2_name, quiet = TRUE)
  if (is.na(crs(glwd2_shape))) {
    # Assume longlat WGS84
    st_crs(glwd2_shape) <- ps
  }
  # Combine level 1 and 2 (select only ID and TYPE attributes)
  glwd_combined <- rbind(
    glwd1_shape[, c("GLWD_ID", "TYPE")],
    glwd2_shape[, c("GLWD_ID", "TYPE")]
  )
  if (any(! glwd_classes %in% glwd_combined$TYPE)) {
    warning(
      "glwd_classes ",
      toString(sQuote(setdiff(glwd_classes, glwd_combined$TYPE))),
      " not available in GLWD source data",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  # Check for invalid polygons (self-intersection)
  invalid <- which(!st_is_valid(glwd_combined))
  if (length(invalid) > 0 && !att_only) {
    warning(
      "There are ", length(invalid),
      " invalid polygons in glwd_combined. Trying to fix.",
      call. = FALSE,
      immediate. = TRUE
    )
    glwd_combined[invalid, ] <- st_make_valid(glwd_combined[invalid, ])
    # Check again
    invalid <- which(!st_is_valid(glwd_combined))
    if (length(invalid) > 0) {
      warning(
        length(invalid),
        " invalid polygons in glwd_combined could not be fixed.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }
  # Return combined dataset
  if (att_only) {
    # Only return attribute table without geometry
    glwd_combined <-  st_drop_geometry(glwd_combined)
    gc(reset = TRUE)
  }
  glwd_combined
}
################################################################################

################################################################################
## Load grid file.                                                            ##
## Functions read_header(), get_headersize(), get_datatype(), cellarea()      ##
## defined in lpjml_format_helper_functions.R                                 ##
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
gridarea <- cellarea(
  griddata[, "lat"],
  gridheader$header["cellsize_lon"],
  gridheader$header["cellsize_lat"]
)
close(gridfile)
# Create grid raster from coordinates
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
# Set projection
proj4string(gridraster) <- ps
# Try to correct numerical inaccuracies of grid settings
if (all(
  (1 / gridheader$header[c("cellsize_lon", "cellsize_lat")]) %% 1 < 1e-6 |
  (1 / gridheader$header[c("cellsize_lon", "cellsize_lat")]) %% 1 > (1 - 1e-6)
)) {
  res(gridraster) <- 1 / round(
    1 / gridheader$header[c("cellsize_lon", "cellsize_lat")]
  )
  extent(gridraster) <- alignExtent(
    extent(gridraster),
    raster(extent(-180, 180, -90, 90), res = res(gridraster))
  )
}
# Determine resolution string to be used in files created by this script
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
# Check if selected output format is compatible with grid file
if (output_format == "RAW" || (output_format == "BIN" && bintype < 3)) {
  if (gridheader$header["cellsize_lon"] != gridheader$header["cellsize_lat"]) {
    # Only bintype 3 supports different lon/lat resolution
    stop(
      "Different longitude and latitude resolutions are not supported by ",
      "output_format ", sQuote(output_format),
      ifelse(output_format == "BIN", paste(" with bintype", bintype), "")
    )
  }
}
# Check if files to be created by this script exist already.
if (length(grep("lake|river", names(glwd_classes), ignore.case = TRUE)) == 2) {
  outputname <- paste0(
    "glwd_lakes_and_rivers_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(output_format)
  )
  assign("lakes_and_rivers_name", outputname)
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
        any(
          outputheader$header[c("cellsize_lon", "cellsize_lat")] !=
          gridheader$header[c("cellsize_lon", "cellsize_lat")]
        )) {
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
for (type in names(glwd_classes)) {
  outputname <- paste0(
    "glwd_", type, "_",
    ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
    lpj_res_string,
    ".", tolower(output_format)
  )
  assign(paste0(type, "_name"), outputname)
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
        any(
          outputheader$header[c("cellsize_lon", "cellsize_lat")] !=
          gridheader$header[c("cellsize_lon", "cellsize_lat")]
        )) {
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
# Assign grid indices to grid
gridraster[cellFromXY(gridraster, griddata)] <-
  seq_len(gridheader$header["ncell"])
names(gridraster) <- "GridcellID"
################################################################################

################################################################################
## Generate a polygon intersection for each type of water body in             ##
## glwd_classes.                                                              ##
for (type in seq_along(glwd_classes)) {
  cat("Water body type", sQuote(glwd_classes[type]), "\n")
  # Load GLWD data
  glwd_combined <- load_glwd(glwd1_name, glwd2_name, ps, FALSE)
  # Select subset of polygons
  type_index <- which(glwd_combined$TYPE == glwd_classes[type])
  if (length(type_index) > 0) {
    cat(
      "Processing intersection between",
      length(type_index), sQuote(glwd_classes[type]), "polygons and",
      gridheader$header["ncell"], "grid cells\n"
    )
    # Polygon intersection with gridcell_polygons
    # Do in chunks to reduce memory requirements
    # Also, the current version of sf is extremely slow if trying to intersect
    # lakes with a global shape of grid cell polygons.
    if (exists("sf_use_s2")) {
      # You may switch off spherical geometry (s2) in newer versions of sf to
      # emulate the behaviour of older versions. There seem to be some
      # differences in processing speed as well. Uncomment the following line to
      # switch off s2 functionality.
      # sf_use_s2(FALSE)
    }
    # Extract polygons for type from glwd_combined
    type_polygons <- glwd_combined[type_index, ]
    # Clean up before hand-off to parallel tasks
    rm(glwd_combined)
    if (exists("type_grid_polygons")) {
      rm(type_grid_polygons)
    }
    gc(reset = TRUE)
    step <- 5000
    if (step > length(type_index) / num_cluster) {
      step <- round(length(type_index) / num_cluster)
    }
    # Time execution
    start <- proc.time()["elapsed"]
    type_intersection <- foreach(
      r = seq(1, length(type_index), by = step),
      .inorder = FALSE,
      .combine = rbind,
      .verbose = FALSE,
      .noexport = "glwd_combined",
      .multicombine = TRUE
    ) %dopar% {
    # Previously used classic for loop
    # for (r in seq(1, nrow(type_polygons), by = step)) {
      # Clean up type_intersection in foreach loop
      # Note: registerDoMPI and registerDoParallel appear to slightly differ in
      # terms of persistence of variables within parallel loops.
      if (exists("type_intersection")) {
        rm(type_intersection)
      }
      # Indices of water bodies processed in this loop iteration
      index <- seq(r, min(r + step - 1, nrow(type_polygons)))
      # If using sf with spherical geometry (sf_use_s2() == TRUE), run polygon
      # intersection for short chunks of water bodies because run time of
      # st_intersection is very sensitive to number of individual polygons
      # Generate grid cell polygons matching water bodies in index
      in_chunk <- integer(0)
      final_w <- index[length(index)]
      for (w in index) {
        if (w %% (length(type_index) %/% 100) == 0) {
          cat(
            round(w / length(type_index) * 100), "% after",
            round(proc.time()["elapsed"] -  start), "seconds\n"
          )
        }
        in_chunk <- c(in_chunk, w)
        # Derive spatial extent of water body
        type_box <- st_bbox(type_polygons[w, ])
        # Expand by buffer
        lower <- grep("min", names(type_box))
        upper <- grep("max", names(type_box))
        type_box[lower] <- type_box[lower] - max(res(gridraster)) / 2
        type_box[upper] <- type_box[upper] + max(res(gridraster)) / 2
        # Crop gridraster to water body
        type_extent <- extent(type_box[c("xmin", "xmax", "ymin", "ymax")])
        type_raster <- crop(gridraster, type_extent, snap = "out")
        # Convert cropped raster into polygon
        if (exists("type_grid_polygons")) {
          # If polygon exists already only convert cells with IDs not in polygon
          # yet
          done <- which(values(type_raster) %in% type_grid_polygons$GridcellID)
          type_raster[done] <- NA
          # Convert to polygons
          if (any(!is.na(values(type_raster)))) {
            tmp_polygons <- rasterToPolygons(type_raster)
            # Convert to sf object
            tmp_polygons <- as(tmp_polygons, "sf")
            # Add cellarea
            tmp_polygons <- cbind(
              tmp_polygons,
              GridcellArea = st_area(tmp_polygons)
            )
            # Append to existing type_grid_polygons
            type_grid_polygons <- rbind(
              type_grid_polygons,
              tmp_polygons,
              deparse.level = 0
            )
          }
        } else if (any(!is.na(values(type_raster)))) {
          tmp_polygons <- rasterToPolygons(type_raster)
          # Convert to sf object
          type_grid_polygons <- as(tmp_polygons, "sf")
          # Add cellarea
          type_grid_polygons <- cbind(
            type_grid_polygons,
            GridcellArea = st_area(type_grid_polygons)
          )
          rm(tmp_polygons)
        }
        # If using sf with spherical geometry do polygon intersection for each
        # water body separately.
        if (#exists("sf_use_s2") && sf_use_s2() &&
          exists("type_grid_polygons") &&
          (nrow(type_grid_polygons) > 1000 || w == final_w ||
            length(in_chunk) >= 50)
        ) {
          if (!exists("type_intersection")) {
            if (exists("sf_use_s2")) {
              type_intersection <- st_intersection(
                type_grid_polygons,
                type_polygons[in_chunk, ],
                dimension = "polygon"
              )
              # "dimension" parameter only available in sf version with s2
              # support
            } else {
              type_intersection <- st_intersection(
                type_grid_polygons,
                type_polygons[in_chunk, ]
              )
            }
          } else {
            if (exists("sf_use_s2")) {
              tmp_intersection <- st_intersection(
                type_grid_polygons,
                type_polygons[in_chunk, ],
                dimension = "polygon"
              )
            } else {
              tmp_intersection <- st_intersection(
                type_grid_polygons,
                type_polygons[in_chunk, ]
              )
            }
            if (nrow(tmp_intersection) > 0)
              type_intersection <- rbind(
                type_intersection,
                tmp_intersection,
                deparse.level = 0
              )
            rm(tmp_intersection)
          }
          rm(type_grid_polygons)
          in_chunk <- integer(0)
        }
      }
      # Return processed polygons to foreach collection
      if (exists("type_intersection"))
        type_intersection
      else
        NULL
    }
    cat(
      "Intersection finished after",
      round(proc.time()["elapsed"] -  start), "seconds.\n"
    )

    # Check for invalid polygons (self-intersection)
    invalid <- which(!st_is_valid(type_intersection))
    if (length(invalid) > 0) {
      warning(
        "There are ", length(invalid),
        " invalid polygons in intersection. Trying to fix.",
        call. = FALSE,
        immediate. = TRUE
      )
      type_intersection[invalid, ] <- st_make_valid(type_intersection[invalid, ])
      # Check again
      invalid <- which(!st_is_valid(type_intersection))
      if (length(invalid) > 0) {
        warning(
          length(invalid),
          " invalid polygons in intersection could not be fixed.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }
    # Add polygon areas
    type_intersection <- cbind(
      type_intersection,
      PolygonArea = st_area(type_intersection)
    )
    # Remove polygons with area == 0
    valid <- which(as.double(type_intersection$PolygonArea) > 0)
    type_intersection <- type_intersection[valid, ]
    # Vector with cell fractions covered by water body type
    type_fraction <- double(gridheader$header["ncell"])
    # First assign values in cells with only one water body
    tmptable <- data.frame(table(type_intersection$GridcellID))
    tmptable$Var1 <- as.integer(as.character(tmptable$Var1))
    grid_index <- which(
      type_intersection$GridcellID %in% tmptable$Var1[which(tmptable$Freq == 1)]
    )
    type_fraction[type_intersection$GridcellID[grid_index]] <- as.double(
      type_intersection$PolygonArea / type_intersection$GridcellArea
    )[grid_index]
    for (cell in tmptable$Var1[which(tmptable$Freq > 1)]) {
      grid_index <- which(type_intersection$GridcellID == cell)
      type_fraction[cell] <- as.double(
        sum(
          type_intersection$PolygonArea[grid_index] /
            type_intersection$GridcellArea[grid_index]
        )
      )
    }
    if (any(type_fraction > 1.00001)) {
      warning(
        length(which(type_fraction > 1.00001)),
        " cells exceed a cell fraction of 1. Maximum value: ",
        max(type_fraction),
        ". Setting to 1.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    type_fraction[which(type_fraction > 1)] <- 1
    rm(type_intersection)
  } else {
    # Vector with cell fractions covered by water body type (all values set
    # to 0)
    type_fraction <- double(gridheader$header["ncell"])
  }
  assign(paste0(names(glwd_classes)[type], "_fraction"), type_fraction)
  rm(type_fraction)
}
################################################################################

################################################################################
## Aggregate lake and river fractions if both have been extracted             ##
if (length(grep("lake|river", names(glwd_classes), ignore.case = TRUE)) == 2) {
  lakes_and_river_fraction <- get(
    paste0(
      grep("lake", names(glwd_classes), ignore.case = TRUE, value = TRUE),
      "_fraction"
    )
  ) + get(
    paste0(
      grep("river", names(glwd_classes), ignore.case = TRUE, value = TRUE),
      "_fraction"
    )
  )
  # Global sum
  if (output_format == "RAW" || (output_format == "BIN" && bintype < 3)) {
    global_lakes_and_rivers <- sum(lakes_and_river_fraction * gridarea)
    global_lakes_and_rivers_round <- sum(
      round(lakes_and_river_fraction, 2) * gridarea
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
  # Reuse name generated at the top
  outputname <- get("lakes_and_rivers_name")
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
      writeBin(as.raw(round(lakes_and_river_fraction / scalar)), outputfile)
    } else if (typeof(get_datatype(outputheader)$type) == "integer") {
      writeBin(
        as.integer(round(lakes_and_river_fraction / scalar)),
        outputfile,
        size = get_datatype(outputheader)$size,
        endian = outputheader$endian
      )
    } else if (typeof(get_datatype(outputheader)$type) == "double") {
      writeBin(
        as.double(lakes_and_river_fraction / scalar),
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
    # Assume output_format is a raster format supported by raster package
    outputraster <- raster(gridraster)
    outputraster[cellFromXY(outputraster, griddata)] <- lakes_and_river_fraction
    # The following will fail if output_format is not supported by the raster
    # package
    writeRaster(
      outputraster,
      filename = outputname,
      varname = "lakes_and_rivers_frac",
      longname = "cell fraction covered by lakes and rivers"
    )
  }
}
################################################################################

################################################################################
## Save individual water body types to files.                                 ##
for (type in names(glwd_classes)) {
  lpj_type_frac <- get(paste0(type, "_fraction"))
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
  # Reuse name generated at the top
  outputname <- get(paste0(type, "_name"))
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
    # Assume output_format is a raster format supported by raster package
    outputraster <- raster(gridraster)
    outputraster[cellFromXY(outputraster, griddata)] <- lpj_type_frac
    # The following will fail if output_format is not supported by the raster
    # package
    writeRaster(
      outputraster,
      filename = outputname,
      varname = paste0(type, "_frac"),
      longname = paste("cell fraction covered by", type)
    )
  }
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
