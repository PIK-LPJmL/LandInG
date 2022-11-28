################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Script to derive neighbour cells for neighbour irrigation.                 ##
## LPJmL allows irrigation water in a cell to be withdrawn from water bodies  ##
## in that cell plus one neighbour cell, suggesting water transfer by         ##
## conveyance systems or transportation by trucks over limited distances.     ##
## The neighbour cell selected is the cell within the search area with the    ##
## largest upstream area (as a proxy for large discharge).                    ##
## The search area may either be restricted to cells directly bordering the   ##
## cell or to cells within a certain radius.                                  ##
## Additionally, upstream and downstream cells of a cell may be excluded as   ##
## neighbour cells.                                                           ##
## Using a search radius larger than 1 cell is much slower than using only    ##
## direct neighbours. Therefore, the script allows parallelization.           ##
## The parallelization mechanism is implemented through foreach which has     ##
## backends for several parallelization mechanisms. This script has options   ##
## for the doMPI backend (using MPI) and the doParallel backend. Further      ##
## are possible. The example below may not work on your system. Try to adjust ##
## parameters or set "cluster <- FALSE" to switch off parallelization.        ##
##                                                                            ##
## Info:                                                                      ##
## Processing a global grid at 0.5° takes a few minutes on a single CPU for   ##
## "adjacent" search or "region" search with a 75km search radius.            ##
## Parallelization provides only marginal speed gains.                        ##
## Processing a global grid at 5 arcmin spatial resolution takes about 2 to 3 ##
## hours on a single CPU for "adjacent" search and about 2 hours with         ##
## parallelization on 48 CPUs for "region" search with a 75km search radius.  ##
################################################################################
# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Basic setup:                                                               ##
## Working directory:                                                         ##
## This is where outputs from this script will be saved.                      ##
drainagedir <- ""
if (nchar(drainagedir) > 0) {
  setwd(drainagedir)
}
## Whether R is running on a cluster with multiple CPUs (used to determine    ##
## whether to try parallelization).                                           ##
## Set to FALSE to run in sequential mode.                                    ##
cluster <- FALSE
##                                                                            ##
## Grid:                                                                      ##
gridname <- "ENTER PATH TO GRID INPUT FILE HERE"
## River routing (such as created by create_river_routing_input.sh):          ##
drainname <- "ENTER PATH TO DRAINAGE INPUT FILE HERE"
## Version string (optional string added to filenames of files created by     ##
## this script or by river_routing.R (spatial resolution is added             ##
## automatically).                                                            ##
version_string <- ""

## Search area, either "adjacent" for direct neighbour cells or "region" for  ##
## all cells within search radius (default: "region"):                        ##
search_area <- "region"
## Search radius in case of using "region" as search_area. Default: 75000 m   ##
search_radius <- 75000
## Exclude upstream cells as neighbour cells since they are on the same river ##
exclude_upstream <- TRUE
## Exclude downstream cells as neighbour cells since they are on the same     ##
## river (which may join with another tributary though).                      ##
exclude_downstream <- TRUE
## Inverse distance weighting (IDW) of neighbour cell upstream areas. This    ##
## allows to introduce a "penalty" for transporting water from further away.  ##
## Higher values of IDW power parameter result in bigger penalty.             ##
## Set idw_power_par to 0 to use no inverse distance weighting.               ##
## Only used with search_area "region".                                       ##
idw_power_par <- 2
################################################################################
## Format specifications for output file created:                             ##
## Format must be either LPJmL input format ("BIN", default) or "CSV" table.  ##
neighbour_format <- "BIN"
## LPJmL input format version, 2 or 3. Only version 3 allows longitude and    ##
## latitude resolution to differ.                                             ##
bintype <- 3
## Header name: Headers of LPJmL input files include a name. Header names are ##
## defined in /include/header.h of the LPJmL source code and do not usually   ##
## need to be changed.                                                        ##
neighbour_headername <- "LPJNIRR"
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
library(geosphere)
################################################################################


################################################################################
## Get grid data, river routing, and information on upstream and downstream   ##
## cells created by create_river_routing_input.sh and river_routing.R         ##
gridheader <- read_header(gridname)
gridfile <- file(gridname, "rb")
seek(gridfile, get_headersize(gridheader))
griddata <- matrix(
  readBin(
    gridfile,
    what = get_datatype(gridheader)$type,
    size = get_datatype(gridheader)$size,
    n = gridheader$header["ncell"] * gridheader$header["nbands"]
  ) * gridheader$header["scalar"],
  ncol = gridheader$header["nbands"],
  byrow = TRUE,
  dimnames = list(NULL, c("lon", "lat"))
)
close(gridfile)
# Determine name string for further filenames based on resolution.
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
# Derive grid raster:
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
# Fill with cell indices.
gridraster[cellFromXY(gridraster, griddata)] <- seq_len(
  gridheader$header["ncell"]
)
# River routing:
drainheader <- read_header(drainname)
drainfile <- file(drainname, "rb")
if (drainheader$header["version"] < 3) {
  # Default data type for version-1 or version-2 headers is incorrect for
  # drainage file.
  warning("Resetting default data type for drainheader to 4-byte integer")
  drainheader$header["datatype"] <- 2
}
seek(drainfile, get_headersize(drainheader))
draindata <- matrix(
  readBin(
    drainfile,
    what = get_datatype(drainheader)$type,
    size = get_datatype(drainheader)$size,
    n = drainheader$header["ncell"] * drainheader$header["nbands"]
  ) * drainheader$header["scalar"],
  ncol = drainheader$header["nbands"],
  byrow = TRUE,
  dimnames = list(NULL, c("nextcell", "distance"))
)
close(drainfile)
# Only nextcell required. Cell indices for nextcell start counting at 0, change
# to 1 to use as R indices; negative values indicate end cell.
nextcell <- ifelse(draindata[, "nextcell"] < 0, -1, draindata[, "nextcell"] + 1)
rm(draindata)

# Upstream and downstream cell lists (created by river_routing.R):
# Filename is constructed automatically from resolution.
drainage_celllists_RData <- paste0(
  "drainage_celllists_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string,
  ".RData"
)
if (file.exists(drainage_celllists_RData) &&
  (exclude_downstream || exclude_upstream)
) {
  print(
    paste(
      "Loading upstream and downstream cells lists from",
      sQuote(drainage_celllists_RData)
    )
  )
  before_load <- ls()
  load(drainage_celllists_RData)
  # Ensure consistency between data in RData file and grid used here.
  if (nrow(griddata) != nrow(drainage_griddata) ||
    any(griddata != drainage_griddata)
  ) {
    stop(
      paste(
        sQuote(drainage_celllists_RData),
        "has been created for a different grid. Please run 'river_routing.R'",
        "again with the correct grid",
        sQuote(gridname)
      )
    )
  }
  if (length(drainage_usclist) < gridheader$header["ncell"]) {
    # Make sure all cells have an entry even if they do not have upstream cells
    drainage_usclist[[gridheader$header["ncell"]]] <- integer(0)
  }
  if (length(drainage_dsclist) < gridheader$header["ncell"]) {
    # Make sure all cells have an entry even if they do not have downstream
    # cells
    drainage_dsclist[[gridheader$header["ncell"]]] <- integer(0)
  }
  # Ensure that same river routing was used
  if (!identical(drainage_nextcell, nextcell)) {
    stop(
      paste(
        "River routing used to create",
        sQuote(drainage_celllists_RData),
        "does not match",
        sQuote(drainname)
      )
    )
  }
  # Remove variables not needed for further processing
  rm(
    drainage_cellstoend,
    drainage_endcell,
    drainage_nextcell,
    drainage_griddata
  )
  gc()
} else if (exclude_downstream || exclude_upstream) {
  stop(
    paste(
      sQuote(drainage_celllists_RData),
      "does not exist. Please make sure to run 'river_routing.R' first."
    )
  )
}
# Upstream areas (created by river_routing.R):
# Filename is constructed automatically from resolution.
upstreamarea_RData <- paste0(
  "drainage_upstreamarea_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string,
  ".RData"
)
if (file.exists(upstreamarea_RData)) {
  gridarea <- cellarea(
    griddata[, "lat"],
    gridheader$header["cellsize_lon"],
    gridheader$header["cellsize_lat"]
  )
  cat("Loading upstream areas from", sQuote(upstreamarea_RData), "\n")
  load(upstreamarea_RData)
  if (nrow(griddata) != nrow(drainage_griddata) ||
    any(griddata != drainage_griddata)
  ) {
    stop(
      sQuote(upstreamarea_RData),
      " has been created for a different grid.\n",
      "Please run 'river_routing.R' again with the correct grid ",
      sQuote(gridname)
    )
  }
  # Ensure that same river routing was used.
  if (!identical(drainage_nextcell, nextcell)) {
    stop(
      "River routing used to create ",
      sQuote(upstreamarea_RData),
      " does not match ",
      sQuote(drainname)
    )
  }
  if (any(drainage_gridarea > gridarea)) {
    stop(
      "gridarea in ", sQuote(upstreamarea_RData),
      " does not match gridarea based on grid input file ",
      sQuote(gridname)
    )
  } else if (any(drainage_gridarea < gridarea)) {
    message(
      "Info: It appears that landfrac was accounted for in computing ",
      "upstream areas in ", sQuote(upstreamarea_RData)
    )
  }
  # Remove variables not needed for further processing
  rm(drainage_gridarea, drainage_griddata, drainage_nextcell)
  gc()
} else {
  stop(
    sQuote(upstreamarea_RData),
    " does not exist.\nPlease make sure to run 'river_routing.R' first."
  )
}
rm(nextcell) # No longer required

################################################################################
## Filename of created neighbour irrigation file.                             ##
neighbour_filename <- paste0(
  "neighbour_irrig_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string,
  switch(
    search_area,
    adjacent = "_adjacent",
    region = paste0("_", search_radius, "m_radius")
  ),
  ifelse(exclude_downstream, "_exclude_downstream", ""),
  ifelse(exclude_upstream, "_exclude_upstream", ""),
  ifelse(idw_power_par != 0 && search_area == "region", "_idw", ""),
  ".", tolower(neighbour_format)
)
if (file.exists(neighbour_filename)) {
  if (neighbour_format == "BIN") {
    neighbour_header <- read_header(neighbour_filename)
    if (neighbour_header$header["ncell"] != gridheader$header["ncell"] ||
      any(neighbour_header$header[c("cellsize_lon", "cellsize_lat")] !=
        gridheader$header[c("cellsize_lon", "cellsize_lat")]
      )
    ) {
      stop(
        sQuote(neighbour_filename),
        " exists but does not match grid file ",
        sQuote(gridname)
      )
    }
    if (gridheader$header["cellsize_lon"] !=
      gridheader$header["cellsize_lat"] &&
      bintype < 3
    ) {
      stop(
        "bintype set to ", bintype,
        " but different longitude and latitude resolutions are only allowed",
        " for bintype 3."
      )
    }
  }
  stop(
    sQuote(neighbour_filename),
    " exists already for the settings you have chosen.\n",
    "Change settings or rename/delete existing file to process again."
  )
}

################################################################################
## Search neighbour cells.                                                    ##
## Depending on the search_area setting, the script finds either all (up to)  ##
## 8 adjacent cells or all cells within search_radius.                        ##
## Depending on the settings exclude_downstream and exclude_upstream,         ##
## downstream and/or upstream cells may be removed from the list of potential ##
## neighbour cells. The cell with the largest upstream area is selected from  ##
## the list of potential neighbour cells. If two potential neighbour cells    ##
## have the exact same upstream area the closer one is chosen.                ##
cat(
  "Searching neighbour cells for grid", sQuote(gridname),
  "with", gridheader$header["ncell"], "cells.\n"
)
cat(
  "Neighbour cells must be",
  switch(
    search_area,
    adjacent = "directly adjacent cells. ",
    region = paste("cells within", search_radius, "m distance. ")
  )
)
if (exclude_downstream) {
  cat("Downstream cells are excluded in neighbour search. ")
}
if (exclude_upstream) {
  cat("Upstream cells are excluded in neighbour search. ")
}
if (idw_power_par != 0 && search_area == "region") {
  cat(
    "\nApplying inverse distance weighting to upstream areas in search radius",
    "with a power parameter of",
    idw_power_par
  )
} else {
  # Do not apply inverse distance weighting if only using adjacent cells.
  idw_power_par <- 0
}
cat("\n")
# Progress report settings. The "percentage done" steps depend on the number of
# cells but may be changed manually to provide more or less updates.
progress_step <- ifelse(gridheader$header["ncell"] < 100000, 10, 200)
progress <- round(
  seq(
    gridheader$header["ncell"] / progress_step,
    gridheader$header["ncell"],
    length.out = progress_step
  )
)
procstart <- proc.time()["elapsed"]
# Utility function to convert angle in degree to radian.
deg2rad <- function(x) {
  return(x * pi / 180)
}
# Parallelized loop using foreach. Packages of 1000 cells each are sent to each
# worker. Workers return results for their package and get the next package
# automatically until all cells have been processed.
# drainage_dsclist and drainage_usclist are not exported to reduce amount of
# data that needs to be sent to workers for MPI-based parallelization.
# The .verbose setting of foreach() is set to TRUE for parallel runs using MPI
# parallelization to give some progress report in the control task. It may be
# switched off by the user if such output is not required.
if (parallel_mpi) {
  noexport <- c("drainage_dsclist", "drainage_usclist")
} else {
  noexport <- NULL
}
sink_output <- parallel_mpi
cell_loop <- foreach(
  cell = seq(1, gridheader$header["ncell"], by = 1000),
  .inorder = FALSE,
  .combine = rbind,
  .verbose = (parallel_mpi && num_cluster > 1),
  .packages = c("raster", "geosphere"),
  .noexport = noexport
) %dopar% {
  if (sink_output) {
    # If running on multiple tasks with MPI-based parallelization redirect
    # output to task-specific file
    sinkWorkerOutput(
      paste0("neighbour_irrigation_worker_", mpi.comm.rank(0), ".out")
    )
    # Set up redirection only once
    sink_output <- FALSE
  }
  if (!exists("drainage_usclist") && (exclude_downstream || exclude_upstream)) {
    # Reload data from drainage_celllists_RData if necessary
    load(drainage_celllists_RData)
    if (length(drainage_usclist) < gridheader$header["ncell"]) {
      # Make sure all cells have an entry even if they do not have upstream
      # cells.
      drainage_usclist[[gridheader$header["ncell"]]] <- integer(0)
    }
    if (length(drainage_dsclist) < gridheader$header["ncell"]) {
      # Make sure all cells have an entry even if they do not have downstream
      # cells.
      drainage_dsclist[[gridheader$header["ncell"]]] <- integer(0)
    }
    # Remove variables not needed for further processing
    rm(
      drainage_cellstoend,
      drainage_endcell,
      drainage_nextcell,
      drainage_griddata
    )
    gc()
  }
  results_table <- cbind(cell = integer(0), neighbour = integer(0))
  for (c in seq(cell, min(cell + 999, gridheader$header["ncell"]))) {
    if (c %in% progress && !parallel_mpi) {
      # Give progress updates only if not running MPI parallel mode. The latter
      # redirects output to files.
      cat(
        round(c / gridheader$header["ncell"] * 100, 2),
        "% finished after",
        round(proc.time()["elapsed"] - procstart),
        "seconds\n"
      )
    }
    if (search_area == "adjacent") {
      # Use adjacent() from raster package to find adjacent cells (queen's case)
      cellindex <- adjacent(
        gridraster,
        cellFromXY(gridraster, griddata[c, c("lon", "lat")]),
        directions = 8,
        pairs = FALSE
      )
      neighbourhood <- gridraster[cellindex]
      rm(cellindex)
      # Filter ocean cells
      neighbourhood <- neighbourhood[which(!is.na(neighbourhood))]
    } else if (search_area == "region") {
      # Because calculating distance between cells is computationally expensive
      # reduce global grid to a smaller lon-by-lat box.
      # Rough estimate of search radius in degree based on location.
      # earthradius defined in lpjml_format_helper_functions.R
      search_radius_cell <- ceiling(
        c(
          lon = as.double(search_radius * 360 /
            (earthradius * 2 * pi * cos(deg2rad(griddata[c, "lat"])))
          ),
          lat = search_radius * 360 / (earthradius * 2 * pi)
        ) * 100
      ) / 100
      # Narrow down total grid to lon-by-lat box
      # Westside border
      lowlon <- (
        griddata[, "lon"] >= griddata[c, "lon"] - search_radius_cell["lon"]
      )
      # Eastside border
      uplon <- (
        griddata[, "lon"] <= griddata[c, "lon"] + search_radius_cell["lon"]
      )
      if (griddata[c, "lon"] > 0) {
        lowlon2 <- (
          griddata[, "lon"] >= griddata[c, "lon"] - search_radius_cell["lon"] -
          360
        ) # Also cross 180°W/E line
        uplon2 <- (
          griddata[, "lon"] <= griddata[c, "lon"] + search_radius_cell["lon"] -
          360
        ) # Also cross 180°W/E line
      } else {
        lowlon2 <- (
          griddata[, "lon"] >= griddata[c, "lon"] - search_radius_cell["lon"] +
          360
        ) # Also cross 180°W/E line
        uplon2 <- (
          griddata[, "lon"] <= griddata[c, "lon"] + search_radius_cell["lon"] +
          360
        ) # Also cross 180°W/E line
      }
      # Southern border
      lowlat <- (
        griddata[, "lat"] >= griddata[c, "lat"] - search_radius_cell["lat"]
      )
      # Northern border
      uplat <- (
        griddata[, "lat"] <= griddata[c, "lat"] + search_radius_cell["lat"]
      )
      # Check that cells are within all box bounds
      in_window <- ((lowlon & uplon) | (lowlon2 & uplon2)) & lowlat & uplat
      box_cells <- which(in_window)
      rm(lowlon, lowlon2, uplon, uplon2, lowlat, uplat, in_window)
      # Use distHaversine() from geosphere package to determine which cells in
      # box_cells are within search_radius.
      griddist <- distHaversine(
        griddata[box_cells, c("lon", "lat")],
        griddata[c, c("lon", "lat")],
        r = earthradius
      )
      # Remove cell c itself from neighbourhood
      neighbourhood  <- setdiff(box_cells[which(griddist <= search_radius)], c)
    } else {
      # stop() does not always work correctly inside dopar.
      message("Error: Invalid value for search_area ", sQuote(search_area))
      return(NULL)
    }
    if (exclude_downstream) {
      # Remove all downstream cells from neighbourhood.
      neighbourhood <- setdiff(neighbourhood, drainage_dsclist[[c]])
    }
    if (exclude_upstream) {
      # Remove all upstream cells from neighbourhood.
      neighbourhood <- setdiff(neighbourhood, drainage_usclist[[c]])
    }
    if (length(neighbourhood) > 0) {
      if (idw_power_par != 0 ||
        length(unique(drainage_upstreamarea[neighbourhood])) <
        length(neighbourhood)
      ) {
        # If inverse distance weighting is selected, apply inverse distance
        # weighting to upstream areas.
        # Even without inverse distance weighting, use distance as 2nd criterion
        # if there are several cells with identical upstream area.
        if (search_area == "region") {
          # Distance has been computed already, reduce to cells included in
          # neighbourhood.
          griddist <- griddist[match(neighbourhood, box_cells)]
          rm(box_cells)
        } else {
          # Compute distance only for neighbourhood
          griddist <- distHaversine(
            griddata[neighbourhood, c("lon", "lat")],
            griddata[c, c("lon", "lat")],
            r = earthradius
          )
        }
      } else {
        # All cells in neighbourhood have unique upstream area, do not require
        # distance as 2nd criterion.
        # Do not use inverse distance weighting.
        griddist <- rep(1, length(neighbourhood))
      }

      # Find neighbour cell with largest upstream area.
      # If there are several cells having the same largest upstream area use the
      # closest one.
      # If using inverse distance weighting, reduce upstream area based on
      # distance to cell and power parameter.
      # Shuffle neighbourhood to avoid bias in chosen neighbour cell if several
      # neighbours have identical distance and upstream area.
      # set.seed should allow for reproducibility despite random shuffling if
      # this script is run several times.
      set.seed(c)
      shuffle <- sample(length(neighbourhood), size = length(neighbourhood))
      neighbourhood <- neighbourhood[shuffle]
      griddist <- griddist[shuffle]
      rm(shuffle)
      # Order cells by decreasing upstream area (applying inverse distance
      # weighting) and select largest cell
      index <- order(
        drainage_upstreamarea[neighbourhood] / (griddist^idw_power_par),
        -griddist,
        decreasing = TRUE
      )[1]
      # Append result to results_table
      results_table <- rbind(
        results_table,
        cbind(cell = c, neighbour = neighbourhood[index]),
        deparse.level = 0
      )
    } else {
      # If no potential neighbour cell is found (e.g. small island) set cell
      # itself.
      # Append result to results_table
      results_table <- rbind(
        results_table,
        cbind(cell = c, neighbour = c),
        deparse.level = 0
      )
    }
  }
  # Return results_table to master task
  results_table
} # End foreach loop

# Output time required for neighbour search.
proctime <- proc.time()["elapsed"] - procstart
cat(
  "Search for neighbour cells took ",
  proctime %/% 3600,
  ":",
  formatC(proctime %% 3600 %/% 60, width = 2, flag = "0"),
  ":",
  formatC(round(proctime %% 3600 %% 60), width = 2, flag = "0"),
  ifelse(
    parallel_mpi | parallel_local,
    paste(" on", num_cluster, "tasks\n"),
    "\n"
  ),
  sep = ""
)
# Consistency checks
if (nrow(cell_loop) != gridheader$header["ncell"]) {
  stop("Unexpected length of cell_loop")
}
if (anyNA(cell_loop)) {
  stop("Unexpected NAs in cell_loop")
}
if (any(cell_loop < 1 || cell_loop > gridheader$header["ncell"])) {
  stop(
    length(which(cell_loop < 1 || cell_loop > gridheader$header["ncell"])),
    " out-of-range values in cell_loop"
  )
}
################################################################################

################################################################################
## Save data to file neighbour_filename.                                      ##
# Initialize data so that each cell is its own neighbour.
neighbour_index <- seq_len(gridheader$header["ncell"])
if (neighbour_format == "BIN") {
  # LPJmL uses cell indices starting at 0 instead of 1 as used by R
  neighbour_index[cell_loop[, "cell"]] <- cell_loop[, "neighbour"] - 1
} else {
  # If not saving in BIN format use R index (starting at 1)
  neighbour_index[cell_loop[, "cell"]] <- cell_loop[, "neighbour"]
}
# Check if neighbours were found
if (all(
  neighbour_index == seq(
    ifelse(neighbour_format == "BIN", 0, 1),
    length.out = length(neighbour_index)
  ))) {
  # All cells refer to themselves, no suitable neighbour found.
  warning(
    "All cells refer to themselves because the algorithm could not ",
    "find a suitable neighbour. Consider relaxing the search criteria ",
    "like increasing the search_radius (", search_radius, " m).",
    call. = FALSE,
    immediate. = TRUE
  )
}
if (neighbour_format == "BIN") {
  neighbour_header <- create_header(
    name = neighbour_headername,
    version = bintype,
    order = 0,
    firstyear = 0,
    nyear = 1,
    ncell = length(neighbour_index),
    nbands = 1,
    cellsize_lon = gridheader$header["cellsize_lon"],
    scalar = 1,
    cellsize_lat = gridheader$header["cellsize_lat"],
    datatype = 2
  )
  write_header(neighbour_filename, neighbour_header)
  neighbour_file <- file(neighbour_filename, "ab")
  if (typeof(get_datatype(neighbour_header)$type) == "integer") {
    writeBin(
      as.integer(round(neighbour_index)),
      neighbour_file,
      size = get_datatype(neighbour_header)$size,
      endian = neighbour_header$endian
    )
  } else {
    stop("Invalid datatype in neighbour_header")
  }
  close(neighbour_file)
} else if (neighbour_format == "CSV") {
  write.csv(
    matrix(neighbour_index, ncol = 1, dimnames = list(NULL, "neighbour")),
    file = neighbour_filename,
    row.names = FALSE
  )
} else {
  stop("Invalid neighbour_format ", sQuote(neighbour_format))
}
cat("Neighbour cells written to", sQuote(neighbour_filename), "\n")

################################################################################
## If running in parallel mode do some clean-up.                              ##
if (parallel_mpi) {
  # Clean out redirected output
  for (i in seq_len(num_cluster)) {
    tmpfilename <- paste0("neighbour_irrigation_worker_", i, ".out")
    if (file.exists(tmpfilename)) {
      if (file.size(tmpfilename) == 0) {
        # Delete if empty
        file.remove(tmpfilename)
      } else {
        message(
          "Please check ", sQuote(tmpfilename),
          " for possible error messages thrown during parallel execution."
        )
      }
    }
  }
  # Release linked R instances and close MPI cluster.
  doMPI::closeCluster(cl)
  Rmpi::mpi.quit()
}
if (parallel_local) {
  # Release parallel cluster
  parallel::stopCluster(cl)
}
################################################################################
