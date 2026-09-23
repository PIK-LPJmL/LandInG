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
## Whether R is running on a cluster with multiple CPUs (used to determine    ##
## whether to try parallelization).                                           ##
## Set to FALSE to run in sequential mode.                                    ##
cluster <- FALSE
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
if (!"foreach" %in% .packages(all.available = TRUE)) {
  stop("Please install missing package foreach")
}
if (cluster) {
  # Try parallelization
  if ("Rmpi" %in% .packages(all.available = TRUE)) {
    # Rmpi = R implementation of MPI interface
    # This is intended for parallelization on high-performance cluster.
    if ("doMPI" %in% .packages(all.available = TRUE)) {
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
        foreach::registerDoSEQ()
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
      foreach::registerDoSEQ() # Tell foreach to use sequential mode
      cat("Falling back to running in sequential mode.\n")
      num_cluster <- 1
    }
  } else if ("doParallel" %in% .packages(all.available = TRUE)) {
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
      doParallel::registerDoParallel(cl)
      parallel_local <- TRUE
      cat("Running in parallel mode on", num_cluster, "CPUs\n")
    } else {
      # Only one task
      foreach::registerDoSEQ() # Tell foreach to use sequential mode
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
    foreach::registerDoSEQ() # Tells foreach to use sequential mode
    cat("Falling back to running in sequential mode.\n")
    num_cluster <- 1
  }
} else {
  # Do not try parallelization
  foreach::registerDoSEQ() # Tells foreach to use sequential mode
  cat("Running in sequential mode.\n")
  num_cluster <- 1
}
################################################################################


################################################################################
## Load setup from river_routing_setup.R:                                     ##
source("river_routing_setup.R")
################################################################################


################################################################################
## Check for required R packages. These may need to be installed first.       ##
required_packages <- c("terra", "geosphere")
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
################################################################################


################################################################################
## Get grid raster and information on upstream and downstream cells created   ##
## by river_routing.R                                                         ##
# Derive grid raster. Grid data is loaded in river_routing_setup.R
gridextent <- terra::ext(
  min(LandInG_setup$river_routing$griddata[, "lon"]) -
    LandInG_setup$river_routing$gridheader$header["cellsize_lon"] / 2,
  max(LandInG_setup$river_routing$griddata[, "lon"]) +
    LandInG_setup$river_routing$gridheader$header["cellsize_lon"] / 2,
  min(LandInG_setup$river_routing$griddata[, "lat"]) -
    LandInG_setup$river_routing$gridheader$header["cellsize_lat"] / 2,
  max(LandInG_setup$river_routing$griddata[, "lat"]) +
    LandInG_setup$river_routing$gridheader$header["cellsize_lat"] / 2
)
col_names <- c("cellsize_lon", "cellsize_lat")
gridraster <- terra::rast(
  gridextent,
  resolution = LandInG_setup$river_routing$gridheader$header[col_names]
)
# Fill with cell indices.
ncell <- LandInG_setup$river_routing$gridheader$header["ncell"]
gridindex <- terra::cellFromXY(gridraster, LandInG_setup$river_routing$griddata)
gridraster[gridindex] <- seq_len(ncell)
if (
  file.exists(LandInG_setup$river_routing$drainage_celllists_RData) && (
    LandInG_setup$river_routing$exclude_downstream ||
      LandInG_setup$river_routing$exclude_upstream
  )
) {
  cat(
    "Loading upstream and downstream cells lists from",
    sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
    "\n"
  )
  drainage_env <- new.env()
  load(
    LandInG_setup$river_routing$drainage_celllists_RData,
    envir = drainage_env
  )
  if (length(drainage_env$usclist) < ncell) {
    # Make sure all cells have an entry even if they do not have upstream cells
    drainage_env$usclist[[ncell]] <- integer(0)
  }
  if (length(drainage_env$dsclist) < ncell) {
    # Make sure all cells have an entry even if they do not have downstream
    # cells
    drainage_env$dsclist[[ncell]] <- integer(0)
  }
  # Ensure that same river routing was used
  if (!identical(drainage_env$nextcell, LandInG_setup$river_routing$nextcell)) {
    stop(
      paste(
        "River routing used to create",
        sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
        "does not match",
        sQuote(LandInG_setup$river_routing$drainname, q = FALSE)
      )
    )
  }
  # Remove variables not needed for further processing
  rm(cellstoend, endcell, nextcell, griddata, envir = drainage_env)
  gc()
} else if (
  LandInG_setup$river_routing$exclude_downstream ||
    LandInG_setup$river_routing$exclude_upstream
) {
  stop(
    paste(
      sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
      "does not exist. Please make sure to run 'river_routing.R' first."
    )
  )
}
if (file.exists(LandInG_setup$river_routing$upstreamarea_RData)) {
  cat(
    "Loading upstream areas from",
    sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE), "\n"
  )
  if (!exists("drainage_env")) {
    drainage_env <- new.env()
  }
  load(LandInG_setup$river_routing$upstreamarea_RData, envir = drainage_env)
  # Remove variables not needed for further processing
  rm(gridarea, griddata, nextcell, envir = drainage_env)
  gc()
} else {
  stop(
    sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE),
    " does not exist.\nPlease make sure to run 'river_routing.R' first."
  )
}

################################################################################
## Filename of created neighbour irrigation file.                             ##
if (file.exists(LandInG_setup$river_routing$neighbour_filename)) {
  if (LandInG_setup$river_routing$neighbour_format == "BIN") {
    neighbour_header <-
      lpjmlkit::read_header(LandInG_setup$river_routing$neighbour_filename)
    chk_vals <- c("cellsize_lon", "cellsize_lat")
    if (
      neighbour_header$header["ncell"] != ncell ||
        !isTRUE(
          all.equal(
            neighbour_header$header[chk_vals],
            LandInG_setup$river_routing$gridheader$header[chk_vals],
            check.attributes = FALSE,
            tolerance = LandInG_setup$single.eps
          )
        )
    ) {
      stop(
        sQuote(LandInG_setup$river_routing$neighbour_filename, q = FALSE),
        " exists but does not match grid file ",
        sQuote(LandInG_setup$river_routing$gridname, q = FALSE)
      )
    }
    if (
      neighbour_header$header["cellsize_lon"] !=
        neighbour_header$header["cellsize_lat"] &&
        LandInG_setup$river_routing$bintype < 3
    ) {
      stop(
        "bintype set to ", LandInG_setup$river_routing$bintype,
        " but different longitude and latitude resolutions are only allowed",
        " for bintype 3."
      )
    }
    expected_size <- prod(
      neighbour_header$header[c("ncell", "nbands", "nyear", "nstep")]
    ) * lpjmlkit::get_datatype(neighbour_header)$size +
      lpjmlkit::get_headersize(neighbour_header)
    if (
      file.size(LandInG_setup$river_routing$neighbour_filename) != expected_size
    ) {
      warning(
        "Existing ", LandInG_setup$river_routing$neighbour_filename,
        " has unexpected size (",
        file.size(LandInG_setup$river_routing$neighbour_filename), " != ",
        expected_size,
        ". Deleting file and creating again.",
        call. = FALSE, immediate. = TRUE
      )
      file.remove(LandInG_setup$river_routing$neighbour_filename)
    } else {
      stop(
        sQuote(LandInG_setup$river_routing$neighbour_filename, q = FALSE),
        " exists already for the settings you have chosen.\n",
        "Change settings or rename/delete existing file to process again."
      )
    }
  } else {
    stop(
      sQuote(LandInG_setup$river_routing$neighbour_filename, q = FALSE),
      " exists already for the settings you have chosen.\n",
      "Change settings or rename/delete existing file to process again."
    )
  }
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
  "Searching neighbour cells for grid",
  sQuote(LandInG_setup$river_routing$gridname, q = FALSE),
  "with", ncell, "cells.\n"
)
cat(
  "Neighbour cells must be",
  switch(
    LandInG_setup$river_routing$search_area,
    adjacent = "directly adjacent cells. ",
    region = paste(
      "cells within", LandInG_setup$river_routing$search_radius, "m distance. "
    )
  )
)
if (LandInG_setup$river_routing$exclude_downstream) {
  cat("Downstream cells are excluded in neighbour search. ")
}
if (LandInG_setup$river_routing$exclude_upstream) {
  cat("Upstream cells are excluded in neighbour search. ")
}
if (
  LandInG_setup$river_routing$idw_power_par != 0 &&
    LandInG_setup$river_routing$search_area == "region"
) {
  cat(
    "\nApplying inverse distance weighting to upstream areas in search radius",
    "with a power parameter of",
    LandInG_setup$river_routing$idw_power_par
  )
} else {
  # Do not apply inverse distance weighting if only using adjacent cells.
  LandInG_setup$river_routing$idw_power_par <- 0
}
cat("\n")
# Progress report settings. The "percentage done" steps depend on the number of
# cells but may be changed manually to provide more or less updates.
progress_step <- ifelse(ncell < 100000, 10, 200)
progress <- round(
  seq(ncell / progress_step, ncell, length.out = progress_step)
)
procstart <- proc.time()["elapsed"]
# Utility function to convert angle in degree to radian.
deg2rad <- function(x) {
  x * pi / 180
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
  # Wrap gridraster for sending it to parallel nodes
  gridraster_wrapped <- terra::wrap(gridraster)
  noexport <- c("drainage_env", "gridraster")
} else {
  noexport <- NULL
}
sink_output <- parallel_mpi
# Make %dopar% from foreach package available.
library(foreach)
cell_loop <- foreach(
  cell = seq(1, ncell, by = 1000),
  .inorder = FALSE,
  .combine = rbind,
  .verbose = (parallel_mpi && num_cluster > 1),
  .noexport = noexport
) %dopar% {
  if (sink_output) {
    # If running on multiple tasks with MPI-based parallelization redirect
    # output to task-specific file
    doMPI::sinkWorkerOutput(
      paste0(
        "neighbour_irrigation_worker_",
        ifelse(
          nchar(Sys.getenv("SLURM_JOB_ID")) > 0,
          paste0(Sys.getenv("SLURM_JOB_ID"), "_"),
          ""
        ),
        Rmpi::mpi.comm.rank(0),
        ".out"
      )
    )
    # Set up redirection only once
    sink_output <- FALSE
  }
  if (!exists("drainage_env")) {
    drainage_env <- new.env()
    if (
      LandInG_setup$river_routing$exclude_downstream ||
        LandInG_setup$river_routing$exclude_upstream
    ) {
      # Reload data from drainage_celllists_RData if necessary
      load(
        LandInG_setup$river_routing$drainage_celllists_RData,
        envir = drainage_env
      )
      if (length(drainage_env$usclist) < ncell) {
        # Make sure all cells have an entry even if they do not have upstream
        # cells.
        drainage_env$usclist[[ncell]] <- integer(0)
      }
      if (length(drainage_env$dsclist) < ncell) {
        # Make sure all cells have an entry even if they do not have downstream
        # cells.
        drainage_env$dsclist[[ncell]] <- integer(0)
      }
    }
    # Reload data from upstreamarea_RData
    load(
      LandInG_setup$river_routing$upstreamarea_RData,
      envir = drainage_env
    )
    # Remove variables not needed for further processing
    rm_var <- intersect(
      c("cellstoend", "endcell", "nextcell", "griddata", "gridarea"),
      names(drainage_env)
    )
    rm(list = rm_var, envir = drainage_env)
    gc()
  }
  # Unwrap gridraster in parallel nodes
  if (!exists("gridraster")) {
    gridraster <- terra::unwrap(gridraster_wrapped)
  }
  results_table <- cbind(cell = integer(0), neighbour = integer(0))
  for (c in seq(cell, min(cell + 999, ncell))) {
    if (c %in% progress && !parallel_mpi) {
      # Give progress updates only if not running MPI parallel mode. The latter
      # redirects output to files.
      cat(
        round(
          c / LandInG_setup$river_routing$gridheader$header["ncell"] * 100,
          2
        ),
        "% finished after",
        round(proc.time()["elapsed"] - procstart),
        "seconds\n"
      )
    }
    if (LandInG_setup$river_routing$search_area == "adjacent") {
      # Use adjacent() from terra package to find adjacent cells (queen's case)
      cellindex <- terra::adjacent(
        gridraster,
        gridindex[c],
        directions = 8,
        pairs = FALSE
      )
      neighbourhood <- unlist(
        gridraster[c(cellindex)],
        recursive = FALSE, use.names = FALSE
      )
      rm(cellindex)
      # Filter ocean cells
      neighbourhood <- neighbourhood[which(!is.na(neighbourhood))]
    } else if (LandInG_setup$river_routing$search_area == "region") {
      # Because calculating distance between cells is computationally expensive
      # reduce global grid to a smaller lon-by-lat box.
      # Rough estimate of search radius in degree based on location.
      # earthradius defined in landing_setup.R.
      search_radius_cell <- ceiling(
        c(
          lon = as.double(
            LandInG_setup$river_routing$search_radius * 360 / (
              LandInG_setup$earthradius * 2 * pi *
                cos(deg2rad(LandInG_setup$river_routing$griddata[c, "lat"]))
            )
          ),
          lat = LandInG_setup$river_routing$search_radius * 360 /
            (LandInG_setup$earthradius * 2 * pi)
        ) * 100
      ) / 100
      # Narrow down total grid to lon-by-lat box
      # Westside border
      lowlon <- (
        LandInG_setup$river_routing$griddata[, "lon"] >=
          LandInG_setup$river_routing$griddata[c, "lon"] -
            search_radius_cell["lon"]
      )
      # Eastside border
      uplon <- (
        LandInG_setup$river_routing$griddata[, "lon"] <=
          LandInG_setup$river_routing$griddata[c, "lon"] +
            search_radius_cell["lon"]
      )
      if (LandInG_setup$river_routing$griddata[c, "lon"] > 0) {
        lowlon2 <- (
          LandInG_setup$river_routing$griddata[, "lon"] >=
            LandInG_setup$river_routing$griddata[c, "lon"] -
              search_radius_cell["lon"] - 360
        ) # Also cross 180°W/E line
        uplon2 <- (
          LandInG_setup$river_routing$griddata[, "lon"] <=
            LandInG_setup$river_routing$griddata[c, "lon"] +
              search_radius_cell["lon"] - 360
        ) # Also cross 180°W/E line
      } else {
        lowlon2 <- (
          LandInG_setup$river_routing$griddata[, "lon"] >=
            LandInG_setup$river_routing$griddata[c, "lon"] -
              search_radius_cell["lon"] + 360
        ) # Also cross 180°W/E line
        uplon2 <- (
          LandInG_setup$river_routing$griddata[, "lon"] <=
            LandInG_setup$river_routing$griddata[c, "lon"] +
              search_radius_cell["lon"] + 360
        ) # Also cross 180°W/E line
      }
      # Southern border
      lowlat <- (
        LandInG_setup$river_routing$griddata[, "lat"] >=
          LandInG_setup$river_routing$griddata[c, "lat"] -
            search_radius_cell["lat"]
      )
      # Northern border
      uplat <- (
        LandInG_setup$river_routing$griddata[, "lat"] <=
          LandInG_setup$river_routing$griddata[c, "lat"] +
            search_radius_cell["lat"]
      )
      # Check that cells are within all box bounds
      in_window <- ((lowlon & uplon) | (lowlon2 & uplon2)) & lowlat & uplat
      box_cells <- which(in_window)
      rm(lowlon, lowlon2, uplon, uplon2, lowlat, uplat, in_window)
      # Use distHaversine() from geosphere package to determine which cells in
      # box_cells are within search_radius.
      griddist <- geosphere::distHaversine(
        LandInG_setup$river_routing$griddata[box_cells, c("lon", "lat")],
        LandInG_setup$river_routing$griddata[c, c("lon", "lat")],
        r = LandInG_setup$earthradius
      )
      # Remove cell c itself from neighbourhood
      neighbourhood  <- setdiff(
        box_cells[which(griddist <= LandInG_setup$river_routing$search_radius)],
        c
      )
    } else {
      # stop() does not always work correctly inside dopar.
      message(
        "Error: Invalid value for search_area ",
        sQuote(LandInG_setup$river_routing$search_area, q = FALSE)
      )
      return(NULL)
    }
    if (LandInG_setup$river_routing$exclude_downstream) {
      # Remove all downstream cells from neighbourhood.
      neighbourhood <- setdiff(neighbourhood, drainage_env$dsclist[[c]])
    }
    if (LandInG_setup$river_routing$exclude_upstream) {
      # Remove all upstream cells from neighbourhood.
      neighbourhood <- setdiff(neighbourhood, drainage_env$usclist[[c]])
    }
    if (length(neighbourhood) > 0) {
      if (
        LandInG_setup$river_routing$idw_power_par != 0 ||
          length(unique(drainage_env$upstreamarea[neighbourhood])) <
            length(neighbourhood)
      ) {
        # If inverse distance weighting is selected, apply inverse distance
        # weighting to upstream areas.
        # Even without inverse distance weighting, use distance as 2nd criterion
        # if there are several cells with identical upstream area.
        if (LandInG_setup$river_routing$search_area == "region") {
          # Distance has been computed already, reduce to cells included in
          # neighbourhood.
          griddist <- griddist[match(neighbourhood, box_cells)]
          rm(box_cells)
        } else {
          # Compute distance only for neighbourhood
          griddist <- geosphere::distHaversine(
            LandInG_setup$river_routing$griddata[neighbourhood, c("lon", "lat")],
            LandInG_setup$river_routing$griddata[c, c("lon", "lat")],
            r = LandInG_setup$earthradius
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
        drainage_env$upstreamarea[neighbourhood] /
          (griddist^LandInG_setup$river_routing$idw_power_par),
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
if (nrow(cell_loop) != ncell) {
  stop("Unexpected length of cell_loop")
}
if (anyNA(cell_loop)) {
  stop("Unexpected NAs in cell_loop")
}
if (any(cell_loop < 1 | cell_loop > ncell)) {
  stop(
    length(which(cell_loop < 1 | cell_loop > ncell)),
    " out-of-range values in cell_loop"
  )
}
################################################################################

################################################################################
## Save data to file neighbour_filename.                                      ##
# Initialize data so that each cell is its own neighbour.
neighbour_index <- seq_len(ncell)
if (LandInG_setup$river_routing$neighbour_format == "BIN") {
  # LPJmL uses cell indices starting at 0 instead of 1 as used by R
  neighbour_index[cell_loop[, "cell"]] <- cell_loop[, "neighbour"] - 1
} else {
  # If not saving in BIN format use R index (starting at 1)
  neighbour_index[cell_loop[, "cell"]] <- cell_loop[, "neighbour"]
}
# Check if neighbours were found
if (
  all(
    neighbour_index == seq(
      ifelse(LandInG_setup$river_routing$neighbour_format == "BIN", 0, 1),
      length.out = length(neighbour_index)
    )
  )
) {
  # All cells refer to themselves, no suitable neighbour found.
  warning(
    "All cells refer to themselves because the algorithm could not ",
    "find a suitable neighbour. Consider relaxing the search criteria ",
    "like increasing the search_radius (",
    LandInG_setup$river_routing$search_radius,
    " m).",
    call. = FALSE,
    immediate. = TRUE
  )
}
if (LandInG_setup$river_routing$neighbour_format == "BIN") {
  neighbour_header <- lpjmlkit::create_header(
    name = LandInG_setup$river_routing$neighbour_headername,
    version = LandInG_setup$river_routing$bintype,
    order = 0,
    firstyear = 0,
    nyear = 1,
    ncell = length(neighbour_index),
    nbands = 1,
    cellsize_lon = LandInG_setup$river_routing$gridheader$header["cellsize_lon"],
    scalar = 1,
    cellsize_lat = LandInG_setup$river_routing$gridheader$header["cellsize_lat"],
    datatype = 2
  )
  lpjmlkit::write_header(
    LandInG_setup$river_routing$neighbour_filename,
    neighbour_header
  )
  neighbour_file <- file(LandInG_setup$river_routing$neighbour_filename, "ab")
  if (typeof(lpjmlkit::get_datatype(neighbour_header)$type) == "integer") {
    writeBin(
      as.integer(round(neighbour_index)),
      neighbour_file,
      size = lpjmlkit::get_datatype(neighbour_header)$size,
      endian = neighbour_header$endian
    )
  } else {
    stop("Invalid datatype in neighbour_header")
  }
  close(neighbour_file)
} else if (LandInG_setup$river_routing$neighbour_format == "CSV") {
  write.csv(
    matrix(neighbour_index, ncol = 1, dimnames = list(NULL, "neighbour")),
    file = LandInG_setup$river_routing$neighbour_filename,
    row.names = FALSE
  )
} else {
  stop(
    "Invalid neighbour_format ",
    sQuote(LandInG_setup$river_routing$neighbour_format, q = FALSE)
  )
}
cat(
  "Neighbour cells written to",
  sQuote(LandInG_setup$river_routing$neighbour_filename, q = FALSE), "\n"
)

################################################################################
## If running in parallel mode do some clean-up.                              ##
if (parallel_mpi) {
  # Clean out redirected output
  for (i in seq_len(num_cluster)) {
    tmpfilename <- paste0(
      "neighbour_irrigation_worker_",
      ifelse(
        nchar(Sys.getenv("SLURM_JOB_ID")) > 0,
        paste0(Sys.getenv("SLURM_JOB_ID"), "_"),
        ""
      ),
      i,
      ".out"
    )
    if (file.exists(tmpfilename)) {
      if (file.size(tmpfilename) == 0) {
        # Delete if empty
        file.remove(tmpfilename)
      } else {
        message(
          "Please check ", sQuote(tmpfilename, q = FALSE),
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
