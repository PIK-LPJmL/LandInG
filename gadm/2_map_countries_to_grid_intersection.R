################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script runs a shape intersection between the grid cell polygons and   ##
## country shapes. The resulting shape includes for each grid cell all the    ##
## countries/regions found in that grid cell, including the area covered by   ##
## each country/region.                                                       ##
## Note: Depending on the spatial resolution of the target grid, this process ##
## can take very long. At 5 min resolution, some countries take several days  ##
## to process. Because of that the script is parallelized.                    ##
## Tested at 5 min resolution, it does not make sense to use too many CPUs    ##
## because only a few countries take very long. At least, if CPUs are blocked ##
## for other tasks while R is running.                                        ##
## Processing is quite memory-intensive. Memory requirements vary depending   ##
## on the country and the spatial resolution of the target grid.              ##
## Tests at 30 min spatial resolution have shown that memory can exceed 3 GB  ##
## per task.                                                                  ##
## At 5 min spatial resolution individual tasks require more than 16 GB RAM.  ##
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
## Load variables and helper functions used across several scripts.           ##
## You should always call gadm_setup.R first and make sure it is in the same  ##
## directory.                                                                 ##
source("gadm_setup.R")
source("gadm_helper.R")
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
  }
} else {
  # Do not try parallelization
  library(foreach)
  registerDoSEQ() # Tells foreach to use sequential mode
  cat("Running in sequential mode.\n")
}
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
## Confirm that directory with shapes per country exists (created in step 1). ##
if (!file.exists(split_directory)) {
 stop(
   paste(
     "split_directory", split_directory, "not found.",
     "Please run script for step 1 first."
   )
 )
}
################################################################################

################################################################################
## Find countries in split_directory (defined in gadm_setup,r).               ##
country_shapes <- list.files(split_directory, ".shp")
# Remove skip_countries (defined in gadm_setup.R)
for (c in skip_countries) {
  if (length(grep(c, country_shapes)) > 0) {
    cat("Skipping", length(grep(c, country_shapes)), "shapes for", c, "\n")
    country_shapes <- country_shapes[grep(c, country_shapes, invert = TRUE)]
  }
}
################################################################################


################################################################################
## Directory for result of shape intersection (defined in gadm_setup.R).      ##
if (!file.exists(intersect_directory)) {
  dir.create(intersect_directory)
}
################################################################################

cat(
  "Country shapes in", split_directory,
  "will be intersected with grid cell polygons in", gridcell_shapefile, "\n"
)

################################################################################
## Check if any result files exist already in intersect_directory. These will ##
## not be processed again. Delete them to run full intersection again.        ##
intersect_shapes <- list.files(intersect_directory, ".shp")
if (length(intersect(country_shapes, intersect_shapes)) > 0) {
  message(
    "Grid intersections for ",
    length(intersect(country_shapes, intersect_shapes)),
    " countries/regions exist already in ", intersect_directory,
    " and will be skipped.\nDelete them to force re-calculation: ",
    toString(intersect(country_shapes, intersect_shapes))
  )
  country_shapes <- setdiff(country_shapes, intersect_shapes)
}
if (length(country_shapes) < 1) {
  message("It seems that all countries have finished processing. Exiting.")
  if (parallel_mpi) {
    doMPI::closeCluster(cl)
    mpi.quit(save = "no")
  } else if (parallel_local) {
    parallel::stopCluster(cl)
  }
  quit(save = "no")
} else {
  cat(
    "Creating intersections between", length(country_shapes),
    "countries/regions and", ncell(lpjgrid_raster), "grid cells.",
    "\nResults saved to:", intersect_directory, "\n"
  )
}
################################################################################

################################################################################
## Check if grid cell shapefile exists. This has been created in step 1.      ##
if (!file.exists(gridcell_shapefile)) {
  stop(
    "gridcell_shapefile ", gridcell_shapefile, " does not exist.\n",
    "Please run script for step 1 first."
  )
}
################################################################################

################################################################################
## Intersection between countries and grid cell polygons is done per country. ##
## foreach is similar to a classic for loop but allows parallel execution.    ##
## Also, in contrast to a for loop foreach has a return value which is only   ##
## used for information purposes here.                                        ##
## Code below should work in parallel or sequential mode. In sequential mode  ##
## all countries are processed in sequence on one CPU. In parallel mode       ##
## countries are distributed among tasks automatically.                       ##
intersection_loop <- foreach(
  cindex = seq_along(country_shapes),
  .inorder = FALSE,
  .combine = rbind,
  .verbose = FALSE
) %dopar% {
  # Read grid cell shape file. This is done on each parallel task to reduce
  # amount of data that needs to be sent from master task to slave tasks.
  # On the other hand, it does not need to be reloaded for each loop iteration.
  if (!exists("lpjgrid_shape")) {
    lpjgrid_shape <- st_read(gridcell_shapefile, quiet = TRUE)
  }
  # Time execution
  country_start <- proc.time()["elapsed"]
  # Read country shapefile
  country_shape <- st_read(
    file.path(split_directory, country_shapes[cindex]),
    quiet = TRUE
  )
  cat("** Processing", unique(country_shape$GID_0), "**\n")
  # Sometimes there are invalid polygons in shape, try to fix
  invalid_country_warn <- ""
  if (any(!st_is_valid(country_shape))) {
    invalid_country_warn <- paste(
      "The shape",  country_shapes[cindex], "has",
      length(which(!st_is_valid(country_shape))),
      "invalid polygons. Trying to fix"
    )
    warning(invalid_country_warn, call. = FALSE, immediate. = TRUE)
    country_shape <- st_make_valid(country_shape)
  }
  # Compare coverage
  if (st_bbox(lpjgrid_shape)["xmin"] > st_bbox(lpjgrid_shape)["xmin"] ||
      st_bbox(lpjgrid_shape)["xmax"] < st_bbox(lpjgrid_shape)["xmax"] ||
      st_bbox(lpjgrid_shape)["ymin"] > st_bbox(lpjgrid_shape)["ymin"] ||
      st_bbox(lpjgrid_shape)["ymax"] < st_bbox(lpjgrid_shape)["ymax"]) {
    coverage_warn <- paste(
      "Parts of", country_shapes[cindex],
      "are outside of area covered by grid cell polygons", gridcell_shapefile
    )
    warning(coverage_warn, call. = FALSE, immediate. = TRUE)
  } else {
    coverage_warn <- ""
  }

  # Run intersection between grid cell shape and country shape
  country_gridpolygons <- st_intersection(lpjgrid_shape, country_shape)

  # Sometimes lines or points are created during the intersection operation
  # which need to be fixed
  correct_features <- which(
    !st_geometry_type(country_gridpolygons) %in% c("POLYGON", "MULTIPOLYGON")
  )
  delete_features <- integer(0)
  if (length(correct_features) > 0) {
    for (i in correct_features) {
      if (st_geometry_type(country_gridpolygons[i, ]) == "GEOMETRYCOLLECTION") {
        # Extract polygons out of GEOMETRYCOLLECTION
        tmp_geo <- st_geometry(
          st_combine(
            st_collection_extract(country_gridpolygons[i, ], "POLYGON")
          )
        )
        st_geometry(country_gridpolygons[i, ]) <- tmp_geo
      } else {
        # Mark feature for deletion because lines or points not wanted
        delete_features <- c(delete_features, i)
      }
    }
  }
  if (length(delete_features) > 0) {
    country_gridpolygons <- country_gridpolygons[-delete_features, ]
  }

  # Sometimes there are invalid polygons in shape after intersection, try to fix
  invalid_intersection_warn <- ""
  if (any(!st_is_valid(country_gridpolygons))) {
    invalid_intersection_warn <- paste(
      "The shape intersection for", country_shapes[cindex], "has",
      length(which(!st_is_valid(country_gridpolygons))),
      "invalid polygons. Trying to fix"
    )
    warning(invalid_intersection_warn, call. = FALSE, immediate. = TRUE)
    country_gridpolygons <- st_make_valid(country_gridpolygons)
  }

  # Add column with area covered by each shape
  country_gridpolygons <- cbind(
    country_gridpolygons,
    Shapearea = st_area(country_gridpolygons)
  )

  # Check for polygons with Shapearea <= 0
  # st_area() assigns unit to values -> comparison object must have unit as well
  zero_area_warn <- ""
  threshold <- 0
  units(threshold) <- units(country_gridpolygons$Shapearea)
  if (any(country_gridpolygons$Shapearea <= threshold)) {
    delete_features <- which(country_gridpolygons$Shapearea <= threshold)
    zero_area_warn <- paste(
      "Removing", length(delete_features),
      "polygons with", threshold, deparse_unit(threshold), "Shapearea from",
      country_shapes[cindex]
    )
    warning(zero_area_warn, call. = FALSE, immediate. = TRUE)
    country_gridpolygons <- country_gridpolygons[-delete_features, ]
  }

  # Save country result to intersect_directory
  st_write(
    country_gridpolygons,
    dsn = file.path(intersect_directory, country_shapes[cindex]),
    delete_dsn = TRUE,
    quiet = TRUE
  )

  # Memory clean up
  rm(country_gridpolygons, country_shape)
  gc()

  # If running in parallel MPI mode return to master task which slave task has
  # processed this country and any warnings
  total_warn <- c(
    if (nchar(coverage_warn) > 0) coverage_warn else NULL,
    if (nchar(invalid_country_warn) > 0) invalid_country_warn else NULL,
    if (nchar(invalid_intersection_warn) > 0)
      invalid_intersection_warn else  NULL,
    if (nchar(zero_area_warn) > 0) zero_area_warn else  NULL
  )
  if (!is.null(total_warn)) {
    total_warn <- paste(total_warn, sep = ". ")
  } else {
    total_warn <- ""
  }
  if (parallel_mpi) {
    data.frame(
      country = country_shapes[cindex],
      worker = Rmpi::mpi.comm.rank(0),
      warn = total_warn,
      runtime = as.double(proc.time()["elapsed"] - country_start)
    )
  } else {
    data.frame(
      country = country_shapes[cindex],
      runtime = as.double(proc.time()["elapsed"] - country_start)
    )
  }
}

# Confirm that all country_shapes have been processed.
if (any(!file.exists(file.path(intersect_directory, country_shapes)))) {
  failed <- which(!file.exists(file.path(intersect_directory, country_shapes)))
  message(
    "Error: The following ", length(failed),
    " grid intersections appear to have failed: ",
    toString(country_shapes[failed])
  )
  if (parallel_mpi) {
    doMPI::closeCluster(cl)
  } else if (parallel_local) {
    parallel::stopCluster(cl)
  }
  stop("Try running script again to check if a second attempt is successful.")
}
################################################################################

################################################################################
## If running in parallel mode do some clean-up.                              ##
if (parallel_mpi) {
  # Info print which task has processed which country and any warnings triggered.
  options(width = 200)
  print(intersection_loop)
  # Release linked R instances and close MPI cluster.
  doMPI::closeCluster(cl)
  Rmpi::mpi.quit()
}
if (parallel_local) {
  parallel::stopCluster(cl)
}
################################################################################
