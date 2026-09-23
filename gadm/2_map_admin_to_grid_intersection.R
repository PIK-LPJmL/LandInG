################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script runs a shape intersection between the gridcell polygons and    ##
## district shapes. The resulting shape includes for each gridcell all the    ##
## countries, regions/states, and districts/counties (admin levels 0, 1, 2)   ##
## found in that gridcell, including the area covered by each admin unit.     ##
## Note: Depending on the spatial resolution of the target grid, this process ##
## can take very long. At 5 min resolution, some countries take several days  ##
## to process. Because of that the script is parallelized.                    ##
## Tested at 5 min resolution, only a few countries take very long. If CPUs   ##
## are blocked for other tasks while R is running it does not make sense to   ##
## use too many CPUs. Check your parallel resource allocation system.         ##
## Processing is quite memory-intensive. Memory requirements vary depending   ##
## on the country and the spatial resolution of the target grid.              ##
## Tests at 30 min spatial resolution have shown that memory can exceed 6 GB  ##
## per task.                                                                  ##
## At 5 min spatial resolution individual tasks require more than 32 GB RAM.  ##
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
      num_cluster <- ceiling(ncores / 4)
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
  }
} else {
  # Do not try parallelization
  foreach::registerDoSEQ() # Tells foreach to use sequential mode
  cat("Running in sequential mode.\n")
}
################################################################################

cat("*** Script run in", getwd(), "***\n")
cat("Spatial resolution:", LandInG_setup$gadm$lpj_res_string, "\n")
if (LandInG_setup$gadm$force_grid) {
  cat(
    "Info: Using predefined grid with", nrow(LandInG_setup$gadm$griddata),
    "cells and a spatial extent:", toString(LandInG_setup$gadm$lpjgrid_extent),
    "\n"
  )
}

################################################################################
## Confirm that directory with shapes per country exists (created in step 1). ##
if (!file.exists(LandInG_setup$gadm$split_directory)) {
  stop(
    "split_directory ",
    LandInG_setup$gadm$split_directory,
    " not found. Please run script for step 1 first."
  )
}
################################################################################

################################################################################
## Find countries in split_directory (defined in gadm_setup,r).               ##
country_shapes <- list.files(
  LandInG_setup$gadm$split_directory,
  ".shp$"
)
# Remove skip_countries (defined in gadm_setup.R)
for (c in LandInG_setup$gadm$skip_countries) {
  if (length(grep(c, country_shapes)) > 0) {
    cat("Skipping", length(grep(c, country_shapes)), "shapes for", c, "\n")
    country_shapes <- country_shapes[grep(c, country_shapes, invert = TRUE)]
  }
}
################################################################################


################################################################################
## Directory for result of shape intersection (defined in gadm_setup.R).      ##
if (!file.exists(LandInG_setup$gadm$intersect_directory)) {
  dir.create(LandInG_setup$gadm$intersect_directory, recursive = TRUE)
}
################################################################################

cat(
  "Country shapes in", LandInG_setup$gadm$split_directory,
  "will be intersected with grid cell polygons in",
  LandInG_setup$gadm$gridcell_shapefile, "\n"
)

################################################################################
## Check if any result files exist already in intersect_directory.            ##
## These will not be processed again. Delete them manually to run full        ##
## intersection again.                                                        ##
intersect_shapes <- list.files(
  LandInG_setup$gadm$intersect_directory,
  ".shp$"
)
if (length(intersect(country_shapes, intersect_shapes)) > 0) {
  message(
    "Grid intersections for ",
    length(intersect(country_shapes, intersect_shapes)),
    " countries/regions exist already in ",
    LandInG_setup$gadm$intersect_directory,
    " and will be skipped.\nDelete them to force re-calculation: ",
    toString(intersect(country_shapes, intersect_shapes))
  )
  country_shapes <- setdiff(country_shapes, intersect_shapes)
}
if (length(setdiff(intersect_shapes, country_shapes)) > 0) {
  # intersect_directory has files not in split_directory, which should not
  # happen
  stop(
    "Grid intersections for ",
    length(setdiff(intersect_shapes, country_shapes)),
    " countries/regions exist already in ",
    LandInG_setup$gadm$intersect_directory,
    " for which there are no source shapes in ",
    LandInG_setup$gadm$split_directory,
    ", suggesting that step 1 was run again with different settings.\n",
    "Please delete files ",
    toString(setdiff(intersect_shapes, country_shapes)),
    " to prevent inconsistencies."
  )
}
if (length(country_shapes) < 1) {
  message("It seems that all countries have finished processing. Exiting.")
  if (parallel_mpi) {
    doMPI::closeCluster(cl)
    Rmpi::mpi.quit(save = "no")
  } else if (parallel_local) {
    parallel::stopCluster(cl)
  }
  quit(save = "no")
} else {
  cat(
    "Creating intersections between", length(country_shapes),
    "countries/regions/counties and",
    terra::ncell(LandInG_setup$gadm$lpjgrid_raster),
    "grid cells. Results saved to:",
    LandInG_setup$gadm$intersect_directory, "\n"
  )
}
################################################################################


################################################################################
## Check if grid cell shapefile exists. This has been created in step 1.      ##
if (!file.exists(LandInG_setup$gadm$gridcell_shapefile)) {
  stop(
    "gridcell_shapefile ", LandInG_setup$gadm$gridcell_shapefile,
    " does not exist.\nPlease run script for step 1 first."
  )
}
################################################################################

################################################################################
## Intersection between GADM level 2 admin units and grid cell polygons is    ##
## done per country (or sub-national collection of admin units for countries  ##
## with a lot of admin units).                                                ##
## foreach is similar to a classic for loop but allows parallel execution.    ##
## Also, in contrast to a for loop foreach has a return value which is only   ##
## used for informational purposes here.                                      ##
## Code below should work in parallel or sequential mode. In sequential mode  ##
## all countries are processed in sequence on one CPU. In parallel mode       ##
## countries are distributed among tasks automatically.                       ##
# Make %dopar% from foreach package available.
library(foreach)
if (parallel_local || parallel_mpi) {
  # Options cannot be exported to parallel tasks automatically, set explicitly
  sf_use_s2 <- options("sf_use_s2")
}
if (!parallel_mpi) {
  # For sequential and locally parallelized application load lpjgrid_shape
  # before loop.
  lpjgrid_shape <- sf::st_read(
    LandInG_setup$gadm$gridcell_shapefile,
    quiet = TRUE
  )
}
intersection_loop <- foreach::foreach(
  cindex = seq_along(country_shapes),
  .inorder = FALSE,
  .combine = rbind,
  .multicombine = TRUE,
  .errorhandling = "remove"
) %dopar% {
  # Read grid cell shapefile. For MPI parallelization this is done on each
  # parallel task to reduce amount of data that needs to be sent from control
  # task to worker tasks.
  # On the other hand, it does not need to be reloaded for each loop iteration.
  if (!exists("lpjgrid_shape")) {
    lpjgrid_shape <- sf::st_read(
      LandInG_setup$gadm$gridcell_shapefile,
      quiet = TRUE
    )
  }
  if (exists("sf_use_s2") && !is.null(sf_use_s2)) {
    options(sf_use_s2)
  }
  # Time execution
  country_start <- proc.time()["elapsed"]
  # Read country shapefile
  country_shape <- sf::st_read(
    file.path(
      LandInG_setup$gadm$split_directory,
      country_shapes[cindex]
    ),
    quiet = TRUE
  )
  cat("** Processing", unique(country_shape$GID_0), "**\n")
  invalid_country_warn <- ""
  if (any(!sf::st_is_valid(country_shape))) {
    invalid_country_warn <- paste(
      "The shape",  country_shapes[cindex], "has",
      length(which(!sf::st_is_valid(country_shape))),
      "invalid polygons. Trying to fix"
    )
    warning(invalid_country_warn, call. = FALSE, immediate. = TRUE)
    # Note: The following line may fail if using a version of sf < 0.9.0.
    # In this case, st_make_valid is provided by the lwgeom package.
    if ("st_make_valid" %in% getNamespaceExports("sf")) {
      country_shape <- sf::st_make_valid(country_shape)
    } else {
      country_shape <- lwgeom::st_make_valid(country_shape)
    }
  }
  # Compare coverage
  if (sf::st_bbox(lpjgrid_shape)["xmin"] > sf::st_bbox(country_shape)["xmin"] ||
      sf::st_bbox(lpjgrid_shape)["xmax"] < sf::st_bbox(country_shape)["xmax"] ||
      sf::st_bbox(lpjgrid_shape)["ymin"] > sf::st_bbox(country_shape)["ymin"] ||
      sf::st_bbox(lpjgrid_shape)["ymax"] < sf::st_bbox(country_shape)["ymax"]
  ) {
    coverage_warn <- paste(
      "Parts of", country_shapes[cindex],
      "are outside of area covered by grid cell polygons",
      LandInG_setup$gadm$gridcell_shapefile
    )
    warning(coverage_warn, call. = FALSE, immediate. = TRUE)
  } else {
    coverage_warn <- ""
  }

  # Crop lpjgrid_shape reduce memory requirements for intersection
  country_bbox <- sf::st_bbox(country_shape)
  # Expand by buffer
  # 1. lower boundary
  low <- grep("min", names(country_bbox))
  country_bbox[low] <- country_bbox[low] - max(LandInG_setup$gadm$lpj_res) * 2
  # 2. upper boundary
  up <- grep("max", names(country_bbox))
  country_bbox[up] <- country_bbox[up] + max(LandInG_setup$gadm$lpj_res) * 2
  # Crop to bounding box
  # Note: Cropping to a rectangular bounding box does not work as expected with
  # spherical geometry. Switch off use of s2 temporarily
  reset <- options(sf_use_s2 = FALSE)
  lpjgrid_shape_country <- sf::st_crop(lpjgrid_shape, country_bbox)
  # Reset s2 usage
  options(reset)

  invalid_grid_warn <- ""
  if (any(!sf::st_is_valid(lpjgrid_shape_country))) {
    invalid_grid_warn <- paste(
      "The cropped grid cell shape for", country_shapes[cindex], "has",
      length(which(!sf::st_is_valid(lpjgrid_shape_country))),
      "invalid polygons. Trying to fix"
    )
    warning(invalid_grid_warn, call. = FALSE, immediate. = TRUE)
    # Note: The following line may fail if using a version of sf < 0.9.0.
    # In this case, st_make_valid is provided by the lwgeom package.
    if ("st_make_valid" %in% getNamespaceExports("sf")) {
      lpjgrid_shape_country <- sf::st_make_valid(lpjgrid_shape_country)
    } else {
      lpjgrid_shape_country <- lwgeom::st_make_valid(lpjgrid_shape_country)
    }
  }

  # Run intersection between grid cell shape and country shape.
  # Split operation into chunks to avoid excessive memory use with
  # high-resolution grids and large countries
  start <- 1
  step <- min(100000, nrow(lpjgrid_shape_country))
  if ("sf_use_s2" %in% getNamespaceExports("sf") && sf::sf_use_s2()) {
    # sf versions with s2 functionality allow passing additional options to
    # s2. Limit returned geometry to polygons (and disallow points and
    # polylines).
    country_gridpolygons <- sf::st_intersection(
      lpjgrid_shape_country[seq_len(step), ],
      country_shape,
      dimensions = "polygon"
    )
  } else {
    country_gridpolygons <- sf::st_intersection(
      lpjgrid_shape_country[seq_len(step), ],
      country_shape
    )
  }
  gc()
  if (nrow(lpjgrid_shape_country) > step) {
    for (start in seq(start + step, nrow(lpjgrid_shape_country), by = step)) {
      rindex <- seq(start, min(start + step - 1, nrow(lpjgrid_shape_country)))
      if ("sf_use_s2" %in% getNamespaceExports("sf") && sf::sf_use_s2()) {
        country_gridpolygons <- rbind(
          country_gridpolygons,
          sf::st_intersection(
            lpjgrid_shape_country[rindex, ],
            country_shape,
            dimensions = "polygon"
          )
        )
      } else {
        country_gridpolygons <- rbind(
          country_gridpolygons,
          sf::st_intersection(lpjgrid_shape_country[rindex, ], country_shape)
        )
      }
      gc()
    }
  }

  rm(lpjgrid_shape_country)
  gc(reset = TRUE)

  # Sometimes lines or points are created during the intersection operation
  # which need to be fixed.
  correct_features <- which(
    !sf::st_geometry_type(country_gridpolygons) %in%
      c("POLYGON", "MULTIPOLYGON")
  )
  # Filter out features that are not of type "GEOMETRYCOLLECTION"
  delete_features <- intersect(
    correct_features,
    which(sf::st_geometry_type(country_gridpolygons) != "GEOMETRYCOLLECTION")
  )
  if (length(correct_features) > 0) {
    for (i in setdiff(correct_features, delete_features)) {
      # Extract polygons out of GEOMETRYCOLLECTION
      tmp_geo <- sf::st_geometry(
        sf::st_combine(
          sf::st_collection_extract(country_gridpolygons[i, ], "POLYGON")
        )
      )
      sf::st_geometry(country_gridpolygons[i, ]) <- tmp_geo
    }
  }
  if (length(delete_features) > 0) {
    country_gridpolygons <- country_gridpolygons[-delete_features, ]
  }

  # Sometimes there are invalid polygons in shape after intersection,
  # try to fix.
  invalid_intersection_warn <- ""
  if (any(!sf::st_is_valid(country_gridpolygons))) {
    invalid_intersection_warn <- paste(
      "The shape intersection for", country_shapes[cindex], "has",
      length(which(!sf::st_is_valid(country_gridpolygons))),
      "invalid polygons. Trying to fix"
    )
    warning(invalid_intersection_warn, call. = FALSE, immediate. = TRUE)
    # Note: The following line may fail if using a version of sf < 0.9.0.
    # In this case, st_make_valid is provided by the lwgeom package.
    if ("st_make_valid" %in% getNamespaceExports("sf")) {
      country_gridpolygons <- sf::st_make_valid(country_gridpolygons)
    } else {
      country_gridpolygons <- lwgeom::st_make_valid(country_gridpolygons)
    }
  }

  # Add column with area covered by each shape
  country_gridpolygons <- cbind(
    country_gridpolygons,
    Shapearea = sf::st_area(country_gridpolygons)
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
      "polygons with <=", threshold, units::deparse_unit(threshold),
      "Shapearea from", country_shapes[cindex]
    )
    warning(zero_area_warn, call. = FALSE, immediate. = TRUE)
    country_gridpolygons <- country_gridpolygons[-delete_features, ]
  }

  # Save country result to intersect_directory
  sf::st_write(
    country_gridpolygons,
    dsn = file.path(
      LandInG_setup$gadm$intersect_directory,
      country_shapes[cindex]
    ),
    delete_dsn = TRUE,
    quiet = TRUE,
    layer_options = "ENCODING=UTF-8"
  )

  # Memory clean up
  rm(country_gridpolygons, country_shape)
  gc()

  # If running in MPI parallel mode return to master task which slave task has
  # processed this country and add warning messages
  total_warn <- c(
    if (nchar(coverage_warn) > 0) coverage_warn else NULL,
    if (nchar(invalid_country_warn) > 0) invalid_country_warn else NULL,
    if (nchar(invalid_grid_warn) > 0) invalid_grid_warn else NULL,
    if (nchar(invalid_intersection_warn) > 0)
      invalid_intersection_warn else NULL,
    if (nchar(zero_area_warn) > 0) zero_area_warn else  NULL
  )
  if (!is.null(total_warn)) {
    total_warn <- paste(total_warn, sep = ". ", collapse = ". ")
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
failed <- which(
  !file.exists(
    file.path(LandInG_setup$gadm$intersect_directory, country_shapes)
  )
)
if (length(failed) > 0) {
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
  # Info print which task has processed which country and any warnings triggered
  options(width = 200)
  print(intersection_loop)
  # Release linked R instances and close MPI cluster.
  doMPI::closeCluster(cl)
  Rmpi::mpi.quit()
} else if (parallel_local) {
  # Close local cluster
  parallel::stopCluster(cl)
}
################################################################################
