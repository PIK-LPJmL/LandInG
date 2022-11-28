################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This function calculates the maximum value across time for each grid cell  ##
## and optionally aggregates data to a target resolution.                     ##
## It uses CDO tools for the temporal aggregation. The code below should work ##
## on Linux if cdo is installed and discoverable in PATH.                     ##
## CDO tools: https://code.mpimet.mpg.de/projects/cdo                         ##
## Parameters:                                                                ##
## source: NetCDF file with HYDE time series                                  ##
## target: Created NetCDF with maximum values                                 ##
## varname: Name of variable in source                                        ##
## sourcearea: raster object giving cell areas of HYDE data in source.        ##
## targetarea: raster object giving cell areas at target resolution.          ##
## targetraster: raster object at target resolution, if coarser than HYDE     ##
##               source, data is aggregated.                                  ##
## force: whether to force calcution if target file exists already.           ##
## Note: hyde_area_units is defined in landuse_setup.R                        ##
################################################################################
create_hyde_timeseries_max <- function(source,
                                       target,
                                       varname,
                                       sourcearea = hyde_area,
                                       targetarea = hyde_target_area,
                                       targetraster = gadm_raster,
                                       force = FALSE
                                      ) {
  if (file.exists(target)) {
    if (!force) {
      cat(
        "Target file", sQuote(target), "exists already.",
        "Set 'force' to TRUE if you want to force reprocessing.\n"
      )
      # Return file name of target file invisibly
      return(invisible(target))
    } else {
      cat(
        "Target file", sQuote(target),
        "exists already but will be processed again.\n"
      )
    }
  }
  # Check spatial extent of source data against targetraster
  tmpraster <- raster(source, level = 0)
  if (matching_extent(
    extent(tmpraster),
    extent(targetraster),
    xres(tmpraster),
    yres(tmpraster)
  )) {
    # Matching extent, next check resolution of source data
    if (ncol(tmpraster) == ncol(targetraster) &&
      nrow(tmpraster) == nrow(targetraster)) {
      ## Source has same extent and resolution as targetraster
      # Try if cdo can be called from within R
      runtest <- system(
        "cdo --version",
        ignore.stdout = TRUE,
        ignore.stderr = TRUE
      )
      if (runtest != 0) {
        # cdo could not be called
        stop(
          "cdo could not be called directly.\n",
          "Please use 'cdo timmax ", source, " ", target, "' to generate ",
          "a file with maximum cropland extent over the whole time series.\n",
          "Alternatively ensure cdo is accessible before running this script."
        )
      } else {
        # Test call to cdo was successful, run cdo temporal aggregation
        cat("Trying to create", sQuote(target), "using cdo tools\n")
        run <- system(paste("cdo timmax", source, target))
        if (run != 0) {
          stop("File creation failed.")
        }
      }
    } else {
      # Source resolution differs from targetraster, first calculate maximum
      # across time, then aggregate to target resolution.
      # Working directory based on resolution
      tmp_res <- unique(
        res(tmpraster) * ifelse(res(tmpraster) >= 1 / 60, 60, 3600)
      )
      tmp_string <- paste(
        round(tmp_res),
        unique(ifelse(res(tmpraster) >= 1 / 60, "min", "sec")),
        sep = "",
        collapse = "_by_"
      )

      hyde_working_dir <- ifelse(
        nchar(landuse_dir) > 0,
        file.path(landuse_dir, "tmp", paste0("work_", tmp_string)),
        file.path("tmp", paste0("work_", tmp_string))
      )
      if (!file.exists(hyde_working_dir)) {
        dir.create(hyde_working_dir, recursive = TRUE)
      }
      # Intermediate file at source resolution, but aggregated over time
      working_target <- file.path(hyde_working_dir, basename(target))
      # Try if cdo can be called from within R
      runtest <- system(
        "cdo --version",
        ignore.stdout = TRUE,
        ignore.stderr = TRUE
      )
      if (runtest != 0) {
        # cdo could not be called
        stop(
          "cdo could not be called directly.\n",
          "Please use 'cdo timmax ", source, " ", working_target, "'",
          "to generate a file with maximum cropland extent over the whole time",
          " series.\n",
          "Alternatively ensure cdo is accessible before running this script."
        )
      } else {
        # Test call to cdo was successful, run cdo temporal aggregation
        cat("Trying to create", sQuote(working_target), "using cdo tools\n")
        run <- system(paste("cdo timmax", source, working_target))
        if (run != 0) {
          stop("File creation failed.")
        }
        # Check unit in created file and compare to hyde_area_units
        zz <- nc_open(working_target)
        if (ud.convert(1, hyde_area_units, zz$var[[varname]]$units) != 1) {
          warning(
            "Unit in file ", sQuote(working_target),
            " [", zz$var$cropland$units, "] ",
            "differs from defined hyde_area_units [", hyde_area_units, "].",
            call. = TRUE,
            immediate. = TRUE
          )
          # Update unit within function
          hyde_area_units <- zz$var[[varname]]$units
          hyde_is_fraction <- !ud.are.convertible(hyde_area_units, "m2")
        }
        nc_close(zz)
        # Load intermediate data
        working_target_data <- raster(working_target)
        hyde2gadm <-  round(res(working_target_data) / res(targetraster), 4)
        # Check compatibility of source and target resolution
        if (max(hyde2gadm %% 1) != 0 || min(hyde2gadm) < 1) {
          stop(
            "Source resolution ",
            toString(round(res(working_target_data), 5)),
            " is not compatible with target resolution in this script ",
            toString(round(res(targetraster), 5))
          )
        }
        if (hyde_is_fraction) {
          # Need to multiply hyde fractions with cell area to aggregate areas
          tmparea <- working_target_data * ud.convert(1, hyde_area_units, "1") *
            sourcearea
          target_data <- aggregate(tmparea, fact = hyde2gadm, fun = sum)
          # Confirm that aggregation hasn't changed global total
          sum1 <- cellStats(target_data, sum)
          sum2 <- cellStats(tmparea, sum)
          if (sum1 != sum2) {
            stop("Error aggregating target_data to target resolution")
          }
          rm(tmparea, sum1, sum2)
          # Save to target with unit "1" (fraction)
          writeRaster(
            target_data / targetarea,
            filename = target,
            varname = varname,
            varunit = "1"
          )
        } else {
          # Data is in absolute area, can be summed up
          target_data <- aggregate(
            working_target_data,
            fact = hyde2gadm,
            fun = sum
          )
          sum1 <- cellStats(target_data, sum)
          sum2 <- cellStats(working_target_data, sum)
          if (sum1 != sum2) {
            stop("Error aggregating target_data to target resolution")
          }
          rm(sum1, sum2)
          # Save to target with unit hyde_area_units
          writeRaster(
            target_data,
            filename = target,
            varname = varname,
            varunit = hyde_area_units
          )
        }
      }
    }
  } else {
    stop(
      "Spatial extent of source file ", sQuote(source),
      " does not match spatial extent of targetraster"
    )
  }
  # Return file name of target file invisibly
  return(invisible(target))
}
