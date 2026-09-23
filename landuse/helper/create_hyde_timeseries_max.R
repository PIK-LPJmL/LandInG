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
## source: NetCDF file with HYDE time series.                                 ##
## target: Created NetCDF with maximum values.                                ##
## varname: Name of variable in source.                                       ##
## sourcearea: rast object giving cell areas of HYDE data in source.          ##
## targetarea: rast object giving cell areas at target resolution; if coarser ##
##             than HYDE source data is aggregated.                           ##
## area_units: expected area unit.
## force: whether to force calcution if target file exists already.           ##
################################################################################
create_hyde_timeseries_max <- function(source,
                                       target,
                                       varname,
                                       sourcearea,
                                       targetarea,
                                       area_units,
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
  # Check spatial extent of source data against targetarea
  tmpraster <- terra::rast(source, lyrs = 1)
  if (matching_extent(
    terra::ext(tmpraster),
    terra::ext(targetarea),
    terra::xres(tmpraster),
    terra::yres(tmpraster)
  )) {
    # Matching extent, next check resolution of source data
    if (terra::ncol(tmpraster) == terra::ncol(targetarea) &&
        terra::nrow(tmpraster) == terra::nrow(targetarea)
    ) {
      ## Source has same extent and resolution as targetarea
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
      # Source resolution differs from targetarea, first calculate maximum
      # across time, then aggregate to target resolution.
      # Working directory based on resolution
      tmp_res <- unique(
        terra::res(tmpraster) * ifelse(terra::res(tmpraster) >= 1 / 60, 60, 3600)
      )
      tmp_string <- paste(
        round(tmp_res),
        unique(ifelse(terra::res(tmpraster) >= 1 / 60, "min", "sec")),
        sep = "",
        collapse = "_by_"
      )

      hyde_working_dir <- ifelse(
        nchar(LandInG_setup$landuse$landuse_dir) > 0,
        file.path(
          LandInG_setup$landuse$landuse_dir,
          "tmp", paste0("work_", tmp_string)
        ),
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
        # Check unit in created file and compare to area_units
        nc <- ncdf4::nc_open(working_target)
        if (units::ud_convert(1, area_units, nc$var[[varname]]$units) != 1) {
          warning(
            "Unit in file ", sQuote(working_target),
            " [", nc$var$cropland$units, "] ",
            "differs from defined area_units [", area_units, "].",
            call. = TRUE,
            immediate. = TRUE
          )
          # Update unit within function
          area_units <- nc$var[[varname]]$units
          hyde_is_fraction <- !units::ud_are_convertible(area_units, "m2")
        }
        ncdf4::nc_close(nc)
        # Load intermediate data
        working_target_data <- terra::rast(working_target)
        hyde2gadm <-  round(
          terra::res(targetarea) / terra::res(working_target_data),
          4
        )
        # Check compatibility of source and target resolution
        if (max(hyde2gadm %% 1) != 0 || min(hyde2gadm) < 1) {
          stop(
            "Source resolution ",
            toString(round(terra::res(working_target_data), 5)),
            " is not compatible with target resolution in this script ",
            toString(round(terra::res(targetarea), 5))
          )
        }
        if (hyde_is_fraction) {
          # Need to multiply hyde fractions with cell area to aggregate areas
          tmparea <- working_target_data *
            units::ud_convert(1, area_units, "1") * sourcearea
          target_data <- terra::aggregate(
            tmparea,
            fact = rev(hyde2gadm), # res() returns lon/lat, fact is lat/lon
            fun = "sum",
            na.rm = TRUE
          )
          # Confirm that aggregation has not changed global total
          sum1 <- terra::global(target_data, "sum", na.rm = TRUE)
          sum2 <- terra::global(tmparea, "sum", na.rm = TRUE)
          if (!isTRUE(all.equal(sum1, sum2))) {
            stop("Error aggregating target_data to target resolution")
          }
          rm(tmparea, sum1, sum2)
          # Save to target with unit "1" (fraction)
          if (grepl(".nc[0-9]*$", target)) {
            terra::writeCDF(
              target_data / targetarea,
              filename = target,
              varname = varname,
              unit = "1"
            )
          } else {
            terra::writeRaster(
              target_data / targetarea,
              filename = target
            )
          }
        } else {
          # Data is in absolute area, can be summed up
          target_data <- terra::aggregate(
            working_target_data,
            fact = rev(hyde2gadm), # res() returns lon/lat, fact is lat/lon
            fun = "sum",
            na.rm = TRUE
          )
          sum1 <- terra::global(target_data, "sum", na.rm = TRUE)
          sum2 <- terra::global(working_target_data, "sum", na.rm = TRUE)
          if (!isTRUE(all.equal(sum1, sum2))) {
            stop("Error aggregating target_data to target resolution")
          }
          rm(sum1, sum2)
          if (grepl(".nc[0-9]*$", target)) {
            # Save to target with unit area_units
            terra::writeCDF(
              target_data,
              filename = target,
              varname = varname,
              unit = area_units
            )
          } else {
            terra::writeRaster(
              target_data,
              filename = target
            )
          }
        }
      }
    }
  } else {
    stop(
      "Spatial extent of source file ", sQuote(source),
      " does not match spatial extent of targetarea"
    )
  }
  # Return file name of target file invisibly
  invisible(target)
}
