################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This function expects a timeseries of harvested areas and a timeseries of  ##
## physical cropland.                                                         ##
## The names attribute for both timeseries needs to contain the years as      ##
## strings.                                                                   ##
## The cropland timeseries can extend beyond the length of harvested area     ##
## timeseries in which case the harvested area timeseries may be extended.    ##
## You may also use this function to fill gaps in a cropland timeseries. For  ##
## this, only provide harv_areas.                                             ##
## By default, uses harv_areas to cropland ratio (crop intensity) for         ##
## gap-filling: Crop intensity values before and after the gap are            ##
## interpolated linearly and multiplied with cropland to derive missing       ##
## harv_areas values.                                                         ##
## Crop intensity values are kept constant when extrapolating harv_areas      ##
## outside its year range.                                                    ##
##                                                                            ##
## Parameters:                                                                ##
## harv_areas: timeseries of harvested area (or cropland)                     ##
## cropland: timeseries of cropland extent (used to gap-fill harvested area   ##
##           timeseries)                                                      ##
## crop: optional character string used in print statements                   ##
## extend_timeseries: if TRUE, years may be reordered (if out of order), and  ##
##                    missing years may be added to the sequence. If FALSE,   ##
##                    the year sequence in harv_areas will be preserved       ##
## harv_is_cropland: boolean stating whether harv_areas is actually cropland  ##
##                   extent. If you want to gap-fill harvested area without   ##
##                   using cropland, set cropland to NULL and                 ##
##                   harv_is_cropland to FALSE.                               ##
## quiet: if TRUE, reduces print statements                                   ##
################################################################################
fill_timeseries <- function(harv_areas,
                            cropland = NULL,
                            crop = NULL,
                            extend_timeseries = TRUE,
                            harv_is_cropland = is.null(cropland),
                            quiet = FALSE
                           ) {
  # If harv_is_cropland == TRUE, unset cropland if it was provided because
  # cropland should be in harv_areas.
  if (harv_is_cropland) {
    cropland <- NULL
  }
  # Create dummy for cropland if none has been provided.
  if (is.null(cropland)) {
    # Dummy cropland timeseries
    cropland <- rep(NA, length(harv_areas))
    names(cropland) <- names(harv_areas)
  }
  # Transfer harv_areas values to cropland if harv_is_cropland == TRUE.
  if (harv_is_cropland) {
    cropland <- harv_areas
  }
  # Check that variables are named.
  if (min(nchar(names(harv_areas))) < 1 ||
    !is.finite(min(nchar(names(harv_areas))))
  ) {
    stop("Please make sure that harv_areas is named using the years as strings")
  }
  if (min(nchar(names(cropland))) < 1 ||
    !is.finite(min(nchar(names(cropland))))
  ) {
    stop("Please make sure that cropland is named using the years as strings")
  }
  # Check if any gap-filling is necessary.
  if (length(which(is.na(harv_areas))) == 0 &&
    length(setdiff(names(cropland), names(harv_areas))) == 0
  ) {
    # Exit function if there are no NAs and if cropland timeseries is not
    # longer than harvested areas timeseries.
    return(harv_areas)
  }

  # Save year sequence to restore if extend_timeseries == FALSE.
  saved_yseq <- names(harv_areas)

  # Check if cropland has years not included in harv_areas.
  if (length(setdiff(names(cropland), names(harv_areas))) > 0 &&
    extend_timeseries
  ) {
    # Add empty years to harvested area timeseries if cropland timeseries is
    # longer.
    harv_areas <- harv_areas[names(cropland)]
    names(harv_areas)[which(is.na(names(harv_areas)))] <- setdiff(
      names(cropland), names(harv_areas)
    )
  }
  # Check if harv_areas no valid values.
  if (length(which(is.na(harv_areas))) == length(harv_areas)) {
    # Cannot fill timeseries that has no values at all, exit.
    return(harv_areas)
  }

  # Check cropland vector.
  # Make sure years are in order.
  yseq <- as.character(sort(as.integer(names(cropland))))
  cropland[seq_along(cropland)] <- cropland[yseq]
  names(cropland) <- yseq
  # Check for missing years.
  yseq <- as.character(
    min(as.integer(names(cropland))):max(as.integer(names(cropland)))
  )
  cropland <- cropland[yseq]
  names(cropland) <- yseq
  if (anyNA(cropland) && any(!is.na(cropland))) {
    # Some, but not all cropland values are missing.
    # First non-NA value
    first_valid <- min(which(!is.na(cropland)))
    # Find next NA value after valid value
    next_NA <- min(
      ifelse(
        which(is.na(cropland)) > first_valid,
        which(is.na(cropland)),
        Inf
      )
    )
    while (is.finite(next_NA)) {
      # next_NA is finite as long as there are NAs in the timeseries
      # Find valid value after next_NA
      non_NA <- which(!is.na(cropland))
      if (any(non_NA > next_NA)) {
        interpolation_target <- min(non_NA[which(non_NA > next_NA)])
        # There is a valid value after next_NA.
        # Interpolate between cropland values before and after next_NA.
        if (!quiet) {
          cat(
            "Interpolating cropland timeseries between",
            names(cropland)[next_NA - 1], # year before next_NA
            "and", names(cropland)[interpolation_target], # next valid value
            "\n"
          )
        }
        fillyears <- names(cropland)[next_NA:(interpolation_target - 1)]
        # Cropland before next_NA
        cbase <- cropland[next_NA - 1]
        # Total cropland change between valid years
        inc <- (cropland[interpolation_target] - cropland[next_NA - 1])
        # Year counter from last valid year
        ycount <- (as.integer(fillyears) -
          as.integer(names(cropland)[next_NA - 1]))
        # Total number of years between valid values
        ysum <- (as.integer(names(cropland)[interpolation_target]) -
          as.integer(names(cropland)[next_NA - 1]))
        cropland[fillyears] <- cbase + inc * ycount / ysum
        # Find next NA value after valid value.
        if (anyNA(cropland)) {
          next_NA <- min(
            ifelse(
              which(is.na(cropland)) > first_valid,
              which(is.na(cropland)),
              Inf
            )
          )
        } else {
          next_NA <- Inf
        }
      } else {
        # No more non-NA values to interpolate between
        next_NA <- Inf
      }
    }
  }
  # Exit function if only gap-filling cropland.
  if (harv_is_cropland) {
    if (extend_timeseries) {
      return(cropland)
    } else {
      # Remove any additional years introduced in time series.
      return(cropland[saved_yseq])
    }
  }

  # Check harvested area timeseries for gaps.
  # Make sure harvested area years are in order.
  yseq <- as.character(sort(as.integer(names(harv_areas))))
  harv_areas[seq_along(harv_areas)] <- harv_areas[yseq]
  names(harv_areas) <- yseq
  # Check for missing years.
  yseq <- as.character(
    min(as.integer(names(harv_areas))):max(as.integer(names(harv_areas)))
  )
  harv_areas <-  harv_areas[yseq]
  names(harv_areas) <- yseq

  # Check for gaps.
  # First non-NA value
  first_valid <- min(which(!is.na(harv_areas)))
  if (first_valid > 1 && is.finite(first_valid) &&
    !is.na(cropland[names(harv_areas)[first_valid]])
  ) {
    # Extend at beginning.
    # This uses a constant crop intensity.
    fillyears <- names(harv_areas[seq(1, first_valid - 1)])
    if (any(cropland[fillyears] > 0, na.rm = TRUE)) {
      # Cropland has values for at least one of fillyears.
      if (cropland[names(harv_areas)[first_valid]] > 0) {
        # Crop intensity harv_areas[first_valid]/cropland[first_valid] can be
        # calculated.
        if (!quiet) {
          cat(
            ifelse(is.null(crop), "", paste0(crop, ":")),
            "Extrapolating harvested area timeseries based on cropland",
            "timeseries for",
            min(
              intersect(fillyears, names(cropland[which(!is.na(cropland))]))
            ),
            "to",
            max(
              intersect(fillyears, names(cropland[which(!is.na(cropland))]))
            ),
            "\n"
          )
        }
        f_start <- harv_areas[first_valid] /
          cropland[names(harv_areas)[first_valid]]
        harv_areas[fillyears] <- cropland[fillyears] * f_start
      } else if (!quiet) {
        cat(
          ifelse(is.null(crop), "", paste0(crop, ":")),
          "Cannot fill beginning of harvested area timeseries because",
          "cropland is zero in reference year\n"
        )
      }
    }
  }

  # Find next NA value if there are any NA values at all.
  if (anyNA(harv_areas)) {
    next_NA <- min(
      ifelse(
        which(is.na(harv_areas)) > first_valid,
        which(is.na(harv_areas)),
        Inf
      )
    )
  } else {
    # No NA values in time series
    next_NA <- Inf
  }
  while (is.finite(next_NA)) {
    # next_NA is finite as long as there are NAs in the timeseries
    # Find valid value after next_NA.
    non_NA <- which(!is.na(harv_areas))
    if (any(non_NA > next_NA)) {
      interpolation_target <- min(non_NA[which(non_NA > next_NA)])
      # There is a valid value after next_NA.
      # Interpolate harvested areas.
      if (!quiet) {
        cat(
          ifelse(is.null(crop), "", paste0(crop, ":")),
          "Interpolating harvested area timeseries between",
          names(harv_areas)[next_NA - 1], "and",
          names(harv_areas)[interpolation_target], "\n"
        )
      }
      fillyears <- names(harv_areas)[next_NA:(interpolation_target - 1)]
      # Check if crop intensity can be used for interpolation or harvested areas
      # need to be interpolated directly.
      if (all(is.na(cropland[fillyears]))) {
        has_cropland <- FALSE
        if (!quiet) {
          cat(
            "Cannot use harvested area to cropland ratio for interpolation",
            "because of missing cropland\n"
          )
        }
      } else {
        if (is.na(cropland[names(harv_areas)[next_NA - 1]]) ||
          cropland[names(harv_areas)[next_NA - 1]] == 0
        ) {
          # Cannot use starting harv_areas/cropland factor for interpolation.
          start_cropland <- FALSE
        } else {
          start_cropland <- TRUE
        }
        if (is.na(cropland[names(harv_areas)[interpolation_target]]) ||
          cropland[names(harv_areas)[interpolation_target]] == 0
        ) {
          # Cannot use ending harv_areas/cropland factor for interpolation.
          end_cropland <- FALSE
        } else {
          end_cropland <- TRUE
        }
        if (start_cropland && end_cropland) {
          # Use cropland for gap filling.
          has_cropland <- TRUE
          # Fill all years that have cropland.
          fillyears <- fillyears[which(!is.na(cropland[fillyears]))]
        } else {
          if (start_cropland) {
            # Interpolate from last year (which has no cropland) backwards
            # until year with cropland using only harvested areas.
            fillyears <- rev(fillyears)
            valid <- min(
              which(!is.na(cropland[fillyears]) & cropland[fillyears] > 0)
            )
            fillyears <- rev(fillyears[1:valid])
            # Cannot use cropland for gap filling.
            has_cropland <- FALSE
            if (!quiet) {
              cat(
                "Incomplete cropland sequence: ",
                "Filling tail of harvested area sequence (",
                toString(fillyears),
                ") without cropland\n",
                sep = ""
              )
            }
          } else if (end_cropland) {
            # Interpolate from start year (which has no cropland) until first
            # year with cropland using only harvested areas.
            valid <- min(
              which(!is.na(cropland[fillyears]) & cropland[fillyears] > 0)
            )
            fillyears <- fillyears[1:valid]
            # Cannot use cropland for gap filling.
            has_cropland <- FALSE
            if (!quiet) {
              cat(
                "Incomplete cropland sequence: ",
                "Filling beginning of harvested area sequence (",
                toString(fillyears),
                ")  without cropland\n",
                sep = ""
              )
            }
          } else {
            # Neither beginning nor end of gap have cropland, interpolate
            # using only harvested areas.
            has_cropland <- FALSE
            if (!quiet) {
              cat(
                "Cannot use harvested area to cropland ratio for",
                "interpolation because of missing cropland\n"
              )
            }
          }
        }
      }
      # Year counter
      ycount <- as.integer(fillyears) -
        as.integer(names(harv_areas)[next_NA - 1])
      # Total number of years that span the gap
      ysum <- as.integer(names(harv_areas)[interpolation_target]) -
        as.integer(names(harv_areas)[next_NA - 1])
      if (has_cropland) {
        # Crop intensity at start of gap
        f_start <- harv_areas[next_NA - 1] /
          cropland[names(harv_areas)[next_NA - 1]]
        # Crop intensity at end of gap
        f_end <- harv_areas[interpolation_target] /
          cropland[names(harv_areas)[interpolation_target]]
        # Crop intensity in fillyears
        harv_areas[fillyears] <- f_start + (f_end - f_start) * ycount / ysum
        # Convert crop intensity into harvested areas during fill years.
        harv_areas[fillyears] <- harv_areas[fillyears] * cropland[fillyears]
      } else {
        # Total change in harvested areas before and after gap
        inc <- harv_areas[interpolation_target] - harv_areas[next_NA - 1]
        harv_areas[fillyears] <- harv_areas[next_NA - 1] + inc * ycount / ysum
      }
      if (anyNA(harv_areas)) {
        next_NA <- min(
          ifelse(
            which(is.na(harv_areas)) > first_valid,
            which(is.na(harv_areas)),
            Inf
          )
        )
      } else {
        next_NA <- Inf
      }
    } else {
      # Extrapolate harvested area timeseries based on cropland timeseries
      # using last available crop intensity.
      fillyears <- names(harv_areas)[next_NA:length(harv_areas)]
      if (any(cropland[fillyears] > 0, na.rm = TRUE)) {
        # At least some years have cropland information.
        if (!is.na(cropland[names(harv_areas)[next_NA - 1]])) {
          if (cropland[names(harv_areas)[next_NA - 1]] > 0) {
            # Last crop intensity
            f_end <- harv_areas[next_NA - 1] /
              cropland[names(harv_areas)[next_NA - 1]]
            harv_areas[fillyears] <- cropland[fillyears] * f_end
            if (!quiet) {
              cat(
                ifelse(is.null(crop), "", paste0(crop, ":")),
                "Extrapolating harvested area timeseries based on cropland",
                "timeseries for",
                min(
                  intersect(
                    fillyears,
                    names(cropland[which(!is.na(cropland))])
                  )
                ),
                "to",
                max(
                  intersect(
                    fillyears,
                    names(cropland[which(!is.na(cropland))])
                  )
                ),
                "\n"
              )
            }
          } else if (!quiet) {
            cat(
              ifelse(is.null(crop), "", paste0(crop, ":")),
              "Cannot fill end of harvested area timeseries because",
              "cropland is zero in reference year\n"
            )
          }
        } else if (!quiet) {
          cat(
            ifelse(is.null(crop), "", paste0(crop, ":")),
            "Cannot fill end of harvested area timeseries because",
            "cropland is NA in reference year\n"
          )
        }
      }
      # Time series extent to the end; if there are still NA values it is
      # because cropland is not available
      next_NA <- Inf
    }
  }
  if (extend_timeseries) {
    return(harv_areas)
  } else {
    # Remove any additional years introduced in time series.
    return(harv_areas[saved_yseq])
  }
}
