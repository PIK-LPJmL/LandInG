################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script computes multiple cropping suitability based on agro-climatic  ##
## variables from GAEZ version 3.                                             ##
################################################################################

# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Setup of variables valid across all scripts related to land use data       ##
## processing.                                                                ##
## - sets many directories and file names                                     ##
## - also loads several helper functions used by various land use processing  ##
##   scripts                                                                  ##
source("landuse_setup.R")
################################################################################


################################################################################
## File name setup                                                            ##
## Variables are expected in subdirectory names under gaez_dir, one variable  ##
## per directory                                                              ##
## ASCII grid file in current download of GAEZ is always named "data.asc".    ##
## If this changes, update file name here.                                    ##
variable_filename <- rep("data.asc", length(gaez_variables))
names(variable_filename) <- gaez_variables
################################################################################

################################################################################
## Thresholds for double cropping and triple cropping                         ##
## These are taken from the GAEZ v. 3.0 documentation (table 3-6, table 3-7)  ##
## Multiple cropping zones C, D & E from GAEZ are considered suitable for     ##
## double cropping.
## Multiple cropping zones F, G & H from GAEZ are considered suitable for     ##
## triple cropping.
## Note: The original GAEZ delineation distinguishes tropics, subtropics and  ##
## temperate zones and further distinguishes high‐land, mid highland, and     ##
## lowland areas in the tropics. For zones C and D the thresholds for         ##
## tropical lowland areas are used because they correspond to the thresholds  ##
## in the subtropics and temperate zones.                                     ##
## The assumption is that:                                                    ##
## - reference_length_growing_period corresponds to LGP                       ##
## - temperature_growing_period corresponds to LGP_t=5                        ##
## - frost_free_period corresponds to LGP_t=10                                ##
## - Tsum_frost_free_period corresponds to TS_t=10                            ##
## - Tsum_growing_temperature_period corresponds to TS-G_t=5                  ##
single_cropping_threshold <- list(
  reference_length_growing_period = 45,
  temperature_growing_period = 120,
  frost_free_period = 90,
  Tsum_frost_free_period = 1000,
  Tsum_growing_temperature_period = NA
)
double_cropping_threshold <- list(
  reference_length_growing_period = c(220, 180, 180),
  # value for tropical highlands, tropical lowlands and subtropics/temperate
  # zones
  temperature_growing_period = c(220, 200, 200),
  # value for tropical highlands, tropical lowlands and subtropics/temperate
  # zones
  frost_free_period = 120,
  Tsum_frost_free_period = 3000,
  # value for subtropics and temperate zones
  Tsum_growing_temperature_period = 3200
)
triple_cropping_threshold <- list(
  reference_length_growing_period = 300,
  temperature_growing_period = 300,
  frost_free_period = 240,
  Tsum_frost_free_period = c(7000, 7000, 5100),
  # value for tropical highlands, tropical lowlands and subtropics/temperate
  # zones
  Tsum_growing_temperature_period = 5100
)
################################################################################

################################################################################
## Check if gadm_raster has (close to) global extent                          ##
## Takes care of precision errors                                             ##
if (matching_extent(
  extent(gadm_raster),
  global_extent,
  xres(gadm_raster),
  yres(gadm_raster)
)) {
  gadm_raster <- setExtent(gadm_raster, global_extent)
}
################################################################################

################################################################################
## Load GAEZ climate data fields                                              ##
for (var in gaez_variables) {
  cat("Loading", var, "\n")
  filename <- file.path(gaez_dir, var, variable_filename[var])
  if (!file.exists(filename)) {
    stop(
      "Filename ", sQuote(filename),
      " for GAEZ agro-climatic resource ", sQuote(var),
      " does not exist.\n",
      "Each variable should be saved in a subdirectory corresponding to its ",
      "name"
    )
  }
  fileraster <- raster(filename)
  # At the time of writing this script GAEZ grids are missing one longitude
  # band. Check if this is true and fix it
  if (!matching_extent(
    extent(fileraster),
    extent(gadm_raster),
    xres(fileraster),
    yres(fileraster)
  )) {
    # Crop any parts outside the extent of gadm_raster
    fileraster <- crop(fileraster, gadm_raster)
    # Check eastern and western boundary
    wextend <- (xmin(fileraster) - xmin(gadm_raster)) / xres(fileraster)
    eextend <- (xmax(gadm_raster) - xmax(fileraster)) / xres(fileraster)
    if (wextend > 2 || eextend > 2) {
      stop(
        "Longitudinal extent of ", sQuote(var),
        " does not match longitudinal extent of GADM mask"
      )
      # Maximum allowed deviation for regridding: 2 cells
    }
    # GAEZ raster files do not cover -90 to +90°N but should at least cover all
    # land cells.
    # Find rows with non-missing data
    fileraster_valid_y <- which(
      apply(
        as.array(fileraster),
        1,
        function(indata) length(which(!is.na(indata)))
      ) > 0
    )
    gadm_raster_valid_y <- which(
      apply(
        as.array(gadm_raster),
        1,
        function(indata) length(which(!is.na(indata)))
      ) > 0
    )
    # Corresponding coordinates
    fileraster_lats <- yFromRow(fileraster)[fileraster_valid_y]
    gadm_raster_lats <- yFromRow(gadm_raster)[gadm_raster_valid_y]
    # Check northern and southern boundary of area with non-missing data
    nextend <- (max(gadm_raster_lats) - max(fileraster_lats)) / yres(fileraster)
    sextend <- (min(fileraster_lats) - min(gadm_raster_lats)) / yres(fileraster)
    if (nextend > 0.01) {
      stop(
        "Northern border does not seem to match between ", sQuote(var),
        " and GADM mask"
      )
    }
    if (sextend > 0.01) {
      warning(
        "Southern-most point in ", sQuote(var), " at ", min(fileraster_lats),
        " degrees does not match southern-most point in GADM mask at ",
        min(gadm_raster_lats), " degrees.\n",
        "Please check that land areas included match.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    # Target extent for regridding
    target_extent <- extent(
      min(c(xmin(fileraster), xmin(gadm_raster))),
      max(c(xmax(fileraster), xmax(gadm_raster))),
      ymin(fileraster),
      ymax(fileraster)
    )
    # Crop GADM mask to that extent
    gadm_tmp <- crop(gadm_raster, target_extent)
    if (target_extent == extent(fileraster)) {
      fileraster_target <- fileraster
    } else {
      cat(
        "Regridding", sQuote(var), "from", toString(extent(fileraster)),
        "to", toString(target_extent), "\n"
      )
      # Interpolate to target grid
      if (var == "thermal_climates") {
        # Use nearest neighbour for class variables
        fileraster_target <- resample(fileraster, gadm_tmp, method = "ngb")
      } else {
        # Use bilinear interpolation for other climatic variables
        fileraster_target <- resample(fileraster, gadm_tmp)
      }
      # Merge original and interpolated data; only cells missing in original
      # data are taken from interpolated data.
      fileraster_target <- merge(fileraster, fileraster_target)
    }
    # Enlarge to full extent of GADM mask, using NA to fill missing cells
    fileraster <- extend(fileraster_target, gadm_raster)
    rm(fileraster_target)
  }
  if (matching_extent(
    extent(fileraster),
    extent(gadm_raster),
    xres(fileraster),
    yres(fileraster)
  )) {
    # Make sure they are identical given possible numerical precision
    fileraster <- setExtent(fileraster, extent(gadm_raster))
  }
  # If GAEZ resolution is finer than GADM aggregate GAEZ data
  if (any(res(fileraster) > res(gadm_raster))) {
    stop(
      "GADM resolution ", toString(round(res(gadm_raster), 5)),
      " is too fine for GAEZ resolution ",
      toString(round(res(fileraster), 5)), ".\n",
      "You need to create a gridded GADM mask at the GAEZ resolution."
    )
  } else if (any(res(fileraster) < res(gadm_raster))) {
    gaez2gadm <- round(res(fileraster) / res(gadm_raster), 4)
    if (max(gaez2gadm %% 1) != 0) {
      stop(
        "GADM resolution ", toString(round(res(gadm_raster), 5)),
        " is not compatible with GAEZ resolution ",
        toString(round(res(fileraster), 5))
      )
    }
    cat("Aggregating", sQuote(var), "to GADM resolution\n")
    if (var == "thermal_climates") {
      # Use most frequent value for climate class, highest in case of ties
      fileraster_target_rescaled <- aggregate(
        fileraster,
        gaez2gadm,
        modal_ties_highest
      )
    } else {
      # Calculate mean for all other variables
      # Turn into array for aggregation
      tmparray <- array(
        values(fileraster),
        dim = c(ncol(fileraster), nrow(fileraster))
      )
      # aggregate_array defined in helper/array_aggregate.R
      fileraster_target_rescaled <- aggregate_array(
        tmparray,
        gaez2gadm,
        "mean",
        FALSE
      )
      # Create new raster out of aggregated array
      fileraster_target_rescaled <- raster(
        t(fileraster_target_rescaled),
        xmn = xmin(fileraster),
        xmx = xmax(fileraster),
        ymn = ymin(fileraster),
        ymx = ymax(fileraster),
        crs = crs(fileraster)
      )
    }
    # Replace original data with aggregated data
    fileraster <- fileraster_target_rescaled
    rm(fileraster_target_rescaled, tmparray)
  } else {
    gaez2gadm <- c(1, 1)
  }
  assign(var, fileraster)
  rm(fileraster)
}
# Bugfix for missing values in temperature_growing_period
# In missing cells set length of temperature_growing_period to length of
# frost_free_period; frost_free_period is generally shorter than
# temperature_growing_period.
mismatch <- which(is.na(values(temperature_growing_period)) &
  !is.na(values(frost_free_period))
)
if (length(mismatch) > 0) {
  message(
    "Info: Using 'frost_free_period' to fill in ", length(mismatch),
    " missing values in 'temperature_growing_period'"
  )
  temperature_growing_period[mismatch] <- frost_free_period[mismatch]
}
################################################################################


################################################################################
## Check suitability for single, double, and triple cropping                  ##
## Each agro-climatic resource is compared to its threshold value. Cells are  ##
## only suitable if all agro-climatic resources fulfill thresholds.           ##
for (cropping in c("single", "double", "triple")) {
  for (irr in c("rainfed", "irrigated")) {
    cat("Deriving", irr, cropping, "cropping suitability\n")
    thresholds <- get(paste0(cropping, "_cropping_threshold"))
    # Set up suitability raster and pre-fill with 1 (TRUE)
    suitability <- raster(get(gaez_variables[1]))
    values(suitability) <- rep(1, ncell(suitability))
    for (thr in names(thresholds)) {
      if (thr == "reference_length_growing_period" && irr == "irrigated") {
        # reference_length_growing_period takes into account both temperature
        # and moisture availability. Test only for rainfed crops since
        # irrigation should take care of moisture deficits limiting the growing
        # period for irrigated crops.
        cat(
          "No test of", sQuote(thr), "for", irr, cropping,
          "cropping suitability\n"
        )
        next
      }
      if (all(is.na(thresholds[[thr]]))) {
        # No threshold available
        next
      }
      if (length(thresholds[[thr]]) == 1) {
        # Only one threshold value for all climate zones
        suitability <- suitability * (get(thr) >= thresholds[[thr]])
      } else if (length(thresholds[[thr]]) == 3) {
        # Threshold depends on climate zone
        # First subtropics/temperate value in all cells
        threshold_mask <- mask(
          thermal_climates, thermal_climates,
          maskvalue = NA,
          updatevalue = thresholds[[thr]][3],
          inverse = TRUE
        )
        # Now tropical highlands (value of 2 in thermal_climates)
        threshold_mask <- mask(
          threshold_mask,
          thermal_climates,
          maskvalue = 2,
          updatevalue = thresholds[[thr]][1]
        )
        # Now tropical lowlands (
        threshold_mask <- mask(
          threshold_mask,
          thermal_climates,
          maskvalue = 1,
          updatevalue = thresholds[[thr]][2]
        )
        suitability <- suitability * (get(thr) >= threshold_mask)
      } else {
        stop("Cannot interpret threshold ", sQuote(toString(thresholds[[thr]])))
      }
    }
    assign(paste0(cropping, "_cropping_suitability_", irr), suitability)
  }
}
################################################################################


################################################################################
## Test range of suitabilities. If this is a global dataset it should contain ##
## all values 0 (no cropping suitability) through 3 (triple cropping          ##
## suitability)                                                               ##
rainfed_stats <- cellStats(
  single_cropping_suitability_rainfed +
    double_cropping_suitability_rainfed +
    triple_cropping_suitability_rainfed,
  unique
)
irrigated_stats <- cellStats(
  single_cropping_suitability_irrigated +
    double_cropping_suitability_irrigated +
    triple_cropping_suitability_irrigated,
  unique
)
if (!all(c(0:3) %in% rainfed_stats)) {
  warning(
    "Your dataset does not include all possible values for rainfed ",
    "multiple cropping suitability. This could either be because you ",
    "are running for a limited spatial extent or point to problems in the ",
    "climatic variables or the defined thresholds.",
    call. = FALSE,
    immediate. = TRUE
  )
}
if (!all(c(0:3) %in% irrigated_stats)) {
  warning(
    "Your dataset does not include all possible values for ",
    "irrigated multiple cropping suitability. This could either be because ",
    "you are running for a limited spatial extent or point to problems in ",
    "the climatic variables or the defined thresholds.",
    call. = FALSE,
    immediate. = TRUE
  )
}
################################################################################


################################################################################
## Write suitability to files gaez_multicropping_suit_rf_file and             ##
## gaez_multicropping_suit_ir_file set up in landuse_setup.R                  ##
cat(
  "Rainfed multiple cropping suitability saved to",
  sQuote(gaez_multicropping_suit_rf_file), "\n"
)
writeRaster(
  (single_cropping_suitability_rainfed +
    double_cropping_suitability_rainfed +
    triple_cropping_suitability_rainfed
  ),
  filename = gaez_multicropping_suit_rf_file,
  datatype = "INT1S",
  NAflag = -9,
  overwrite = TRUE
)
cat(
  "Irrigated multiple cropping suitability saved to",
  sQuote(gaez_multicropping_suit_ir_file), "\n"
)
writeRaster(
  (single_cropping_suitability_irrigated +
    double_cropping_suitability_irrigated +
    triple_cropping_suitability_irrigated
  ),
  filename = gaez_multicropping_suit_ir_file,
  datatype = "INT1S",
  NAflag = -9,
  overwrite = TRUE
)
################################################################################
