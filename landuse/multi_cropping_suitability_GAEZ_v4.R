################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script computes multiple cropping suitability based on agro-climatic  ##
## variables from GAEZ version 4.                                             ##
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
## Variables are expected in gaez_dir. Filenames usually consist of the       ##
## variable name and a suffix describing the time period the data refer to.   ##
## Adjust to specific data you downloaded.                                    ##
variable_filename <- paste0(
  LandInG_setup$landuse$gaez_v4_variables,
  "_CRUTS32_Hist_8110.tif"
)
names(variable_filename) <- LandInG_setup$landuse$gaez_v4_variables
################################################################################

################################################################################
## Thresholds for double cropping and triple cropping                         ##
## These are taken from the GAEZ v. 4.0 documentation (table 3-6, table 3-7)  ##
## Multiple cropping zones C, D & E from GAEZ are considered suitable for     ##
## double cropping.                                                           ##
## Multiple cropping zones F, G & H from GAEZ are considered suitable for     ##
## triple cropping.                                                           ##
## Note: The original GAEZ delineation distinguishes tropics, subtropics and  ##
## temperate zones and further distinguishes three different regimes in       ##
## lowland areas in the tropics. For zones C and D the thresholds for         ##
## tropical lowland areas are used because they correspond to the thresholds  ##
## in the subtropics and temperate zones.                                     ##
## The assumption is that:                                                    ##
## - lgd corresponds to LGP                                                   ##
## - lt2 corresponds to LGPt5                                                 ##
## - lt3 corresponds to LGPt10                                                ##
## - ts3 corresponds to TS10                                                  ##
## - ts2 corresponds to TSG5                                                  ##
## - mcl used for thermal climate class to distinguish tropical lowlands from ##
##   tropical highlands.                                                      ##
single_cropping_threshold <- list(
  lgd = 45,
  lt2 = 120,
  lt3 = 90,
  ts3 = 1200,
  ts2 = NA
)
double_cropping_threshold <- list(
  lgd = c(220, 180, 180),
  # value for tropical highlands, tropical lowlands and subtropics/temperate
  # zones
  lt2 = c(220, 200, 200),
  # value for tropical highlands, tropical lowlands and subtropics/temperate
  # zones
  lt3 = 120,
  ts3 = 3000,
  # value for subtropics and temperate zones
  ts2 = 3200
)
triple_cropping_threshold <- list(
  lgd = 300,
  lt2 = 300,
  lt3 = 240,
  ts3 = c(7000, 7000, 5100),
  # value for tropical highlands, tropical lowlands and subtropics/temperate
  # zones
  ts2 = 5100
)
## If output spatial resolution equals source resolution multiple cropping    ##
## suitability can be derived directly from multi cropping class in GAEZ v4.  ##
## Names of variables providing multi-cropping class directly                 ##
multi_class_rainfed <- "mcr"
multi_class_irrigated <- "mci"
# Class A to H are assigned values 1 through 8 in Multi-cropping class data
single_cropping_from_class <- 2
double_cropping_from_class <- c(3, 4, 5)
triple_cropping_from_class <- c(6, 7, 8)
################################################################################

################################################################################
## Check if gadm_raster has (close to) global extent                          ##
## Takes care of precision errors                                             ##
if (matching_extent(
  terra::ext(LandInG_setup$landuse$gadm_raster),
  global_extent,
  terra::xres(LandInG_setup$landuse$gadm_raster),
  terra::yres(LandInG_setup$landuse$gadm_raster)
)) {
  terra::ext(LandInG_setup$landuse$gadm_raster) <- global_extent
}
################################################################################

################################################################################
## Load GAEZ climate data fields                                              ##
res_match <- TRUE # whether input resolution matches output resolution
for (var in LandInG_setup$landuse$gaez_v4_variables) {
  cat("Loading", sQuote(var), "\n")
  filename <- file.path(LandInG_setup$landuse$gaez_dir, variable_filename[var])
  if (!file.exists(filename) &&
      !var %in% c(multi_class_irrigated, multi_class_rainfed)
  ) {
    # Fail if any of the agroclimatic variables is missing
    stop(
      "Filename ", sQuote(filename), " for GAEZ agro-climatic resource ",
      sQuote(var), " does not exist."
    )
  } else if (!file.exists(filename)) {
    warning(
      "Filename ", sQuote(filename), " for GAEZ agro-climatic resource ",
      sQuote(var), " does not exist.",
      call. = FALSE,
      immediate. = TRUE
    )
    # Skip to next
    next
  }
  fileraster <- terra::rast(filename)
  # At the time of writing this script GAEZ grids are missing one longitude
  # band. Check if this is true and fix it
  if (!matching_extent(
    terra::ext(fileraster),
    terra::ext(LandInG_setup$landuse$gadm_raster),
    terra::xres(fileraster),
    terra::yres(fileraster)
  )) {
    # Crop any parts outside the extent of gadm_raster
    fileraster <- terra::crop(fileraster, LandInG_setup$landuse$gadm_raster)
    # Check eastern and western boundary
    wextend <- (
      terra::xmin(fileraster) - terra::xmin(LandInG_setup$landuse$gadm_raster)
    ) / terra::xres(fileraster)
    eextend <- (
      terra::xmax(LandInG_setup$landuse$gadm_raster) - terra::xmax(fileraster)
    ) / terra::xres(fileraster)
    if (wextend > 2 || eextend > 2) {
      stop(
        "Longitudinal extent of ", sQuote(var),
        " does not match longitudinal extent of GADM mask"
      )
      # Maximum allowed deviation for regridding: 2 cells
    }
    # Find rows with non-missing data (if source data do not cover full north-
    # south extent
    fileraster_valid_y <- which(
      apply(
        terra::as.array(fileraster),
        1,
        function(indata) length(which(!is.na(indata)))
      ) > 0
    )
    gadm_raster_valid_y <- which(
      apply(
        terra::as.array(LandInG_setup$landuse$gadm_raster),
        1,
        function(indata) length(which(!is.na(indata)))
      ) > 0
    )
    # Corresponding coordinates
    fileraster_lats <- terra::yFromRow(fileraster)[fileraster_valid_y]
    gadm_raster_lats <-
      terra::yFromRow(LandInG_setup$landuse$gadm_raster)[gadm_raster_valid_y]
    # Check northern and southern boundary of area with non-missing data
    nextend <- (max(gadm_raster_lats) - max(fileraster_lats)) /
      terra:: yres(fileraster)
    sextend <- (min(fileraster_lats) - min(gadm_raster_lats)) /
      terra::yres(fileraster)
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
    target_extent <- terra::ext(
      min(
        c(terra::xmin(fileraster),
          terra::xmin(LandInG_setup$landuse$gadm_raster))
      ),
      max(
        c(terra::xmax(fileraster),
          terra::xmax(LandInG_setup$landuse$gadm_raster))
      ),
      terra::ymin(fileraster),
      terra::ymax(fileraster)
    )
    # Crop GADM mask to that extent
    gadm_tmp <- terra::crop(LandInG_setup$landuse$gadm_raster, target_extent)
    if (target_extent == terra::ext(fileraster)) {
      fileraster_target <- fileraster
    } else {
      cat(
        "Regridding", sQuote(var), "from", toString(terra::ext(fileraster)),
        "to", toString(target_extent), "\n"
      )
      # Interpolate to target grid
      if (var %in% c("mcl", multi_class_irrigated, multi_class_rainfed)) {
        # Use nearest neighbour for class variables
        fileraster_target <- terra::resample(
          fileraster,
          gadm_tmp,
          method = "near"
        )
      } else {
        # Use bilinear interpolation for other climatic variables
        fileraster_target <- terra::resample(
          fileraster,
          gadm_tmp,
          method = "bilinear"
        )
      }
      # Merge original and interpolated data; only cells missing in original
      # data are taken from interpolated data.
      fileraster_target <- terra::merge(fileraster, fileraster_target)
    }
    # Enlarge to full extent of GADM mask, using NA to fill missing cells
    fileraster <- terra::extend(
      fileraster_target,
      LandInG_setup$landuse$gadm_raster
    )
    rm(fileraster_target)
  }
  if (matching_extent(
    terra::ext(fileraster),
    terra::ext(LandInG_setup$landuse$gadm_raster),
    terra::xres(fileraster),
    terra::yres(fileraster)
  )) {
    # Make sure they are identical given possible numerical precision
    terra::ext(fileraster) <- terra::ext(LandInG_setup$landuse$gadm_raster)
  }
  # If GAEZ resolution in finer than GADM aggregate GAEZ data, but not for
  # multiple cropping classes
  if (
    any(
      terra::res(fileraster) / terra::res(LandInG_setup$landuse$gadm_raster) >
        1.00001
    )
  ) {
    stop(
      "GADM resolution ",
      toString(round(terra::res(LandInG_setup$landuse$gadm_raster), 5)),
      " is too fine for GAEZ resolution ",
      toString(round(terra::res(fileraster), 5)), ".\n",
      "You need to create a gridded GADM mask at the GAEZ resolution."
    )
  } else if (
    any(
      terra::res(fileraster) < terra::res(LandInG_setup$landuse$gadm_raster)) &&
        !var %in% c(multi_class_irrigated, multi_class_rainfed)
  ) {
    res_match <- FALSE
    gaez2gadm <- round(
      terra::res(LandInG_setup$landuse$gadm_raster) / terra::res(fileraster),
      4
    )
    if (max(gaez2gadm %% 1) != 0) {
      stop(
        "GADM resolution ",
        toString(round(terra::res(LandInG_setup$landuse$gadm_raster), 5)),
        " is not compatible with GAEZ resolution ",
        toString(round(terra::res(fileraster), 5))
      )
    }
    cat("Aggregating", sQuote(var), "to GADM resolution\n")
    if (var == "mcl") {
      # Use most frequent value for climate class, highest in case of ties
      fileraster_target_rescaled <- terra::aggregate(
        fileraster,
        rev(gaez2gadm), # res() returns lon/lat, fact is lat/lon
        modal_ties_highest
      )
    } else {
      # Calculate mean for all other variables
      # Turn into array for aggregation
      tmparray <- array(
        ul(fileraster[]),
        dim = c(terra::ncol(fileraster), terra::nrow(fileraster))
      )
      # aggregate_array defined in helper/array_aggregate.R
      fileraster_target_rescaled <- aggregate_array(tmparray, gaez2gadm, "mean")
      # Create new raster out of aggregated array
      fileraster_target_rescaled <- terra::rast(
        t(fileraster_target_rescaled),
        extent = terra::ext(fileraster),
        crs = terra::crs(fileraster)
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
# Bugfix for missing values in lt2
# In missing cells set length of lt2 to length of
# lt3; lt3 is generally shorter than
# lt2.
mismatch <- which(is.na(terra::values(lt2)) & !is.na(terra::values(lt3)))
if (length(mismatch) > 0) {
  message(
    "Info: Using 'lt3' to fill in ", length(mismatch),
    " missing values in 'lt2'"
  )
  lt2[mismatch] <- lt3[mismatch]
}
################################################################################


################################################################################
## Check suitability for single, double, and triple cropping                  ##
## Each agro-climatic resource is compared to its threshold value. Cells are  ##
## only suitable if all agro-climatic resources fulfill thresholds.           ##
if (res_match) {
  # Input resolution is identical to output resolution, derive suitability from
  # Multi-cropping class
  for (irr in c("rainfed", "irrigated")) {
    if (exists(get(paste0("multi_class_", irr)))) {
      thr <- get(get(paste0("multi_class_", irr)))
      for (cropping in c("single", "double", "triple")) {
        cat(
          "Aggregating", irr, "multi-cropping class",
          sQuote(get(paste0("multi_class_", irr))), "from GAEZ directly to",
          paste0(cropping, "_cropping_suitability_", irr), "\n"
        )
        suitability <- terra::rast(
          get(intersect(LandInG_setup$landuse$gaez_v4_variables, ls())[1])
        )
        # Set all cells with any value to 1
        suitability <- terra::mask(thr, thr, updatevalue = 1, inverse = TRUE)
        suitable <- switch(
          cropping,
          single = c(
            single_cropping_from_class,
            double_cropping_from_class,
            triple_cropping_from_class
          ),
          double = c(double_cropping_from_class, triple_cropping_from_class),
          triple = c(triple_cropping_from_class),
          stop("Invalid cropping ", sQuote(cropping))
        )
        suitability <- suitability * terra::rast(
          thr,
          vals = thr[] %in% suitable
        )
        names(suitability) <- paste0(
          cropping, "_cropping_suitability_", irr, "_direct"
        )
        assign(
          paste0(cropping, "_cropping_suitability_", irr, "_direct"),
          suitability
        )
      }
    }
  }
}
for (cropping in c("single", "double", "triple")) {
  for (irr in c("rainfed", "irrigated")) {
    cat(
      "Deriving", irr, cropping,
      "cropping suitability based on agroclimatic variables\n"
    )
    thresholds <- get(paste0(cropping, "_cropping_threshold"))
    # Set up suitability raster and pre-fill with 1 (TRUE)
    template <- setdiff(
      intersect(LandInG_setup$landuse$gaez_v4_variables, ls()),
      c(multi_class_rainfed, multi_class_irrigated)
    )[1]
    suitability <- terra::rast(get(template))
    suitability <- terra::mask(
      get(template),
      get(template),
      updatevalue = 1,
      inverse = TRUE
    )
    for (thr in names(thresholds)) {
      if (thr == "lgd" && irr == "irrigated") {
        # lgd takes into account both temperature and moisture availability.
        # Test only for rainfed crops since irrigation should take care of
        # moisture deficits limiting the growing period for irrigated crops.
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
        threshold_mask <- terra::mask(
          mcl,
          mcl,
          maskvalues = NA,
          updatevalue = thresholds[[thr]][3],
          inverse = TRUE
        )
        # Now tropical highlands (value of 2 in mcl)
        threshold_mask <- terra::mask(
          threshold_mask,
          mcl,
          maskvalues = 2,
          updatevalue = thresholds[[thr]][1]
        )
        # Now tropical lowlands (
        threshold_mask <- terra::mask(
          threshold_mask,
          mcl,
          maskvalues = 1,
          updatevalue = thresholds[[thr]][2]
        )
        suitability <- suitability * (get(thr) >= threshold_mask)
      } else {
        stop("Cannot interpret threshold ", sQuote(toString(thresholds[[thr]])))
      }
    }
    names(suitability) <- paste0(cropping, "_cropping_suitability_", irr)
    assign(paste0(cropping, "_cropping_suitability_", irr), suitability)
  }
}
################################################################################


################################################################################
## Test range of suitabilities. If this is a global dataset it should contain ##
## all values 0 (no cropping suitability) through 3 (triple cropping          ##
## suitability)                                                               ##
if (res_match) {
  if (exists("single_cropping_suitability_rainfed_direct")) {
    rainfed_stats_direct <- terra::unique(
      single_cropping_suitability_rainfed_direct +
        double_cropping_suitability_rainfed_direct +
        triple_cropping_suitability_rainfed_direct
    )
    if (!all(seq(0, 3) %in% unlist(rainfed_stats_direct))) {
      warning(
        "Your dataset does not include all possible values for rainfed ",
        "multiple cropping suitability. This could either be because you ",
        "are running for a limited spatial extent or point to problems in the ",
        "source data. Multiple cropping suitability is aggregated from GAEZ ",
        "Multi-cropping class.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }
  if (exists("single_cropping_suitability_irrigated_direct")) {
    irrigated_stats_direct <- terra::unique(
      single_cropping_suitability_irrigated_direct +
        double_cropping_suitability_irrigated_direct +
        triple_cropping_suitability_irrigated_direct
    )
    if (!all(seq(0, 3) %in% unlist(irrigated_stats_direct))) {
      warning(
        "Your dataset does not include all possible values for ",
        "irrigated multiple cropping suitability. This could either be because ",
        "you are running for a limited spatial extent or point to problems in ",
        "the source data. Multiple cropping suitability is aggregated from ",
        "GAEZ Multi-cropping class.",
        call. = FALSE,
        immediate. = FALSE
      )
    }
  }
}
rainfed_stats <- terra::unique(
  single_cropping_suitability_rainfed +
    double_cropping_suitability_rainfed +
    triple_cropping_suitability_rainfed
)
irrigated_stats <- terra::unique(
  single_cropping_suitability_irrigated +
    double_cropping_suitability_irrigated +
    triple_cropping_suitability_irrigated
)
if (!all(seq(0, 3) %in% unlist(rainfed_stats))) {
  warning(
    "Your dataset does not include all possible values for rainfed ",
    "multiple cropping suitability. This could either be because you ",
    "are running for a limited spatial extent or point to problems in the ",
    "climatic variables or the defined thresholds.",
    call. = FALSE,
    immediate. = TRUE
  )
}
if (!all(seq(0, 3) %in% unlist(irrigated_stats))) {
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
if (res_match && exists(multi_class_rainfed) && exists(multi_class_irrigated)) {
  gaez_multicropping_suit_rf_file_direct <- strsplit(
    LandInG_setup$landuse$gaez_multicropping_suit_rf_file,
    split = ".",
    fixed = TRUE
  )
  gaez_multicropping_suit_rf_file_direct <- sapply(
    gaez_multicropping_suit_rf_file_direct,
    function(indata, insert) {
      paste0(
        paste(indata[-length(indata)], sep = ".", collapse = "."),
        insert,
        ".", indata[length(indata)]
      )
    },
    insert = "_direct"
  )
  gaez_multicropping_suit_ir_file_direct <- strsplit(
    LandInG_setup$landuse$gaez_multicropping_suit_ir_file,
    split = ".",
    fixed = TRUE
  )
  gaez_multicropping_suit_ir_file_direct <- sapply(
    gaez_multicropping_suit_ir_file_direct,
    function(indata, insert) {
      paste0(
        paste(indata[-length(indata)], sep = ".", collapse = "."),
        insert,
        ".", indata[length(indata)]
      )
    },
    insert = "_direct"
  )
  cat(
    "Rainfed multiple cropping suitability derived directly saved to",
    sQuote(gaez_multicropping_suit_rf_file_direct), "\n"
  )
  terra::writeRaster(
    (single_cropping_suitability_rainfed_direct +
       double_cropping_suitability_rainfed_direct +
       triple_cropping_suitability_rainfed_direct
    ),
    filename = gaez_multicropping_suit_rf_file_direct,
    datatype = "INT1S",
    NAflag = -9,
    overwrite = TRUE,
    names = "multi_cropping_suitability_rainfed_direct"
  )
  cat(
    "Irrigated multiple cropping suitability derived directly saved to",
    sQuote(gaez_multicropping_suit_ir_file_direct), "\n"
  )
  terra::writeRaster(
    (single_cropping_suitability_irrigated_direct +
       double_cropping_suitability_irrigated_direct +
       triple_cropping_suitability_irrigated_direct
    ),
    filename = gaez_multicropping_suit_ir_file_direct,
    datatype = "INT1S",
    NAflag = -9,
    overwrite = TRUE,
    names = "multi_cropping_suitability_irrigated_direct"
  )

}
cat(
  "Rainfed multiple cropping suitability saved to",
  sQuote(LandInG_setup$landuse$gaez_multicropping_suit_rf_file), "\n"
)
terra::writeRaster(
  (single_cropping_suitability_rainfed +
     double_cropping_suitability_rainfed +
     triple_cropping_suitability_rainfed
  ),
  filename = LandInG_setup$landuse$gaez_multicropping_suit_rf_file,
  datatype = "INT1S",
  NAflag = -9,
  overwrite = TRUE,
  names = "multi_cropping_suitability_rainfed"
)
cat(
  "Irrigated multiple cropping suitability saved to",
  sQuote(LandInG_setup$landuse$gaez_multicropping_suit_ir_file), "\n"
)
terra::writeRaster(
  (single_cropping_suitability_irrigated +
     double_cropping_suitability_irrigated +
     triple_cropping_suitability_irrigated
  ),
  filename = LandInG_setup$landuse$gaez_multicropping_suit_ir_file,
  datatype = "INT1S",
  NAflag = -9,
  overwrite = TRUE,
  names = "multi_cropping_suitability_irrigated"
)
################################################################################
