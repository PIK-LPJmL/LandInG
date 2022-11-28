################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## The script derives total, rainfed und irrigated harvested areas at country ##
## scale that should fit into rainfed and irrigated cropland given  climatic  ##
## suitability for single, double or triple cropping. Uses intermediate data  ##
## created by country_level_data.R and harvested_fraction.R so run these      ##
## scripts first. Also used GAEZ multicropping suitability created by         ##
## multi_cropping_suitability_GAEZ.R.                                         ##
##                                                                            ##
## Harvested area time series created by this script:                         ##
## - tot_ha_timeseries (country-level, crop-specific total harvested areas    ##
##   constrained to fit into HYDE total cropland)                             ##
## - tot_ha_timeseries_unconstrained (country-level, crop-specific total      ##
##   harvested areas, not constrained to fit into HYDE total cropland)        ##
## "tot_ha_timeseries" should be used in spatial disaggregation, set as       ##
## total_version_to_use in landuse_setup.R                                    ##
##                                                                            ##
## - irr_ha_timeseries_unconstrained (country-level, crop-specific irrigated  ##
##   harvested areas; only constrained to "tot_ha_timeseries_unconstrained"   ##
##   after extrapolation;                                                     ##
##   finally constrained to fit into HYDE irrigated cropland)                 ##
## - irr_ha_timeseries_unconstrained_faostat (country-level, crop-specific    ##
##   irrigated harvested areas; constrained to                                ##
##   "tot_ha_timeseries_unconstrained" before and after extrapolation;        ##
##   finally constrained to fit into HYDE irrigated cropland)                 ##
## - irr_ha_timeseries (country-level, crop-specific irrigated harvested      ##
##   areas only constrained to "tot_ha_timeseries" after extrapolation;       ##
##   finally constrained to fit into HYDE irrigated cropland)                 ##
## - irr_ha_timeseries_faostat (country-level, crop-specific irrigated        ##
##   harvested areas; constrained to "tot_ha_timeseries" before and after     ##
##   extrapolation;                                                           ##
##   finally constrained to fit into HYDE irrigated cropland)                 ##
## Define which one of these four to use for spatial disaggregation in        ##
## irrigated_version_to_use in landuse_setup.R. We suggest to use             ##
## "irr_ha_timeseries_unconstrained".                                         ##
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
## Load pre-processed data calculated by other scripts, FAOSTAT definitions,  ##
## GADM country mapping and other pre-requisits.                              ##
## Load matching FAOSTAT and Monfreda crops (created by country_level_data.R) ##
cat(
  "Loading FAOSTAT data preprocessed by country_level_data.R and",
  "harvested_fraction.R from",
  sQuote(fao_monfreda_country_RData), "\n"
)
load(fao_monfreda_country_RData)
if (!harvested_fraction_processed) {
  stop(
    "It seems that ", sQuote(fao_monfreda_country_RData),
    "has not been processed by harvested_fraction.R yet. After generating the ",
    "file with country_level_data.R, please run harvested_fraction.R before ",
    "running this script."
  )
}
cat("*** Output period:", paste(range(output_period), collapse = "-"), "***\n")
if (min(output_period) < min(hyde_period) ||
  max(output_period) > max(hyde_period)
) {
  warning(
    "Some years of the output period ",
    min(output_period), "-", max(output_period),
    " are outside the range of HYDE cropland data (",
    min(hyde_period), "-", max(hyde_period),
    ")",
    call. = FALSE,
    immediate. = TRUE
  )
}
years <- as.integer(dimnames(production_array_filled)[[4]])
if (min(output_period) > min(years) || max(output_period) < max(years)) {
  warning(
    "Your defined output period ",
    min(output_period), "-", max(output_period),
    " is shorter than the period covered by FAOSTAT data (",
    min(years), "-", max(years),
    ")",
    call. = FALSE,
    immediate. = TRUE
  )
}
# FAOSTAT definitions and standards
cat(
  "FAOSTAT crop item definitions loaded from",
  toString(sQuote(c(fao_production_item_group_file, fao_production_item_file))),
  "\n"
)
fao_production_item_group_def <- fread(
  fao_production_item_group_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_production_item_def <- fread(
  fao_production_item_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
cat(
  "FAO country definitions:",
  toString(
    sQuote(
      c(fao_production_country_file, fao_production_country_group_file,
        fao_landuse_country_file, fao_landuse_country_group_file)
    )
  ),
  "\n"
)
fao_production_country_def <- fread(
  fao_production_country_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_production_country_group_def <- fread(
  fao_production_country_group_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_landuse_country_def <- fread(
  fao_landuse_country_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_landuse_country_group_def <- fread(
  fao_landuse_country_group_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
# Country mapping
cat("FAO to GADM mapping:", toString(sQuote(fao_gadm_mapping_file)), "\n")
for (filename in fao_gadm_mapping_file) {
  source(filename)
}
cat("FAO to MIRCA mapping:", toString(sQuote(fao_mirca_mapping_file)), "\n")
source(fao_mirca_mapping_file)

# GADM administrative levels
# The files gadmlevel_file and gadmlevel_names_file are generated by scripts
# in ../gadm so make sure to run these first.
cat(
  "Loading gridded GADM administrative unit data from",
  toString(sQuote(c(gadmlevel_file))), "\n"
)
gadmlevel_raster <- brick(gadmlevel_file)
gadmlevel_names <- read.csv(
  gadmlevel_names_file,
  stringsAsFactors = FALSE,
  comment.char = "#"
)
# Extract country IDs, codes and names from table
index <- which(!is.na(gadmlevel_names$level0_ID))
cols <- c("level0_ID", "level0_code", "country")
gadm_country_names <- gadmlevel_names[index, cols]
# Load GAEZ multicropping suitability
cat(
  "Loading GAEZ multiple cropping suitability from",
  toString(
    sQuote(c(gaez_multicropping_suit_ir_file, gaez_multicropping_suit_rf_file))
  ),
  "\n"
)
gaez_multicropping_suit_ir <- raster(gaez_multicropping_suit_ir_file)
gaez_multicropping_suit_rf <- raster(gaez_multicropping_suit_rf_file)
# Set minimum cropping suitability to 1
# -> Assume that any cropland according to HYDE cropland supports at least
#    single cropping
# -> Take care of differences in land masks
# Set missing values (NA) to 1
gaez_multicropping_suit_rf <- mask(
  gaez_multicropping_suit_rf,
  gaez_multicropping_suit_rf,
  maskvalue = NA,
  updatevalue = 1,
  updateNA = TRUE
)
gaez_multicropping_suit_ir <- mask(
  gaez_multicropping_suit_ir,
  gaez_multicropping_suit_ir,
  maskvalue = NA,
  updatevalue = 1,
  updateNA = TRUE
)
# Set 0 (no cropping suitability) to 1
gaez_multicropping_suit_rf <- mask(
  gaez_multicropping_suit_rf,
  gaez_multicropping_suit_rf,
  maskvalue = 0,
  updatevalue = 1
)
gaez_multicropping_suit_ir <- mask(
  gaez_multicropping_suit_ir,
  gaez_multicropping_suit_ir,
  maskvalue = 0,
  updatevalue = 1
)
# Check if extent and resolutions of various raster objects match
for (rastercheck in c(
  "gadmlevel_raster",
  "gaez_multicropping_suit_ir",
  "gaez_multicropping_suit_rf"
)) {
  tmpraster <- get(rastercheck)
  if (matching_extent(
    extent(tmpraster),
    global_extent,
    xres(tmpraster),
    yres(tmpraster)
  )) {
    tmpraster <- setExtent(tmpraster, global_extent)
  }
  assign(rastercheck, tmpraster)
  if (!matching_extent(
    extent(gadm_raster),
    extent(tmpraster),
    xres(gadm_raster),
    yres(gadm_raster)
  )) {
    stop(
      sQuote(gadmlevel_file), " and ", rastercheck,
      " have different spatial extent"
    )
  }
  if (any(res(gadm_raster) != res(tmpraster))) {
    stop(
      sQuote(gadmlevel_file), " and ", rastercheck, " have different resolution"
    )
  }
  rm(tmpraster)
}
# Clean encoding of various tables read in from external files
message("Converting special characters read from text files if necessary.")
for (table in c(
  "fao_production_country_def",
  "fao_production_item_group_def",
  "gadm_country_names",
  "gadmlevel_names",
  "crop_type_mapping",
  "fao_production_country_group_def",
  "fao_production_item_def",
  "fao_landuse_country_group_def",
  "fao_landuse_country_def"
)) {
  if (!exists(table))
    next
  table_data <- get(table)
  for (c in colnames(table_data)) {
    if (typeof(table_data[, c]) == "character") {
      if (!all(stri_enc_isascii(table_data[, c]), na.rm = TRUE)) {
        # String has non-ASCII characters
        if (!all(stri_enc_isutf8(table_data[, c]), na.rm = TRUE)) {
          # String has non-UTF8 characters -> assume windows-1252 encoding and
          # convert to UTF-8
          nessage(
            "Converting column ", sQuote(c),
            " from windows-1252 to UTF-8 encoding in ", table
          )
          table_data[, c] <- stri_encode(
            table_data[, c],
            "windows-1252",
            "UTF-8"
          )
        }
        # Convert UTF-8 strings to ASCII strings, if necessary translating
        # special characters
        message(
          "Converting column ", sQuote(c),
          " from UTF-8 to ASCII encoding in ", table
        )
        table_data[, c] <- stri_encode(table_data[, c], "UTF-8", "UTF-8")
        table_data[, c] <- stri_trans_general(table_data[, c], "latin-ascii")
      }
    }
  }
  assign(table, table_data)
}
################################################################################

################################################################################
## Which crops are included in "nes" groups?                                  ##
## Tries to match names (punctuation and white space is not always            ##
## consistently used in FAOSTAT)                                              ##
## Check fao_production_item_group_file manually if all of these group        ##
## associations are still valid.                                              ##
## These are used to disaggregate "nes" crops into individual crops belonging ##
## to that group if possible.                                                 ##
nes_crops <- setup_nes_groups(
  dimnames(fao_monfreda_production_array)[[2]],
  fao_production_item_group_def,
  fao_drop_crops
)
################################################################################

################################################################################
## Consistency checks between FAOSTAT and GADM datasets                       ##
# Check consistency between compound_countries, fao_production_country_def,
# fao_landuse_country_def
# Combine country definitions lists because there are sometimes inconsistencies
# between them
clist <- rbind(
  fao_production_country_def[, c("Country", "End.Year")],
  fao_landuse_country_def[, c("Country", "End.Year")]
)
# Countries with end year
index <- which(!is.na(clist$End.Year))
if (length(setdiff(names(compound_countries), clist$Country[index])) > 0) {
  # compound_countries has countries not mentioned in
  # fao_production_country_def or fao_landuse_country_def,
  # remove
  for (c in setdiff(names(compound_countries), clist$Country[index])) {
    compound_countries[[c]] <- NULL
  }
}
# Check that successor countries actually exist in FAOSTAT data.
# Note: Depending on the version, FAOSTAT may list countries as ceasing to exist
# while not listing any successor countries.
for (country in intersect(names(compound_countries), clist$Country[index])) {
  country_mismatch <-  which(
    is.na(match(compound_countries[[country]], clist$Country))
  )
  if (length(country_mismatch) == length(compound_countries[[country]])) {
    # All successor countries listed in compound_countries are missing in
    # FAOSTAT. Remove entry from compound_countries.
    warning(
      "None of the successor countries listed in compound_countries ",
      "for country ", sQuote(country), " are available in FAOSTAT data.\n",
      "Dropping from compound_countries",
      call. = FALSE,
      immediate. = TRUE
    )
    # Remove country from compound_countries
    compound_countries[[country]] <- NULL
  } else if (length(country_mismatch) > 0) {
    # Not all listed successor countries are available in FAOSTAT
    for (country2 in compound_countries[[country]][country_mismatch]) {
      # Find potential duplicates
      index2 <- match(country2, names(fao_gadm_country_mapping))
      dupl <- names(
        which(
          sapply(
            fao_gadm_country_mapping[-index2],
            identical,
            y = fao_gadm_country_mapping[[country2]]
          )
        )
      )
      if (any(!dupl %in% clist$Country)) {
        warning(
          sQuote(country2), " listed as successor country to ",
          sQuote(country), " in compound_countries is missing in FAOSTAT data",
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }
  }
}
# Check for countries with endyear in FAOSTAT not listed in
# fao_gadm_country_mapping
if (length(setdiff(clist$Country[index], names(compound_countries))) > 0) {
  # FAOSTAT has countries with end year missing in compound_countries
  for (country in setdiff(clist$Country[index], names(compound_countries))) {
    warning(
      sQuote(country), " is missing in compound_countries ",
      "but is listed as ceasing to exist in ",
      unique(clist[which(clist$Country == country), "End.Year"]),
      ".\nChanging end year. Please update compound_countries if there are ",
      "successor countries.",
      call. = FALSE,
      immediate. = TRUE
    )
    # Remove end year because we do not have information on successor countries
    index <- which(fao_production_country_def$Country == country)
    fao_production_country_def[index, "End.Year"] <- NA
    index <- which(fao_landuse_country_def$Country == country)
    fao_landuse_country_def[, "End.Year"] <- NA
  }
}
# Determine country groups based on fao_production_country_group_def and
# fao_landuse_country_group_def
fao_groups <- list()
# Combine country definitions lists because there are sometimes inconsistencies
# between them
cols <- c("Country.Group", "Country.Group.Code")
clist <- abind(
  fao_production_country_def[, c("Country", "Country.Code")],
  fao_production_country_group_def[, c("Country", "Country.Code")],
  fao_production_country_group_def[, cols],
  fao_landuse_country_group_def[, c("Country", "Country.Code")],
  fao_landuse_country_group_def[, cols],
  along = 1
)
# All country group codes
group_codes <- unique(
  c(
    fao_production_country_group_def$Country.Group.Code,
    fao_landuse_country_group_def$Country.Group.Code
  )
)
for (group_code in intersect(clist[, 2], group_codes)) {
  # Countries belonging to that group code in
  # fao_production_country_group_def
  index1 <- which(
    fao_production_country_group_def$Country.Group.Code == group_code
  )
  # Countries belonging to that group code in
  # fao_landuse_country_group_def
  index2 <- which(
    fao_landuse_country_group_def$Country.Group.Code == group_code
  )
  # Country names according to both fao_production_country_group_def and
  # fao_landuse_country_group_def
  group_country_codes <- union(
    fao_production_country_group_def$Country.Code[index1],
    fao_landuse_country_group_def$Country.Code[index2]
  )
  # Match country codes in fao_production_country_group_def
  index1 <- match(
    group_country_codes,
    fao_production_country_group_def$Country.Code
  )
  # Match country codes in fao_landuse_country_group_def
  index2 <- match(
    group_country_codes,
    fao_landuse_country_group_def$Country.Code
  )
  # Match codes with names
  group_country_names <- ifelse(
    group_country_codes %in% as.integer(clist[, 2]),
    clist[match(group_country_codes, as.integer(clist[, 2])), 1],
    ifelse(
      group_country_codes %in% fao_production_country_group_def$Country.Code,
      fao_production_country_group_def$Country[index1],
      fao_landuse_country_group_def$Country[index2]
    )
  )
  index3 <- match(group_code, clist[, 2])
  # Set group to countries in group that are included in
  # fao_gadm_country_mapping
  fao_groups[[clist[index3, 1]]] <- intersect(
    group_country_names,
    names(fao_gadm_country_mapping)
  )
  rm(index1, index2, index3)
}
# Add country groups to fao_gadm_country_mapping and fao_mirca_country_mapping
cat(
  "*** Filling country groups from FAOSTAT in GADM country mapping",
  "and MIRCA country mapping ***\n"
)
cat(
  "Country groups derived from fao_production_country_group_def",
  "and fao_landuse_country_group_def\n"
)
# Manual additions
# Allows to add countries to groups if they are missing in
# fao_production_country_group_def
man_add <- list(
  "Saint-Martin (French Part)" = c("Americas", "Caribbean"),
  "Saint-Martin (French part)" = c("Americas", "Caribbean")
)
for (country in intersect(names(man_add), names(fao_gadm_country_mapping))) {
  for (group in intersect(man_add[[country]], names(fao_groups))) {
    message(
      "Manually adding ", sQuote(country),
      " to country group ", sQuote(group)
    )
    fao_groups[[group]] <- unique(c(fao_groups[[group]], country))
  }
}
for (country in names(fao_groups)) {
  cat(
    "Filling", sQuote(country),
    "in GADM country list and MIRCA country list\n"
  )
  fao_gadm_country_mapping[[country]] <- unique(
    unlist(fao_gadm_country_mapping[fao_groups[[country]]])
  )
  fao_mirca_country_mapping[[country]] <- unique(
    unlist(fao_mirca_country_mapping[fao_groups[[country]]])
  )
}
# Check for GADM countries not included in FAOSTAT
mismatch <- which(!gadm_country_names$level0_code %in%
  unlist(fao_gadm_country_mapping)
)
if (length(mismatch) > 0) {
  cat(
    "The following GADM units have no corresponding FAO country:",
    toString(sQuote(gadm_country_names$country[mismatch])), "\n"
  )
  # Add GADM countries missing in FAOSTAT to group "World"
  if ("World" %in% names(fao_gadm_country_mapping)) {
    cat("Adding them to FAO country group 'World'\n")
    fao_gadm_country_mapping[["World"]] <- c(
      fao_gadm_country_mapping[["World"]],
      gadm_country_names$level0_code[mismatch]
    )
  }
}
# Remove potential duplicates in fao_gadm_country_mapping.
# There are some duplicates in fao_gadm_country_mapping.R because FAOSTAT
# renamed some countries and we keep old names for backwards compatibility.
for (country in setdiff(
  names(fao_gadm_country_mapping),
  dimnames(fao_monfreda_production_array)[[1]]
)) {
  index <- match(country, names(fao_gadm_country_mapping))
  dupl <- which(
    sapply(
      fao_gadm_country_mapping[-index],
      identical,
      y = fao_gadm_country_mapping[[country]]
    )
  )
  if (length(dupl) > 0) {
    cat(
      "Removing", sQuote(country),
      "from fao_gadm_country_mapping because it seems to be a duplicate of",
      toString(sQuote(names(dupl))), "\n"
    )
    fao_gadm_country_mapping[[country]] <- NULL
  }
}
# Now check again for duplicates
for (country in names(fao_gadm_country_mapping)) {
  index <- match(country, names(fao_gadm_country_mapping))
  dupl <- which(
    sapply(
      fao_gadm_country_mapping[-index],
      identical,
      y = fao_gadm_country_mapping[[country]]
    )
  )
  if (length(dupl) > 0) {
    warning(
      sQuote(country),
      " seems to be a duplicate of ", toString(sQuote(names(dupl))),
      " in fao_gadm_country_mapping",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}
# Associate grid cells with countries
cat("Associating GADM grid cells with FAO countries\n")
fao_gadm_country_cells <- list()
tmpcodes <- values(subset(gadmlevel_raster, 1))
for (country in names(fao_gadm_country_mapping)) {
  # Find country IDs used in  gadmlevel_raster.
  # Each country may consist of several GADM units.
  index1 <- match(
    fao_gadm_country_mapping[[country]],
    gadm_country_names$level0_code
  )
  # Find cells with matching IDs.
  index2 <-  which(tmpcodes %in% gadm_country_names$level0_ID[index1])
  fao_gadm_country_cells[[country]] <- index2
}
rm(tmpcodes)
################################################################################

################################################################################
## Load HYDE cropland data for full output_period and aggregate to country    ##
## level.                                                                     ##
## Also create country-level timeseries of maximum harvested areas based on   ##
## GAEZ multicropping suitability.                                            ##
hyde_cropland_country_timeseries <- array(
  dim = c(
    dim(fao_monfreda_production_array)[1],
    length(min(output_period):max(output_period))
  ),
  dimnames = list(
    dimnames(fao_monfreda_production_array)[[1]],
    format(
      min(output_period):max(output_period),
      scientific = FALSE,
      trim = TRUE
    )
  )
)
# Country time series variables
hyde_irrigated_country_timeseries <-
  hyde_gaez_max_ha_country_timeseries <-
  hyde_gaez_max_ir_ha_country_timeseries <-
  hyde_gaez_max_rf_ha_on_rf_country_timeseries <-
  hyde_gaez_max_rf_ha_on_ir_country_timeseries <-
  hyde_cropland_country_timeseries
# Country variables for maximum cropland extent
hyde_max_cropland_country <- hyde_max_irrigated_country <- array(
  dim = dim(fao_monfreda_production_array)[1],
  dimnames = dimnames(fao_monfreda_production_array)[1]
)

hyde_is_fraction <- !ud.are.convertible(hyde_area_units, "m2")
# Load HYDE area in native HYDE resolution
# Function load_hyde_area defined in helper/load_hyde_area.R
hyde_area <- load_hyde_area(
  filename = hyde_area_file,
  fileunits = hyde_area_file_units,
  faounits = fao_area_units, # Make sure to use same unit below
  unitraster = NULL, # Keep native HYDE spatial resolution
  gextent = global_extent
)
hyde_area_file_units <- hyde_area$unit
hyde_area <- hyde_area$area

# Generate file with time-maximum rainfed, irrigated and total cropland
for (var in c("cropland", "irrigated", "rainfed")) {
  # NetCDF file containing full time series
  filename <- get(paste0("hyde_max_", var, "_filename"))
  # NetCDF variable name
  varname <- get(paste0("hyde_", var, "_varname"))
  if (!file.exists(filename)) {
    # Only create file if it does not exist yet because it takes some time,
    # otherwise reuse existing file.
    cat("Need to create", sQuote(filename), "\n")
    source_filename <- get(paste0("hyde_", var, "_filename"))
    if (!exists("hyde_target_area")) {
      # Load HYDE area and aggregate to GADM resolution
      hyde_target_area <- load_hyde_area(
        filename = hyde_area_file,
        fileunits = hyde_area_file_units,
        faounits = fao_area_units, # Make sure to use same unit as above
        unitraster = gadm_raster,
        gextent = global_extent
      )
      hyde_area_file_units <- hyde_target_area$unit
      hyde_target_area <- hyde_target_area$area
    }
    # Function create_hyde_timeseries_max defined in
    # helper/create_hyde_timeseries_max.R
    # This function requires access to CDO tools.
    create_hyde_timeseries_max(
      source = source_filename,
      target = filename,
      varname = varname,
      sourcearea = hyde_area,
      targetarea = hyde_target_area,
      targetraster = gadm_raster
    )
  }
  # Load maximum area (either created above or in previous script run)
  if (file.exists(filename)) {
    cat("Loading maximum", sQuote(var), "extent from", sQuote(filename), "\n")
    zz <- nc_open(filename)
    tmpdata <- raster(filename)
    # Check that units match
    if (hyde_area_units != zz$var[[varname]]$units) {
      if (ud.are.convertible(hyde_area_units, zz$var[[varname]]$units)) {
        # Convert unit
        warning(
          "Unit in ", sQuote(filename), " [", zz$var[[varname]]$units,
          "] differs from hyde_area_units [", hyde_area_units, "]. Fixing.",
          call. = FALSE,
          immediate. = TRUE
        )
        tmpdata <- tmpdata *
          ud.convert(1, zz$var[[varname]]$units, hyde_area_units)
      } else {
        # Units cannot be converted
        stop(
          "Unit in ", sQuote(filename), " [", zz$var[[varname]]$units,
          "] differs from hyde_area_units [", hyde_area_units, "].\n",
          "They are not directly convertible. Aborting."
        )
      }
    }
    if (matching_extent(
      extent(tmpdata),
      global_extent,
      xres(tmpdata),
      yres(tmpdata)
    )) {
      tmpdata <- setExtent(tmpdata, global_extent)
    }
    if (!matching_extent(
      extent(tmpdata),
      extent(gadm_raster),
      xres(tmpdata),
      yres(tmpdata)
    )) {
      stop("Spatial extent of ", sQuote(filename), " differs from gadm_raster")
    }
  } else {
    # File with maximum area does not exist.
    stop(
      sQuote(filename),
      " should either have been created by this script or been created before",
      " but it does not exist."
    )
  }
  # Make sure raster tmpdata is in memory and not just a pointer to a file.
  # raster package sometimes does not load values into memory automatically.
  if (fromDisk(tmpdata)) {
    # Load values from file into memory
    tmpdata <- readAll(tmpdata)
  }
  # Assign to variable name specific to cropland, rainfed or irrigated
  assign(paste0("hyde_max_", var), tmpdata)
  rm(tmpdata)
}
# Aggregate time-maximum rainfed, irrigated and total cropland to countries
for (country in dimnames(hyde_max_cropland_country)[[1]]) {
  hyde_max_cropland_country[country] <- sum(
    hyde_max_cropland[fao_gadm_country_cells[[country]]],
    na.rm = TRUE
  )
  hyde_max_irrigated_country[country] <- sum(
    hyde_max_irrigated[fao_gadm_country_cells[[country]]],
    na.rm = TRUE
  )
}
# Consistency check
if (any(values(hyde_max_rainfed) > values(hyde_max_cropland), na.rm = TRUE)) {
  incon <- length(which(values(hyde_max_rainfed) > values(hyde_max_cropland)))
  warning(
    "There seems to be a consistency problem between rainfed and ",
    "total cropland because hyde_max_rainfed has ",
    incon, ifelse(incon != 1, " values that are", " value that is"),
    " larger than total cropland hyde_max_cropland.",
    call. = FALSE,
    immediate. = TRUE
  )
}
if (any(values(hyde_max_irrigated) > values(hyde_max_cropland), na.rm = TRUE)) {
  incon <- length(
    which(values(hyde_max_irrigated) > values(hyde_max_cropland))
  )
  warning(
    "There seems to be a consistency problem between irrigated and ",
    "total cropland because hyde_max_irrigated has ",
    incon, ifelse(incon != 1, " values that are", " value that is"),
    " larger than total cropland hyde_max_cropland.",
    call. = FALSE,
    immediate. = TRUE
  )
}
# Load time series of rainfed, irrigated and total cropland per year because
# whole time series is probably too large for memory.
# Since this aggregation step takes quite long results are saved as in RData
# file so they can be reused if running the script again.
if (file.exists(hyde_country_sums_RData)) {
  cat(
    "Reloading HYDE cropland and maximum harvested areas aggregated to",
    "countries from",
    sQuote(hyde_country_sums_RData), "\n"
  )
  load(hyde_country_sums_RData)
  if (!identical(
    dimnames(hyde_cropland_country_timeseries),
    list(
      dimnames(fao_monfreda_production_array)[[1]],
      format(
        min(output_period):max(output_period),
        scientific = FALSE,
        trim = TRUE
      )
    )
  )) {
    stop(
      "Data in ", sQuote(hyde_country_sums_RData),
      " is incompatible with this script run.\n",
      "Delete file to force re-computation."
    )
  }
  # Print inconsistencies detected during calculation of time series
  cat(paste(incon_msg, collapse = "\n"), "\n")
} else {
  # Collect inconsistencies to tell user even if skipping calculation of time
  # series
  incon_msg <- character(0)
  for (year in intersect(
    min(output_period):max(output_period),
    min(hyde_period):max(hyde_period)
  )) {
    for (var in c("hyde_cropland", "hyde_irrigated", "hyde_rainfed")) {
      # NetCDF variable name
      varname <- get(paste0(var, "_varname"))
      if (exists(paste0(var, "_file"))) {
        # Select previously opened NetCDF file
        assign("ncfile", get(paste0(var, "_file")))
      } else {
        # Only open NetCDF files for first year, then keep opened
        ncfile <- nc_open(get(paste0(var, "_filename")))
        # Save opened NetCDF file pointer in file-specific variable
        assign(paste0(var, "_file"), ncfile)
      }
      # Check that units in file match hyde_area_units
      if (!is.null(ncfile$var[[varname]]$units) &&
        ud.convert(1, ncfile$var[[varname]]$units, hyde_area_units) != 1
      ) {
        # Print warning about unit mismatch only once for first year
        if (year == max(min(output_period), min(hyde_period))) {
          warning(
            "Unit in NetCDF ", sQuote(ncfile$filename), " ",
            sQuote(ncfile$var[[varname]]$units),
            " does not match hyde_area_units ", sQuote(hyde_area_units),
            call. = FALSE,
            immediate. = TRUE
          )
        }
        hyde_area_units <- ncfile$var[[varname]]$units
      }
      if (year == max(min(output_period), min(hyde_period))) {
        # Print message only once for first year
        cat(
          "Loading", sQuote(var), "from", sQuote(ncfile$filename),
          "and aggregating to countries\n"
        )
      }
      # Check latitudinal direction of data
      hyde_flip <- (yFromRow(gadm_raster, 1) > yFromRow(gadm_raster, 2)) !=
        (ncfile$dim$lat$vals[1] > ncfile$dim$lat$vals[2])
      # NetCDF resolution
      res_x <- abs(ncfile$dim$lon$vals[2] - ncfile$dim$lon$vals[1])
      res_y <- abs(ncfile$dim$lat$vals[2] - ncfile$dim$lat$vals[1])
      hyde_extent <- extent(
        min(ncfile$dim$lon$vals) - res_x / 2,
        max(ncfile$dim$lon$vals) + res_x / 2,
        min(ncfile$dim$lat$vals) - res_y / 2,
        max(ncfile$dim$lat$vals) + res_y / 2
      )
      # Compare with GADM spatial extent
      if (!matching_extent(
        hyde_extent,
        extent(gadm_raster),
        xres(gadm_raster),
        yres(gadm_raster)
      )) {
        stop(
          "Spatial extent of HYDE variable ", sQuote(var),
          " does not match GADM extent"
        )
      }
      # Check whether resolutions match or aggregation is necessary
      hyde2gadm <- round(c(res_x, res_y) / res(gadm_raster), 4)
      if (max(hyde2gadm %% 1) != 0) {
        stop(
          "HYDE resolution ", toString(round(c(res_x, res_y), 5)),
          " is not an integer multiple of GADM resolution ",
          toString(round(res(gadm_raster), 5))
        )
      }
      if (any(hyde2gadm < 1)) {
        stop(
          "HYDE resolution ", toString(round(c(res_x, res_y), 5)),
          " is too coarse for GADM resolution ",
          toString(round(res(gadm_raster), 5)), "\n",
          "Target resolution cannot be finer than any of the source datasets."
        )
      }

      ncfiledata <- ncvar_get(
        nc = ncfile,
        varid = varname,
        start = c(1, 1, year - min(hyde_period) + 1),
        count = c(-1, -1, 1)
      )
      if (hyde_flip) {
        ncfiledata <- ncfiledata[, ncfile$dim$lat$len:1]
      }
      # Convert spatial unit to fao_area_units
      if (hyde_is_fraction) {
        if (matching_extent(
          extent(hyde_area),
          hyde_extent,
          xres(hyde_area),
          yres(hyde_area)
        )) {
          ncfiledata <- ncfiledata * ud.convert(1, hyde_area_units, "1") *
            values(hyde_area)
        } else {
          stop(
            "HYDE area from ", sQuote(hyde_area_file),
            " and HYDE cropland from ", sQuote(ncfile$filename),
            " have different spatial extent."
          )
        }
      } else {
        ncfiledata <- ncfiledata *
          ud.convert(1, hyde_area_units, fao_area_units)
      }
      # Aggregate to target resolution if necessary
      if (max(hyde2gadm) > 1) {
        if (year == max(min(output_period), min(hyde_period))) {
          cat("Aggregating", sQuote(var), "to GADM resolution\n")
        }
        sum1 <- sum(ncfiledata, na.rm = TRUE)
        ncfiledata <- aggregate_array(ncfiledata, hyde2gadm, "sum", FALSE)
        if (abs(sum(ncfiledata, na.rm = TRUE) - sum1) > 1e-6) {
          stop("Error aggregating ", sQuote(var), " in year ", year)
        }
      }
      # Dissolve separate lon and lat dimensions
      dim(ncfiledata) <- c(ncol(ncfiledata) * nrow(ncfiledata))
      assign(var, ncfiledata)
      rm(ncfiledata)
    }
    if (year %% 50 == 0) {
      cat(year, "\n")
    }
    # Consistency check. There are a few cases in HYDE 3.2.1 where rainfed and
    # irrigated cropland do not add up to total cropland.
    sum_total <- hyde_cropland - hyde_rainfed - hyde_irrigated
    index <- which(sum_total > 1e-3)
    if (length(index) > 0) {
      # Rainfed + irrigated do not add up to total
      incon_msg <- c(
        incon_msg,
        paste(
          "Expanding rainfed cropland in",
          length(index),
          "cells because rainfed + irrigated cropland < total cropland in year",
          year
        )
      )
      cat(incon_msg[length(incon_msg)], "\n")
      hyde_rainfed[index] <- (hyde_cropland - hyde_irrigated)[index]
    }
    index <- which(sum_total < -1e-3)
    if (length(index) > 0) {
      # Rainfed + irrigated too big
      mismatch_both <- which(
        hyde_cropland - hyde_rainfed < -1e-3 &
        hyde_cropland - hyde_irrigated < -1e-3
      )
      mismatch_rf <- which(hyde_cropland - hyde_rainfed < -1e-3)
      if (length(mismatch_both) > 0) {
        stop(
          "There are ", length(mismatch_both),
          " cells where both rainfed and irrigated cropland are each larger ",
          "than total cropland in year ", year
        )
      } else if (length(mismatch_rf) > 0) {
        # Rainfed too big
        # Rainfed is only reduced if it is bigger than total cropland, then
        # reduced to total cropland
        incon_msg <- c(
          incon_msg,
          paste(
            "Reducing rainfed cropland in",
            length(mismatch_rf),
            "cells because it is larger than total cropland in year",
            year
          )
        )
        cat(incon_msg[length(incon_msg)], "\n")
        hyde_rainfed[mismatch_rf] <- hyde_cropland[mismatch_rf]
        # Update sum of all three
        sum_total <- hyde_cropland - hyde_rainfed - hyde_irrigated
      }
      # Irrigated too big
      # Irrigated is always reduced if rainfed + irrigated bigger than total
      # cropland -> priority on rainfed cropland
      mismatch_ir <- which(sum_total < -1e-3)
      if (length(mismatch_ir) > 0) {
        incon_msg <- c(
          incon_msg,
          paste(
            "Reducing irrigated cropland in",
            length(mismatch_ir), "cells",
            "because it is larger than total cropland-rainfed cropland in year",
            year
          )
        )
        cat(incon_msg[length(incon_msg)], "\n")
        hyde_irrigated[mismatch_ir] <-
          (hyde_cropland - hyde_rainfed)[mismatch_ir]
      }
      rm(mismatch_both, mismatch_rf, mismatch_ir)
    }
    rm(index)
    # Aggregate to country level
    # Convert integer year into textual array index
    year <- format(year, scientific = FALSE, trim = TRUE)
    for (country in dimnames(hyde_cropland_country_timeseries)[[1]]) {
      cells <- fao_gadm_country_cells[[country]]
      # Cropland in country
      hyde_cropland_country_timeseries[country, year] <- sum(
        hyde_cropland[cells],
        na.rm = TRUE
      )
      # Irrigated cropland in country
      hyde_irrigated_country_timeseries[country, year] <- sum(
        hyde_irrigated[cells],
        na.rm = TRUE
      )
      # Maximum irrigated harvested areas
      hyde_gaez_max_ir_ha_country_timeseries[country, year] <- sum(
        hyde_irrigated[cells] * gaez_multicropping_suit_ir[cells],
        na.rm = TRUE
      )
      # Maximum rainfed harvested area on rainfed cropland
      # Use hyde_cropland - hyde_irrigated for hyde_rainfed from now on to
      # reduce required datasets
      hyde_gaez_max_rf_ha_on_rf_country_timeseries[country, year] <-
        sum(
          (hyde_cropland - hyde_irrigated)[cells] *
            gaez_multicropping_suit_rf[cells],
          na.rm = TRUE
        )
      # Maximum total harvested area (sum of the two above)
      hyde_gaez_max_ha_country_timeseries[country, year] <-
         hyde_gaez_max_ir_ha_country_timeseries[country, year] +
         hyde_gaez_max_rf_ha_on_rf_country_timeseries[country, year]
      # Maximum rainfed harvested area on irrigated cropland
      hyde_gaez_max_rf_ha_on_ir_country_timeseries[country, year] <-
        sum(
          hyde_irrigated[cells] * gaez_multicropping_suit_rf[cells],
          na.rm = TRUE
        )
    }
  }
  # Ensure irrigated does not exceed total
  mismatch <- which(
    hyde_irrigated_country_timeseries > hyde_cropland_country_timeseries
  )
  hyde_irrigated_country_timeseries[mismatch] <-
    hyde_cropland_country_timeseries[mismatch]
  mismatch <- which(
    hyde_gaez_max_ir_ha_country_timeseries >
    hyde_gaez_max_ha_country_timeseries
  )
  hyde_gaez_max_ir_ha_country_timeseries[mismatch] <-
    hyde_gaez_max_ha_country_timeseries[mismatch]
  rm(mismatch)
  # Clean-up
  for (var in c("hyde_cropland", "hyde_irrigated", "hyde_rainfed")) {
    nc_close(get(paste0(var, "_file")))
    rm(list = c(var, paste0(var, "_file")))
  }
  cat(
    "Saving HYDE cropland and maximum harvested areas aggregated to",
    "countries to", sQuote(hyde_country_sums_RData), "\n"
  )

  save(
    hyde_cropland_country_timeseries,
    hyde_irrigated_country_timeseries,
    hyde_gaez_max_ha_country_timeseries,
    hyde_gaez_max_ir_ha_country_timeseries,
    hyde_gaez_max_rf_ha_on_rf_country_timeseries,
    hyde_gaez_max_rf_ha_on_ir_country_timeseries,
    incon_msg,
    file = hyde_country_sums_RData
  )
}
################################################################################


################################################################################
## Update FAOSTAT timeseries data (production_array_filled) based on matched  ##
## Monfreda/FAOSTAT crops (fao_monfreda_production_array)                     ##
## Result is saved in tot_ha_timeseries                                       ##
cat("*** Feeding additional crops into FAOSTAT production time series ***\n")
cat(
  "Adding harvested areas for",
  length(
    setdiff(
      dimnames(fao_monfreda_production_array)[[2]],
      dimnames(production_array_filled)[[2]]
    )
  ),
  "crops not covered by FAOSTAT to FAOSTAT time series data.\n"
)
cat(
  "Harvested areas at the country scale from Monfreda for the year(s)",
  toString(dimnames(fao_monfreda_production_array)[[3]]),
  "are extrapolated across time using changes in FAOSTAT cropland extent.\n"
)
cat(
  "In cases where FAOSTAT cropland extent is missing",
  "HYDE cropland is used instead.\n"
)
# Drop any years outside of output_period
years <- intersect(
  format(
    seq(min(output_period), max(output_period)),
    scientific = FALSE,
    trim = TRUE
  ),
  dimnames(production_array_filled)[[4]]
)
tot_ha_timeseries <-
  production_array_filled[, , "Area harvested", years]
years <- intersect(
  format(
    seq(min(output_period), max(output_period)),
    scientific = FALSE,
    trim = TRUE
  ),
  dimnames(production_array_gapfilling)[[3]]
)
tot_ha_timeseries_gapfilling <-
  production_array_gapfilling[, , years]
# Extend array to include crops not part of FAOSTAT production statistics
tot_ha_timeseries <- abind(
  tot_ha_timeseries,
  array(
    dim = c(
      dim(tot_ha_timeseries)[1],
      length(
        setdiff(
          dimnames(fao_monfreda_production_array)[[2]],
          dimnames(production_array_filled)[[2]]
        )
      ),
      dim(tot_ha_timeseries)[3]
    ),
    dimnames = list(
      dimnames(tot_ha_timeseries)[[1]],
      setdiff(
        dimnames(fao_monfreda_production_array)[[2]],
        dimnames(production_array_filled)[[2]]
      ),
      dimnames(tot_ha_timeseries)[[3]]
    )
  ),
  along = 2
)
tot_ha_timeseries_gapfilling <- abind(
  tot_ha_timeseries_gapfilling,
  array(
    FALSE, # pre-fill with FALSE
    dim = c(
      dim(tot_ha_timeseries_gapfilling)[1],
      length(
        setdiff(
          dimnames(fao_monfreda_production_array)[[2]],
          dimnames(production_array_gapfilling)[[2]]
        )
      ),
      dim(tot_ha_timeseries_gapfilling)[3]
    ),
    dimnames = list(
      dimnames(tot_ha_timeseries_gapfilling)[[1]],
      setdiff(
        dimnames(fao_monfreda_production_array)[[2]],
        dimnames(production_array_gapfilling)[[2]]
      ),
      dimnames(tot_ha_timeseries_gapfilling)[[3]]
    )
  ),
  along = 2
)
# Ensure that crops match fao_monfreda_production_array
if (length(
  intersect(
    dimnames(tot_ha_timeseries)[[2]],
    dimnames(fao_monfreda_production_array)[[2]]
  )
) == dim(tot_ha_timeseries)[2]) {
  # Same names but different order
  index <- dimnames(fao_monfreda_production_array)[[2]]
  tot_ha_timeseries <- tot_ha_timeseries[, index, ]
} else {
  stop(
    "tot_ha_timeseries and ",
    "fao_monfreda_production_array contain different crops"
  )
}
if (length(
  intersect(
    dimnames(tot_ha_timeseries_gapfilling)[[2]],
    dimnames(fao_monfreda_production_array)[[2]]
  )
) == dim(tot_ha_timeseries_gapfilling)[2]) {
  # Same names but different order
  index <- dimnames(fao_monfreda_production_array)[[2]]
  tot_ha_timeseries_gapfilling <- tot_ha_timeseries_gapfilling[, index, ]
} else {
  stop(
    "tot_ha_timeseries_gapfilling and ",
    "fao_monfreda_production_array contain different crops"
  )
}

# Add countries missing in production_array_filled
tot_ha_timeseries <- abind(
  tot_ha_timeseries,
  array(
    dim = c(
      length(
        setdiff(
          dimnames(fao_monfreda_production_array)[[1]],
          dimnames(tot_ha_timeseries)[[1]]
        )
      ),
      dim(tot_ha_timeseries)[-1]
    ),
    dimnames = list(
      setdiff(
        dimnames(fao_monfreda_production_array)[[1]],
        dimnames(tot_ha_timeseries)[[1]]
      ),
      dimnames(tot_ha_timeseries)[[2]],
      dimnames(tot_ha_timeseries)[[3]]
    )
  ),
  along = 1
)
tot_ha_timeseries_gapfilling <- abind(
  tot_ha_timeseries_gapfilling,
  array(
    FALSE,
    dim = c(
      length(
        setdiff(
          dimnames(fao_monfreda_production_array)[[1]],
          dimnames(tot_ha_timeseries_gapfilling)[[1]]
        )
      ),
      dim(tot_ha_timeseries_gapfilling)[-1]
    ),
    dimnames = list(
      setdiff(
        dimnames(fao_monfreda_production_array)[[1]],
        dimnames(tot_ha_timeseries_gapfilling)[[1]]
      ),
      dimnames(tot_ha_timeseries_gapfilling)[[2]],
      dimnames(tot_ha_timeseries_gapfilling)[[3]]
    )
  ),
  along = 1
)
# Ensure order of countries matches
if (length(
  intersect(
    dimnames(tot_ha_timeseries)[[1]],
    dimnames(fao_monfreda_production_array)[[1]]
  )
) == dim(tot_ha_timeseries)[1]) {
  # Same names but different order
  index <- dimnames(fao_monfreda_production_array)[[1]]
  tot_ha_timeseries <- tot_ha_timeseries[index, , ]
} else {
  stop(
    "tot_ha_timeseries and ",
    "fao_monfreda_production_array contain different countries"
  )
}
if (length(
  intersect(
    dimnames(tot_ha_timeseries_gapfilling)[[1]],
    dimnames(fao_monfreda_production_array)[[1]]
  )
) == dim(tot_ha_timeseries_gapfilling)[1]) {
  # Same names but different order
  index <- dimnames(fao_monfreda_production_array)[[1]]
  tot_ha_timeseries_gapfilling <- tot_ha_timeseries_gapfilling[index, , ]
} else {
  stop(
    "tot_ha_timeseries_gapfilling and ",
    "fao_monfreda_production_array contain different countries"
  )
}

# Also extend landuse_array_expanded
if (length(
  intersect(
    dimnames(landuse_array_expanded)[[1]],
    dimnames(fao_monfreda_production_array)[[1]]
  )
) == dim(landuse_array_expanded)[1]) {
  index <- dimnames(fao_monfreda_production_array)[[1]]
  bak_dim <- dim(landuse_array_expanded)
  bak_dimnames <- dimnames(landuse_array_expanded)
  landuse_array_expanded <- landuse_array_expanded[index, , , ]
  dim(landuse_array_expanded) <- bak_dim
  dimnames(landuse_array_expanded) <- c(
    dimnames(fao_monfreda_production_array)[1],
    bak_dimnames[-1]
  )
} else {
  stop(
    "landuse_array_expanded and ",
    "fao_monfreda_production_array contain different countries"
  )
}
# Drop any years outside of output_period
bak_dim <- dim(landuse_array_expanded)
bak_dimnames <- dimnames(landuse_array_expanded)
years <- intersect(
  format(
    seq(min(output_period), max(output_period)),
    scientific = FALSE,
    trim = TRUE
  ),
  dimnames(landuse_array_expanded)[[4]]
)
landuse_array_expanded <- landuse_array_expanded[, , , years]
bak_dim[length(bak_dim)] <-
  dim(landuse_array_expanded)[length(dim(landuse_array_expanded))]
bak_dimnames[[length(bak_dimnames)]] <-
  dimnames(landuse_array_expanded)[[length(dim(landuse_array_expanded))]]
dim(landuse_array_expanded) <- bak_dim
dimnames(landuse_array_expanded) <- bak_dimnames
# Gap-filling of data.
# Full gap-filling info is logged to gapfilling_log, reduced information is
# returned to user directly
gapfilling_log <- file.path(
  working_dir,
  paste0(
    "gapfilling_global_harvested_area_timeseries_",
    min(output_period), "-", max(output_period),
    ifelse(
      exists("aquastat_version_string") && nchar(aquastat_version_string) > 0,
      paste0("_", aquastat_version_string),
      ""
    ),
    ifelse(
      exists("fao_version_string") && nchar(fao_version_string) > 0,
      paste0("_", fao_version_string),
      ""
    ),
    ifelse(
      exists("gadm_version_string") && nchar(gadm_version_string) > 0,
      paste0("_", gadm_version_string),
      ""
    ),
    ifelse(
      exists("gaez_version_string") && nchar(gaez_version_string) > 0,
      paste0("_", gaez_version_string),
      ""
    ),
    ifelse(
      exists("hyde_version_string") && nchar(hyde_version_string) > 0,
      paste0("_", hyde_version_string),
      ""
    ),
    ifelse(
      exists("mirca_version_string") && nchar(mirca_version_string) > 0,
      paste0("_", mirca_version_string),
      ""
    ),
    ifelse(
      exists("monfreda_version_string") && nchar(monfreda_version_string) > 0,
      paste0("_", monfreda_version_string),
      ""
    ),
    ifelse(
      exists("ramankutty_version_string") &&
        nchar(ramankutty_version_string) > 0,
      paste0("_", ramankutty_version_string),
      ""
    ),
    ".txt"
  )
)
if (!identical(
  dim(fao_monfreda_use_pattern),
  dim(fao_monfreda_production_array)[-4]
) || !identical(
  dimnames(fao_monfreda_use_pattern),
  dimnames(fao_monfreda_production_array)[-4]
)) {
  stop(
    "Dimensions do not match between fao_monfreda_use_pattern and ",
    "fao_monfreda_production_array. This should not happen.\n",
    "Please check country_level_data.R where these arrays are created."
  )
}
cat("All time series gap-filling is logged to", sQuote(gapfilling_log), "\n")
sink(file = gapfilling_log)
cat("Script run:", date(), "\n")
cat("++++++ Total harvested areas ++++++\n")
sink()
# Check if fao_monfreda_production_array and tot_ha_timeseries
# overlap temporally
overlap <- intersect(
  dimnames(fao_monfreda_production_array)[[3]],
  dimnames(tot_ha_timeseries)[[3]]
)
if (length(overlap) == 0) {
  sink(file = gapfilling_log, append = TRUE)
  cat(
    "Warning: Years in fao_monfreda_production_array",
    toString(dimnames(fao_monfreda_production_array)[[3]]),
    "are outside range of harvested area time series. Cannot fill in.\n"
  )
  sink()
  warning(
    "Years in fao_monfreda_production_array ",
    toString(dimnames(fao_monfreda_production_array)[[3]]),
    " are outside range of harvested area time series. Cannot fill in.",
    call. = FALSE,
    immediate. = TRUE
  )
} else {
  crops <- setdiff(
    dimnames(fao_monfreda_production_array)[[2]],
    dimnames(production_array_filled)[[2]]
  )
  for (crop in crops) {
    sink(file = gapfilling_log, append = TRUE, split = TRUE)
    # sink(... split = TRUE) outputs both to gapfilling_log and screen
    cat("++ Adding", crop, "++\n")
    sink()
    ylist <- intersect(
      dimnames(fao_monfreda_production_array)[[3]],
      dimnames(tot_ha_timeseries)[[3]]
    )
    cindex <- dimnames(fao_monfreda_production_array)[[1]]
    tot_ha_timeseries[, crop, ylist] <- ifelse(
      fao_monfreda_use_pattern[cindex, crop, ylist],
      fao_monfreda_production_array[cindex, crop, ylist, "Monfreda_GADM"],
      NA
    )
    rm(ylist, cindex)
    # Extrapolate Monfreda crops to cover FAOSTAT timeseries, not for country
    # groups
    clist <- setdiff(
      intersect(
        dimnames(tot_ha_timeseries)[[1]],
        dimnames(landuse_array_expanded)[[1]]
      ),
      names(fao_groups)
    )
    for (country in clist) {
      if (any(tot_ha_timeseries[country, crop, ] > 0, na.rm = TRUE)) {
        sink(file = gapfilling_log, append = TRUE)
        cat(country, "\n")
        tmpfill <- fill_timeseries(
          tot_ha_timeseries[country, crop, ],
          landuse_array_expanded[country, "Cropland", "Area", ],
          crop,
          extend_timeseries = FALSE
        )
        # Check if timeseries could be filled
        if (any(is.na(tmpfill))) {
          if (
            !any(!is.na(tmpfill) & is.na(tot_ha_timeseries[country, crop, ]))
          ) {
            # No gap filling was possible at all
            cat(
              "Using HYDE cropland instead of FAOSTAT cropland to extrapolate",
              "harvested area time series because FAOSTAT is missing.\n"
            )
          } else {
            # Partial gap filling was possible, but not complete
            cat(
              "Using HYDE cropland in addition to FAOSTAT cropland to",
              "extrapolate harvested area time series because FAOSTAT is",
              "incomplete.\n"
            )
          }
          tmpfill <- fill_timeseries(
            tmpfill,
            hyde_cropland_country_timeseries[country, ],
            crop,
            extend_timeseries = FALSE
          )
        }
        sink()
        # Set tot_ha_timeseries_gapfilling
        index <- which(tmpfill != tot_ha_timeseries[country, crop, ])
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        index <- which(
          is.na(tot_ha_timeseries[country, crop, ]) &
          !is.na(tmpfill)
        )
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        # Update tot_ha_timeseries
        tot_ha_timeseries[country, crop, ] <- tmpfill
        rm(tmpfill)
      }
    }
    # Update country groups for added crop
    clist <- intersect(
      names(fao_groups),
      dimnames(tot_ha_timeseries)[[1]]
    )
    for (country in clist) {
      # Countries that belong to group
      clist2 <- intersect(
        dimnames(tot_ha_timeseries)[[1]],
        fao_groups[[country]]
      )
      # Set to NA if all countries are NA, otherwise sum up countries
      tot_ha_timeseries[country, crop, ] <- ifelse(
        apply(
          tot_ha_timeseries[clist2, crop, ],
          2,
          function(indata) all(is.na(indata))
        ),
        NA,
        apply(
          tot_ha_timeseries[clist2, crop, ],
          2,
          sum,
          na.rm = TRUE
        )
      )
      clist2 <- intersect(
        dimnames(tot_ha_timeseries_gapfilling)[[1]],
        fao_groups[[country]]
      )
      # Set group to TRUE if any country in group is TRUE
      tot_ha_timeseries_gapfilling[country, crop, ] <- apply(
        tot_ha_timeseries_gapfilling[clist2, crop, ],
        2,
        any,
        na.rm = TRUE
      )
      rm(clist2)
    }
    rm(clist)
  }
}

# Remove fao_drop_crops
if (length(fao_drop_crops) != 0) {
  remove <- intersect(fao_drop_crops, dimnames(tot_ha_timeseries)[[2]])
  tot_ha_timeseries[, remove, ] <- NA
}

# In some cases, FAOSTAT may only have "nes" group whereas Monfreda has
# specific crops, try to replace with more specific crops
cat("*** Crop group replacement ***\n")
if (length(overlap) == 0) {
  warning(
    "Years in fao_monfreda_production_array ",
    toString(dimnames(fao_monfreda_production_array)[[3]]),
    " are outside range of harvested area time series. Cannot replace groups.",
    call. = FALSE,
    immediate. = TRUE
  )
} else {
  if (dim(fao_monfreda_production_array)[3] > 1) {
    sum_dim <- c(1, 3)
  } else {
    sum_dim <- 1
  }
  for (nes in names(nes_crops)) {
    crops1 <- c(nes, nes_crops[[nes]])
    if (dim(fao_monfreda_production_array)[3] > 1) {
      # Several Monfreda years
      countries <- which(
        apply(
          # nes crop = sum all contributing crops in FAOSTAT in all reference
          # years
          apply(
            fao_monfreda_production_array[, crops1, , "FAOSTAT"],
            sum_dim,
            sum,
            na.rm = TRUE
          ) == fao_monfreda_production_array[, nes, , "FAOSTAT"],
          1,
          function(indata) all(indata, na.rm = TRUE)
        ) &
        apply(
          # nes crop actually present in FAOSTAT in at least one reference year
          fao_monfreda_production_array[, nes, , "FAOSTAT"] > 0,
          1,
          function(indata) any(indata, na.rm = TRUE)
        ) &
        apply(
          # nes crop != sum all contributing crops in Monfreda in at least 1
          # reference year
          apply(
            fao_monfreda_production_array[, crops1, , "Monfreda_GADM"],
            sum_dim,
            sum,
            na.rm = TRUE
          ) != fao_monfreda_production_array[, nes, , "Monfreda_GADM"],
          1,
          function(indata) any(indata, na.rm = TRUE)
        ) &
        apply(
          # Any contributing crop in Monfreda actually present in at least 1
          # reference year
          apply(
            fao_monfreda_production_array[, crops1, , "Monfreda_GADM"],
            sum_dim,
            sum,
            na.rm = TRUE
          ) > 0,
          1,
          function(indata) any(indata, na.rm = TRUE)
        )
      )
    } else {
      countries <- which(
        # nes crop = sum all contributing crops in FAOSTAT
        apply(
          fao_monfreda_production_array[, crops1, , "FAOSTAT"],
          sum_dim,
          sum,
          na.rm = TRUE
        ) == fao_monfreda_production_array[, nes, , "FAOSTAT"] &
        # nes crop actually present in FAOSTAT
        fao_monfreda_production_array[, nes, , "FAOSTAT"] > 0 &
        # nes crop != sum all contributing crops in Monfreda
        apply(
          fao_monfreda_production_array[, crops1, , "Monfreda_GADM"],
          sum_dim,
          sum,
          na.rm = TRUE
        ) != fao_monfreda_production_array[, nes, , "Monfreda_GADM"] &
        # Any contributing crop present in Monfreda
        apply(
          fao_monfreda_production_array[, crops1, , "Monfreda_GADM"],
          sum_dim,
          sum,
          na.rm = TRUE
        ) > 0
      )
    }
    # First loop only prints information.
    for (country in names(countries)) {
      if (any(
        tot_ha_timeseries[country, nes_crops[[nes]], ] > 0,
        na.rm = TRUE
      )) {
        # Do not replace if contributing crops are != 0 in FAOSTAT timeseries
        # at any time
        next
      }
      tmp_monfreda <-
        fao_monfreda_production_array[country, crops1, , "Monfreda_GADM"]
      cat(
        "Replacing FAOSTAT", sQuote(nes), "with Monfreda",
        toString(sQuote(names(which(tmp_monfreda > 0)))),
        "in", sQuote(country), "\n"
      )
    }
    # Second loop replaces data.
    for (country in countries) {
      if (any(
        tot_ha_timeseries[country, nes_crops[[nes]], ] > 0,
        na.rm = TRUE
      )) {
        # Do not replace if contributing crops are != 0 in FAOSTAT timeseries
        # at any time
        next
      }
      tmp_monfreda <-
        fao_monfreda_production_array[country, crops1, , "Monfreda_GADM"]
      crop_factor <- array(
        switch(
          # More than 1 year of Monfreda data?
          (dim(fao_monfreda_production_array)[3] > 1) + 1,
          # FALSE
          tmp_monfreda / sum(tmp_monfreda, na.rm = TRUE),
          # TRUE
          tmp_monfreda / apply(tmp_monfreda, 2, sum, na.rm = TRUE)
        ),
        dim = c(length(crops1), dim(fao_monfreda_production_array)[3]),
        dimnames = list(crops1, dimnames(fao_monfreda_production_array)[[3]])
      )
      # Expand crop_factor to cover full period of
      # tot_ha_timeseries
      years <- match(
        dimnames(tot_ha_timeseries)[[3]],
        dimnames(crop_factor)[[2]]
      )
      crop_factor <- crop_factor[, years]
      dimnames(crop_factor)[[2]] <- dimnames(tot_ha_timeseries)[[3]]
      if (dim(fao_monfreda_production_array)[3] > 1) {
        # Do gap-filling of crop_factor if Monfreda has more than one year
        sink(file = gapfilling_log, append = TRUE)
        crop_factor <- t(apply(crop_factor, 1, fill_timeseries))
        sink()
      }
      # Extend before first valid value
      first_index <- min(which(!is.na(apply(crop_factor, 2, sum))))
      years <- which(
        as.integer(dimnames(crop_factor)[[2]]) <
        as.integer(dimnames(crop_factor)[[2]][first_index])
      )
      crop_factor[, years] <- crop_factor[, first_index]
      # Extend after last valid value
      last_index <- max(which(!is.na(apply(crop_factor, 2, sum))))
      years <- which(
        as.integer(dimnames(crop_factor)[[2]]) >
        as.integer(dimnames(crop_factor)[[2]][last_index])
      )
      crop_factor[, years] <- crop_factor[, last_index]
      # Calculate new areas for "nes_crops[[nes]]" and "nes" based on
      # crop_factor and FAOSTAT crop area of "nes"
      tot_ha_timeseries[country, crops1, ] <-
        rep(tot_ha_timeseries[country, nes, ], each = length(crops1)) *
        crop_factor
      # Update tot_ha_timeseries_gapfilling only crops/years
      # with values
      tot_ha_timeseries_gapfilling[country, crops1, ] <- ifelse(
        is.na(tot_ha_timeseries[country, crops1, ]),
        tot_ha_timeseries_gapfilling[country, crops1, ],
        TRUE
      )
    }
  }
}

# Add in Monfreda crops which are part of FAOSTAT but for which FAOSTAT has no
# values.
sink(file = gapfilling_log, append = TRUE, split = TRUE)
cat(
  "*** Adding in Monfreda crops which should be covered by FAOSTAT",
  "but are missing in FAOSTAT in certain countries ***\n"
)
sink()

if (length(overlap) == 0) {
  sink(file = gapfilling_log, append = TRUE)
  cat(
    "Warning: Years in fao_monfreda_production_array",
    toString(dimnames(fao_monfreda_production_array)[[3]]),
    "are outside range of harvested area time series. Cannot fill in.\n"
  )
  sink()
  warning(
    "Years in fao_monfreda_production_array ",
    toString(dimnames(fao_monfreda_production_array)[[3]]),
    " are outside range of harvested area time series. Cannot fill in.",
    call. = FALSE,
    immediate. = TRUE
  )
} else {
  for (country in dimnames(tot_ha_timeseries)[[1]]) {
    if (country %in% names(fao_groups)) {
      next
    }
    # Find index of crops without any values in time series data
    if (dim(fao_monfreda_use_pattern)[3] > 1) {
      # More than one Monfreda reference year
      add_crops <- which(
        # All NAs
        apply(
          tot_ha_timeseries[country, , ],
          1,
          function(indata) all(is.na(indata))
        ) &
        # Valid Monfreda pattern
        apply(fao_monfreda_use_pattern[country, , ], 1, any, na.rm = TRUE)
      )
    } else {
      add_crops <- which(
        apply(
          tot_ha_timeseries[country, , ],
          1,
          function(indata) all(is.na(indata))
        ) & fao_monfreda_use_pattern[country, , ]
      )
    }
    cols <- "Monfreda_GADM"
    # Replace crop index by crop name
    add_crops <- dimnames(tot_ha_timeseries)[[2]][add_crops]
    # Exclude crops where Monfreda has no data
    if (dim(fao_monfreda_production_array)[3] > 1) {
      add_crops <- setdiff(
        add_crops,
        names(
          which(
            # All Monfreda NA
            apply(
              fao_monfreda_production_array[country, , , cols],
              1,
              function(indata) all(is.na(indata))
            )
          )
        )
      )
    } else {
      add_crops <- setdiff(
        add_crops,
        names(
          which(
            # Monfreda NA
            is.na(fao_monfreda_production_array[country, , , cols])
          )
        )
      )
    }
    if (length(add_crops) > 0) {
      sink(file = gapfilling_log, append = TRUE, split = TRUE)
      cat(
        "++ Adding Monfreda crop(s)", toString(sQuote(add_crops)),
        "to FAOSTAT timeseries in", country, "++\n"
      )
      tot_ha_timeseries[country, add_crops, overlap] <-
        fao_monfreda_production_array[country, add_crops, overlap, cols]
      sink()
      # Gap-fill time series
      sink(file = gapfilling_log, append = TRUE)
      for (crop in add_crops) {
        tmpfill <- fill_timeseries(
          tot_ha_timeseries[country, crop, ],
          landuse_array_expanded[country, "Cropland", "Area", ],
          crop,
          extend_timeseries = FALSE
        )
        # check if timeseries could be filled
        if (any(is.na(tmpfill))) {
          if (
            !any(!is.na(tmpfill) & is.na(tot_ha_timeseries[country, crop, ]))
          ) {
            # No gap filling was possible at all
            cat(
              "Using HYDE cropland instead of FAOSTAT cropland to",
              "extrapolate harvested area time series of", sQuote(crop),
              "because FAOSTAT is missing.\n"
            )
          } else {
            # Partial gap filling was possible, but not complete
            cat(
              "Using HYDE cropland in addition to FAOSTAT cropland to",
              "extrapolate harvested area time series of", sQuote(crop),
              "because FAOSTAT is incomplete.\n"
            )
          }
          tmpfill <- fill_timeseries(
            tmpfill,
            hyde_cropland_country_timeseries[country, ],
            crop,
            extend_timeseries = FALSE
          )
        }
        # Set tot_ha_timeseries_gapfilling
        index <- which(tmpfill != tot_ha_timeseries[country, crop, ])
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        index <- which(
          is.na(tot_ha_timeseries[country, crop, ]) &
          !is.na(tmpfill)
        )
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        # Update tot_ha_timeseries
        tot_ha_timeseries[country, crop, ] <- tmpfill
        rm(tmpfill)
      }
      sink()
    }
  }
}

# Check if landuse_array_expanded does not cover full time period of
# tot_ha_timeseries
# If necessary, fill gaps in tot_ha_timeseries using
# hyde_cropland_country_timeseries
mismatch_fao_landuse <- setdiff(
  dimnames(tot_ha_timeseries)[[3]],
  dimnames(landuse_array_expanded)[[4]]
)
if (length(mismatch_fao_landuse) > 0) {
  cat(
    "*** Year(s)", toString(mismatch_fao_landuse),
    "of the FAOSTAT production time series is/are missing in FAOSTAT",
    "cropland time series. ***\n"
  )
  if (any(mismatch_fao_landuse %in%
    dimnames(hyde_cropland_country_timeseries)[[2]]
  )) {
    cat(
      "Attempting to fill any gaps in FAOSTAT production time series during",
      "this time using HYDE cropland country sums.\n"
    )
    sink(file = gapfilling_log, append = TRUE)
    cat(
        "Attempting to fill gaps in FAOSTAT production time series during",
        toString(mismatch_fao_landuse), "using HYDE cropland country sums.\n"
    )
    fill_candidates <- which(
      # Missing values in years not covered by landuse_array_expanded
      apply(
        tot_ha_timeseries[, , mismatch_fao_landuse],
        c(1, 2),
        function(indata) any(is.na(indata))
      ) &
      # FAOSTAT has some years with valid values
      apply(
        tot_ha_timeseries,
        c(1, 2),
        max,
        na.rm = TRUE
      ) > 0,
      arr.ind = TRUE
    )
    # Remove countries for which HYDE does not have any cropland anyway
    hyde_zero <- which(
      rownames(fill_candidates) %in%
      names(which(hyde_max_cropland_country == 0))
    )
    fill_candidates <- fill_candidates[-hyde_zero, ]
    # Remove FAO production groups
    all_crops <- dimnames(tot_ha_timeseries)[[2]]
    group_crops <- which(
      all_crops[fill_candidates[, 2]] %in%
      fao_production_item_group_def$Item.Group
    )
    fill_candidates <- fill_candidates[-group_crops, ]
    # Remove country groups
    clist <- sort(setdiff(unique(rownames(fill_candidates)), names(fao_groups)))
    ylist <- intersect(
      dimnames(hyde_cropland_country_timeseries)[[2]],
      dimnames(tot_ha_timeseries)[[3]]
    )
    for (country in clist) {
      cat("++", country, "++\n")
      index <- which(rownames(fill_candidates) == country)
      crops <- all_crops[fill_candidates[index, 2]]
      for (crop in crops) {
        tmpfill <- fill_timeseries(
          tot_ha_timeseries[country, crop, ],
          hyde_cropland_country_timeseries[country, ylist],
          crop = crop
        )
        # Set tot_ha_timeseries_gapfilling
        index <- which(tmpfill != tot_ha_timeseries[country, crop, ])
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        index <- which(
          is.na(tot_ha_timeseries[country, crop, ]) &
          !is.na(tmpfill)
        )
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        # Update tot_ha_timeseries
        tot_ha_timeseries[country, crop, ] <- tmpfill
        rm(tmpfill)
      }
    }
    sink()
    rm(all_crops)
  }
}

# Check for countries which have no harvested area of any crops in
# tot_ha_timeseries but do have cropland according to FAOSTAT
# or HYDE. Fill these using crop shares of the shortest FAOSTAT country group
# that contains the country.
fill_countries <- names(
  which(
    # Production time series has no values at all
    rowSums(tot_ha_timeseries, na.rm = TRUE) == 0 &
    (
      # FAOSTAT has cropland
      rowSums(landuse_array_expanded[, "Cropland", "Area", ], na.rm = TRUE) > 0 |
      # or HYDE has cropland
      hyde_max_cropland_country > 0
    )
  )
)
if (length(fill_countries) > 0) {
  sink(file = gapfilling_log, append = TRUE, split = TRUE)
  cat(
    "*** There are", length(fill_countries),
    "countries without any harvested areas but with cropland.",
    "Trying to fill in representative crop mix. ***\n"
  )
  sink()
  ylist1 <- match(
    dimnames(tot_ha_timeseries)[[3]],
    dimnames(landuse_array_expanded)[[4]]
  )
  ylist2 <- match(
    dimnames(tot_ha_timeseries)[[3]],
    dimnames(hyde_cropland_country_timeseries)[[2]]
  )
  for (country in fill_countries)  {
    # Find FAO country groups that contain the country
    groups <- names(
      which(sapply(fao_groups, function(indata) country %in% indata))
    )
    if (length(groups) > 0) {
      # Find group with fewest members; assumption is that it may be closer in
      # crop mix to country than larger group
      fill_group <- names(sort(sapply(fao_groups[groups], length)))[1]
      if (any(
        landuse_array_expanded[fill_group, "Cropland", "Area", ylist1] > 0,
        na.rm = TRUE
      ) && any(
        landuse_array_expanded[country, "Cropland", "Area", ylist1] > 0,
        na.rm = TRUE
      )) {
        # Use FAOSTAT cropland
        fill_group_cropland <-
          landuse_array_expanded[fill_group, "Cropland", "Area", ylist1]
        # Reduce by cropland of fill_countries because their cropland does not
        # have any harvested areas
        for (c in intersect(fill_countries, fao_groups[[fill_group]])) {
          fill_group_cropland <- fill_group_cropland - ifelse(
            is.na(landuse_array_expanded[c, "Cropland", "Area", ylist1]),
            0,
            landuse_array_expanded[c, "Cropland", "Area", ylist1]
          )
        }
        sink(file = gapfilling_log, append = TRUE, split = TRUE)
        cat(
          "++ Using FAOSTAT harvested areas from", sQuote(fill_group),
          "and FAOSTAT cropland extent from",
          sQuote(fill_group), "and", sQuote(country),
          "to derive harvested areas for", sQuote(country), "++\n"
        )
        sink()
        source_cropland <- rep(
          fill_group_cropland,
          each = dim(tot_ha_timeseries)[2]
        )
        target_cropland <- rep(
          landuse_array_expanded[country, "Cropland", "Area", ylist1],
          each = dim(tot_ha_timeseries)[2]
        )
        tot_ha_timeseries[country, , ] <- ifelse(
          source_cropland > 0,
          tot_ha_timeseries[fill_group, , ] / source_cropland * target_cropland,
          0
        )
        tot_ha_timeseries_gapfilling[country, , ] <- ifelse(
          is.na(tot_ha_timeseries[country, , ]),
          tot_ha_timeseries_gapfilling[country, , ],
          TRUE
        )
        sink(file = gapfilling_log, append = TRUE)
        if (length(mismatch_fao_landuse) > 0) {
          # Try to fill missing years using HYDE cropland
          crops <- setdiff(
            names(
              which(rowSums(tot_ha_timeseries[country, , ], na.rm = TRUE) > 0)
            ),
            fao_production_item_group_def$Item.Group
          )
          for (crop in crops) {
            tmpfill <- fill_timeseries(
              tot_ha_timeseries[country, crop, ],
              hyde_cropland_country_timeseries[country, ],
              crop = crop,
              extend_timeseries = FALSE
            )
            # Set tot_ha_timeseries_gapfilling
            index <- which(tmpfill != tot_ha_timeseries[country, crop, ])
            tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
            index <- which(
              is.na(tot_ha_timeseries[country, crop, ]) &
              !is.na(tmpfill)
            )
            tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
            # Update tot_ha_timeseries
            tot_ha_timeseries[country, crop, ] <- tmpfill
            rm(tmpfill)
          }
        }
        sink()
      } else {
        # Use HYDE cropland
        sink(file = gapfilling_log, append = TRUE, split = TRUE)
        cat(
          "++ Using FAOSTAT harvested areas from", sQuote(fill_group),
          "and HYDE cropland extent from",
          sQuote(fill_group), "and", sQuote(country),
          "to derive harvested areas for", sQuote(country), "++\n"
        )
        sink()
        fill_group_cropland <-
          hyde_cropland_country_timeseries[fill_group, ylist2]
        # Reduce by cropland of fill_countries because their cropland does not
        # have any harvested areas
        for (c in intersect(fill_countries, fao_groups[[fill_group]])) {
          fill_group_cropland <- fill_group_cropland - ifelse(
            is.na(hyde_cropland_country_timeseries[c, ylist2]),
            0,
            hyde_cropland_country_timeseries[c, ylist2]
          )
        }
        source_cropland <- rep(
          fill_group_cropland,
          each = dim(tot_ha_timeseries)[2]
        )
        target_cropland <- rep(
          hyde_cropland_country_timeseries[country, ylist2],
          each = dim(tot_ha_timeseries)[2]
        )
        tot_ha_timeseries[country, , ] <- ifelse(
          source_cropland > 0,
          tot_ha_timeseries[fill_group, , ] / source_cropland * target_cropland,
          0
        )
        tot_ha_timeseries_gapfilling[country, , ] <- ifelse(
          is.na(tot_ha_timeseries[country, , ]),
          tot_ha_timeseries_gapfilling[country, , ],
          TRUE
        )
      }
    } else {
      sink(file = gapfilling_log, append = TRUE, split = TRUE)
      cat("No representative country group to fill", sQuote(country), "\n")
      sink()
    }
  }
  rm(ylist1, ylist2)
}
# Crops for which a gridded timeseries will be created; this does not include
# any fao_drop_crops or item groups from FAOSTAT.
if (length(fao_drop_crops) > 0) {
  ts_crops <- setdiff(dimnames(tot_ha_timeseries)[[2]], fao_drop_crops)
} else {
  ts_crops <- dimnames(tot_ha_timeseries)[[2]]
}
# Remove crop groups
ts_crops <- setdiff(ts_crops, unique(fao_production_item_group_def$Item.Group))

# Check for any "equivalent" crop names in ts_crops. These should probably be
# removed because they are likely to be duplicates of other crop names.
if (any(grepl("equivalent", ts_crops, ignore.case = TRUE))) {
  warning(
    "*** ts_crops contains crop names including 'equivalent' in",
    " their names: ",
    toString(
      sQuote(grep("equivalent", ts_crops, ignore.case = TRUE, value = TRUE))
    ),
    ".\nThese may be duplicates of other crops.",
    " If so, please include them in fao_drop_crops in landuse_setup.R ***",
    call. = FALSE,
    immediate. = TRUE
  )
}


# Fill in gaps where FAOSTAT cropland data (landuse_array_expanded) is
# incomplete.
sink(file = gapfilling_log, append = TRUE, split = TRUE)
cat(
  "Checking if any countries need additional gap-filling because of missing",
  "FAOSTAT cropland extent. If so, using HYDE cropland extent.\n"
)
sink()
for (country in dimnames(tot_ha_timeseries)[[1]]) {
  if (country %in% names(fao_groups)) {
    # Skip country groups
    next
  }
  cindex <- which(fao_landuse_country_def$Country == country)
  startyear <- ifelse(
    is.finite(fao_landuse_country_def$Start.Year[cindex]),
    fao_landuse_country_def$Start.Year[cindex],
    -1e10
  )
  endyear <- ifelse(
    is.finite(fao_landuse_country_def$End.Year[cindex]),
    fao_landuse_country_def$End.Year[cindex],
    1e10
  )
  ylist <- which(
    as.integer(dimnames(landuse_array_expanded)[[4]]) >= startyear &
    as.integer(dimnames(landuse_array_expanded)[[4]]) <= endyear
  )
  if (any(is.na(landuse_array_expanded[country, "Cropland", "Area", ylist])) &&
    hyde_max_cropland_country[country] > 0) {
    sink(file = gapfilling_log, append = TRUE, split = TRUE)
    cat(country, "\n")
    sink()
    sink(file = gapfilling_log, append = TRUE)
    for (crop in ts_crops) {
      if (any(tot_ha_timeseries[country, crop, ] > 0, na.rm = TRUE)) {
        tmpfill <- fill_timeseries(
          tot_ha_timeseries[country, crop, ],
          hyde_cropland_country_timeseries[country, ],
          crop,
          extend_timeseries = FALSE
        )
        # Set tot_ha_timeseries_gapfilling
        index <- which(tmpfill != tot_ha_timeseries[country, crop, ])
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        index <- which(
          is.na(tot_ha_timeseries[country, crop, ]) &
          !is.na(tmpfill)
        )
        tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
        # Update tot_ha_timeseries
        tot_ha_timeseries[country, crop, ] <- tmpfill
        rm(tmpfill)
      }
    }
    sink()
  }
}
################################################################################


################################################################################
## Extend tot_ha_timeseries to cover full output_period      ##
if (
  length(
    intersect(
      format(
        seq(min(output_period), max(output_period)),
        scientific = FALSE,
        trim = TRUE
      ),
      dimnames(tot_ha_timeseries)[[3]]
    )
  ) != length(seq(min(output_period), max(output_period)))
) {
  sink(file = gapfilling_log, append = TRUE, split = TRUE)
  cat(
    "Extending temporal coverage of tot_ha_timeseries (",
    min(as.integer(dimnames(production_array_filled)[[4]])), "-",
    max(as.integer(dimnames(production_array_filled)[[4]])),
    ") to cover the full output period ",
    min(output_period), "-", max(output_period), "\n",
    sep = ""
  )
  sink()
  cat("See", sQuote(gapfilling_log), "for details.\n")
}
# Extend tot_ha_timeseries to full output_period
ylist <- match(
  dimnames(hyde_cropland_country_timeseries)[[2]],
  dimnames(tot_ha_timeseries)[[3]]
)
tot_ha_timeseries <- tot_ha_timeseries[, , ylist]
yindex <- which(is.na(dimnames(tot_ha_timeseries)[[3]]))
dimnames(tot_ha_timeseries)[[3]][yindex] <- setdiff(
  dimnames(hyde_cropland_country_timeseries)[[2]],
  na.omit(dimnames(tot_ha_timeseries)[[3]])
)
# Extend tot_ha_timeseries_gapfilling to full output_period
ylist <- match(
  dimnames(hyde_cropland_country_timeseries)[[2]],
  dimnames(tot_ha_timeseries_gapfilling)[[3]]
)
tot_ha_timeseries_gapfilling <- tot_ha_timeseries_gapfilling[, , ylist]
yindex <- which(is.na(dimnames(tot_ha_timeseries_gapfilling)[[3]]))
dimnames(tot_ha_timeseries_gapfilling)[[3]][yindex] <- setdiff(
  dimnames(hyde_cropland_country_timeseries)[[2]],
  na.omit(dimnames(tot_ha_timeseries_gapfilling)[[3]])
)
# Initialize values in tot_ha_timeseries_gapfilling
tot_ha_timeseries_gapfilling[, , yindex] <- FALSE

# Gap fill individual countries first
sink(file = gapfilling_log, append = TRUE)
clist <- setdiff(dimnames(tot_ha_timeseries)[[1]], names(fao_groups))
for (country in clist) {
  cat(country, "\n")
  # ts_crops present in country
  crops <- names(
    which(
      rowSums(tot_ha_timeseries[country, ts_crops, ], na.rm = TRUE) > 0
    )
  )
  for (crop in crops) {
    tmpfill <- fill_timeseries(
      tot_ha_timeseries[country, crop, ],
      hyde_cropland_country_timeseries[country, ],
      crop,
      extend_timeseries = TRUE
    )
    # Set tot_ha_timeseries_gapfilling
    index <- which(tmpfill != tot_ha_timeseries[country, crop, ])
    tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
    index <- which(is.na(tot_ha_timeseries[country, crop, ]) & !is.na(tmpfill))
    tot_ha_timeseries_gapfilling[country, crop, index] <- TRUE
    # Update tot_ha_timeseries
    tot_ha_timeseries[country, crop, ] <- tmpfill
    rm(tmpfill)
  }
}
# Update country groups based on gap-filled individual countries
clist <- intersect(dimnames(tot_ha_timeseries)[[1]], names(fao_groups))
for (country in clist) {
  clist2 <- intersect(fao_groups[[country]], dimnames(tot_ha_timeseries)[[1]])
  clist3 <- intersect(
    fao_groups[[country]],
    dimnames(tot_ha_timeseries_gapfilling)[[1]]
  )
  crops <- names(
    which(
      apply(
        tot_ha_timeseries[clist2, ts_crops, ],
        2,
        max,
        na.rm = TRUE
      ) > 0
    )
  )
  for (crop in crops) {
    tot_ha_timeseries[country, crop, ] <- ifelse(
      apply(
        tot_ha_timeseries[clist2, crop, ],
        2,
        function(indata) all(is.na(indata))
      ),
      NA,
      apply(tot_ha_timeseries[clist2, crop, ], 2, sum, na.rm = TRUE)
    )
    tot_ha_timeseries_gapfilling[country, crop, ] <- apply(
      tot_ha_timeseries_gapfilling[clist3, crop, ],
      2,
      any,
      na.rm = TRUE
    )
  }
}
sink()

# Inconsistencies at country scale. This assumes that GAEZ multicropping
# suitability determines maximum possible harvested area. Cut off any areas
# exceeding that.
# Sum across all crops
tot_ha_timeseries_sum <- apply(
  tot_ha_timeseries[, ts_crops, ],
  c(1, 3),
  sum,
  na.rm = TRUE
)
clist <- intersect(
  intersect(
    # Countries in cropland variable
    intersect(
      dimnames(hyde_cropland_country_timeseries)[[1]],
      dimnames(landuse_array_expanded)[[1]]
    ),
    # Countries in maximum harvested area variable
    dimnames(hyde_gaez_max_ha_country_timeseries)[[1]]
  ),
  # Countries in production time series
  dimnames(tot_ha_timeseries)[[1]]
)
ylist <- intersect(
  intersect(
    dimnames(hyde_cropland_country_timeseries)[[2]],
    dimnames(hyde_gaez_max_ha_country_timeseries)[[2]]
  ),
  dimnames(tot_ha_timeseries)[[3]]
)
# This version leaves harvested areas if HYDE cropland is zero.
# check_countries <- unique(
#   rownames(
#     which(
#       hyde_gaez_max_ha_country_timeseries[clist, ylist] > 0 &
#       tot_ha_timeseries_sum[clist, ylist] >
#       hyde_gaez_max_ha_country_timeseries[clist, ylist],
#       arr.ind = TRUE
#     )
#   )
# )
# This version also removes harvested areas if HYDE cropland is zero.
check_countries <- unique(
  rownames(
    which(
      tot_ha_timeseries_sum[clist, ylist] >
      hyde_gaez_max_ha_country_timeseries[clist, ylist],
      arr.ind = TRUE
    )
  )
)
# Keep a copy without changes
tot_ha_timeseries_unconstrained <- tot_ha_timeseries
if (length(check_countries) > 0) {
  sink(file = gapfilling_log, append = TRUE, split = TRUE)
  cat(
    "*** Checking consistency between FAOSTAT harvested areas and",
    "HYDE cropland ***\n"
  )
  sink()
  for (country in check_countries) {
    exceeded <- which(
      tot_ha_timeseries_sum[country, ylist] >
      hyde_gaez_max_ha_country_timeseries[country, ylist]
    )
    hyde_zero <- which(
      tot_ha_timeseries_sum[country, ylist] > 0 &
      hyde_gaez_max_ha_country_timeseries[country, ylist] == 0
    )
    sink(file = gapfilling_log, append = TRUE)
    cat(
      "Correcting FAOSTAT harvested areas for", country,
      "because they exceed maximum possible harvested area during",
      length(exceeded), "years.\n"
    )
    if (length(hyde_zero) > 0) {
      cat(
        "Warning: HYDE has no cropland at all during", length(hyde_zero),
        "of those years.\n"
      )
    }
    sink()
    # Note: this scales down all crops present in the country. Another option
    # would be to scale down gap-filled crops first because their numbers are
    # more uncertain, especially for crops added from Monfreda dataset.
    scalar <- rep(
      hyde_gaez_max_ha_country_timeseries[country, ylist] /
        tot_ha_timeseries_sum[country, ylist],
      each = length(ts_crops)
    )
    # Do not scale up production if it is below
    # hyde_gaez_max_ha_country_timeseries
    scalar[which(scalar > 1)] <- 1
    tot_ha_timeseries[country, ts_crops, ylist] <-
      tot_ha_timeseries[country, ts_crops, ylist] * scalar
    rm(exceeded, hyde_zero)
  }
  clist <- setdiff(dimnames(tot_ha_timeseries)[[1]], names(fao_groups))
  # Sum of scaled down data
  sum1 <- apply(tot_ha_timeseries[clist, ts_crops, ], 3, sum, na.rm = TRUE)
  # Sum of original data
  sum2 <- apply(
    tot_ha_timeseries_unconstrained[clist, ts_crops, ],
    3,
    sum,
    na.rm = TRUE
  )
  sink(file = gapfilling_log, append = TRUE, split = TRUE)
  cat(
    "Between",
    paste(
      abs(round(range((sum1 / sum2 - 1) * 100), 2)),
      "%",
      collapse = " and "
    ),
    "of global harvested areas have been removed per year.\n"
  )
  # Years within period covered by FAOSTAT
  ylist <- intersect(
    dimnames(tot_ha_timeseries)[[3]],
    dimnames(production_array_filled)[[4]]
  )
  # Scaled down data
  sum1 <- apply(tot_ha_timeseries[clist, ts_crops, ylist], 3, sum, na.rm = TRUE)
  # Original data
  sum2 <- apply(
    tot_ha_timeseries_unconstrained[clist, ts_crops, ylist],
    3,
    sum,
    na.rm = TRUE
  )
  cat(
    "The range for years covered by FAOSTAT data is:",
    paste(
      abs(round(range((sum1 / sum2 - 1) * 100), 2)), "%",
      collapse = " to "
    ),
    "\n"
  )
  # Years outside period covered by FAOSTAT
  ylist <- setdiff(
    dimnames(tot_ha_timeseries)[[3]],
    dimnames(production_array_filled)[[4]]
  )
  # Scaled down data
  sum1 <- apply(tot_ha_timeseries[clist, ts_crops, ylist], 3, sum, na.rm = TRUE)
  # Original data
  sum2 <- apply(
    tot_ha_timeseries_unconstrained[clist, ts_crops, ylist],
    3,
    sum,
    na.rm = TRUE
  )
  cat(
    "The range for years not covered by FAOSTAT data is:",
    paste(
      abs(round(range((sum1 / sum2 - 1) * 100), 2)), "%",
      collapse = " to "
    ),
    "\n"
  )
  sink()
}
# Update sum across all crops
tot_ha_timeseries_sum <- apply(
  tot_ha_timeseries[, ts_crops, ],
  c(1, 3),
  sum,
  na.rm = TRUE
)
rm(ylist, clist, check_countries)
cat("*** Finished processing total harvested areas. ***\n")
################################################################################


################################################################################
## Irrigated harvested areas at country scale                                 ##
## - AQUASTAT provides some country numbers for years starting in 1961        ##
## - MIRCA provides full set of harvested areas for limited set of crops but  ##
## only 1 reference year                                                      ##
cat("*** Starting processing irrigated harvested areas. ***\n")

# Load AQUASTAT data read by read_AQUASTAT.R
cat(
  "Loading AQUASTAT data preprocessed by read_AQUASTAT.R from",
  sQuote(aquastat_rdata), "\n"
)
load(aquastat_rdata)
aquastat_is_fraction <- !ud.are.convertible(aquastat_area_units, "m2")
if (aquastat_is_fraction) {
  stop(
    "AQUASTAT data need to be in an absolute spatial unit such as m2.\n",
    "Provided unit: ", sQuote(aquastat_area_units)
  )
}

# Read MIRCA condensed cropping calendars
mirca_is_fraction <- !ud.are.convertible(mirca_area_units, "m2")
if (mirca_is_fraction) {
  stop(
    "MIRCA condensed cropping calendars need to be in absolute spatial unit",
    "such as m2.\n",
    "Provided unit: ", sQuote(mirca_area_units)
  )
}
# Note: rainfed crop calendar not used in current implementation
cat(
  "MIRCA2000 condensed crop calendars loaded from",
  toString(sQuote(irrigated_cropping_calendar_file)), "\n"
)
# If you want to read both rainfed and irrigated crop calendars change to:
# for (irr in c("irrigated", "rainfed")) {
for (irr in c("irrigated")) {
  for (year in mirca_refyear) {
    if (exists(paste0("mirca_", irr, "_units"))) {
      tmpunits <- get(paste0("mirca_", irr, "_units"))
    } else {
      tmpunits <- list()
    }
    condensed_cropping_calendar_file <- readLines(
      get(paste0(irr, "_cropping_calendar_file"))[as.character(year)]
    )

    for (row in seq_along(condensed_cropping_calendar_file)) {
      line <- condensed_cropping_calendar_file[row]
      # Check if first 4 characters are digits; skips empty rows or comments
      if (grepl("[[:digit:]]", substr(line, 1, 4))) {
        # Split text line into numbers
        values <- scan(text = line, quiet = TRUE)
        # First number in each row is unit code
        unit_code <- format(values[1], scientific = FALSE)
        if (is.null(tmpunits[[unit_code]])) {
          tmpunits[[unit_code]] <- array(
            0,
            dim = c(length(mirca_names), length(mirca_refyear)),
            dimnames = list(
              mirca_names,
              format(mirca_refyear, scientific = FALSE, trim = TRUE)
            )
          )
        }
        # Second value in line is crop index
        crop <- values[2]
        # Third value in line is number of sub-crops for this crop
        ncrop <- values[3]
        if (ncrop > 0) {
          # For each sub-crop there is harvested area, sowing month and harvest
          # month
          # Select only indices of harvested area entries
          careas <- seq(4, by = 3, length.out = ncrop)
          # Sum up harvested areas of sub-crops
          tmpunits[[unit_code]][crop, as.character(year)] <- sum(values[careas])
        } else {
          # MIRCA2000 does not explicitly distinguish between 0 area of a crop
          # and missing data (NA) of a crop.
          # The initialization of tmpunits[[unit_code]] assumes that 0 means
          # area of 0, not missing data.
          # Alternatively, set 0 values to NA so they may be replaced by
          # AQUASTAT below:
          # tmpunits[[unit_code]][crop, as.character(year)] <- NA
        }
      }
    }
    assign(paste0("mirca_", irr, "_units"), tmpunits)
    rm(tmpunits)
  }
}
# Consistency checks
# Check that all defined MIRCA2000 unit codes are in data
mismatch <- which(!mirca_unit_names$UnitCode %in% names(mirca_irrigated_units))
if (length(mismatch) > 0) {
  warning(
    "MIRCA2000 spatial unit(s) ",
    sQuote(
      paste(
        mirca_unit_names[mismatch, "UnitCode"],
        mirca_unit_names[mismatch, "UnitName"],
        sep = ": ",
        collapse = "', '"
      )
    ),
    " missing in condensed cropping calendars.",
    call. = FALSE,
    immediate. = TRUE
  )
}
# Check that all MIRCA2000 unit codes from data are assigned to a country in
# fao_mirca_country_mapping
mismatch <- which(!names(mirca_irrigated_units) %in%
  format(unlist(fao_mirca_country_mapping), scientific = FALSE, trim = TRUE)
)
if (length(mismatch) > 0) {
  cat(
    "MIRCA2000 spatial unit(s)",
    sQuote(
      paste(
        mirca_unit_names[mismatch, "UnitCode"],
        mirca_unit_names[mismatch, "UnitName"],
        sep = ": ",
        collapse = "', '"
      )
    ),
    "are not assigned to any FAOSTAT country.\n"
  )
  # Assign any non-assigned unit codes to "World"
  if ("World" %in% names(fao_mirca_country_mapping)) {
    cat("Adding them to FAO country group 'World'\n")
    fao_mirca_country_mapping[["World"]] <- c(
      fao_mirca_country_mapping[["World"]],
      mirca_unit_names[mismatch, "UnitCode"]
    )
  }
}
# Remove potential duplicates in fao_mirca_country_mapping
# There are some duplicates in fao_mirca_country_mapping.R because FAOSTAT
# renamed some countries and we keep old names for backwards compatibility.
for (country in setdiff(
  names(fao_mirca_country_mapping),
  dimnames(fao_monfreda_production_array)[[1]]
)) {
  index <- match(country, names(fao_mirca_country_mapping))
  dupl <- which(
    sapply(
      fao_mirca_country_mapping[-index],
      identical,
      y = fao_mirca_country_mapping[[country]]
    )
  )
  if (length(dupl) > 0 && length(fao_mirca_country_mapping[[country]]) > 0) {
    cat(
      "Removing", sQuote(country),
      "from fao_mirca_country_mapping because it seems to be a duplicate of",
      toString(sQuote(names(dupl))), "\n"
    )
    fao_mirca_country_mapping[[country]] <- NULL
  }
}
# Now check again for duplicates
for (country in setdiff(
  names(fao_mirca_country_mapping),
  # Do not check countries with no assigned unit codes for duplicates
  names(which(sapply(fao_mirca_country_mapping, length) < 1))
)) {
  index <- match(country, names(fao_mirca_country_mapping))
  dupl <- which(
    sapply(
      fao_mirca_country_mapping[-index],
      identical,
      y = fao_mirca_country_mapping[[country]]
    )
  )
  if (length(dupl) > 0 && length(fao_mirca_country_mapping[[country]]) > 0) {
    warning(
      sQuote(country), " seems to be a duplicate of ",
      toString(sQuote(names(dupl))),
      " in fao_mirca_country_mapping.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}
# Countries without MIRCA units
if (any(sapply(fao_mirca_country_mapping, length) < 1)) {
  warning(
    "FAOSTAT countries ",
    toString(
      sQuote(names(which(sapply(fao_mirca_country_mapping, length) < 1)))
    ),
    " have no corresponding MIRCA2000 spatial unit.",
    call. = FALSE,
    immediate. = TRUE
  )
}

# Reduce crop calendar lists to arrays
# If you want to process both rainfed and irrigated crop calendars change to:
# for (irr in c("irrigated", "rainfed")) {
for (irr in c("irrigated")) {
  tmpunits <- get(paste0("mirca_", irr, "_units"))
  tmpnames <- names(tmpunits)
  tmpunits <- matrix(
    unlist(tmpunits),
    ncol = length(mirca_names) * length(mirca_refyear),
    byrow = TRUE
  )
  dim(tmpunits) <- c(
    dim(tmpunits)[1],
    length(mirca_names),
    length(mirca_refyear)
  )
  dimnames(tmpunits) <- list(
    tmpnames,
    mirca_names,
    format(mirca_refyear, scientific = FALSE, trim = TRUE)
  )
  assign(paste0("mirca_", irr, "_units"), tmpunits)
  rm(tmpunits)
}
# Aggregate MIRCA units to FAOSTAT/GADM countries
if (length(mirca_refyear) > 1) {
  margin <- c(2, 3)
} else {
  margin <- 2
}
# If you want to process both rainfed and irrigated crop calendars change to:
# for (irr in c("irrigated", "rainfed")) {
for (irr in c("irrigated")) {
  tmp_fao_gadm_units <- array(
    NA,
    dim = c(
      length(fao_gadm_country_mapping),
      length(mirca_names),
      length(mirca_refyear)
    ),
    dimnames = list(
      names(fao_gadm_country_mapping),
      mirca_names,
      format(mirca_refyear, scientific = FALSE, trim = TRUE)
    )
  )
  tmpunits <- get(paste0("mirca_", irr, "_units"))
  for (country in names(fao_gadm_country_mapping)) {
    clist <- format(
      fao_mirca_country_mapping[[country]],
      scientific = FALSE,
      trim = TRUE
    )
    if (length(fao_mirca_country_mapping[[country]]) > 1) {
      tmp_fao_gadm_units[country, , ] <- ifelse(
        apply(tmpunits[clist, , ], margin, function(indata) all(is.na(indata))),
        NA,
        apply(tmpunits[clist, , ], margin, sum, na.rm = TRUE)
      )
    } else if (length(fao_mirca_country_mapping[[country]]) == 1) {
      tmp_fao_gadm_units[country, , ] <- tmpunits[clist, , ]
    }
    rm(clist)
  }
  assign(paste0("fao_gadm_mirca_", irr, "_units"), tmp_fao_gadm_units)
}
# Aggregate FAOSTAT total harvested areas to MIRCA types
# FAOSTAT/Monfreda have close to 200 crops while MIRCA has 16 types.
# Assign FAOSTAT/Monfreda crops to corresponding MIRCA crop
fao2mirca <- rep(NA, length(ts_crops))
names(fao2mirca) <- ts_crops
# Write FAO item code for FAO crops
index <- na.omit(match(names(fao2mirca), fao_production_item_def$Item))
fao2mirca[fao_production_item_def$Item[index]] <-
  fao_production_item_def$Item.Code[index]
# Replace FAOSTAT crops with MIRCA code
index <- match(na.omit(fao2mirca), crop_type_mapping$FAO.item.code)
fao2mirca[names(na.omit(fao2mirca))] <- crop_type_mapping$MIRCA[index]
# Additional crops (without FAOSTAT item code), assume crop with Monfreda name
index <- match(names(which(is.na(fao2mirca))), crop_type_mapping$Monfreda)
fao2mirca[names(which(is.na(fao2mirca)))] <- crop_type_mapping$MIRCA[index]

# Kapok fix: Monfreda has kapokfibre and kapokseed while FAOSTAT has Kapok fruit
# Kapok crops in ts_crops
kapok <- grep("kapok", names(fao2mirca), ignore.case = TRUE)
# Kapok crops in Monfreda list
kapok2 <- grep("kapok", crop_type_mapping$Monfreda)
if (any(is.na(fao2mirca[kapok]))) {
  cat(
    "Kapok fix:", toString(sQuote(names(which(is.na(fao2mirca[kapok]))))),
    "assigned to MIRCA crop",
    toString(sQuote(unique(crop_type_mapping$MIRCA[kapok2]))), "\n"
  )
  fao2mirca[names(which(is.na(fao2mirca[kapok])))] <-
    unique(crop_type_mapping$MIRCA[kapok2])
}
# Crops without a corresponding MIRCA type
no_mirca_type <- list()
mismatch <- names(which(is.na(fao2mirca)))
if (any(is.na(fao2mirca))) {
  if (any(sapply(no_mirca_type[mismatch], is.null))) {
    warning(
      "The following crops are not assigned to any MIRCA class: ",
      toString(sQuote(setdiff(mismatch, names(no_mirca_type)))),
      call. = FALSE,
      immediate. = TRUE
    )
    no_mirca_type[mismatch] <- TRUE
  }
}

# Aggregated time series for constrained and unconstrained version of total
# harvested areas
tot_ha_timeseries_mirca <- tot_ha_timeseries_unconstrained_mirca <- array(
  dim = c(
    dim(tot_ha_timeseries)[1],
    length(mirca_names),
    dim(tot_ha_timeseries)[3]
  ),
  dimnames = list(
    dimnames(tot_ha_timeseries)[[1]],
    mirca_names,
    dimnames(tot_ha_timeseries)[[3]]
  )
)
# Crops that can be mapped directly to one MIRCA class
# Simply sum up areas of ts_crops corresponding to respective MIRCA crop
for (crop in mirca_names) {
  # Corresponding ts_crops
  clist <- fao2mirca[which(fao2mirca == crop)]
  if (length(clist) > 1) {
    # More than one ts_crops mapping to MIRCA crop
    tot_ha_timeseries_mirca[, crop, ] <- apply(
      tot_ha_timeseries[, names(clist), ],
      c(1, 3),
      sum,
      na.rm = TRUE
    )
    tot_ha_timeseries_unconstrained_mirca[, crop, ] <- apply(
      tot_ha_timeseries_unconstrained[, names(clist), ],
      c(1, 3),
      sum,
      na.rm = TRUE
    )
  } else if (length(clist) == 1) {
    # One-to-one mapping between 1 ts_crops and 1 MIRCA crop
    tot_ha_timeseries_mirca[, crop, ] <- tot_ha_timeseries[, names(clist), ]
    tot_ha_timeseries_unconstrained_mirca[, crop, ] <-
      tot_ha_timeseries_unconstrained[, names(clist), ]
  }
}
# Crops distributed to several MIRCA classes
# Monfreda crop "fornes" is mapped to MIRCA Fodder grasses and Others annual
# Split source area and assign partions to each.
for (crop in na.omit(unique(setdiff(fao2mirca, mirca_names)))) {
  # Split string into crop names (types) and crop fractions
  types <- unlist(
    regmatches(
      crop,
      gregexpr("[[:alpha:]][[:alpha:][:space:][:punct:]]*[[:alpha:]]", crop)
    )
  )
  fractions <- as.double(
    unlist(regmatches(crop, gregexpr("[[:digit:]]+[[\\.]]?[[:digit:]]*", crop)))
  )
  # Check that names and fractions have been detected for each crop
  cindex <- which(fao2mirca == crop)
  if (length(types) != length(fractions)) {
    stop(
      "Cannot determine how to distribute Monfreda crop(s) ",
      toString(sQuote(names(cindex))),
      " to MIRCA classes. crop_type_mapping says: ", sQuote(crop)
    )
  }
  # Check that crop names are valid MIRCA crops
  if (any(!types %in% mirca_names)) {
    stop(
      "Invalid MIRCA association for crop ", sQuote(crop), ": ",
      sQuote(fao2mirca[crop])
    )
  }
  # Check that fractions add up to 100%
  if (sum(fractions) != 1) {
    warning(
      "Fractions ", toString(sQuote(fractions)),
      " for crop ", sQuote(crop), " do not add up to 1.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  # Aggregate both tot_ha_timeseries and
  # tot_ha_timeseries_unconstrained
  for (ds in c(
    "tot_ha_timeseries",
    "tot_ha_timeseries_unconstrained"
  )) {
    tmp_mirca <- get(paste0(ds, "_mirca"))
    tmp_all <- get(ds)
    if (length(fao2mirca[cindex]) > 1) {
      for (year in dimnames(tmp_mirca)[[3]]) {
        tmp_mirca[, types, year] <- ifelse(
          # Is MIRCA2000 value still empty?
          is.na(tmp_mirca[, types, year]),
          # If so add sum over crops
          rowSums(tmp_all[, names(cindex), year], na.rm = TRUE) *
            rep(fractions, each = dim(tmp_all)[1]),
          # Otherwise add to MIRCA2000 value
          ifelse(
            # Check that value to add is not NA
            is.na(
              rowSums(tmp_all[, names(cindex), year], na.rm = TRUE) *
                rep(fractions, each = dim(tmp_all)[1])
            ),
            tmp_mirca[, types, year],
            tmp_mirca[, types, year] + rowSums(
              tmp_all[, names(fao2mirca[cindex]), year],
              na.rm = TRUE
            ) * rep(fractions, each = dim(tmp_all)[1])
          )
        )
      }
    } else if (length(fao2mirca[cindex]) == 1) {
      for (year in dimnames(tmp_mirca)[[3]]) {
        tmp_mirca[, types, year] <- ifelse(
          is.na(tmp_mirca[, types, year]),
          tmp_all[, names(cindex), year] *
            rep(fractions, each = dim(tmp_all)[1]),
          ifelse(
            is.na(
              tmp_all[, names(cindex), year] *
                rep(fractions, each = dim(tmp_all)[1])
            ),
            tmp_mirca[, types, year],
            tmp_mirca[, types, year] + tmp_all[, names(cindex), year] *
              rep(fractions, each = dim(tmp_all)[1])
          )
        )
      }
    }
    assign(paste0(ds, "_mirca"), tmp_mirca)
    rm(tmp_mirca, tmp_all)
  }
}

# Check for countries with no data in MIRCA but irrigated cropland in HYDE, and
# try to fill. This includes countries missing completely in MIRCA or having no
# data in MIRCA.
fao_gadm_mirca_irrigated_units_gapfilling <- array(
  FALSE,
  dim = dim(fao_gadm_mirca_irrigated_units),
  dimnames = dimnames(fao_gadm_mirca_irrigated_units)
)
clist <- setdiff(
  dimnames(fao_gadm_mirca_irrigated_units)[[1]],
  names(fao_groups)
)
fill_countries <- names(
  which(rowSums(fao_gadm_mirca_irrigated_units[clist, , ], na.rm = TRUE) == 0 &
    hyde_max_irrigated_country[clist] > 0)
)
# Temporal overlap between MIRCA2000 and HYDE country time series
overlap_mirca <- intersect(
  dimnames(fao_gadm_mirca_irrigated_units)[[3]],
  dimnames(hyde_irrigated_country_timeseries)[[2]]
)
if (length(fill_countries) > 0) {
  cat(
    "***", length(fill_countries),
    "countries have no irrigated harvested areas in MIRCA but irrigated",
    "cropland in HYDE. Trying to fill with representative shares. ***\n"
  )
  if (length(overlap_mirca) < 1) {
    warning(
      "No temporal overlap between MIRCA and HYDE aggregated ",
      "cropland data. Cannot fill data.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}
for (country in fill_countries) {
  # First try if MIRCA has data for the predecessor country for countries that
  # cease to exist.
  parent <- names(
    which(
      sapply(
        compound_countries,
        function(indata, search) search %in% indata,
        search = country
      )
    )
  )
  if (sum(fao_gadm_mirca_irrigated_units[parent, , ], na.rm = TRUE) == 0) {
    # If not, try FAOSTAT country group
    # fao_groups that the country belongs to
    groups <- names(
      which(
        sapply(
          fao_groups,
          function(indata, search) search %in% indata,
          search = country
        )
      )
    )
    # Sort from shortest to longest group
    for (parent in names(sort(sapply(fao_groups[groups], length)))) {
      if (sum(fao_gadm_mirca_irrigated_units[parent, , ], na.rm = TRUE) > 0) {
        break
      }
    }
  }
  if (sum(fao_gadm_mirca_irrigated_units[parent, , ], na.rm = TRUE) > 0 &&
    length(parent) == 1
  ) {
    # Parent source with irrigated harvested areas found

    # Determine irrigated cropland in parent and take out cropland of countries
    # that do not have any harvested areas to avoid bias when computing
    # HA/cropland
    fill_group_cropland <- hyde_irrigated_country_timeseries[parent, ]
    for (c in intersect(fill_countries, fao_groups[[parent]])) {
      fill_group_cropland <- fill_group_cropland - ifelse(
        is.na(hyde_irrigated_country_timeseries[c, ]),
        0,
        hyde_irrigated_country_timeseries[c, ]
      )
    }
    for (year in overlap_mirca) {
      if (
        #  Irrigated cropland in parent
        fill_group_cropland[year] > 0 &&
        # Irrigated cropland in country
        hyde_irrigated_country_timeseries[country, year] > 0 &&
        # Total harvested areas at least 10% of cropland extent;
        # if not suggests incomplete data
        sum(
          tot_ha_timeseries_mirca[country, , year],
          na.rm = TRUE
        ) > hyde_cropland_country_timeseries[country, year] * 0.1
      ) {
        cat(
          "Ingesting irrigated harvested areas for", year,
          "for", sQuote(country),
          "based on irrigated harvested areas of",
          ifelse(
            parent %in% names(compound_countries),
            "parent country",
            "FAOSTAT country group"
          ),
          sQuote(parent),
          "and shares of irrigated cropland in each.\n"
        )
        fao_gadm_mirca_irrigated_units[country, , year] <-
          fao_gadm_mirca_irrigated_units[parent, , year] /
            fill_group_cropland[year] *
            hyde_irrigated_country_timeseries[country, year]
        fao_gadm_mirca_irrigated_units_gapfilling[country, , year] <- TRUE
      }
    }
  } else {
    cat(
      "No alternative source for irrigated harvested areas in",
      sQuote(country), "\n"
    )
  }
}

# Create timeseries of irrigated harvested areas at the country scale
# Merge AQUASTAT and MIRCA data.
irr_ha_timeseries_mirca <- irr_ha_timeseries_unconstrained_mirca <-
  irr_ha_timeseries_mirca_gapfilling <- array(
    dim = dim(tot_ha_timeseries_mirca),
    dimnames = dimnames(tot_ha_timeseries_mirca)
  )
# Initialize gap-filling array with FALSE
irr_ha_timeseries_mirca_gapfilling[] <- FALSE

# Feed in AQUASTAT first because we trust MIRCA more. MIRCA2000 may overwrite
# AQUASTAT values later.
# Check AQUASTAT to MIRCA mapping
if (any(!dimnames(aquastat_array)[[2]] %in% aquastat_mapping$AQUASTAT)) {
  warning(
    "The following crops in AQUASTAT data are missing in AQUASTAT to MIRCA ",
    "mapping: ",
    toString(
      sQuote(
        setdiff(dimnames(aquastat_array)[[2]], aquastat_mapping$AQUASTAT)
      )
    ),
    ".\nPlease update aquastat_mapping to make sure they are not lost.",
    call. = FALSE,
    immediate. = TRUE
  )
}
cat("Crop mapping between AQUASTAT and MIRCA data:\n")
# Matching crops
cindex <- which(
  nchar(aquastat_mapping$MIRCA) > 0 &
  aquastat_mapping$AQUASTAT %in% dimnames(aquastat_array)[[2]]
)
print(
  data.frame(
    AQUASTAT = aquastat_mapping$AQUASTAT[cindex],
    MIRCA = aquastat_mapping$MIRCA[cindex]
  )
)
# Non-matching crops
cindex <- which(
  nchar(aquastat_mapping$MIRCA) == 0 | is.na(aquastat_mapping$MIRCA)
)
if (length(cindex) > 0) {
  cat(
    "The following AQUASTAT crops have no corresponding MIRCA crops.",
    "Data will not be used.\n"
  )
  print(
    data.frame(
      AQUASTAT = aquastat_mapping$AQUASTAT[cindex],
      MIRCA = rep("--", length(cindex))
    )
  )
}
# Crops in AQUASTAT mapping file also present in AQUASTAT data
cindex1 <- which(aquastat_mapping$AQUASTAT %in% dimnames(aquastat_array)[[2]])
# MIRCA2000 crops without AQUASTAT crop
cindex2 <- which(!mirca_names %in% aquastat_mapping$MIRCA[cindex1])
if (length(cindex2) > 0) {
  cat("The following MIRCA crops have no corresponding AQUASTAT crop.\n")
  print(
    data.frame(
      AQUASTAT = rep("--", length(cindex2)),
      MIRCA = mirca_names[cindex2]
    )
  )
}
# Remove crops from aquastat_mapping that do not exist in aquastat_array, e.g.
# duplicates to account for name changes.
cindex <- match(dimnames(aquastat_array)[[2]], aquastat_mapping$AQUASTAT)
aquastat_mapping <- aquastat_mapping[cindex, ]
# Check country names
cols <- c("Country", aquastat_file_use_FAOSTAT_country_col)
clist <- rbind(
  fao_production_country_def[, cols],
  fao_landuse_country_def[, cols]
)
# AQUASTAT countries not in FAOSTAT data
mismatch <- setdiff(
  dimnames(aquastat_array)[[1]],
  dimnames(irr_ha_timeseries_mirca)[[1]]
)
if (length(mismatch) > 0) {
  index <- match(dimnames(aquastat_array)[[1]], aquastat_countries$Area)
  if (any(
    !aquastat_countries[index, "Area Id"] %in%
    clist[, aquastat_file_use_FAOSTAT_country_col]
  )) {
    # Name and ID mismatch
    warning(
      "The following AQUASTAT countries are not found in harvested area data: ",
      toString(sQuote(mismatch)),
      call. = FALSE,
      immediate. = TRUE
    )
  } else {
    # Unify names based on country IDs
    # Locate ID in clist
    index2 <- match(
      aquastat_countries[index, "Area Id"],
      clist[, aquastat_file_use_FAOSTAT_country_col]
    )
    dimnames(aquastat_array)[[1]] <- clist[index2, "Country"]
  }
}
# Check if all FAOSTAT countries (without FAOSTAT groups) are in AQUASTAT
clist <- setdiff(
  dimnames(irr_ha_timeseries_mirca)[[1]],
  names(fao_groups)
)
if (any(!clist %in% dimnames(aquastat_array)[[1]])) {
  cat(
    "The following", length(setdiff(clist, dimnames(aquastat_array)[[1]])),
    "countries in harvested area time series have no corresponding entry in",
    "AQUASTAT data:",
    toString(sQuote(setdiff(clist, dimnames(aquastat_array)[[1]]))), "\n"
  )
}
# Check if AQUASTAT has years outside FAOSTAT range
mismatch <- setdiff(
  dimnames(aquastat_array)[[3]],
  dimnames(irr_ha_timeseries_mirca)[[3]]
)
if (length(mismatch) > 0) {
  cat(
    "The following years in AQUASTAT data are outside the range of",
    "harvested area time series:", toString(mismatch), "\n"
  )
}
# Harmonize crop names between AQUASTAT and MIRCA
cindex <- match(dimnames(aquastat_array)[[2]], aquastat_mapping$AQUASTAT)
dimnames(aquastat_array)[[2]] <- ifelse(
  nchar(aquastat_mapping$MIRCA[cindex]) > 0,
  aquastat_mapping$MIRCA[cindex],
  dimnames(aquastat_array)[[2]]
)
# Check which entries in irr_ha_timeseries_mirca can be filled
# using AQUASTAT
countrylist <- intersect(
  dimnames(aquastat_array)[[1]],
  dimnames(irr_ha_timeseries_mirca)[[1]]
)
croplist <- intersect(
  dimnames(aquastat_array)[[2]],
  dimnames(irr_ha_timeseries_mirca)[[2]]
)
ylist <- intersect(
  dimnames(aquastat_array)[[3]],
  dimnames(irr_ha_timeseries_mirca)[[3]]
)
if (length(ylist) > 0) {
  irr_ha_timeseries_mirca[countrylist, croplist, ylist] <-
    aquastat_array[countrylist, croplist, ylist]
} else {
  warning(
    "No temporal overlap between AQUASTAT and output_period",
    call. = FALSE,
    immediate. = TRUE
  )
}

# Feed in MIRCA
# Check which entries in irr_ha_timeseries_mirca can be filled
# using MIRCA2000
countrylist <- intersect(
  dimnames(fao_gadm_mirca_irrigated_units)[[1]],
  dimnames(irr_ha_timeseries_mirca)[[1]]
)
croplist <- intersect(
  dimnames(fao_gadm_mirca_irrigated_units)[[2]],
  dimnames(irr_ha_timeseries_mirca)[[2]]
)
ylist <- intersect(
  dimnames(fao_gadm_mirca_irrigated_units)[[3]],
  dimnames(irr_ha_timeseries_mirca)[[3]]
)
if (length(ylist) > 0) {
  # If MIRCA is NA and AQUASTAT has value in mirca_refyear, keep AQUASTAT value,
  # do not overwrite with NA
  irr_ha_timeseries_mirca[countrylist, croplist, ylist] <- ifelse(
    is.na(fao_gadm_mirca_irrigated_units[countrylist, croplist, ylist]),
    irr_ha_timeseries_mirca[countrylist, croplist, ylist],
    fao_gadm_mirca_irrigated_units[countrylist, croplist, ylist]
  )
  irr_ha_timeseries_mirca_gapfilling[countrylist, croplist, ylist] <- ifelse(
    is.na(fao_gadm_mirca_irrigated_units[countrylist, croplist, ylist]),
    irr_ha_timeseries_mirca_gapfilling[countrylist, croplist, ylist],
    fao_gadm_mirca_irrigated_units_gapfilling[countrylist, croplist, ylist]
  )
  rm(countrylist, croplist, ylist)
} else {
  warning(
    "No temporal overlap between MIRCA and output_period",
    call. = FALSE,
    immediate. = TRUE
  )
}
# Check if data from successor countries can be used for parent country
# Only if data is available in all successor countries for the same year/crop
clist <- intersect(
  names(compound_countries),
  dimnames(irr_ha_timeseries_mirca)[[1]]
)
for (country in clist) {
  # Successor countries in data
  clist2 <- intersect(
    compound_countries[[country]],
    dimnames(irr_ha_timeseries_mirca)[[1]]
  )
  if (length(clist2) > 0) {
    corr <- which(
      is.na(irr_ha_timeseries_mirca[country, , ]) &
      apply(
        irr_ha_timeseries_mirca[clist2, , ],
        c(2, 3),
        function(indata) all(!is.na(indata))
      ),
      arr.ind = TRUE
    )
    if (nrow(corr) > 0) {
      cat(
        "Updating", nrow(corr), "values in", sQuote(country),
        "based on values in successor countries", toString(sQuote(clist2)), "\n"
      )
      for (r in seq_len(nrow(corr))) {
        irr_ha_timeseries_mirca[country, corr[r, 1], corr[r, 2]] <-
          sum(irr_ha_timeseries_mirca[clist2, corr[r, 1], corr[r, 2]])
        irr_ha_timeseries_mirca_gapfilling[country, corr[r, 1], corr[r, 2]] <-
          TRUE
      }
    }
  }
}
################################################################################


################################################################################
## Check compatibility between irrigated harvested areas from MIRCA2000/      ##
## AQUASTAT and total harvested areas from FAOSTAT/Monfreda.                  ##
##                                                                            ##
# Constrain irrigated harvested areas to be <= total harvested areas before
# extrapolation across time. Two versions are created:
# irr_ha_timeseries_unconstrained_mirca_faostat:
#   irrigated areas constrained to "tot_ha_timeseries_unconstrained_mirca"
# irr_ha_timeseries_mirca_faostat: irrigated areas constrained to
#   "tot_ha_timeseries_mirca"

# Create unconstrained copy of irrigated harvested areas
irr_ha_timeseries_unconstrained_mirca <- irr_ha_timeseries_mirca
cat(
  "*** Making sure irrigated harvested areas are not bigger than total",
  "harvested areas in source data before extrapolation ***\n"
)
for (var in c(
  "irr_ha_timeseries_unconstrained_mirca",
  "irr_ha_timeseries_mirca"
)) {
  cat(var, "<=", sub("irr_", "tot_", var), "\n")
  tmpirrig <- get(var)
  tmptotal <- get(sub("irr_", "tot_", var))
  tmpirrig_faostat <- ifelse(
    is.na(tmptotal),
    ifelse(is.na(tmpirrig), tmpirrig, 0),
    ifelse(tmpirrig > tmptotal, tmptotal, tmpirrig)
  )
  assign(paste0(var, "_faostat"), tmpirrig_faostat)
}

# Gapfilling and extrapolation to cover full output_period
# Four versions are extrapolated:
# 1. irr_ha_timeseries_mirca
# 2. irr_ha_timeseries_mirca_faostat
# 3. irr_ha_timeseries_unconstrained_mirca
# 4. irr_ha_timeseries_unconstrained_mirca_faostat
sink(file = gapfilling_log, append = TRUE, split = TRUE)
cat(
  "*** Extending coverage of irrigated area source data (",
  paste(
    range(
      c(
        as.integer(dimnames(aquastat_array)[[3]]),
        as.integer(dimnames(fao_gadm_mirca_irrigated_units)[[3]])
      )
    ),
    collapse = "-"
  ),
  ") to cover the full output period ",
  min(output_period), "-", max(output_period), " ***\n",
  sep = ""
)
sink()
cat("Gapfilling logged to", sQuote(gapfilling_log), "\n")
# Variables to store information about gap-filling
irr_ha_timeseries_mirca_faostat_gapfilling <-
  irr_ha_timeseries_unconstrained_mirca_gapfilling <-
  irr_ha_timeseries_unconstrained_mirca_faostat_gapfilling <-
  irr_ha_timeseries_mirca_gapfilling
# Gap-filling per variable
for (var in c(
  "irr_ha_timeseries_mirca",
  "irr_ha_timeseries_mirca_faostat",
  "irr_ha_timeseries_unconstrained_mirca",
  "irr_ha_timeseries_unconstrained_mirca_faostat"
)) {
  sink(file = gapfilling_log, append = TRUE, split = TRUE)
  cat("Version", sQuote(var), "\n")
  sink()
  sink(file = gapfilling_log, append = TRUE)
  # Version-specific harvested area time series
  var_seq <- get(var)
  # Version-specific gap-filling time series
  gapfilling_seq <- get(paste0(var, "_gapfilling"))
  # Process only individual countries, not country groups
  clist <- setdiff(dimnames(irr_ha_timeseries_mirca)[[1]], names(fao_groups))
  years <- intersect(
    dimnames(irr_ha_timeseries_mirca)[[3]],
    dimnames(hyde_irrigated_country_timeseries)[[2]]
  )
  for (country in clist) {
    if (any(hyde_irrigated_country_timeseries[country, years] > 0)) {
      cat(country, "\n")
      # HYDE has irrigated cropland during some years.
      # Need to check that HYDE irrigated cropland is not 0 during years with
      # data in MIRCA/AQUASTAT
      mismatch <- which(
        colSums(var_seq[country, , ], na.rm = TRUE) > 0 &
        hyde_irrigated_country_timeseries[country, ] == 0
      )
      if (length(mismatch) > 0) {
        cat(
          "HYDE irrigated cropland in", sQuote(country),
          "is zero during", length(mismatch), "year(s)",
          "for which MIRCA/AQUASTAT has irrigated harvested areas.",
          "Need to fill in dummy cropland value to extrapolate across time.\n"
        )
        tmpci <- ifelse(
          hyde_irrigated_country_timeseries[country, ] > 0,
          hyde_gaez_max_ir_ha_country_timeseries[country, ] /
            hyde_irrigated_country_timeseries[country, ],
          NA
        )
        tmpci <- fill_timeseries(tmpci)
        tmpcropland <- ifelse(
          hyde_irrigated_country_timeseries[country, ] > 0,
          hyde_irrigated_country_timeseries[country, ],
          ifelse(
            colSums(var_seq[country, , ], na.rm = TRUE) > 0 & !is.na(tmpci),
            colSums(var_seq[country, , ], na.rm = TRUE) / tmpci,
            0
          )
        )
      } else {
        tmpcropland <- hyde_irrigated_country_timeseries[country, ]
      }
      # Select crops present in country
      croplist <- names(
        which(apply(var_seq[country, , ] > 0, 1, any, na.rm = TRUE))
      )
      for (crop in croplist) {
        tmpfill <- fill_timeseries(
          var_seq[country, crop, ],
          tmpcropland,
          crop,
          quiet = FALSE
        )
        # Take out values based on cropland dummy values
        hyde_zero <- names(
          which(hyde_irrigated_country_timeseries[country, ] == 0)
        )
        tmpfill[hyde_zero] <- 0
        # Update gap-filling variable
        index <- which(tmpfill != var_seq[country, crop, ])
        gapfilling_seq[country, crop, index] <- TRUE
        index <- which(!is.na(tmpfill) & is.na(var_seq[country, crop, ]))
        gapfilling_seq[country, crop, index] <- TRUE
        var_seq[country, crop, ] <- tmpfill
      }
    }
  }
  # Write back to version-specific variables
  assign(var, var_seq)
  assign(paste0(var, "_gapfilling"), gapfilling_seq)
  sink()
  rm(var_seq, gapfilling_seq)
}

# Constrain irrigated harvested areas to be <= total harvested areas after
# extrapolation, which was done on values either constrained or unconstrained by
# FAOSTAT; constrain resulting time series to total harvested areas (again)
# If total_version_to_use is unconstrained:
# - use "tot_ha_timeseries_unconstrained_mirca" for
#   "irr_ha_timeseries_unconstrained_mirca" and
#   "irr_ha_timeseries_unconstrained_mirca_faostat";
# - "use tot_ha_timeseries_mirca" for "irr_ha_timeseries_mirca" and
#   "irr_ha_timeseries_mirca_faostat".
# If total_version_to_use is constrained:
# - use "tot_ha_timeseries_mirca" for all
cat(
  "*** Making sure irrigated harvested areas are not bigger than total",
  "harvested areas in source data after extrapolation ***\n"
)
for (var in c(
  "irr_ha_timeseries_unconstrained_mirca",
  "irr_ha_timeseries_mirca",
  "irr_ha_timeseries_unconstrained_mirca_faostat",
  "irr_ha_timeseries_mirca_faostat"
)) {
  # Version-specific irrigated harvested area time series
  tmpirrig <- get(var)
  # Version-specific total harvested area time series
  tmptotal <- get(
    ifelse(
      grepl("unconstrained", total_version_to_use),
      sub("_faostat", "", sub("irr_", "tot_", var)),
      paste0(total_version_to_use, "_mirca")
    )
  )
  cat(
    var, "<=",
    ifelse(
      grepl("unconstrained", total_version_to_use),
      sub("_faostat", "", sub("irr_", "tot_", var)),
      paste0(total_version_to_use, "_mirca")
    ),
    "\n"
  )
  tmpirrig_faostat <- ifelse(
    # If total harvested areas missing
    is.na(tmptotal),
    # Set positive values to 0, keep missing values as NAs
    ifelse(is.na(tmpirrig), tmpirrig, 0),
    # Reduce values that are too high, keep others
    ifelse(tmpirrig > tmptotal, tmptotal, tmpirrig)
  )
  # Write back to version-specific variables
  assign(paste0(var), tmpirrig_faostat)
}

# Constrain irrigated areas to fit into maximum possible harvested areas
# Maximum possible harvested areas: HYDE irrigated cropland * GAEZ multicropping
# suitability
# This is done for all four versions created above
irr_ha_timeseries_mirca_final <- array(
  dim = dim(irr_ha_timeseries_mirca),
  dimnames = dimnames(irr_ha_timeseries_mirca)
)
irr_ha_timeseries_unconstrained_mirca_final <-
  irr_ha_timeseries_mirca_faostat_final <-
  irr_ha_timeseries_unconstrained_mirca_faostat_final <-
  irr_ha_timeseries_mirca_final
# Years to process
ylist <- intersect(
  dimnames(irr_ha_timeseries_mirca)[[3]],
  dimnames(hyde_gaez_max_ir_ha_country_timeseries)[[2]]
)
# Countries to process (only individual countries, not country groups)
countrylist <- setdiff(
  intersect(
    dimnames(irr_ha_timeseries_mirca)[[1]],
    dimnames(hyde_gaez_max_ir_ha_country_timeseries)[[1]]
  ),
  # Exclude country groups
  names(fao_groups)
)
for (var in c(
  "irr_ha_timeseries_mirca",
  "irr_ha_timeseries_unconstrained_mirca",
  "irr_ha_timeseries_mirca_faostat",
  "irr_ha_timeseries_unconstrained_mirca_faostat"
)) {
  sink(file = gapfilling_log, append = TRUE, split = TRUE)
  cat(
    "++ Constraining", sQuote(var),
    "to make sure it fits into HYDE irrigated cropland. ++\n"
  )
  sink()
  tmpvar <- get(var)
  sink(file = gapfilling_log, append = TRUE)
  for (country in countrylist) {
    # Sum over all crops
    tmpsums <- colSums(tmpvar[country, , ylist], na.rm = TRUE)
    # Check if any years exceed available space
    if (any(tmpsums > hyde_gaez_max_ir_ha_country_timeseries[country, ylist])) {
      # Update harvested areas if necessary
      tmpvar[country, , ylist] <- ifelse(
        # Are there any crops in the country?
        rep(tmpsums > 0, each = dim(tmpvar)[2]),
        ifelse(
          # Is there any space in the country?
          rep(
            hyde_gaez_max_ir_ha_country_timeseries[country, ylist] > 0,
            each = dim(tmpvar)[2]
          ),
          ifelse(
            # Do harvested areas exceed available space?
            rep(
              tmpsums > hyde_gaez_max_ir_ha_country_timeseries[country, ylist],
              each = dim(tmpvar)[2]
            ),
            # If so, scale down
            tmpvar[country, , ylist] * rep(
              hyde_gaez_max_ir_ha_country_timeseries[country, ylist] / tmpsums,
              each = dim(tmpvar)[2]
            ),
            # Otherwise keep original value
            tmpvar[country, , ylist]
          ),
          # If no space, set to zero
          0
        ),
        # If no crops, no need to change
        tmpvar[country, , ylist]
      )
    }
    # Sum over all crops after update
    tmpsums_final <- colSums(tmpvar[country, , ylist], na.rm = TRUE)
    # Check how much harvested area has been lost in country
    index <- which(tmpsums_final < tmpsums)
    if (length(index) > 0) {
      change <- ((tmpsums_final / tmpsums)[index] - 1) * (-100)
      cat(
        sQuote(var), " reduced by ",
        round(min(change), 2), "% to ", round(max(change), 2), "% in ",
        length(index), " years in ", sQuote(country), "\n", sep = ""
      )
    }
  }
  sink()
  # Check how much harvested area has been lost globally
  tmptotals_final <- apply(tmpvar[countrylist, , ylist], 3, sum, na.rm = TRUE)
  # Global sum before update
  tmptotals <- apply(get(var)[countrylist, , ylist], 3, sum, na.rm = TRUE)
  index <- which(tmptotals_final < tmptotals)
  if (length(index) > 0) {
    change <- ((tmptotals_final / tmptotals)[index] - 1) * (-100)
    sink(file = gapfilling_log, append = TRUE, split = TRUE)
    cat(
      "Globally, ", sQuote(var), " has been reduced by ",
      round(min(change), 2), "% to ", round(max(change), 2), "% in ",
      length(index), " years.\n", sep = ""
    )
    sink()
  }
  assign(paste0(var, "_final"), tmpvar)
}

# Compute rainfed harvested areas as total - irrigated and check that rainfed
# harvested areas fit into cropland. Rainfed harvested areas can occupy rainfed
# cropland as well as irrigated cropland that is not already occupied by
# irrigated harvested areas.
# Only do this if total_version_to_use is constrained to HYDE.
# If necessary change some rainfed harvested areas to irrigated.
if (!grepl("unconstrained", total_version_to_use)) {
  for (var in c(
    "irr_ha_timeseries_mirca_final",
    "irr_ha_timeseries_unconstrained_mirca_final",
    "irr_ha_timeseries_mirca_faostat_final",
    "irr_ha_timeseries_unconstrained_mirca_faostat_final"
  )) {
    sink(file = gapfilling_log, append = TRUE, split = TRUE)
    cat(
      "++ Constraining rainfed areas in", sQuote(var),
      "to fit into HYDE rainfed and available irrigated cropland. ++\n"
    )
    sink()
    # Version-specific irrigated harvested area time series
    irrig_ha <- get(var)
    # Version-specific total harvested area time series
    total_ha <- get(paste0(total_version_to_use, "_mirca"))
    # Version-specific gap-filling variable
    gapfilling_seq <- get(gsub("_final", "_gapfilling", var))

    # Determine maximum available rainfed harvested areas on "free" irrigated
    # cropland, which is defined as the minimum of two values:
    # - irrigated cropland times rainfed multicropping suitability
    # - maximum available irrigated harvested areas - irrigated harvested areas
    rainfed_on_irrig_ha_max <- apply(
      abind(
        # Irrigated cropland times rainfed multicropping suitability
        hyde_gaez_max_rf_ha_on_ir_country_timeseries,
        # Maximum available irrigated harvested area - irrigated harvested area
        hyde_gaez_max_ir_ha_country_timeseries - apply(
          irrig_ha,
          c(1, 3),
          sum,
          na.rm = TRUE
        ),
        along = 3
      ),
      c(1, 2),
      min,
      na.rm = TRUE
    )
    # Rainfed harvested area is defined as total - irrigated
    rainfed_ha <- total_ha - ifelse(is.na(irrig_ha), 0, irrig_ha)
    # Find instances where there is too much rainfed harvested area
    rainfed_on_irrig <- apply(rainfed_ha, c(1, 3), sum, na.rm = TRUE) -
      hyde_gaez_max_rf_ha_on_rf_country_timeseries
    error <- which(
      rainfed_on_irrig > rainfed_on_irrig_ha_max + 1e-6 &
      hyde_gaez_max_ha_country_timeseries > 0,
      arr.ind = TRUE
    )
    error_table <- data.frame(
      country = character(0),
      year = character(0),
      # Rainfed harvested area sum
      rf_ha_sum = double(0),
      # Total harvested area sum
      total_ha_sum = double(0),
      # Rainfed and irrigated harvested areas currently on irrigated cropland
      total_ha_on_irrig = double(0),
      # Rainfed harvested area required on irrigated cropland to fit both
      # rainfed and irrigated harvested areas of the country
      rf_ha_irrig = double(0),
      # Amount of ha to shift from rainfed to irrigated
      shift_ha = double(0),
      # Rainfed harvested areas of those crops that also have irrigated
      # harvested areas
      irrig_exp_pot = double(0),
      # Maximum rainfed harvested area on rainfed cropland
      rf_ha_rf_max = double(0),
      stringsAsFactors = FALSE
    )
    # Derive new rainfed-irrigated distribution for each case of too much
    # rainfed harvested area
    for (r in seq_len(nrow(error))) {
      if (rownames(error)[r] %in% names(fao_groups)) {
        # Do not process errors in country groups
        next
      }
      # Shortcut to specific value in country time series
      rf_ha_ir_max <-
        hyde_gaez_max_rf_ha_on_ir_country_timeseries[error[r, 1], error[r, 2]]
      rf_ha_rf_max <-
        hyde_gaez_max_rf_ha_on_rf_country_timeseries[error[r, 1], error[r, 2]]
      ir_ha_max <-
        hyde_gaez_max_ir_ha_country_timeseries[error[r, 1], error[r, 2]]
      # Rainfed harvested area sum
      rf_ha_sum <- sum(rainfed_ha[error[r, 1], , error[r, 2]], na.rm = TRUE)
      # Total harvested area sum
      total_ha_sum <- sum(total_ha[error[r, 1], , error[r, 2]], na.rm = TRUE)
      # Rainfed and irrigated harvested areas currently on irrigated cropland
      total_ha_on_irrig <- total_ha_sum - rf_ha_rf_max
      # Rainfed harvested area required on irrigated cropland to fit both
      # rainfed and irrigated harvested areas of the country
      rf_ha_irrig <- rf_ha_ir_max * (ir_ha_max - total_ha_on_irrig) /
        (ir_ha_max - rf_ha_ir_max)
      # Rainfed harvested areas of those crops that also have irrigated
      # harvested areas (excludes purely rainfed crops)
      irrig_exp_pot <- sum(total_ha[error[r, 1], , error[r, 2]] -
        ifelse(
          irrig_ha[error[r, 1], , error[r, 2]] > 0,
          irrig_ha[error[r, 1], , error[r, 2]],
          NA
        ),
        na.rm = TRUE
      )
      error_table <- rbind(
        error_table,
        data.frame(
          country = rownames(error)[r],
          year = dimnames(rainfed_ha)[[3]][error[r, 2]],
          rf_ha_sum = rf_ha_sum,
          total_ha_sum = total_ha_sum,
          total_ha_on_irrig = total_ha_on_irrig,
          rf_ha_irrig = rf_ha_irrig,
          shift_ha = rf_ha_sum - (rf_ha_irrig + rf_ha_rf_max),
          irrig_exp_pot = irrig_exp_pot,
          rf_ha_rf_max = rf_ha_rf_max,
          stringsAsFactors = FALSE
        ),
        make.row.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    # Cases where existing irrigated crops do not have enough rainfed harvested
    # areas to fulfill irrigated expansion requirement -> need to switch some
    # rainfed-only crops to irrigated
    exp_mismatch <- which(error_table$shift_ha > error_table$irrig_exp_pot)
    # Process countries
    sink(file = gapfilling_log, append = TRUE)
    for (country in unique(error_table$country)) {
      # All error rows belonging to this country
      cindex <- which(error_table$country == country)
      cat(
        "Need to shift",
        paste(
          unique(
            round(
              range(
                error_table$shift_ha[cindex] /
                  error_table$rf_ha_sum[cindex],
                na.rm = TRUE
              ) * 100,
              2
            )
          ),
          "%",
          sep = "",
          collapse = " - "
        ),
        "of rainfed harvested areas to irrigated in", length(cindex),
        "year(s) in", sQuote(country), "\n"
      )
      # Country cases with missing irrigated expansion potential
      exp_mismatch_country <- intersect(exp_mismatch, cindex)
      if (length(exp_mismatch_country) > 0) {
        # Existing irrigated crops do not have sufficient rainfed harvested
        # areas to satisfy expansion demand
        cat(
          "Existing irrigated crops do not have sufficient rainfed harvested",
          "areas to satisfy expansion demand during",
          length(exp_mismatch_country), "of those years.",
          "Need to expand into purely rainfed crops.\n"
        )
        for (r in exp_mismatch_country) {
          year <- error_table$year[r]
          # Calculate missing expansion potential
          missing_exp_pot <- error_table$shift_ha[r] -
            error_table$irrig_exp_pot[r]
          if (error_table$irrig_exp_pot[r] > 0) {
            # First expand existing irrigated crops as much as possible
            croplist <- which(
              rainfed_ha[country, , year] < total_ha[country, , year]
            )
            irrig_ha[country, croplist, year] <-
              irrig_ha[country, croplist, year] +
              rainfed_ha[country, croplist, year]
          }
          # Now expand into purely rainfed crops
          croplist <- which(
            rainfed_ha[country, , year] == total_ha[country, , year]
          )
          # Sum of purely rainfed crops
          sum1 <- sum(rainfed_ha[country, croplist, year], na.rm = TRUE)
          irrig_ha[country, croplist, year] <-
            rainfed_ha[country, croplist, year] * missing_exp_pot / sum1
          # Set gapfilling to TRUE for filled in crops
          gapfilling_seq[country, croplist, year] <- TRUE
          # Update rainfed_ha
          rainfed_ha[country, , year] <- total_ha[country, , year] - ifelse(
            is.na(irrig_ha[country, , year]),
            0,
            irrig_ha[country, , year]
          )
          # Update irrigation expansion potential
          irrig_exp_pot <- sum(total_ha[country, , year] -
            ifelse(
              irrig_ha[country, , year] > 0,
              irrig_ha[country, , year],
              NA
            ),
            na.rm = TRUE
          )
          error_table$irrig_exp_pot[r] <- irrig_exp_pot
          # Update sum over all rainfed crops
          rf_ha_sum <- sum(rainfed_ha[country, , year], na.rm = TRUE)
          # Update rainfed harvested areas that need to be shifted to irrigated
          error_table$shift_ha[r] <- rf_ha_sum -
            (error_table$rf_ha_irrig[r] + error_table$rf_ha_rf_max[r])
          # Check that redistribution was successful
          if (abs(error_table$shift_ha[r]) > 1e-8) {
            stop(
              "Redistribution error after missing expansion potential in ",
              sQuote(country), " in ", year
            )
          }
        }
      }
      # Irrigated crops have sufficient rainfed harvested area to satisfy
      # expansion demand
      for (r in intersect(which(error_table$shift_ha > 1e-8), cindex)) {
        year <- error_table$year[r]
        # Select crops which already have some irrigated areas
        croplist <- which(
          rainfed_ha[country, , year] < total_ha[country, , year]
        )
        irrig_ha[country, croplist, year] <- irrig_ha[country, croplist, year] +
          min(error_table[r, "shift_ha"] / error_table[r, "irrig_exp_pot"], 1) *
          rainfed_ha[country, croplist, year]
        # Update rainfed_ha
        rainfed_ha[country, , year] <- total_ha[country, , year] - ifelse(
          is.na(irrig_ha[country, , year]),
          0,
          irrig_ha[country, , year]
        )
        # Update sum over all rainfed crops
        rf_ha_sum <- sum(rainfed_ha[country, , year], na.rm = TRUE)
        # Update rainfed harvested areas that need to be shifted to irrigated
        error_table$shift_ha[r] <- rf_ha_sum -
          (error_table$rf_ha_irrig[r] + error_table$rf_ha_rf_max[r])
        if (abs(error_table$shift_ha[r]) > 1e-8) {
          stop(
            "Redistribution error with sufficient expansion potential in ",
            sQuote(country), " in ", year
          )
        }
      }
    }
    sink()
    # Write to version-specific variables
    assign(var, irrig_ha)
    assign(gsub("_final", "_gapfilling", var), gapfilling_seq)
  }
}

# Process countries to start or cease to exist during the time covered by
# FAOSTAT. Earlier changes in country definitions are not accounted for.
clist <- rbind(
  fao_production_country_def[, c("Country", "Start.Year", "End.Year")],
  fao_landuse_country_def[, c("Country", "Start.Year", "End.Year")]
)
cindex <- which(!is.na(clist$Start.Year) | !is.na(clist$End.Year))
cat("*** Processing countries that start or cease to exist over time ***\n")
process_vars <- c(
  "irr_ha_timeseries_mirca_final",
  "irr_ha_timeseries_unconstrained_mirca_final",
  "irr_ha_timeseries_mirca_faostat_final",
  "irr_ha_timeseries_unconstrained_mirca_faostat_final",
  "tot_ha_timeseries_mirca",
  "tot_ha_timeseries_unconstrained_mirca",
  "tot_ha_timeseries",
  "tot_ha_timeseries_unconstrained"
)
for (country in intersect(
  clist$Country[cindex],
  dimnames(irr_ha_timeseries_mirca)[[1]]
)) {
  endyear <- unique(na.omit(clist$End.Year[which(clist$Country == country)]))
  startyear <- unique(na.omit(clist$Start.Year[which(clist$Country == country)]))
  if (length(startyear) == 1) {
    cat(
      "Removing all data before", startyear, "in", sQuote(country),
      "because it does not exist before this year.\n"
    )
    for (var in process_vars) {
      tmpvar <- get(var)
      years <- which(as.integer(dimnames(tmpvar)[[3]]) < startyear)
      tmpvar[country, , years] <- NA
      assign(var, tmpvar)
    }
  } else if (length(startyear) > 1) {
    stop(
      "More than one start year defined for country ", sQuote(country),
      " in FAOSTAT country definitions: ", toString(startyear)
    )
  }
  # Only remove data for countries that cease to exist if at least one successor
  # country is included in data, otherwise do not remove.
  if (length(endyear) == 1 &&
    length(
      intersect(compound_countries[[country]], dimnames(tot_ha_timeseries)[[1]])
    ) > 0
  ) {
    cat(
      "Removing all data after", endyear, "in", sQuote(country),
      "because it ceases to exist this year.\n"
    )
    for (var in process_vars) {
      tmpvar <- get(var)
      years <- which(as.integer(dimnames(tmpvar)[[3]]) > endyear)
      tmpvar[country, , years] <- NA
      assign(var, tmpvar)
    }
  } else if (length(endyear) > 1) {
    stop(
      "More than one end year defined for country ", sQuote(country),
      " in FAOSTAT country definitions: ", toString(endyear)
    )
  }
}

# Check for double entries (such as "China" for mainland China, Hongkong,
# Taiwan).
dupl_list <- intersect(
  names(which(sapply(fao_gadm_country_mapping, length) > 1)),
  setdiff(dimnames(irr_ha_timeseries_mirca_final)[[1]], names(fao_groups))
)
for (country in dupl_list) {
  # Indices of countries from irr_ha_timeseries_mirca_final in
  # fao_gadm_country_mapping, except for country itself and country groups
  cindex <- match(
    setdiff(
      setdiff(dimnames(irr_ha_timeseries_mirca_final)[[1]], names(fao_groups)),
      country
    ),
    names(fao_gadm_country_mapping)
  )
  # Check if all ISO codes assigned to country are also in
  # fao_gadm_country_mapping when excluding country itself
  if (all(
    fao_gadm_country_mapping[[country]] %in%
    unlist(fao_gadm_country_mapping[cindex])
  )) {
    replacements <- names(
      which(
        sapply(
          fao_gadm_country_mapping[cindex],
          function(haystack, needle) any(needle %in% haystack),
          needle = fao_gadm_country_mapping[[country]]
        )
      )
    )
    if (any(replacements %in% clist$Country)) {
      # Make sure country and replacements exist during the same time
      endyear_repl <- unique(
        clist$End.Year[which(clist$Country %in% replacements)]
      )
      startyear_repl <- unique(
        clist$Start.Year[which(clist$Country %in% replacements)]
      )
      endyear <- unique(clist$End.Year[which(clist$Country == country)])
      startyear <- unique(clist$Start.Year[which(clist$Country == country)])
      if (all(
        ifelse(
          is.na(startyear_repl),
          min(output_period),
          startyear_repl
        ) %in% ifelse(
          is.na(startyear),
          min(output_period),
          startyear
        )
      ) && all(
        ifelse(
          is.na(endyear_repl),
          max(output_period),
          endyear_repl
        ) %in% ifelse(
          is.na(endyear),
          max(output_period),
          endyear
        )
      )) {
        cat(
          "Removing all data in", sQuote(country),
          "because it seems to be a group entry for",
          toString(sQuote(replacements)), "\n"
        )
        for (var in process_vars) {
          tmpvar <- get(var)
          tmpvar[country, , ] <- NA
          assign(var, tmpvar)
        }
      }
    }
  }
}
################################################################################


################################################################################
## Disaggregate MIRCA2000 irrigated harvested areas to full set of crops      ##
## irr_ha_timeseries_unconstrained: largest irrigated areas; only constrained ##
##   to fit into "tot_ha_timeseries_unconstrained_mirca" after extrapolation  ##
##   and into "hyde_gaez_max_ir_ha_country_timeseries"                        ##
## irr_ha_timeseries_unconstrained_faostat: constrained to fit into           ##
##   "tot_ha_timeseries_unconstrained_mirca" before and after extrapolation,  ##
##   and into "hyde_gaez_max_ir_ha_country_timeseries"                        ##
## irr_ha_timeseries: constrained to fit into                                 ##
##   "tot_ha_timeseries_mirca" after extrapolation and into                   ##
##   "hyde_gaez_max_ir_ha_country_timeseries"                                 ##
## irr_ha_timeseries_faostat: smallest irrigated areas; constrained to fit    ##
##   into "tot_ha_timeseries_mirca" before and after extrapolation, and into  ##
##   "hyde_gaez_max_ir_ha_country_timeseries"                                 ##
irrigated_vars <- c(
  "irr_ha_timeseries_unconstrained",
  "irr_ha_timeseries_unconstrained_faostat",
  "irr_ha_timeseries",
  "irr_ha_timeseries_faostat"
)
for (var in irrigated_vars) {
  # Expanded array covering full set of irrigated crops
  fullirrig <- fullirrig_gapfilling <- array(
    dim = dim(tot_ha_timeseries),
    dimnames = dimnames(tot_ha_timeseries)
  )
  # Initialize gap-filling array with FALSE
  fullirrig_gapfilling[] <- FALSE
  # Corresponding array with MIRCA2000 crops
  mircairrig <- get(
    paste0(
      ifelse(grepl("faostat", var), sub("_faostat", "", var), var),
      "_mirca",
      ifelse(grepl("faostat", var), "_faostat_final", "_final")
    )
  )
  # Corresponding MIRCA2000 gap-filling array
  mircairrig_gapfilling <- get(
    sub(
      "_final",
      "_gapfilling",
      paste0(
        ifelse(grepl("faostat", var), sub("_faostat", "", var), var),
        "_mirca",
        ifelse(grepl("faostat", var), "_faostat_final", "_final")
      )
    )
  )
  # Corresponding array covering full set of total harvested areas
  fulltotal <- get(
    paste0(
      "tot_ha_timeseries",
      ifelse(grep("unconstrained", var), "_unconstrained", "")
    )
  )
  # Corresponding array covering total harvested areas aggregated to MIRCA2000
  # crops
  mircatotal <- get(
    paste0(
      "tot_ha_timeseries",
      ifelse(grep("unconstrained", var), "_unconstrained", ""),
      "_mirca"
    )
  )
  # Crops that can be mapped to MIRCA types directly
  for (mcrop in dimnames(mircatotal)[[2]]) {
    crops <- names(which(fao2mirca == mcrop))
    if (length(crops) == 1) {
      # Direct one-to-one match
      fullirrig[, crops, ] <- mircairrig[, mcrop, ]
      fullirrig_gapfilling[, crops, ] <- mircairrig_gapfilling[, mcrop, ]
    } else if (length(crops) > 1) {
      for (country in setdiff(dimnames(fullirrig)[[1]], names(fao_groups))) {
        fullirrig[country, crops, ] <- ifelse(
          rep(mircatotal[country, mcrop, ] > 0, each = length(crops)),
          fulltotal[country, crops, ] /
            rep(mircatotal[country, mcrop, ], each = length(crops)) *
            rep(mircairrig[country, mcrop, ], each = length(crops)),
          ifelse(is.na(fulltotal[country, crops, ]), NA, 0)
        )
        fullirrig_gapfilling[country, crops, ] <- ifelse(
          is.na(fullirrig[country, crops, ]),
          FALSE,
          rep(mircairrig_gapfilling[country, mcrop, ], each = length(crops))
        )
      }
    }
  }
  # Crops not mapped to MIRCA types or mapped to more than 1 MIRCA type
  for (crop in names(fao2mirca)[which(!fao2mirca %in% mirca_names)]) {
    if (is.na(fao2mirca[crop])) {
      # No matching MIRCA type
      fullirrig[, crop, ] <- ifelse(is.na(fulltotal[, crop, ]), NA, 0)
    } else {
      # Mapped to several MIRCA types
      types <- unlist(
        regmatches(
          fao2mirca[crop],
          gregexpr(
            "[[:alpha:]][[:alpha:][:space:][:punct:]]*[[:alpha:]]",
            fao2mirca[crop]
          )
        )
      )
      fractions <- as.double(
        unlist(
          regmatches(
            fao2mirca[crop],
            gregexpr("[[:digit:]]+[[\\.]]?[[:digit:]]*", fao2mirca[crop])
          )
        )
      )
      if (length(types) != length(fractions)) {
        stop(
          "Cannot determine how to distribute Monfreda crop(s) ",
          toString(sQuote(names(which(fao2mirca == crop)))),
          " to MIRCA classes. crop_type_mapping says: ", sQuote(crop)
        )
      }
      if (!all(types %in% mirca_names)) {
        stop(
          "Invalid MIRCA association for crop ", sQuote(crop), ": ",
          sQuote(fao2mirca[crop])
        )
      }
      for (t in seq_along(types)) {
        newirrig <- ifelse(
          mircatotal[, types[t], ] > 0,
          fulltotal[, crop, ] * fractions[t] / mircatotal[, types[t], ] *
            mircairrig[, types[t], ],
          ifelse(is.na(fulltotal[, crop, ]), NA, 0)
        )
        newirrig_gapfilling <- ifelse(
          is.na(newirrig),
          FALSE,
          mircairrig_gapfilling[, types[t], ]
        )
        fullirrig[, crop, ] <- ifelse(
          is.na(fullirrig[, crop, ]),
          newirrig,
          ifelse(
            is.na(newirrig),
            fullirrig[, crop, ],
            fullirrig[, crop, ] + newirrig
          )
        )
        fullirrig_gapfilling[, crop, ] <- fullirrig_gapfilling[, crop, ] |
          newirrig_gapfilling
      }
    }
  }
  # Consistency check
  clist <- setdiff(dimnames(fullirrig)[[1]], names(fao_groups))
  sum_full <- apply(fullirrig[clist, , ], c(1, 3), sum, na.rm = TRUE)
  sum_mirca <- apply(mircairrig[clist, , ], c(1, 3), sum, na.rm = TRUE)
  if (any(abs(sum_full - sum_mirca) > 1e-7)) {
    stop("Error disaggregating ", sQuote(var), " to full crop list")
  }
  if (any(sum_full - hyde_gaez_max_ir_ha_country_timeseries[clist, ] > 1e-7)) {
    stop(
      "Irrigated areas in ", sQuote(var),
      " do not fit into HYDE irrigated cropland.\n",
      "This should have been taken care of at an earlier point in this script."
    )
  }
  # Write to version-specific variables
  assign(var, fullirrig)
  assign(paste0(var, "_gapfilling"), fullirrig_gapfilling)
}
################################################################################


################################################################################
## Save total harvested area timeseries and irrigated harvested timeseries    ##
## for use in spatial disaggregation.                                         ##
save_vars <- c(
  irrigated_version_to_use,
  paste0(irrigated_version_to_use, "_gapfilling"),
  total_version_to_use,
  paste0(total_version_to_use, "_gapfilling"),
  "ts_crops",
  "hyde_gaez_max_ha_country_timeseries",
  "hyde_gaez_max_ir_ha_country_timeseries",
  "hyde_gaez_max_rf_ha_on_ir_country_timeseries",
  "hyde_gaez_max_rf_ha_on_rf_country_timeseries",
  "hyde_irrigated_country_timeseries",
  "hyde_cropland_country_timeseries",
  "landuse_array_expanded"
)
if (any(!save_vars %in% ls())) {
  stop(
    "Variable(s) ", toString(sQuote(setdiff(save_vars, ls()))),
    " missing while trying to save to ",
    sQuote(ha_country_timeseries_RData)
  )
}
cat(
  "*** Saving country-level, crop-specific total and irrigated harvested",
  "areas to", sQuote(ha_country_timeseries_RData), "***\n"
)
save(list = save_vars, file = ha_country_timeseries_RData)
## Also save alternative versions of harvested area timeseries                ##
alternative_save_vars <- c(
  setdiff(irrigated_vars, irrigated_version_to_use),
  paste0(setdiff(irrigated_vars, irrigated_version_to_use), "_gapfilling"),
  setdiff(
    c("tot_ha_timeseries", "tot_ha_timeseries_unconstrained"),
    total_version_to_use
  ),
  intersect(
    paste0(
      c("tot_ha_timeseries", "tot_ha_timeseries_unconstrained"),
      "_gapfilling"
    ),
    ls()
  )
)
if (any(!alternative_save_vars %in% ls())) {
  stop(
    "Variable(s) ", toString(sQuote(setdiff(alternative_save_vars, ls()))),
    " missing while trying to save to ",
    sQuote(alt_ha_country_timeseries_RData)
  )
}
cat(
  "*** Saving alternative versions country-level, crop-specific total and",
  "irrigated harvested areas to", sQuote(alt_ha_country_timeseries_RData),
  "***\n"
)
save(
  list = alternative_save_vars,
  file = alt_ha_country_timeseries_RData
)
################################################################################
