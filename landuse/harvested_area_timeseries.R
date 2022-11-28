################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script is used to distribute country-level total and irrigated        ##
## harvested timeseries to the grid. This process can take very long. While   ##
## the code is not parallelized explicitly you can split processing of the    ##
## whole output_period into chunks by providing start_year and end_year as    ##
## command line arguments and process different chunks in parallel.           ##
## We suggest not to have chunks overlap.                                     ##
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
## Parameter setup for spatial redistribution algorithm                       ##
## Maximum number of iterations for spatial redistribution algorithm          ##
## If you set this very low you need to adjust threshold for expansion below  ##
redist_max_it <- 1000
## Threshold for expansion outside of base patterns                           ##
## If crops have not been redistributed successfully after this number of     ##
## iterations allow crops to grow in cells where they are not present         ##
## according to harvested_fraction.                                           ##
redist_exp_thresh <- 100
################################################################################

################################################################################
## Check whether start_year and/or end_year have been provided as command     ##
## line arguments.                                                            ##
## These must be specified after "--args" and be in the form of               ##
## start_year=[year value] end_year=[year value]                              ##
## You may provide one, both or neither.                                      ##
## If not provided, min(output_period) and max(output_period) are used.       ##
if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  if (any(grepl("start_year", commandArgs(trailingOnly = TRUE)))) {
    start_year <- strsplit(
      grep("start_year", commandArgs(trailingOnly = TRUE), value = TRUE)[1],
      split = "="
    )
    start_year <- as.integer(
      grep("start_year", unlist(start_year), invert = TRUE, value = TRUE)
    )
    cat(
      "Parameter start_year provided as command line argument:",
      start_year, "\n"
    )
    if (start_year < min(output_period)) {
      # Cannot exceed output_period.
      start_year <- min(output_period)
    }
  } else {
    start_year <- min(output_period)
  }
  if (any(grepl("end_year", commandArgs(trailingOnly = TRUE)))) {
    end_year <- strsplit(
      grep("end_year", commandArgs(trailingOnly = TRUE), value = TRUE)[1],
      split = "="
    )
    end_year <- as.integer(
      grep("end_year", unlist(end_year), invert = TRUE, value = TRUE)
    )
    cat("Parameter end_year provided as command line argument:", end_year, "\n")
    if (end_year > max(output_period)) {
      # Cannot exceed output_period.
      end_year <- max(output_period)
    }
    if (end_year < start_year) {
      stop(
        "Parameter end_year (", end_year,
        ") must be larger than start_year (",
        start_year, ")"
      )
    }
  } else {
    end_year <- max(output_period)
  }
} else  {
  start_year <- min(output_period)
  end_year <- max(output_period)
}
################################################################################


################################################################################
## Load country-level, crop-specific total and irrigated harvested areas      ##
## compiled by split_global_harvested_areas_into_rainfed_irrigated.R          ##
## and run some consistency checks.                                           ##
cat(
  "*** Loading country-level, crop-specific total and irrigated harvested",
  "areas from", sQuote(ha_country_timeseries_RData), "***\n"
)
load(ha_country_timeseries_RData)
# Check all required variables have been loaded from RData file
varcheck <- c(
  irrigated_version_to_use,
  total_version_to_use,
  "ts_crops",
  "hyde_cropland_country_timeseries",
  "hyde_irrigated_country_timeseries"
)
if (any(!varcheck %in% ls())) {
  stop(
    "Variable(s) ", toString(sQuote(setdiff(varcheck, ls()))), " missing in ",
    sQuote(harvested_area_country_timeseries_RData)
  )
}
# Check that loaded data covers output_period
years <- seq(min(output_period), max(output_period))
if (any(!years %in% dimnames(get(irrigated_version_to_use))[[3]])) {
  stop(
    "Defined output_period (", paste(range(output_period), collapse = "-"),
    ") is not fully covered by data in ", irrigated_version_to_use
  )
}
if (any(!years %in% dimnames(get(total_version_to_use))[[3]])) {
  stop(
    "Defined output_period (", paste(range(output_period), collapse = "-"),
    ") is not fully covered by data in ", total_version_to_use
  )
}
# Check that loaded data covers all ts_crops
if (any(!ts_crops %in% dimnames(get(total_version_to_use))[[2]])) {
  stop(
    "Crop(s) ",
    toString(
      sQuote(setdiff(ts_crops, dimnames(get(total_version_to_use))[[2]]))
    ),
    " missing in ",
    total_version_to_use
  )
}
if (any(!ts_crops %in% dimnames(get(irrigated_version_to_use))[[2]])) {
  stop(
    "Crop(s) ",
    toString(
      sQuote(setdiff(ts_crops, dimnames(get(irrigated_version_to_use))[[2]]))
    ),
    " missing in ",
    irrigated_version_to_use
  )
}
# Check that output_period does not exceed period covered by HYDE data
if (min(output_period) < min(hyde_period) ||
  max(output_period) > max(hyde_period)
) {
  stop(
    "Some years of output_period ", paste(range(output_period), collapse = "-"),
    " are outside the range of HYDE cropland data (",
    paste(range(hyde_period), collapse = "-"), ")"
  )
}
cat(
  "Country-level total harvested areas cover the range",
  paste(
    range(as.integer(dimnames(get(total_version_to_use))[[3]])),
    collapse = "-"
  ),
  "\n"
)
cat(
  "Country-level irrigated harvested areas cover the range",
  paste(
    range(as.integer(dimnames(get(irrigated_version_to_use))[[3]])),
    collapse = "-"
  ),
  "\n"
)
cat(
  "NetCDFs created for output period",
  paste(range(output_period), collapse = "-"), "\n"
)
if (start_year > min(output_period) || end_year < max(output_period)) {
  cat(
    "Only years",
    paste(start_year, end_year, sep = "-"),
    "will be processed in this script run.\n"
  )
}

cat("*** Beginning processing of data ***\n")
################################################################################
## Load further required data and FAOSTAT definitions                         ##
# FAO country mapping
cat("Reading country definitions:\n")
cat("FAO to GADM mapping:", toString(sQuote(fao_gadm_mapping_file)), "\n")
for (filename in fao_gadm_mapping_file) {
  source(filename)
}
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
  # Avoid country code "NA" for Namibia to be mistaken for missing value
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_production_country_group_def <- fread(
  fao_production_country_group_file,
  na.strings = "...",
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_landuse_country_def <- fread(
  fao_landuse_country_file,
  na.strings = "...",
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_landuse_country_group_def <- fread(
  fao_landuse_country_group_file,
  na.strings = "...",
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
# GADM administrative levels
# The files gadmlevel_file and gadmlevel_names_file are generated by scripts in
# ../gadm so make sure to run these first.
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
# Extract country names and codes
index <- which(!is.na(gadmlevel_names$level0_ID))
cols <- c("level0_ID", "level0_code", "country")
gadm_country_names <- gadmlevel_names[index, cols]
rm(gadmlevel_names)

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
# -> Take care of differences in land mask
# Replace NA by 1
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
# Replace 0 by 1
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

# Harvested fractions per crop (created by harvested_fraction.R)
cat(
  "Using crop-specific harvested area shares from",
  sQuote(harvested_fraction_filename), "\n"
)
harvested_fraction_raster <- raster(harvested_fraction_filename, layer = 1)
harvested_fraction_file <- nc_open(harvested_fraction_filename)
# Detect if vertical orientation needs to be flipped
lats <- harvested_fraction_file$dim$lat$vals
harvested_fraction_flip <-
  (yFromRow(gadm_raster, 1) > yFromRow(gadm_raster, 2)) != (lats[1] > lats[2])
# Check that all ts_crops are present in NetCDF file
harvested_fraction_crops <- ncvar_get(harvested_fraction_file, "crop")
if (any(!ts_crops %in% harvested_fraction_crops)) {
  stop(
    "Crop(s) ",
    toString(sQuote(setdiff(ts_crops, harvested_fraction_crops))),
    " missing in ",
    sQuote(harvested_fraction_file)
  )
}

# Check if extent and resolutions of various raster objects match
varcheck <- c(
  "gadmlevel_raster",
  "gaez_multicropping_suit_ir",
  "gaez_multicropping_suit_rf",
  "harvested_fraction_raster"
)
for (rastercheck in varcheck) {
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
      sQuote(gadmlevel_file), " and ", rastercheck,
      " have different resolution"
    )
  }
  rm(tmpraster)
}
rm(harvested_fraction_raster)

# HYDE area
hyde_is_fraction <- !ud.are.convertible(hyde_area_units, "m2")
if (hyde_is_fraction) {
  # Load HYDE area at native resolution and do not aggregate to output
  # resolution (default settings in load_hyde_area() aggregate data)
  hyde_area <- load_hyde_area(
    hyde_area_file,
    unitraster = NULL
  )
  hyde_area_file_units <- hyde_area$unit
  hyde_area <- hyde_area$area
}

# Clean encoding of various tables read in from external files
message("Converting special characters read from text files if necessary.")
varcheck <- c(
  "fao_production_country_def",
  "gadm_names",
  "fao_production_country_group_def",
  "fao_landuse_country_def",
  "fao_landuse_country_group_def"
)
for (table in varcheck) {
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
          message(
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
## Process country mappings and check for consistency.                        ##
# Check consistency between compound_countries, fao_production_country_def,
# fao_landuse_country_def
clist <- rbind(
  fao_production_country_def[, c("Country", "End.Year")],
  fao_landuse_country_def[, c("Country", "End.Year")]
)
# Countries with end year
index <- which(!is.na(clist$End.Year))
if (length(setdiff(names(compound_countries), clist$Country[index])) > 0) {
  # Compound_countries has countries not mentioned in fao_production_country_def
  # or fao_landuse_country_def, remove
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
      "for country ", sQuote(country), " are available in FAOSTAT data.",
      "\nDropping from compound_countries",
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
  # FAOSTAT has countries listed as ceasing to exist that are missing in
  # compound_countries
  for (country in setdiff(clist$Country[index], names(compound_countries))) {
    warning(
      sQuote(country), " is missing ",
      "in compound_countries but is listed as ceasing to exist in ",
      unique(clist[which(clist$Country == country), "End.Year"]),
      ". Resetting end year.\n",
      "Please update compound_countries if there are successor countries.",
      call. = FALSE,
      immediate. = TRUE
    )
    index2 <- which(fao_production_country_def$Country == country)
    fao_production_country_def[index2, "End.Year"] <- NA
    index2 <- which(fao_landuse_country_def$Country == country)
    fao_landuse_country_def[index2, "End.Year"] <- NA
  }
}
# Determine country groups based on fao_production_country_group_def and
# fao_landuse_country_group_def
fao_groups <- list()
clist <- abind(
  fao_production_country_def[, c("Country", "Country.Code")],
  fao_production_country_group_def[, c("Country", "Country.Code")],
  fao_production_country_group_def[, c("Country.Group", "Country.Group.Code")],
  fao_landuse_country_group_def[, c("Country", "Country.Code")],
  fao_landuse_country_group_def[, c("Country.Group", "Country.Group.Code")],
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
# Add country groups to fao_gadm_country_mapping
cat("*** Filling country groups in FAOSTAT to GADM country mapping ***\n")
cat(
  "Country groups derived from fao_production_country_group_def and",
  "fao_landuse_country_group_def\n"
)
# Manual additions
# Allows to add countries to groups if they are missing in
# fao_production_country_group_def
# Use the country name as the name of the list member and set the country groups
# that the country should belong to as values.
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
  cat("Filling", sQuote(country), "in GADM country list\n")
  fao_gadm_country_mapping[[country]] <- unique(
    unlist(fao_gadm_country_mapping[fao_groups[[country]]])
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
  dimnames(get(total_version_to_use))[[1]]
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
      sQuote(country), " seems to be a duplicate of ",
      toString(sQuote(names(dupl))), " in fao_gadm_country_mapping",
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
rm(tmpcodes, gadmlevel_raster)
################################################################################


################################################################################
## Define aggregation functions used later                                    ##
# These either sum all crops within a cell or all cells per crop in a country.
cellsum <- function(country_data, na.rm = FALSE) {
  return(rowSums(country_data, na.rm = na.rm))
}
countrysum <- function(country_data, na.rm = FALSE) {
  return(colSums(country_data, na.rm = na.rm))
}
################################################################################


################################################################################
## Set up file for crop-specific timeseries of harvested areas                ##
# Concatenate all possible version strings for file names
file_version_string <- paste0(
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
    exists("ramankutty_version_string") && nchar(ramankutty_version_string) > 0,
    paste0("_", ramankutty_version_string),
    ""
  )
)
# Set up NetCDF variables
# Take over longitude and latitude dimensions from harvested_fraction_file
lon_dim <- harvested_fraction_file$dim$lon
lat_dim <- harvested_fraction_file$dim$lat
# Character variables in NetCDF files are two-dimensional arrays of single
# characters. Define both dimensions.
# Number of characters in name string
nchar_dim <- ncdim_def(
  "nchar",
  units = "",
  vals = seq_len(max(nchar(ts_crops))),
  create_dimvar = FALSE
)
# Number of name strings
crop_dim <- ncdim_def(
  "crop",
  units = "",
  vals = seq_along(ts_crops),
  create_dimvar = FALSE
)
cropvar <- ncvar_def(
  "crop",
  units = "",
  dim = list(nchar_dim, crop_dim),
  longname = "crop name",
  prec = "char"
)

# Determine filenames and create files
# NetCDF files are created for the full output_period, not just the period given
# given by start_year and end_year.
chunk_start <- seq(
  min(output_period),
  max(output_period),
  by = ha_timeseries_chunk_length
)
chunk_end <- chunk_start + ha_timeseries_chunk_length - 1
chunk_end <- pmin(chunk_end, max(output_period))

ha_timeseries_filenames <- paste0(
  ha_timeseries_filename_base,
  "_", chunk_start, "-", chunk_end,
  file_version_string,
  ".nc"
)
# Names of variables in NetCDF file
# Most of these are defined in landuse_setup.R
nc_vars <- c(
  rainfed_output_name,
  irrigated_output_name,
  rainfed_output_sum_name,
  irrigated_output_sum_name,
  total_output_sum_name,
  "year_processed"
)
for (fileindex in seq_along(ha_timeseries_filenames)) {
  # Time dimension is specific to each file
  time_dim <- ncdim_def(
    "time",
    units = paste0("years since ", min(output_period), "-01-01"),
    vals = (chunk_start[fileindex]:chunk_end[fileindex]) - min(output_period),
    unlim = TRUE
  )
  # If this script is run for only a subset of years from output_period files
  # may have already been generated by a previous script run for another subset
  # from output_period. Generate files only if they do not exist, yet.
  if (!file.exists(ha_timeseries_filenames[fileindex])) {
    cat("*** Creating", sQuote(ha_timeseries_filenames[fileindex]), "***\n")
    # Create crop-specific variables
    rainfed_area_var <- ncvar_def(
      rainfed_output_name,
      units = fao_area_units,
      dim = list(lon_dim, lat_dim, crop_dim, time_dim),
      longname = "rainfed harvested area",
      missval = 1e30,
      chunksizes = c(lon_dim$len, lat_dim$len, 1, 1),
      prec = "double",
      compression = 5
    )
    irrigated_area_var <- ncvar_def(
      irrigated_output_name,
      units = fao_area_units,
      dim = list(lon_dim, lat_dim, crop_dim, time_dim),
      longname = "irrigated harvested area",
      missval = 1e30,
      chunksizes = c(lon_dim$len, lat_dim$len, 1, 1),
      prec = "double",
      compression = 5
    )
    # Create variables for sums over all crops
    rainfed_sum_var <- ncvar_def(
      rainfed_output_sum_name,
      units = fao_area_units,
      dim = list(lon_dim, lat_dim, time_dim),
      longname = "sum of rainfed harvested areas over all crops",
      missval = 1e30,
      chunksizes = c(lon_dim$len, lat_dim$len, 1),
      prec = "double",
      compression = 5
    )
    irrigated_sum_var <- ncvar_def(
      irrigated_output_sum_name,
      units = fao_area_units,
      dim = list(lon_dim, lat_dim, time_dim),
      longname = "sum of irrigated harvested areas over all crops",
      missval = 1e30,
      chunksizes = c(lon_dim$len, lat_dim$len, 1),
      prec = "double",
      compression = 5
    )
    total_sum_var <- ncvar_def(
      total_output_sum_name,
      units = fao_area_units,
      dim = list(lon_dim, lat_dim, time_dim),
      longname = c(
        "sum of total (rainfed + irrigated) harvested areas over all crops"
      ),
      missval = 1e30,
      chunksizes = c(lon_dim$len, lat_dim$len, 1),
      prec = "double",
      compression = 5
    )
    # Status variable (this tracks which years have already been processed)
    year_processed_var <- ncvar_def(
      "year_processed",
      units = "",
      dim = time_dim,
      longname = "year already fully processed",
      prec = "short",
      missval = -9
    )
    # Create file
    ha_timeseries_file <- nc_create(
      ha_timeseries_filenames[fileindex],
      list(
        rainfed_area_var,
        irrigated_area_var,
        rainfed_sum_var,
        irrigated_sum_var,
        total_sum_var,
        cropvar,
        year_processed_var
      ),
      force_v4 = TRUE
    )
    # Set initial values
    # No years processed yet
    year_processed <- rep(0, time_dim$len)
    ncvar_put(
      ha_timeseries_file,
      "year_processed",
      year_processed,
      count = length(year_processed)
    )
    # Crop names
    ncvar_put(ha_timeseries_file, "crop", ts_crops)
    # Set values of latitude axis if flipped vertically compared to
    # harvested_fraction_file
    ncvar_put(ha_timeseries_file, lat_dim$name, yFromRow(gadm_raster))
    nc_close(ha_timeseries_file)
  } else {
    # If file exists already check if it is compatible or if relevant settings
    # have changed.
    file_years <- seq(chunk_start[fileindex], chunk_end[fileindex])
    if (any(file_years >= start_year) && any(file_years <= end_year)) {
      cat(
        "*** Trying to re-use previously created",
        sQuote(ha_timeseries_filenames[fileindex]), "***\n"
      )
      ha_timeseries_file <- nc_open(ha_timeseries_filenames[fileindex])
      # Check if all required variables are in file
      if (any(!nc_vars %in% names(ha_timeseries_file$var))) {
        stop(
          "Variable(s) ",
          toString(sQuote(setdiff(nc_vars, names(ha_timeseries_file$var)))),
          " missing in ", sQuote(ha_timeseries_filenames[fileindex])
        )
      }
      # Check if dimension lengths match
      if (lon_dim$len != ha_timeseries_file$dim$lon$len ||
          lat_dim$len != ha_timeseries_file$dim$lat$len ||
          crop_dim$len != ha_timeseries_file$dim$crop$len
      ) {
        stop(
          "Dimensions of ", sQuote(ha_timeseries_filenames[fileindex]),
          " do not match script run"
        )
      }
      # Check if crops match
      if (any(ncvar_get(ha_timeseries_file, "crop") != ts_crops)) {
        stop(
          "Crop names in ", sQuote(ha_timeseries_filenames[fileindex]),
          " do not match ts_crops"
        )
      }
      # Check that time axis matches (e.g. reference year has not changed)
      if (ha_timeseries_file$dim$time$units != time_dim$units ||
          any(ha_timeseries_file$dim$time$vals != time_dim$vals)
      ) {
        stop(
          "Time axis in ", sQuote(ha_timeseries_filenames[fileindex]),
          " does not match script run"
        )
      }
      nc_close(ha_timeseries_file)
    }
  }
}
if (exists("ha_timeseries_file")) {
  rm(ha_timeseries_file)
}
# Variables to keep track of which messages have been reported already
empty_hyde <- list()
empty_pattern_msg <- list()
ncountry_previous <- 0
redist_stats <- list()
# Log files generated by this file in addition to NetCDF files
# RData file containing information on spatial redistribution algorithm
redist_stats_file <- file.path(
  working_dir,
  paste0(
    "harvested_area_GADM_timeseries_",
    start_year, "-", end_year,
    "_redistribution_stats",
    file_version_string,
    ".RData"
  )
)
# Log file for any gap filling that might be required
gapfilling_log <- file.path(
  working_dir,
  paste0(
    "harvested_area_GADM_timeseries_",
    start_year, "-", end_year,
    "_gapfilling",
    file_version_string,
    ".txt"
  )
)
# Load redist_stats_file if it exists from a previous script run
if (file.exists(redist_stats_file)) {
  load(redist_stats_file)
}
# Delete any existing gap filling log file
if (file.exists(gapfilling_log)) {
  file.remove(gapfilling_log)
}
################################################################################

################################################################################
## Processing of timeseries from start_year to end_year                       ##
## The general sequence of operations per year is:                            ##
## 1) open NetCDF file of harvested area timeseries if not open already       ##
## 2) read gridded cropland (irrigated, rainfed, total) for year              ##
## 3) check which countries exist during this year                            ##
## 4) first spatial disaggregation using harvested_fraction patterns (each    ##
##    crop separately)                                                        ##
## 5) save preliminary patterns to NetCDF file                                ##
## 6) spatial redistribution if first disaggregation leads to harvested area  ##
##    sums exceeding multicropping suitability (all crops together)           ##
## 7) save final patterns to NetCDF file                                      ##
## 8) mark year as processed in NetCDF file                                   ##
cat(
  "Creating harvested area timeseries for",
  length(ts_crops), "crops spanning the years",
  start_year, "to", end_year, "\n"
)
# Track run time
runtime_start <- proc.time()
# List with start and end years of countries
clist <- rbind(
  fao_production_country_def[, c("Country", "Start.Year", "End.Year")],
  fao_landuse_country_def[, c("Country", "Start.Year", "End.Year")]
)
for (year in seq(start_year, end_year)) {
  # 1) Open NetCDF file for first year or whenever starting a new chunk, files
  # generated above.
  if (year %in% chunk_start || !exists("ha_timeseries_file")) {
    fileindex <- which(chunk_start <= year & chunk_end >= year)
    ha_timeseries_file <- nc_open(
      ha_timeseries_filenames[fileindex],
      write = TRUE
    )
    # Read which years have finished processing
    year_processed <- ncvar_get(ha_timeseries_file, "year_processed")
    min_fileyear <- min(output_period) + min(ha_timeseries_file$dim$time$vals)
    cat("Output file:", ha_timeseries_file$filename, "\n")
  }
  # Check if year has finished processing already
  if (year_processed[year - min_fileyear + 1] == 1) {
    message(
      "*** Skipping year ", year,
      " because it is marked as 'processed already' in ",
      sQuote(ha_timeseries_file$filename), " ***"
    )
    # Close file if last year of chunk
    if (year %in% chunk_end) {
      nc_close(ha_timeseries_file)
      rm(ha_timeseries_file)
    }
    # Skip to next year
    next
  }
  cat("***", year, "***\n")
  # 2) Load cropland for current year
  for (var in c("hyde_cropland", "hyde_irrigated", "hyde_rainfed")) {
    # Only open NetCDF file for first year, then keep open
    if (exists(paste0(var, "_file"))) {
      assign("ncfile", get(paste0(var, "_file")))
    } else {
      # Open NetCDF file and save in HYDE variable-specific variable
      ncfile <- nc_open(get(paste0(var, "_filename")))
      assign(paste0(var, "_file"), ncfile)
    }
    # Check if HYDE data needs to be flipped vertically
    lats <- ncfile$dim$lat$vals
    lons <- ncfile$dim$lon$vals
    hyde_flip <- (yFromRow(gadm_raster, 1) > yFromRow(gadm_raster, 2)) !=
      (lats[1] > lats[2])
    # Spatial extent of HYDE file
    hyde_extent <- extent(
      c(
        min(lons) - abs(lons[2] - lons[1]) / 2,
        max(lons) + abs(lons[2] - lons[1]) / 2,
        min(lats) - abs(lats[2] - lats[1]) / 2,
        max(lats) + abs(lats[2] - lats[1]) / 2
      )
    )
    # Raster object corresponding to HYDE file
    hyde_raster <- raster(
      hyde_extent,
      resolution = c(abs(lons[1] - lons[2]), abs(lats[1] - lats[2]))
    )
    # Check that spatial extent matches to GADM data
    if (!matching_extent(
      hyde_extent,
      extent(gadm_raster),
      xres(gadm_raster),
      yres(gadm_raster)
    )) {
      stop("Spatial extent of HYDE cropland does not match GADM extent")
    }
    # Aggregation factor form HYDE resolution to output resolution
    hyde2gadm <- round(res(hyde_raster) / res(gadm_raster), 4)
    if (max(hyde2gadm %% 1) != 0) {
      stop(
        "GADM resolution ",
        toString(round(res(gadm_raster), 5)),
        " is not compatible with HYDE resolution ",
        toString(round(res(hyde_raster), 5))
      )
    }
    # Load HYDE data for this year
    ncfiledata <- ncvar_get(
      ncfile,
      get(paste0(var, "_varname")),
      start = c(1, 1, year - min(hyde_period) + 1),
      count = c(-1, -1, 1)
    )
    # Flip data vertically if necessary
    if (hyde_flip) {
      ncfiledata <- ncfiledata[, seq(ncfile$dim$lat$len, 1)]
    }
    # Unit conversion to FAOSTAT area unit
    if (hyde_is_fraction) {
      if (matching_extent(
        extent(hyde_area),
        hyde_extent,
        xres(hyde_area),
        yres(hyde_area)
      )) {
        # Convert from fractional unit to absolute unit using hyde_area
        # hyde_area is in fao_area_units
        ncfiledata <- ncfiledata * ud.convert(1, hyde_area_units, "1") *
          values(hyde_area)
      } else {
        stop(
          "HYDE area from ", sQuote(hyde_area_file),
          " and HYDE cropland from ", sQuote(ncfile$filename),
          "have different spatial extent."
        )
      }
    } else {
      # Convert from HYDE area unit to FAOSTAT area unit
      ncfiledata <- ncfiledata * ud.convert(1, hyde_area_units, fao_area_units)
    }
    if (max(hyde2gadm) > 1) {
      cat("Aggregating", sQuote(var), "to GADM resolution\n")
      sum1 <- sum(ncfiledata, na.rm = TRUE)
      ncfiledata <- aggregate_array(ncfiledata, hyde2gadm, "sum", FALSE)
      if (abs(sum(ncfiledata, na.rm = TRUE) - sum1) > 1e-8) {
        stop("Error aggregating ", sQuote(var), " data to output resolution")
      }
    }
    # Dissolve separate longitude and latitude dimensions
    dim(ncfiledata) <- NULL
    assign(var, ncfiledata)
    rm(ncfiledata, hyde_extent)
  }
  # Consistency check. There are a few cases in HYDE 3.2.1 where rainfed and
  # irrigated cropland do not add up to total cropland.
  sum_total <- hyde_cropland - hyde_rainfed - hyde_irrigated
  index <- which(sum_total > 1e-3)
  if (length(index) > 0) {
    # Rainfed + irrigated do not add up to total
    message(
      "Expanding rainfed cropland in ",
      length(index),
      " cells because rainfed + irrigated cropland < total cropland in year ",
       year
    )
    hyde_rainfed[index] <- hyde_cropland[index] - hyde_irrigated[index]
  }
  rm(index)
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
        " cells where both rainfed and irrigated cropland are each larger",
        " than total cropland in year ", year
      )
    } else if (length(mismatch_rf) > 0) {
      # Rainfed too big
      # Rainfed is only reduced if it is bigger than total cropland, then
      # reduced to total cropland
      message(
        "Reducing rainfed cropland in ",
        length(mismatch_rf),
        " cells because it is larger than total cropland in year ",
        year
      )
      hyde_rainfed[mismatch_rf] <- hyde_cropland[mismatch_rf]
      # Update sum of all three
      sum_total <- hyde_cropland - hyde_rainfed - hyde_irrigated
    }
    # Irrigated too big
    # Irrigated is always reduced if rainfed + irrigated bigger than total
    # cropland -> priority on rainfed cropland
    mismatch_ir <- which(sum_total < -1e-3)
    if (length(mismatch_ir) > 0) {
      message(
        "Reducing irrigated cropland in ",
        length(mismatch_ir), " cells ",
        "because it is larger than total cropland-rainfed cropland in year ",
        year
      )
      hyde_irrigated[mismatch_ir] <- hyde_cropland[mismatch_ir] -
        hyde_rainfed[mismatch_ir]
    }
    rm(mismatch_both, mismatch_rf, mismatch_ir)
  }
  rm(index)

  # Check that year is present in country-scale harvested area timeseries
  if (as.character(year) %in% dimnames(get(total_version_to_use))[[3]]) {
    tot_ha_year <- get(total_version_to_use)[, , as.character(year)]
  } else {
    warning(
      year, " missing in ", total_version_to_use,
      ". Processing by previous script should have ensured all years are",
      "present. Trying to extrapolate.",
      call. = FALSE,
      immediate. = TRUE
    )
    tot_ha_year <- array(
      0,
      dim = dim(get(total_version_to_use))[-3],
      dimnames = dimnames(get(total_version_to_use))[-3]
    )
  }
  if (as.character(year) %in% dimnames(get(irrigated_version_to_use))[[3]]) {
    ir_ha_year <- get(irrigated_version_to_use)[, , as.character(year)]
  } else {
    warning(
      year, " missing in ", irrigated_version_to_use,
      ". Processing by previous script should have ensured all years are",
      "present. Trying to extrapolate.",
      call. = FALSE,
      immediate. = TRUE
    )
    ir_ha_year <- array(
      0,
      dim = dim(get(irrigated_version_to_use))[-3],
      dimnames = dimnames(get(irrigated_version_to_use))[-3]
    )
  }

  # 3) countries to process
  # Exclude country groups
  countries_year <- setdiff(
    dimnames(get(total_version_to_use))[[1]],
    names(fao_groups)
  )
  # Exclude countries that have ended or have not started to exist yet
  countries_year <- setdiff(
    countries_year,
    clist$Country[which(clist$Start.Year > year | clist$End.Year < year)]
  )
  # Check for double entries (such as "China" for mainland China, Hongkong,
  # Taiwan)
  clist_dupl <- intersect(
    names(which(sapply(fao_gadm_country_mapping, length) > 1)),
    countries_year
  )
  for (country in clist_dupl) {
    # Indices of countries_year in fao_gadm_country_mapping, except for country
    # itself
    cindex <- match(
      setdiff(countries_year, country),
      names(fao_gadm_country_mapping)
    )
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
      # Make sure replacements have data before replacing country.
      if (any(tot_ha_year[replacements, ts_crops] >= 0, na.rm = TRUE)) {
        message(
          "Removing ", sQuote(country),
          " from list of countries because it seems to be a group entry for ",
          toString(sQuote(replacements)), "."
        )
        countries_year <- setdiff(countries_year, country)
      } else {
        # Remove replacements if they have no data
        message(
          "Keeping group entry ", sQuote(country),
          " because none of the replacements ", toString(sQuote(replacements)),
          " have any data. Removing replacements."
        )
        countries_year <- setdiff(countries_year, replacements)
      }
      rm(replacements)
    } else if (any(fao_gadm_country_mapping[[country]] %in%
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
      mismatch <- setdiff(
        fao_gadm_country_mapping[[country]],
        unlist(fao_gadm_country_mapping[replacements])
      )
      if (any(tot_ha_year[replacements, ts_crops] >= 0, na.rm = TRUE)) {
        warning(
          "Cannot use data for ", toString(sQuote(replacements)),
          " because ",
          ifelse(length(replacements) > 1, "they are ", "it is "),
          "part of ", sQuote(country), " but other ", length(mismatch),
          ifelse(length(mismatch) > 1, " parts ", "part "),
          toString(sQuote(mismatch)),
          ifelse(length(mismatch) > 1, " are ", "is "),
          "not available as a separate country entry.\n",
          "Please check fao_gadm_country_mapping.",
          call. = FALSE,
          immediate. = TRUE
        )
      } else {
        message(
          "Removing ", toString(sQuote(replacements)),
          " from list of countries because ",
          ifelse(length(replacements) > 1, "they are ", "it is "),
          "part of ", sQuote(country)
        )
      }
      message(
        paste(
          c(country, replacements),
          sapply(
            fao_gadm_country_mapping[c(country, replacements)],
            function(x) toString(sQuote(x))
          ),
          sep = ": ",
          collapse = "\n"
        )
      )
      countries_year <- setdiff(countries_year, replacements)
      rm(replacements, mismatch)
    }
    rm(cindex)
  }
  # Check that there are no undetected duplicates left.
  if (length(unlist(fao_gadm_country_cells[countries_year])) !=
    length(unique(unlist(fao_gadm_country_cells[countries_year])))
  ) {
    cindex <- match(countries_year, names(fao_gadm_country_cells))
    freq_table <- as.data.frame(
      table(unlist(fao_gadm_country_cells[cindex])),
      stringsAsFactors = FALSE
    )
    clist_dupl <- names(
      which(
        sapply(
          fao_gadm_country_cells[cindex],
          function(haystack, needle) any(needle %in% haystack),
          needle = as.integer(freq_table$Var1[which(freq_table$Freq > 1)])
        )
      )
    )
    message(
      "There is spatial overlap in ", length(which(freq_table$Freq > 1)),
      " cells belonging to more than one of the following countries:\n",
      paste(
        c(clist_dupl),
        sapply(
          fao_gadm_country_mapping[clist_dupl],
          function(x) toString(sQuote(x))
        ),
        sep = ": ",
        collapse = "\n"
      )
    )
    rm(cindex, freq_table, clist_dupl)
    stop(
      "While country definitions can change over time, each cell must only be ",
      "assigned to one country at each point in time. ",
      "Please check fao_gadm_country_mapping, compound_countries, and ",
      "start and end years defined in fao_production_country_def and ",
      "fao_landuse_country_def."
    )
  }

  # Sum up this year's cropland in all countries to be processed
  yearly_vars <- c(
    "cropland_year_country",
    "rainfed_year_country",
    "irrigated_year_country",
    "total_suit_year_country",
    "rf_suit_rf_year_country",
    "ir_suit_year_country",
    "rf_suit_ir_year_country"
  )
  for (var in yearly_vars) {
    vals <- double(length(countries_year))
    names(vals) <- countries_year
    assign(var, vals)
  }
  for (country in countries_year) {
    ccells <- fao_gadm_country_cells[[country]]
    cropland_year_country[country] <- sum(hyde_cropland[ccells], na.rm = TRUE)
    rainfed_year_country[country] <- sum(hyde_rainfed[ccells], na.rm = TRUE)
    irrigated_year_country[country] <- sum(hyde_irrigated[ccells], na.rm = TRUE)
    # Maximum possible rainfed harvested areas on rainfed cropland
    rf_suit_rf_year_country[country] <- sum(
      (hyde_cropland[ccells] - hyde_irrigated[ccells]) *
        gaez_multicropping_suit_rf[ccells],
      na.rm = TRUE
    )
    # Maximum possible rainfed harvested areas on irrigated cropland (assuming
    # no irrigated crops present)
    rf_suit_ir_year_country[country] <- sum(
      hyde_irrigated[ccells] * gaez_multicropping_suit_rf[ccells],
      na.rm = TRUE
    )
    # Maximum possible irrigated harvested areas
    ir_suit_year_country[country] <- sum(
      hyde_irrigated[ccells] * gaez_multicropping_suit_ir[ccells],
      na.rm = TRUE
    )
    # Maximum possible total harvested areas
    total_suit_year_country[country] <- rf_suit_rf_year_country[country] +
      ir_suit_year_country[country]
    # Check that country is included in HYDE land mask. Some small islands are
    # missing in HYDE version 3.2.1.
    # Message about missing countries is printed only once during first year
    # processed.
    if (is.null(empty_hyde[[country]])) {
      if (all(is.na(hyde_cropland[ccells]))) {
        # All cells for this country are NA in HYDE cropland
        if (any(
          landuse_array_expanded[country, "Cropland", "Area", ] > 0,
          na.rm = TRUE
        )) {
          # FAOSTAT has cropland at some point in time for this country
          message(
            sQuote(country),
            " appears to be missing completely in HYDE land mask.",
            " Dropping any cropland present according to FAOSTAT landuse",
            " statistics"
          )
        } else {
          # FAOSTAT has no cropland in this country either
          message(
            sQuote(country),
            " appears to be missing completely in HYDE land mask.",
            " It also does not have cropland according to FAOSTAT landuse",
            " statistics"
          )
        }
        # Remember message about missing data has been displayed already
        empty_hyde[[country]] <- TRUE
      }
    } else if (any(!is.na(hyde_cropland[ccells]))) {
      # HYDE was missing this country in previous years but has data now, clear
      # missing-data message status
      empty_hyde[[country]] <- NULL
    }
    rm(ccells)
  }
  # Check consistency with data used in
  # split_global_harvested_areas_into_rainfed_irrigated.R
  if (exists("hyde_cropland_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_cropland_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_cropland_country_timeseries[clist2, as.character(year)] -
        cropland_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between HYDE cropland used here and ",
        "HYDE cropland used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }
  if (exists("hyde_irrigated_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_irrigated_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_irrigated_country_timeseries[clist2, as.character(year)] -
        irrigated_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between HYDE irrigated cropland used here ",
        "and HYDE irrigated cropland used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }
  if (exists("hyde_rainfed_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_rainfed_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_rainfed_country_timeseries[clist2, as.character(year)] -
        rainfed_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between HYDE rainfed cropland used here ",
        "and HYDE rainfed cropland used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }
  if (exists("hyde_gaez_max_ha_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_gaez_max_ha_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_gaez_max_ha_country_timeseries[clist2, as.character(year)] -
        total_suit_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between multicropping suitability used ",
        "here and multicropping suitability used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }
  if (exists("hyde_gaez_max_ir_ha_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_gaez_max_ir_ha_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_gaez_max_ir_ha_country_timeseries[clist2, as.character(year)] -
        ir_suit_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between irrigated multicropping suitability used ",
        "here and irrigated multicropping suitability used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }
  if (exists("hyde_gaez_max_rf_ha_on_rf_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_gaez_max_rf_ha_on_rf_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_gaez_max_rf_ha_on_rf_country_timeseries[clist2, as.character(year)] -
        rf_suit_rf_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between rainfed multicropping suitability used ",
        "here and rainfed multicropping suitability used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }
  if (exists("hyde_gaez_max_rf_ha_on_ir_country_timeseries")) {
    clist2 <- intersect(
      dimnames(hyde_gaez_max_rf_ha_on_ir_country_timeseries)[[1]],
      countries_year
    )
    mismatch <- abs(
      hyde_gaez_max_rf_ha_on_ir_country_timeseries[clist2, as.character(year)] -
        rf_suit_ir_year_country[clist2]
    )
    if (any(mismatch > 1e-3)) {
      warning(
        "Inconsistencies between rainfed multicropping suitability ",
        "on irrigated cropland used here and rainfed multicropping ",
        "suitability on irrigated cropland used in previous script",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    rm(clist2, mismatch)
  }

  # Remove countries which are missing in HYDE land mask
  countries_year <- setdiff(countries_year, names(empty_hyde))
  # Check if number of countries has changed compared to previous year, if so
  # inform user
  if (length(countries_year) != ncountry_previous) {
    cat("++", length(countries_year), "countries exist ++\n")
  }
  if (length(which(clist$Start.Year == year)) > 0) {
    cat(
      toString(sQuote(unique(clist$Country[which(clist$Start.Year == year)]))),
      "start(s) to exist this year\n"
    )
  }
  if (length(which(clist$End.Year == year)) > 0) {
    cat(
      toString(sQuote(unique(clist$Country[which(clist$End.Year == year)]))),
      "cease(s) to exist at the end of this year\n"
    )
  }
  ncountry_previous <- length(countries_year)

  # Array for harvested area sum over all crops in each cell, used to check
  # cropping factor at the end
  for (var in c("irrigated_area_sum", "rainfed_area_sum", "total_area_sum")) {
    vals <- double(length(hyde_cropland))
    # Set cells outside of GADM countries to NA
    vals[which(is.na(gadm_raster[]))] <- NA
    assign(var, vals)
  }

  # Check for missing data in total harvested areas
  if (!any(tot_ha_year[countries_year, ] > 0, na.rm = TRUE) &&
    any(cropland_year_country[countries_year] > 0, na.rm = TRUE)
  ) {
    # Need to fill tot_ha_year
    warning(
      "No total harvested areas anywhere in year ", year,
      ". Normally, country-level data should be prepared for the whole",
      " time series by the previous script.",
      call. = FALSE,
      immediate. = TRUE
    )
    sink(file = gapfilling_log, append = TRUE, split = TRUE)
    cat("Extend FAOSTAT total time series to", year, "\n")
    sink()
    for (country in countries_year) {
      if (cropland_year_country[country] == 0) {
        # No need to fill countries without cropland
        next
      }
      # Cropland time series
      years <- as.integer(dimnames(get(total_version_to_use))[[3]])
      if (year < min(years)) {
        # Append year to beginning of time series
        tmp_cropseq <- c(
          cropland_year_country[country],
          hyde_cropland_country_timeseries[country, ]
        )
        names(tmp_cropseq)[1] <- as.character(year)
      } else if (year > max(years)) {
        # Append year to end of time series
        tmp_cropseq <- c(
          hyde_cropland_country_timeseries[country, ],
          cropland_year_country[country]
        )
        names(tmp_cropseq)[length(tmp_cropseq)] <- as.character(year)
      } else {
        tmp_cropseq <- hyde_cropland_country_timeseries[country, ]
      }
      for (crop in ts_crops) {
        if (any(get(total_version_to_use)[country, crop, ] > 0, na.rm = TRUE)) {
          sink(file = gapfilling_log, append = TRUE, split = FALSE)
          country_ha <- fill_timeseries(
            get(total_version_to_use)[country, crop, ],
            tmp_cropseq,
            paste(crop, "in", country),
            quiet = TRUE
          )
          sink()
          tot_ha_year[country, crop] <- country_ha[as.character(year)]
        } else {
          tot_ha_year[country, crop] <- NA
        }
      }
    }
  }
  # Check that maximum possible harvested area is not exceeded
  tot_ha_sum_year <- rowSums(
    tot_ha_year[countries_year, ts_crops],
    na.rm = TRUE
  )
  if (any(tot_ha_sum_year - total_suit_year_country[countries_year] > 1e-4)) {
    scalar <- tot_ha_sum_year / total_suit_year_country[countries_year]
    scalar <- pmax(scalar, 1)
    tmpdata <- tot_ha_year[countries_year, ts_crops] / scalar
    cat(
      round(
        100 - sum(tmpdata[countries_year, ts_crops], na.rm = TRUE) /
          sum(tot_ha_year[countries_year, ts_crops], na.rm = TRUE) * 100,
        2
      ),
      "% of global harvested areas have been removed this year because they",
      "exceeded maximum possible harvested areas\n"
    )
    tot_ha_year[countries_year, ts_crops] <- tmpdata
    rm(tmpdata)
  }
  # Update sum
  tot_ha_sum_year <- rowSums(
    tot_ha_year[countries_year, ts_crops],
    na.rm = TRUE
  )

  # Check for missing data in irrigated harvested areas
  if (!any(ir_ha_year[countries_year, ] > 0, na.rm = TRUE) &&
    any(irrigated_year_country[countries_year] > 0, na.rm = TRUE)
  ) {
    # Need to fill ir_ha_year
    warning(
      "No irrigated harvested areas anywhere in year ", year,
      ". Normally, country-level data should be prepared for the whole",
      "time series by the previous script.",
      call. = FALSE,
      immediate. = TRUE
    )
    sink(file = gapfilling_log, append = TRUE, split = TRUE)
    cat("Extend FAOSTAT irrigated time series to", year, "\n")
    sink()
    for (country in countries_year) {
      if (irrigated_year_country[country] == 0) {
        next
      }
      years <- as.integer(dimnames(get(irrigated_version_to_use))[[3]])
      if (year < min(years)) {
        # Append year to beginning of time series
        tmp_cropseq <- c(
          irrigated_year_country[country],
          hyde_irrigated_country_timeseries[country, ]
        )
        names(tmp_cropseq)[1] <- as.character(year)
      } else if (year > max(years)) {
        # Append year to end of time series
        tmp_cropseq <- c(
          hyde_irrigated_country_timeseries[country, ],
          irrigated_year_country[country]
        )
        names(tmp_cropseq)[length(tmp_cropseq)] <- as.character(year)
      } else {
        tmp_cropseq <- hyde_irrigated_country_timeseries[country, ]
      }
      for (crop in ts_crops) {
        if (any(
          get(irrigated_version_to_use)[country, crop, ] > 0,
          na.rm = TRUE
        )) {
          sink(file = gapfilling_log, append = TRUE, split = FALSE)
          country_ha <- fill_timeseries(
            get(irrigated_version_to_use)[country, crop, ],
            tmp_cropseq,
            paste(crop, "in", country),
            quiet = TRUE
          )
          sink()
          ir_ha_year[country, crop] <- country_ha[as.character(year)]
        } else {
          ir_ha_year[country, crop] <- NA
        }
      }
    }
  }

  # Check that irrigated harvested areas are not bigger than total harvested
  # areas
  ir_ha_year <- pmin(tot_ha_year, ir_ha_year)
  # Check that maximum possible harvested area is not exceeded
  ir_ha_sum_year <- rowSums(ir_ha_year[countries_year, ts_crops], na.rm = TRUE)
  if (any(ir_ha_sum_year - ir_suit_year_country[countries_year] > 1e-4)) {
    scalar <- ir_ha_sum_year / ir_suit_year_country[countries_year]
    scalar <- pmax(scalar, 1)
    tmpdata <- ir_ha_year[countries_year, ts_crops] / scalar
    cat(
      round(
        100 - sum(tmpdata[countries_year, ts_crops], na.rm = TRUE) /
          sum(ir_ha_year[countries_year, ts_crops], na.rm = TRUE) * 100,
        2
      ),
      "% of global irrigated harvested areas have been removed this year",
      "because they exceeded maximum possible irrigated harvested areas\n"
    )
    ir_ha_year[countries_year, ts_crops] <- tmpdata
    rm(tmpdata, scalar)
  }
  # Update sum
  ir_ha_sum_year <- rowSums(ir_ha_year[countries_year, ts_crops], na.rm = TRUE)

  # Now derive rainfed harvested areas from total - irrigated
  rf_ha_year <- tot_ha_year - pmax(ir_ha_year, 0, na.rm = TRUE)
  rf_ha_year <- pmax(rf_ha_year, 0)
  # Sum over all crops
  rf_ha_sum_year <- rowSums(rf_ha_year[countries_year, ts_crops], na.rm = TRUE)

  # Check that rainfed areas are not too big for rainfed cropland and remaining
  # irrigated cropland
  # Determine maximum available rainfed harvested areas on "free" irrigated
  # cropland, which is defined as the minimum of two values:
  # - irrigated cropland times rainfed multicropping suitability
  # - maximum available irrigated harvested areas - irrigated harvested areas
  rainfed_on_irrig_ha_max <- pmin(
    ir_suit_year_country[countries_year] - ir_ha_sum_year,
    rf_suit_ir_year_country[countries_year],
    na.rm = TRUE
  )
  mismatch <- which(rf_ha_sum_year - rf_suit_rf_year_country[countries_year] -
    rainfed_on_irrig_ha_max > 1e-4)
  if (length(mismatch) > 0) {
    for (country in names(mismatch)) {
      total_ha_on_irrig <- tot_ha_sum_year[country] -
        rf_suit_rf_year_country[country]
      # Rainfed harvested area required on irrigated cropland to fit both
      # rainfed and irrigated harvested areas of the country
      rainfed_ha_irrig <- rf_suit_ir_year_country[country] *
        (ir_suit_year_country[country] - total_ha_on_irrig) /
        (ir_suit_year_country[country] - rf_suit_ir_year_country[country])
      # Amount of ha to shift from rainfed to irrigated
      shift_ha <- rf_ha_sum_year[country] - rf_suit_rf_year_country[country] -
        rainfed_ha_irrig
      sink(file = gapfilling_log, append = TRUE)
      cat(
        "Need to shift", round(shift_ha / rf_ha_sum_year[country] * 100, 2),
        "% of rainfed harvested areas to irrigated in", sQuote(country), "\n"
      )
      sink()
      # Rainfed harvested areas of those crops that also have irrigated
      # harvested areas (excludes purely rainfed crops)
      irrig_exp_pot <- sum(
        tot_ha_year[country, ts_crops] - ifelse(
          ir_ha_year[country, ts_crops] > 0,
          ir_ha_year[country, ts_crops],
          NA
        ), na.rm = TRUE
      )
      # Missing expansion potential
      missing_exp_pot <- max(shift_ha - irrig_exp_pot, 0)
      if (irrig_exp_pot > 0) {
        # Expand existing irrigated crops
        croplist <- names(
          which(rf_ha_year[country, ts_crops] < tot_ha_year[country, ts_crops])
        )
        ir_ha_year[country, croplist] <- ir_ha_year[country, croplist] +
        min(shift_ha / irrig_exp_pot, 1) * rf_ha_year[country, croplist]
      }
      rm(irrig_exp_pot)
      # Take some purely rainfed crop areas
      croplist <- names(
        which(rf_ha_year[country, ts_crops] == tot_ha_year[country, ts_crops])
      )
      ir_ha_year[country, croplist] <- rf_ha_year[country, croplist] *
        missing_exp_pot / sum(rf_ha_year[country, croplist], na.rm = TRUE)
      # Update rainfed harvested areas
      rf_ha_year <- tot_ha_year - pmax(ir_ha_year, 0, na.rm = TRUE)
      rf_ha_year <- pmax(rf_ha_year, 0)
      # Update amount of ha to shift from rainfed to irrigated
      shift_ha <- sum(rf_ha_year[country, ts_crops], na.rm = TRUE) -
        rf_suit_rf_year_country[country] - rainfed_ha_irrig
      if (shift_ha > 1e-8) {
        stop("Redistribution error in ", sQuote(country), " in ", year)
      }
    }
  }
  rm(mismatch, ir_ha_sum_year, rf_ha_sum_year, rainfed_on_irrig_ha_max)

  # 4) first pass of spatial disaggregation
  # Process each crop individually
  for (crop in ts_crops) {
    # Variables for gridded crop-specific rainfed and irrigated harvested area
    # this year
    rf_ha_crop_year <- ir_ha_crop_year <- array(NA, dim = length(hyde_cropland))
    # Load Monfreda-based pattern
    harvested_fraction <- c(
      ncvar_get(
        harvested_fraction_file,
        "harvested_fraction",
        start = c(1, 1, which(harvested_fraction_crops == crop)),
        count = c(-1, -1, 1)
      )
    )
    # Check if pattern has values
    if (all(is.na(harvested_fraction)) &&
      !crop %in% empty_pattern_msg[["all"]]
    ) {
      # FAO crop with no Monfreda pattern
      message(
        "There is no harvested area pattern for ", sQuote(crop),
        " globally. Distributing based on cropland distribution in countries ",
        "where FAOSTAT has the crop."
      )
      # Do not display message again
      empty_pattern_msg[["all"]] <- c(empty_pattern_msg[["all"]], crop)
    }
    # Processing is conduct country by country
    for (country in countries_year) {
      # Cells belonging to country
      ccells <- fao_gadm_country_cells[[country]]
      process_rainfed <- process_irrigated <- TRUE
      if (cropland_year_country[country] == 0) {
        # No cropland, no need to process harvested areas
        # Set grid cells to zero
        rf_ha_crop_year[ccells] <- 0
        ir_ha_crop_year[ccells] <- 0
        # Skip to next country
        next
      }
      if (is.na(tot_ha_year[country, crop]) ||
        tot_ha_year[country, crop] == 0
      ) {
        # No harvested area data, no need to process
        # Set grid cells to zero.
        rf_ha_crop_year[ccells] <- 0
        ir_ha_crop_year[ccells] <- 0
        next
      }
      if (is.na(rf_ha_year[country, crop]) || rf_ha_year[country, crop] == 0) {
        # No rainfed harvested areas
        # Set grid cells to zero.
        rf_ha_crop_year[ccells] <- 0
        # No need to do disaggregation step for rainfed crop
        process_rainfed <- FALSE
      }
      if (is.na(ir_ha_year[country, crop]) || ir_ha_year[country, crop] == 0) {
        # No irrigated harvested areas
        # Set grid cells to zero.
        ir_ha_crop_year[ccells] <- 0
        # No need to do disaggregation step for irrigated crop
        process_irrigated <- FALSE
      }
      if (all(is.na(harvested_fraction[ccells]))) {
        # No pattern for crop in this country, need to create one.
        if (!crop %in% empty_pattern_msg[[country]] &&
          !crop %in% empty_pattern_msg[["all"]]
        ) {
          message(
            "There is no harvested area pattern for ", sQuote(crop),
            " in ", sQuote(country),
            ". Distributing across all cells with cropland"
          )
          # Do not display message for this crop and this country again.
          empty_pattern_msg[[country]] <- c(empty_pattern_msg[[country]], crop)
        }
        if (rainfed_year_country[country] > 0 && process_rainfed) {
          # Process rainfed harvested areas using rainfed cropland.
          rf_ha_crop_year[ccells] <- hyde_rainfed[ccells] /
            rainfed_year_country[country] * rf_ha_year[country, crop]
        } else if (process_rainfed) {
          # Process rainfed harvested areas using total cropland if no rainfed
          # cropland is available. Rainfed harvested areas can be allocated to
          # irrigated cropland.
          rf_ha_crop_year[ccells] <- hyde_cropland[ccells] /
            cropland_year_country[country] * rf_ha_year[country, crop]
        }
        if (process_irrigated) {
          # Process irrigated harvested areas using irrigated cropland
          ir_ha_crop_year[ccells] <- hyde_irrigated[ccells] /
            irrigated_year_country[country] * ir_ha_year[country, crop]
        }
      } else {
        # Use Monfreda pattern and scale to this year's country sum
        if (rainfed_year_country[country] > 0 && process_rainfed) {
          # Process rainfed harvested areas using rainfed cropland.
          rf_ha_crop_year[ccells] <- harvested_fraction[ccells] *
            hyde_rainfed[ccells]
          if (sum(rf_ha_crop_year[ccells], na.rm = TRUE) == 0) {
            # Despite there being a Monfreda pattern for the country, harvested
            # areas could not be assigned. Assign to all cropland cells as if
            # there was no pattern. This could happen if past cropland areas are
            # outside present-day harvested areas.
            rf_ha_crop_year[ccells] <- hyde_rainfed[ccells] /
              rainfed_year_country[country] * rf_ha_year[country, crop]
          }
          # Base pattern * cropland may not match FAOSTAT country sum, scale to
          # match
          rf_ha_crop_year[ccells] <- rf_ha_crop_year[ccells] *
            rf_ha_year[country, crop] /
            sum(rf_ha_crop_year[ccells], na.rm = TRUE)
        } else if (process_rainfed) {
          # Process rainfed harvested areas using total cropland if no rainfed
          # cropland is available. Rainfed harvested areas can be allocated to
          # irrigated cropland.
          rf_ha_crop_year[ccells] <- harvested_fraction[ccells] *
            hyde_cropland[ccells]
          if (sum(rf_ha_crop_year[ccells], na.rm = TRUE) == 0) {
            # Despite there being a Monfreda pattern for the country, harvested
            # areas could not be assigned. Assign to all cropland cells as if
            # there was no pattern. This could happen if past cropland areas are
            # outside present-day harvested areas.
            rf_ha_crop_year[ccells] <- hyde_cropland[ccells] /
              cropland_year_country[country] * rf_ha_year[country, crop]
          }
          # Base pattern * cropland may not match FAOSTAT country sum, scale to
          # match
          rf_ha_crop_year[ccells] <- rf_ha_crop_year[ccells] *
            rf_ha_year[country, crop] /
            sum(rf_ha_crop_year[ccells], na.rm = TRUE)
        }
        if (process_irrigated) {
          ir_ha_crop_year[ccells] <- harvested_fraction[ccells] *
            hyde_irrigated[ccells]
          if (sum(ir_ha_crop_year[ccells], na.rm = TRUE) == 0) {
            # Despite there being a Monfreda pattern for the country, harvested
            # areas could not be assigned. Assign to all cropland cells as if
            # there was no pattern. This could happen if past cropland areas are
            # outside present-day harvested areas.
            ir_ha_crop_year[ccells] <- hyde_irrigated[ccells] /
              irrigated_year_country[country] * ir_ha_year[country, crop]
          }
          # Base pattern * cropland may not match FAOSTAT country sum, scale to
          # match
          ir_ha_crop_year[ccells] <- ir_ha_crop_year[ccells] *
            ir_ha_year[country, crop] /
            sum(ir_ha_crop_year[ccells], na.rm = TRUE)
        }
      }
      # Set missing values within countries to 0
      rf_ha_crop_year[ccells] <- pmax(rf_ha_crop_year[ccells], 0, na.rm = TRUE)
      ir_ha_crop_year[ccells] <- pmax(ir_ha_crop_year[ccells], 0, na.rm = TRUE)

      # Check if country sum matches areas actually allocated to cells.
      # There should be no errors here.
      if (process_rainfed) {
        sum1 <- sum(rf_ha_crop_year[ccells])
        if (abs(sum1 - rf_ha_year[country, crop]) > 1e-8) {
          stop(
            "Allocation error in ", sQuote(country), ", year ", year,
            ", crop 'rainfed ", crop, "': ",
            round(sum1, 2), "ha instead of ",
            round(rf_ha_year[country, crop], 2), "ha"
          )
        }
        rm(sum1)
      }
      if (process_irrigated) {
        sum1 <- sum(ir_ha_crop_year[ccells])
        if (abs(sum1 - ir_ha_year[country, crop]) > 1e-8) {
          stop(
            "Allocation error in ", sQuote(country), ", year ", year,
            ", crop 'irrigated ", crop, "': ",
            round(sum1, 2), "ha instead of ",
            round(ir_ha_year[country, crop], 2), "ha"
          )
        }
        rm(sum1)
      }
      rm(ccells)
    }
    # Sums over all crops
    rainfed_area_sum <- rainfed_area_sum +
      pmax(rf_ha_crop_year, 0, na.rm = TRUE)
    irrigated_area_sum <- irrigated_area_sum +
      pmax(ir_ha_crop_year, 0, na.rm = TRUE)

    # 5) Save preliminary pattern of this crop and this year to NetCDF
    ncvar_put(
      ha_timeseries_file,
      rainfed_output_name,
      rf_ha_crop_year,
      start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
      count = c(-1, -1, 1, 1)
    )
    ncvar_put(
      ha_timeseries_file,
      irrigated_output_name,
      ir_ha_crop_year,
      start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
      count = c(-1, -1, 1, 1)
    )
    rm(ir_ha_crop_year, rf_ha_crop_year)
  } # End crop loop for first disaggregation pass

  nc_sync(ha_timeseries_file)
  total_area_sum <- rainfed_area_sum + irrigated_area_sum

  # 6) Check if spatial redistribution is necessary because sums over all crops
  # exceed multicropping suitability
  cf <- total_area_sum / ifelse(hyde_cropland > 0, hyde_cropland, NA)
  cat(
    "Range of cropping factor this year:",
    paste(round(range(cf, na.rm = TRUE), 2), collapse = " - "), "\n"
  )
  rm(cf)

  # Maximum possible irrigated harvested areas based on GAEZ multicropping
  # suitability
  irrigated_suit <- hyde_irrigated * values(gaez_multicropping_suit_ir)
  # There are some small inconsistencies between HYDE total cropland, HYDE
  # irrigated cropland and HYDE rainfed cropland. Total harvested areas at
  # country scale were derived using HYDE total cropland, irrigated harvested
  # areas were derived using HYDE irrigated cropland; rainfed harvested areas
  # defined as the rest, that's why use hyde_cropland - hyde_irrigated here
  # instead of hyde_rainfed
  rainfed_suit_rainfed <- (hyde_cropland - hyde_irrigated) *
    values(gaez_multicropping_suit_rf)

  # Determine countries that need redistribution
  redist_units <- character(0)
  mismatch_ir <- which(irrigated_area_sum - irrigated_suit > 1e-4)
  if (length(mismatch_ir) > 0) {
    # Irrigated multicropping suitability exceeded in some cells, identify
    # unique countries
    index <- match(gadm_raster[mismatch_ir], gadm_country_names$level0_ID)
    redist_units <- union(redist_units, gadm_country_names$level0_code[index])
  }
  mismatch_rf <- which(rainfed_area_sum - rainfed_suit_rainfed > 1e-4)
  if (length(mismatch_rf) > 0) {
    # Rainfed harvested areas exceed rainfed multicropping suitability on
    # rainfed cropland; they may still fit using free irrigated cropland, but
    # need to check. Identify unique countries
    index <- match(gadm_raster[mismatch_rf], gadm_country_names$level0_ID)
    redist_units <- union(redist_units, gadm_country_names$level0_code[index])
  }
  rm(mismatch_ir, mismatch_rf, irrigated_suit, rainfed_suit_rainfed)

  if (length(redist_units) > 0) {
    # Some countries need redistribution
    redist_stats[[as.character(year)]] <- list()
    # Detected GADM codes may be for smaller successor countries, identify
    # matching countries existing this year
    redist_countries <- names(
      which(
        sapply(
          fao_gadm_country_mapping,
          function(indata) any(redist_units %in% indata)
        )
      )
    )
    redist_countries <- intersect(countries_year, redist_countries)

    if (length(redist_countries) == 1) {
      croplist <- names(which(tot_ha_year[redist_countries, ts_crops] > 0))
      cat(
        "Need to redistribute harvested areas in 1 country.",
        "Reloading spatial patterns for",
        length(croplist), "crops\n"
      )
    } else {
      croplist <- names(
        which(
          colSums(tot_ha_year[redist_countries, ts_crops], na.rm = TRUE) > 0
        )
      )
      cat(
        "Need to redistribute harvested areas in", length(redist_countries),
        "countries. Reloading spatial patterns for",
        length(croplist), "crops\n"
      )
    }
    # Delete sums over all crops since these will be recalculated after
    # redistribution.
    rm(irrigated_area_sum, rainfed_area_sum, total_area_sum)
    # Reload crop-specific harvested areas for countries that need
    # redistribution. Redistribution is done for all crops at once and requires
    # all crops to be loaded at once. To save space in memory, create array per
    # country which only includes those crops grown in the respective country.
    # Only save cells per country that actually have cropland to further reduce
    # memory.
    crop_cells <- which(hyde_cropland > 0)
    # Country-specific arrays:
    # - redist_country_ir_ha
    # - redist_country_pattern
    # - redist_country_rf_ha
    for (crop in croplist) {
      # Reload harvested area patterns generated in first disaggregation
      tmp_rf_ha <- ncvar_get(
        ha_timeseries_file,
        rainfed_output_name,
        start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
        count = c(-1, -1, 1, 1)
      )
      tmp_ir_ha <- ncvar_get(
        ha_timeseries_file,
        irrigated_output_name,
        start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
        count = c(-1, -1, 1, 1)
      )
      # Reload harvested_fraction for crop
      tmppattern <- c(
        ncvar_get(
          harvested_fraction_file,
          "harvested_fraction",
          start = c(1, 1, which(harvested_fraction_crops == crop)),
          count = c(-1, -1, 1)
        )
      )
      if (any(tmp_rf_ha[-crop_cells] > 0, na.rm = TRUE) ||
        any(tmp_ir_ha[-crop_cells] > 0, na.rm = TRUE)) {
        stop("Harvested areas outside cropland for crop ", sQuote(crop))
      }
      for (country in redist_countries) {
        # Cells belonging to country
        ccells <- fao_gadm_country_cells[[country]]
        # Reduce to cells that actually have cropland
        ccells_crop <- intersect(ccells, crop_cells)
        cindex <- which(redist_countries == country)
        # Rainfed crop
        array_name <- paste0("redist_country_rf_ha", cindex)
        if (!exists(array_name)) {
          # Country-specific array of crop-specific harvested areas, only for
          # crops present in this country. Create for first crops, then reuse
          # for consecutive crops.
          country_rf_ha <- array(
            dim = c(
              length(ccells_crop),
              length(which(rf_ha_year[country, ts_crops] > 0))
            ),
            dimnames = list(
              NULL,
              names(which(rf_ha_year[country, ts_crops] > 0))
            )
          )
          assign(array_name, country_rf_ha)
        } else {
          country_rf_ha <- get(array_name)
        }
        if (crop %in% dimnames(country_rf_ha)[[2]]) {
          country_rf_ha[, crop] <- tmp_rf_ha[ccells_crop]
          assign(array_name, country_rf_ha)
        }
        rm(country_rf_ha, array_name)

        # Irrigated crop
        array_name <- paste0("redist_country_ir_ha", cindex)
        if (!exists(array_name)) {
          # Country-specific array of crop-specific harvested areas, only for
          # crops present in this country. Create for first crops, then reuse
          # for consecutive crops.
          country_ir_ha <- array(
            dim = c(
              length(ccells_crop),
              length(which(ir_ha_year[country, ts_crops] > 0))
            ),
            dimnames = list(
              NULL,
              names(which(ir_ha_year[country, ts_crops] > 0))
            )
          )
          assign(array_name, country_ir_ha)
        } else {
          country_ir_ha <- get(array_name)
        }
        if (crop %in% dimnames(country_ir_ha)[[2]]) {
          country_ir_ha[, crop] <- tmp_ir_ha[ccells_crop]
          assign(array_name, country_ir_ha)
        }
        rm(country_ir_ha, array_name)

        # Monfreda pattern
        array_name <- paste0("redist_country_pattern", cindex)
        if (!exists(array_name)) {
          # Country-specific array of crop-specific harvested areas, only for
          # crops present in this country. Create for first crops, then reuse
          # for consecutive crops.
          country_pattern <- array(
            dim = c(
              length(ccells_crop),
              length(which(tot_ha_year[country, ts_crops] > 0))
            ),
            dimnames = list(
              NULL,
              names(which(tot_ha_year[country, ts_crops] > 0))
            )
          )
          assign(array_name, country_pattern)
        } else {
          country_pattern <- get(array_name)
        }
        if (crop %in% dimnames(country_pattern)[[2]]) {
          country_pattern[, crop] <- tmppattern[ccells_crop]
          assign(array_name, country_pattern)
        }
        rm(country_pattern, ccells, ccells_crop, cindex, array_name)
      }
      # Delete global data fields
      rm(tmp_rf_ha, tmp_ir_ha, tmppattern)
      gc(full = (which(croplist == crop) %% 5 == 0))
    } # End reload data
    gc()

    # Set NAs in ir_ha_year and rf_ha_year to 0 to avoid problems
    ir_ha_year <- pmax(ir_ha_year, 0, na.rm = TRUE)
    rf_ha_year <- pmax(rf_ha_year, 0, na.rm = TRUE)
    # Redistribution is per country, processing all crops at once
    for (country in redist_countries) {
      # Get country-specific variables
      cindex <- which(redist_countries == country)
      ccells <- fao_gadm_country_cells[[country]]
      ccells_crop <- intersect(ccells, crop_cells)
      # Rainfed harvested areas
      country_rf_ha <- get(paste0("redist_country_rf_ha", cindex))
      # Store dimensions
      country_rf_dim <- dim(country_rf_ha)
      # Set all NAs to zero
      country_rf_ha <- pmax(country_rf_ha, 0, na.rm = TRUE)
      # Irrigated harvested areas
      country_ir_ha <- get(paste0("redist_country_ir_ha", cindex))
      # Store dimensions
      country_ir_dim <- dim(country_ir_ha)
      # Set all NAs to zero
      country_ir_ha <- pmax(country_ir_ha, 0, na.rm = TRUE)
      # Irrigated suitability
      country_ir_suit <- hyde_irrigated[ccells_crop] *
        gaez_multicropping_suit_ir[ccells_crop]
      # Set all NAs to zero
      country_ir_suit <- pmax(country_ir_suit, 0, na.rm = TRUE)
      # Rainfed suitability on rainfed cropland
      country_rf_suit_rf <-
        (hyde_cropland[ccells_crop] - hyde_irrigated[ccells_crop]) *
        gaez_multicropping_suit_rf[ccells_crop]
      # Set all NAs to zero
      country_rf_suit_rf <- pmax(country_rf_suit_rf, 0, na.rm = TRUE)
      # Rainfed suitability on irrigated cropland
      country_rf_suit_ir <- hyde_irrigated[ccells_crop] *
        gaez_multicropping_suit_rf[ccells_crop]
      # Set all NAs to zero
      country_rf_suit_ir <- pmax(country_rf_suit_ir, 0, na.rm = TRUE)
      # Crop base pattern
      country_pattern <- get(paste0("redist_country_pattern", cindex))
      # Check that saving to and loading from NetCDF has not overshot country
      # sums
      mismatch_rf <- which(countrysum(country_rf_ha) -
        rf_ha_year[country, dimnames(country_rf_ha)[[2]]] > 1e-8
      )
      for (crop in names(mismatch_rf)) {
        country_rf_ha[, crop] <- country_rf_ha[, crop] *
          rf_ha_year[country, crop] / sum(country_rf_ha[, crop])
      }
      mismatch_ir <- which(countrysum(country_ir_ha) -
        ir_ha_year[country, dimnames(country_ir_ha)[[2]]] > 1e-8
      )
      for (crop in names(mismatch_ir)) {
        country_ir_ha[, crop] <- country_ir_ha[, crop] *
          ir_ha_year[country, crop] / sum(country_ir_ha[, crop])
      }
      # Apply limitations to suitability
      if (abs(
        sum(country_rf_ha) - sum(country_rf_suit_rf) - sum(country_rf_suit_ir)
      ) < 1e-4) {
        # Rainfed harvested areas require full rainfed cropland and full
        # irrigated cropland with rainfed multicropping suitability
        # This means irrigated crops are limited to
        # (gaez_multicropping_suit_ir - gaez_multicropping_suit_rf)
        country_ir_suit <- country_ir_suit - country_rf_suit_ir
        country_ir_suit <- pmax(country_ir_suit, 0)
      }
      if (abs(sum(country_ir_ha) - sum(country_ir_suit)) < 1e-4) {
        # Irrigated harvested areas require full irrigated cropland, no room for
        # rainfed harvested areas on irrigated cropland
        country_rf_suit_ir[] <- 0
      }
      # First reduce irrigated harvested areas in cells where suitability is
      # definitely exceeded.
      # Scale down all crops linearly in cells where their sum exceeds available
      # space.
      tmp_suit <- pmax(cellsum(country_ir_ha) / country_ir_suit, 1, na.rm = TRUE)
      country_ir_ha <- country_ir_ha / tmp_suit
      # Filter division by zero.
      index <- which(!is.finite(country_ir_ha))
      country_ir_ha[index] <- 0
      rm(index, tmp_suit)

      # Compute available rainfed space in each cell based on multicropping
      # suitability and currently assigned irrigated areas.
      # If irrigated harvested areas do not require full irrigated cropland
      # allow rainfed crops to occupy part of irrigated cropland.
      # Irrigated pattern may change later.
      # Rainfed harvested areas can always occupy full rainfed cropland.
      tmp_suit <- pmin(
        country_ir_suit - cellsum(country_ir_ha),
        country_rf_suit_ir,
        na.rm = TRUE
      )
      tmp_rf_suit <- country_rf_suit_rf + tmp_suit
      rm(tmp_suit)
      tmp_rf_suit <- pmax(tmp_rf_suit, 0)
      # Also create copy for irrigated suitability
      # Irrigated suitability may need to be reduced below GAEZ multicropping
      # suitability to allow sufficient tmp_rf_suit
      tmp_ir_suit <- country_ir_suit

      # Reduce rainfed harvested areas to fit into this available space.
      tmp_suit <- pmax(cellsum(country_rf_ha) / tmp_rf_suit, 1, na.rm = TRUE)
      country_rf_ha <- country_rf_ha / tmp_suit
      # Filter division by zero.
      index <- which(!is.finite(country_rf_ha))
      country_rf_ha[index] <- 0
      rm(index, tmp_suit)

      # Keep two copies of rainfed, irrigated harvested areas
      tmp_ir_ha <- country_ir_ha
      country_ir_ha_sums <- countrysum(country_ir_ha)
      tmp_ir_ha_sums <- country_ir_ha_sums
      tmp_rf_ha <- country_rf_ha
      country_rf_ha_sums <- countrysum(country_rf_ha)
      tmp_rf_ha_sums <- country_rf_ha_sums
      # Update missing areas per cell
      diff_rf <- cellsum(country_rf_ha) -
        cellsum(get(paste0("redist_country_rf_ha", cindex)))
      diff_ir <- cellsum(country_ir_ha) -
        cellsum(get(paste0("redist_country_ir_ha", cindex)))
      mismatch_any <- which(diff_rf < -1e-6 | diff_ir < -1e-6)
      if (length(mismatch_any) > 0) {
        index <- which(country_ir_suit + tmp_rf_suit > 0)
        message(
          "Need to redistribute spatial patterns in at least ",
          length(mismatch_any),
          " cells (",
          round(length(mismatch_any) / length(index) * 100, 1),
          "% of cropland cells) of",
          sQuote(country)
        )
        rm(index)
      }
      rm(mismatch_any, diff_ir, diff_rf)

      # Iteration counters
      i <- 1
      rainfed_i <- rep(1, dim(tmp_rf_ha)[2])
      irrigated_i <- rep(1, dim(tmp_ir_ha)[2])
      names(rainfed_i) <- dimnames(tmp_rf_ha)[[2]]
      names(irrigated_i) <- dimnames(tmp_ir_ha)[[2]]
      # Iterative process that expands harvested areas in cells where there is
      # still space until country sum is met.
      # Country sum must be met within 1e-4 ha for all crops, rainfed and
      # irrigated separately.
      # Update missing areas per crop
      diff_ir <- tmp_ir_ha_sums -
        ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
      index <- which(is.na(diff_ir))
      diff_ir[index] <- 0
      rm(index)
      diff_rf <- tmp_rf_ha_sums -
        rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
      index <- which(is.na(diff_rf))
      diff_rf[index] <- 0
      rm(index)
      while (
        # Either missing rainfed or irrigated harvested areas
        (any(diff_ir < (-1e-4)) || any(diff_rf < (-1e-4))) &&
        # Not yet reached maximum number of iterations
        i <= redist_max_it
      ) {
        # First start with irrigated harvested areas
        # - irrigated cropland usually smaller than rainfed cropland
        # - rainfed harvested areas can occupy some irrigated cropland if not
        #   needed for irrigated harvested areas, so determine irrigated first

        # If irrigated harvested areas have not been redistributed successfully
        # after redist_exp_thresh iterations allow expansion outside of the base
        # spatial patterns (harvested_fraction).
        # After trying to redistribute within existing growing patterns, allow
        # crops that do not grow everywhere in a country to expand to regions
        # where they do not grow, yet.
        # Country-level harvested areas are extrapolated across time without
        # knowledge of spatial patterns of cropland. There may be no cropland
        # (or not enough) in areas where crops are grown today.
        # Note: This may put crops into regions that are climatically
        # unsuitable.
        if (i == redist_exp_thresh + 1 && any(ir_ha_year[country, ] > 0)) {
          message(
            "Allowing irrigated crop expansion outside of base pattern ",
            "to accomodate redistributed harvested areas"
          )
          for (crop in dimnames(tmp_ir_ha)[[2]]) {
            if (sum(tmp_ir_ha[, crop]) - ir_ha_year[country, crop] < -1e-4 &&
              sum(tmp_ir_ha[, crop]) > 0
            ) {
              # Determine lowest non-zero cropping intensity and write 1/10
              # of that into cells that have irrigated cropland but no
              # harvested areas of this crop,
              fillcells <- which(tmp_ir_ha[, crop] == 0 & tmp_ir_suit > 0)
              present <- which(tmp_ir_ha[, crop] > 0)
              fillshare <- min(
                (tmp_ir_ha[, crop] / hyde_irrigated[ccells_crop])[present]
              ) / 10
              tmp_ir_ha[fillcells, crop] <-
                hyde_irrigated[ccells_crop[fillcells]] * fillshare
              # Make sure expansion has not overshot country sum.
              if (sum(tmp_ir_ha[, crop]) > ir_ha_year[country, crop]) {
                tmp_ir_ha[, crop] <- tmp_ir_ha[, crop] *
                  ir_ha_year[country, crop] / sum(tmp_ir_ha[, crop])
              }
              rm(fillcells, fillshare, present)
              if (sum(tmp_ir_ha[, crop]) > sum(country_ir_ha[, crop])) {
                # Only show message if pattern actually expanded
                message("Expand pattern for ", sQuote(crop))
              }
            } else if (
              sum(tmp_ir_ha[, crop]) - ir_ha_year[country, crop] < -1e-4
            ) {
              # If no non-zero cells, spread evenly across all cells with
              # irrigated cropland
              tmp_ir_ha[, crop] <- hyde_irrigated[ccells_crop] /
                irrigated_year_country[country] * ir_ha_year[country, crop]
              tmp_ir_ha[, crop] <- pmax(tmp_ir_ha[, crop], 0, na.rm = TRUE)
              if (sum(tmp_ir_ha[, crop]) > sum(country_ir_ha[, crop])) {
                # Only show message if pattern actually expanded
                message("New pattern for ", sQuote(crop))
              }
            }
          }
          # Check if expansion has exceeded available space, if so, scale back.
          tmp_suit <- pmax(cellsum(tmp_ir_ha) / tmp_ir_suit, 1, na.rm = TRUE)
          tmp_ir_ha <- tmp_ir_ha / tmp_suit
          # Fix division by zero
          index <- which(tmp_ir_suit == 0)
          tmp_ir_ha[index, ] <- 0
          rm(index, tmp_suit)
          # Update "backup" copy
          tmp_ir_ha_sums <- country_ir_ha_sums <- countrysum(tmp_ir_ha)
          country_ir_ha <- tmp_ir_ha
          gc(reset = TRUE)
        }

        # Determine which crops need expansion of harvested areas.
        # Only increase areas of crops that are below their prescribed country
        # sum.
        incr <- rep(0, dim(tmp_ir_ha)[2])
        croplist <- which(
          ir_ha_year[country, dimnames(tmp_ir_ha)[[2]]] - tmp_ir_ha_sums > 0
        )
        incr[croplist] <- 1
        rm(croplist)

        if (any(incr > 0)) {
          # Magnitude of increment depends on how much of the crop area is
          # missing. Increment decreases as values approach target country sum
          # (to avoid overshooting too much).
          incr <- incr * (1 - ifelse(
            ir_ha_year[country, dimnames(tmp_ir_ha)[[2]]] > 0,
            tmp_ir_ha_sums / ir_ha_year[country, dimnames(tmp_ir_ha)[[2]]],
            0
          ))

          # Expand increment vector per crop to all cells.
          incr2 <- tmp_ir_ha
          for (r in seq_len(dim(incr2)[2])) {
            incr2[, r] <- incr[r]
          }
          incr <- incr2
          rm(incr2)

          # Mask cells where harvested area of all crops equals suitability
          # already. If only 1 crop present in cell, also prevents NaNs in
          # transformation below.
          index <- which(cellsum(tmp_ir_ha) - tmp_ir_suit >= 0)
          incr[index, ] <- 0
          rm(index)

          if (min(tmp_ir_ha) < 0) {
            # Safety test, should not happen
            stop(
              "Negative tmp_ir_ha in country ",
              sQuote(country), ", year ", year, ", iteration ", i, "\n",
              "This should not happen at all."
            )
          }
          # Harvested areas expressed as a fraction of available space
          index <- which(tmp_ir_suit > 0)
          tmp_suit <- rep(
            tmp_ir_suit[index],
            dim(tmp_ir_ha)[2]
          )
          tmp_ir_ha[index, ] <- tmp_ir_ha[index, ] / tmp_suit
          rm(index, tmp_suit)
          if (any(tmp_ir_ha > 1.00000001)) {
            stop(
              "tmp_ir_ha > tmp_ir_suit in country ",
              sQuote(country), ", year ", year, ", iteration ", i, "\n",
              "This should not happen at this point."
            )
          }
          # Numerical inaccuracies
          tmp_ir_ha <- pmin(tmp_ir_ha, 1)
          # Logit transformation of harvested areas
          # Resulting values are between:
          # Harvested areas / available space = 0.0: -Inf
          # Harvested areas / available space = 0.5: 0
          # Harvested areas / available space = 1.0: Inf
          # This is per crop, not the sum over all crops.
          tmp_ir_ha <- logit_trans(tmp_ir_ha)

          # Apply increase and transform back.
          # Increment has absolutely no effect on:
          # - cells with no harvested areas
          # - cells where harvested area (per crop) already equals available
          #   space
          # Effects are largest for:
          # - cells where harvested areas / available space close to 0.5
          # logistic_trans increases harvested areas / available space, then
          # multiply with available space to get back to harvested areas
          tmp_suit <- rep(
            tmp_ir_suit,
            dim(tmp_ir_ha)[2]
          )
          tmp_ir_ha <- logistic_trans(tmp_ir_ha + incr) * tmp_suit
          rm(incr, tmp_suit)

          # Increase of harvested areas may have overshot country sum of each
          # crop; if so, apply only a fraction of increment
          fact <- pmax(
            pmin(
              (ir_ha_year[country, dimnames(tmp_ir_ha)[[2]]] -
                country_ir_ha_sums) / countrysum(tmp_ir_ha - country_ir_ha),
              1,
              na.rm = TRUE
              # Constrain fact not to exceed 1
            ),
            0,
            na.rm = TRUE
            # Constrain fact not to go below zero
          )
          index <- which(ir_ha_year[country, dimnames(tmp_ir_ha)[[2]]] == 0)
          fact[index] <- 0
          rm(index)
          # fact is between 0 and 1, country_ir_ha has crop-specific harvested
          # areas before increase.
          for (r in seq_len(dim(tmp_ir_ha)[2])) {
            tmp_ir_ha[, r] <- (tmp_ir_ha[, r] - country_ir_ha[, r]) * fact[r] +
              country_ir_ha[, r]
          }
          rm(fact)

          # Increase of harvested areas may have overshot available space in
          # individual cells; if so, scale back harvested areas of all crops in
          # that cell linearly.
          tmp_suit <- pmax(cellsum(tmp_ir_ha) / tmp_ir_suit, 1)
          tmp_ir_ha <- tmp_ir_ha / tmp_suit
          # Fix division by zero
          index <- which(tmp_ir_suit == 0)
          tmp_ir_ha[index, ] <- 0
          rm(index, tmp_suit)

          if (anyNA(tmp_ir_ha)) {
            # Safety test, should not happen
            stop(
              "NAs in tmp_ir_ha in ",
              sQuote(country), ", year ", year, ", iteration ", i, "\n",
              "This should not happen at this point."
            )
          }
          tmp_ir_ha_sums <- countrysum(tmp_ir_ha)

          # Update areas available for rainfed crops based on irrigated
          # harvested areas now assigned to each cell.
          tmp_suit <- pmin(
            country_ir_suit - cellsum(tmp_ir_ha),
            country_rf_suit_ir,
            na.rm = TRUE
          )
          tmp_rf_suit <- country_rf_suit_rf + tmp_suit
          rm(tmp_suit)
          tmp_rf_suit <- pmax(tmp_rf_suit, 0)
        } # End incr > 0

        # Reduce rainfed harvested areas to fit into this new available space.
        tmp_suit <- pmax(cellsum(country_rf_ha) / tmp_rf_suit, 1, na.rm = TRUE)
        country_rf_ha <- country_rf_ha / tmp_suit
        # Fix division by zero
        index <- which(!is.finite(country_rf_ha))
        country_rf_ha[index] <- 0
        rm(index, tmp_suit)
        # Update copy
        tmp_rf_ha <- country_rf_ha
        tmp_rf_ha_sums <- country_rf_ha_sums <- countrysum(country_rf_ha)

        # Update missing areas per crop
        diff_ir <- tmp_ir_ha_sums -
          ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
        index <- which(is.na(diff_ir))
        diff_ir[index] <- 0
        rm(index)
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        index <- which(is.na(diff_rf))
        diff_rf[index] <- 0
        rm(index)
        if (any(ir_ha_year[country, ] > 0)) {
          # Once irrigated areas have been redistributed successfully or once
          # 80% of maximum number of iterations have passed check that irrigated
          # harvested area patterns leave enough space for rainfed harvested
          # areas on irrigated cropland.
          if (
            # Rainfed crops in need of redistribution
            sum(tmp_rf_suit) - sum(rf_ha_year[country, ts_crops]) < -1e-4 &&
            # 80% of iterations or all irrigated crops redistributed
            (i > 0.8 * redist_max_it || all(diff_ir > (-1e-4)))
          ) {
            if (all(diff_ir > (-1e-4))) {
              # All irrigated crops redistributed successfully
              message(
                "Irrigated harvested areas redistributed successfully after ",
                i, " iterations but not enough space left for rainfed ",
                "harvested areas on irrigated cropland. Allocating more ",
                "irrigated cropland for rainfed harvested areas"
              )
            } else {
              message(
                "Pattern of irrigated harvested areas does not leave enough ",
                "space for rainfed harvested areas on irrigated cropland. ",
                "Allocating more irrigated cropland for rainfed harvested areas"
              )
            }
            # Expand tmp_rf_suit incrementally until space is sufficient
            # for rainfed harvested areas
            tmp_rainfed_exp <- tmp_rf_suit
            r <- 1
            while (sum(tmp_rainfed_exp) -
              sum(rf_ha_year[country, ts_crops]) < -1e-4
            ) {
              tot_rf_suit <- country_rf_suit_rf + country_rf_suit_ir
              index_exp <- which(tmp_rainfed_exp > 0)
              if (sum(tot_rf_suit[index_exp]) -
                sum(rf_ha_year[country, ts_crops]) < 0
              ) {
                # Cells where (tmp_rainfed_exp > 0) do not have enough
                # expansion potential; need to expand to other cells.
                message(
                  "Allow expansion to irrigated cropland that does not have ",
                  "any rainfed harvested areas so far"
                )
                tmp_rainfed_filled <- pmin(
                  ifelse(tot_rf_suit > 0, tmp_rainfed_exp / tot_rf_suit, 0),
                  1
                )
                # Find lowest non-zero value
                index <- which(tmp_rainfed_filled > 0)
                fill_val <- min(
                  tmp_rainfed_filled[index]
                )
                rm(index, tmp_rainfed_filled)
                # Fill into cells with expansion potential, minimum value
                # divided by number of cells with expansion potential
                exp_cells <- which(tmp_rainfed_exp == 0 & tot_rf_suit > 0)
                tmp_rainfed_filled <- fill_val / length(exp_cells) *
                  tot_rf_suit[exp_cells]
                rm(fill_val)
                # Check for overshoot
                fact <- min(
                  1,
                  (sum(rf_ha_year[country, ts_crops]) -
                    sum(tot_rf_suit[index_exp])
                  ) / sum(tmp_rainfed_filled)
                )
                tmp_rainfed_exp[exp_cells] <- tmp_rainfed_filled * fact
                rm(tmp_rainfed_filled, exp_cells, fact)
              }
              rm(index_exp)
              # Increment for logistic transformation; depends on how much
              # rainfed area is missing
              incr <- max(
                0,
                1 - sum(tmp_rainfed_exp) / sum(rf_ha_year[country, ts_crops])
              )
              # Logit transformation of available rainfed area / potentially
              # available rainfed area
              tmp_trans <- ifelse(
                tot_rf_suit > 0, tmp_rainfed_exp / tot_rf_suit, 0
              )
              tmp_rainfed_exp <- logit_trans(tmp_trans)
              rm(tmp_trans)
              # Back transformation after addition of increment
              tmp_rainfed_exp <- logistic_trans(tmp_rainfed_exp + incr) *
                tot_rf_suit
              rm(incr, tot_rf_suit)
              # Check if there is still enough space for irrigated crops
              # Unused irrigated area
              ir_exp_pot <- sum(country_ir_suit) -
                sum(ir_ha_year[country, ], na.rm = TRUE)
              # Rainfed area that would be assigned to irrigated cropland
              rf_exp_pot <- sum(tmp_rainfed_exp - country_rf_suit_rf)

              fact <- min(
                1,
                # Currently missing rainfed area divided by potentially new
                # rainfed area on irrigated cropland
                (sum(rf_ha_year[country, ts_crops], na.rm = TRUE) -
                  sum(tmp_rf_suit)) / sum(tmp_rainfed_exp - tmp_rf_suit)
              )
              if (ir_exp_pot - rf_exp_pot < -1e-4) {
                tmp_rainfed_exp <- (tmp_rainfed_exp - tmp_rf_suit) * fact +
                  tmp_rf_suit
              }
              rm(fact)
              r <- r + 1
            }
            tmp_rainfed_exp <- pmax(tmp_rainfed_exp, 0)
            # Update space available for irrigated harvested areas.
            tmp_ir_suit <- pmax(
              country_ir_suit - tmp_rainfed_exp + country_rf_suit_rf,
              0
            )
            # Scale down irrigated harvested areas which now exceed available
            # space.
            tmp_suit <- pmax(cellsum(tmp_ir_ha) / tmp_ir_suit, 1)
            tmp_ir_ha <- tmp_ir_ha / tmp_suit
            # Fix division by zero
            index <- which(tmp_ir_suit == 0)
            tmp_ir_ha[index, ] <- 0
            rm(index, tmp_suit)
            # Update areas available for rainfed crops
            message(
              "Added ",
              round(sum(tmp_rainfed_exp - tmp_rf_suit), 3),
              " ", fao_area_units,
              " to space available for rainfed harvested areas."
            )
            tmp_rf_suit <- tmp_rainfed_exp
            rm(tmp_rainfed_exp)
            gc(reset = TRUE)
          }
        }
        # Increase counter for iterations
        if (length(irrigated_i) > 0) {
          for (crop in seq_along(irrigated_i)) {
            incr <- any(country_ir_ha[, crop] != tmp_ir_ha[, crop])
            irrigated_i[crop] <- irrigated_i[crop] + incr
            rm(incr)
          }
        }
        # Update country sums and update "backup copy" country_rf_ha for
        # next iteration.
        country_ir_ha <- tmp_ir_ha
        tmp_ir_ha_sums <- countrysum(tmp_ir_ha)
        country_ir_ha_sums <- tmp_ir_ha_sums
        # Update missing areas per crop
        diff_ir <- tmp_ir_ha_sums -
          ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
        index <- which(is.na(diff_ir))
        diff_ir[index] <- 0
        rm(index)
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        index <- which(is.na(diff_rf))
        diff_rf[index] <- 0
        rm(index)

        # After irrigated harvested areas try redistribution of rainfed
        # harvested areas. Repeated until all crops are within 1e-4 ha of
        # country sums.
        if (any(diff_rf < (-1e-4))) {
          # If rainfed harvested areas have not been redistributed successfully
          # after redist_exp_thresh iterations allow expansion outside of the
          # base spatial patterns (harvested_fraction).
          # Retry every redist_exp_thresh iterations if tmp_rf_suit has
          # changed due to changes in irrigated harvested areas.
          if (i > redist_exp_thresh && i %% redist_exp_thresh == 1) {
            message(
              "Allowing rainfed crop expansion outside of base pattern ",
              "to accomodate redistributed harvested areas"
            )
            for (crop in dimnames(tmp_rf_ha)[[2]]) {
              if (
                sum(tmp_rf_ha[, crop]) - rf_ha_year[country, crop] < -1e-4 &&
                sum(tmp_rf_ha[, crop]) > 0
              ) {
                tmp_cropland <- ifelse(
                  hyde_rainfed[ccells_crop] > 0,
                  hyde_rainfed[ccells_crop],
                  hyde_cropland[ccells_crop]
                )
                present <- which(tmp_rf_ha[, crop] > 0)
                tmp_rf_ha[, crop] <- ifelse(
                  # Any space available?
                  tmp_rf_suit > 0,
                  ifelse(
                    # Crop already present?
                    tmp_rf_ha[, crop] > 0,
                    # If so, keep crop
                    tmp_rf_ha[, crop],
                    # Otherwise add crop
                    min(
                      # Find lowest non-zero cropping intensity, based on
                      # rainfed (or, if not available, irrigated) cropland
                      (tmp_rf_ha[, crop] / tmp_cropland)[present] / 10
                    ) * tmp_cropland
                  ),
                  # If no space available, do not add
                  0
                )
                rm(tmp_cropland, present)
                # Make sure expansion has not overshot country sum.
                if (sum(tmp_rf_ha[, crop]) > rf_ha_year[country, crop]) {
                  tmp_rf_ha[, crop] <- tmp_rf_ha[, crop] *
                    rf_ha_year[country, crop] / sum(tmp_rf_ha[, crop])
                }
                if (sum(tmp_rf_ha[, crop]) > sum(country_rf_ha[, crop])) {
                  # Only show message if pattern actually expanded
                  message("Expand pattern for ", sQuote(crop))
                }
              } else if (
                sum(tmp_rf_ha[, crop]) - rf_ha_year[country, crop] < -1e-4
              ) {
                # Have to create pattern
                if (rainfed_year_country[country] > 0) {
                  tmp_rf_ha[, crop] <- hyde_rainfed[ccells_crop] /
                    rainfed_year_country[country] * rf_ha_year[country, crop]
                } else {
                  tmp_rf_ha[, crop] <- hyde_cropland[ccells_crop] /
                    cropland_year_country[country] * rf_ha_year[country, crop]
                }
                # Fix division bx zero
                tmp_rf_ha[, crop] <- pmax(tmp_rf_ha[, crop], 0, na.rm = TRUE)
                if (sum(tmp_rf_ha[, crop]) > sum(country_rf_ha[, crop])) {
                  # Only show message if pattern actually expanded
                  message("New pattern for ", sQuote(crop))
                }
              }
            }
            # Check if expansion has exceeded available space, if so, scale
            # back.
            tmp_suit <- pmax(cellsum(tmp_rf_ha) / tmp_rf_suit, 1, na.rm = TRUE)
            tmp_rf_ha <- tmp_rf_ha / tmp_suit
            # Fix division by zero
            index <- which(tmp_rf_suit <= 0)
            tmp_rf_ha[index, ] <- 0
            rm(index, tmp_suit)
            # Update "backup" copy
            tmp_rf_ha_sums <- country_rf_ha_sums <- countrysum(tmp_rf_ha)
            country_rf_ha <- tmp_rf_ha
            gc(reset = TRUE)
          }

          # Determine which crops need expansion of harvested areas.
          # Only increase areas of crops with insufficient area.
          incr <- rep(0, dim(tmp_rf_ha)[2])
          croplist <- which(
            rf_ha_year[country, dimnames(tmp_rf_ha)[[2]]] - tmp_rf_ha_sums > 0
          )
          incr[croplist] <- 1
          rm(croplist)

          # Magnitude of increment depends on how much of the crop area is
          # missing.
          incr <- incr * (1 - ifelse(
            rf_ha_year[country, dimnames(tmp_rf_ha)[[2]]] > 0,
            tmp_rf_ha_sums / rf_ha_year[country, dimnames(tmp_rf_ha)[[2]]],
            0
          ))
          # Expand increment vector per crop to all cells.
          incr2 <- tmp_rf_ha
          for (r in seq_len(dim(incr2)[2])) {
            incr2[, r] <- incr[r]
          }
          incr <- incr2
          rm(incr2)

          # Mask cells where harvested area of all crops equals suitability
          # already; if only 1 crop present, also prevents NaNs in
          # transformation below.
          index <- which(cellsum(tmp_rf_ha) - tmp_rf_suit >= 0)
          incr[index, ] <- 0
          rm(index)

          if (min(tmp_rf_ha) < 0) {
            # Safety test, should not happen
            stop(
              "Negative tmp_rf_ha in country ",
              sQuote(country), ", year ", year, ", iteration ", i, "\n",
              "This should not happen at all."
            )
          }

          # Harvested areas expressed as a fraction of available space
          index <- which(tmp_rf_suit > 0)
          tmp_suit <- rep(
            tmp_rf_suit[index],
            dim(tmp_rf_ha)[2]
          )
          tmp_rf_ha[index, ] <- tmp_rf_ha[index, ] / tmp_suit
          rm(index, tmp_suit)
          if (any(tmp_rf_ha > 1.00000001)) {
            stop(
              "tmp_rf_ha > tmp_rf_suit in country ",
              sQuote(country), ", year ", year, ", iteration ", i, "\n",
              "This should not happen at this point."
            )
          }
          # Numerical inaccuracies
          tmp_rf_ha <- pmin(tmp_rf_ha, 1)
          # Logit transformation of harvested areas.
          tmp_rf_ha <- logit_trans(tmp_rf_ha)

          # Apply increase and transform back
          tmp_suit <- rep(
            tmp_rf_suit,
            dim(tmp_rf_ha)[2]
          )
          tmp_rf_ha <- logistic_trans(tmp_rf_ha + incr) * tmp_suit
          rm(incr, tmp_suit)

          # Increase of harvested areas may have overshot country sum of each
          # crop; if so, apply only part of increase
          fact <- pmax(
            pmin(
              (rf_ha_year[country, dimnames(tmp_rf_ha)[[2]]] -
                country_rf_ha_sums) / countrysum(tmp_rf_ha - country_rf_ha),
              1,
              na.rm = TRUE
              # Constrain fact not to exceed 1
            ),
            0,
            na.rm = TRUE
            # Constrain fact not to go below zero
          )
          index <- which(rf_ha_year[country, dimnames(tmp_rf_ha)[[2]]] == 0)
          fact[index] <- 0
          rm(index)
          # Fact is between 0 and 1
          for (r in seq_len(dim(tmp_rf_ha)[2])) {
            tmp_rf_ha[, r] <- (tmp_rf_ha[, r] - country_rf_ha[, r]) * fact[r] +
              country_rf_ha[, r]
          }
          rm(fact)

          # Increase of harvested areas may have overshot available space in
          # individual cells.
          tmp_suit <- pmax(cellsum(tmp_rf_ha) / tmp_rf_suit, 1)
          tmp_rf_ha <- tmp_rf_ha / tmp_suit
          # Fix division by zero
          index <- which(tmp_rf_suit <= 0)
          tmp_rf_ha[index, ] <- 0
          rm(index, tmp_suit)

          if (anyNA(tmp_rf_ha)) {
            stop(
              "NAs in tmp_rf_ha in ",
              sQuote(country), ", year ", year, ", iteration ", i, "\n",
              "This should not happen at this point."
            )
          }

          # Update crop-specific iteration counter
          for (crop in seq_along(rainfed_i)) {
            incr <- any(tmp_rf_ha[, crop] != country_rf_ha[, crop])
            rainfed_i[crop] <- rainfed_i[crop] + incr
            rm(incr)
          }
          # Update country sums and update "backup copy" country_rf_ha for
          # next iteration.
          country_rf_ha <- tmp_rf_ha
          tmp_rf_ha_sums <- countrysum(tmp_rf_ha)
          country_rf_ha_sums <- tmp_rf_ha_sums
        }
        # Update missing areas per crop
        diff_ir <- tmp_ir_ha_sums -
          ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
        index <- which(is.na(diff_ir))
        diff_ir[index] <- 0
        rm(index)
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        index <- which(is.na(diff_rf))
        diff_rf[index] <- 0
        rm(index)
        # Total iteration counter
        i <- i + 1
      } # End of iterative redistribution mechanism
      gc()

      # After at most redist_max_it iterations check if spatial patterns match
      # with country sums.
      print_stats <- (i > 1)
      if (any(diff_rf > 1e-4) || any(diff_ir > 1e-4)) {
        # Overshoot (should not normally happen)
        for (crop in names(which(diff_rf > 1e-4))) {
          tmp_rf_ha[, crop] <- tmp_rf_ha[, crop] * rf_ha_year[country, crop] /
            sum(tmp_rf_ha[, crop])
          print_stats <- TRUE
        }
        tmp_rf_ha_sums <- countrysum(tmp_rf_ha)
        for (crop in names(which(diff_ir > 1e-4))) {
          tmp_ir_ha[, crop] <- tmp_ir_ha[, crop] * ir_ha_year[country, crop] /
            sum(tmp_ir_ha[, crop])
          print_stats <- TRUE
        }
        tmp_ir_ha_sums <- countrysum(tmp_ir_ha)
        # Update missing areas per crop
        diff_ir <- tmp_ir_ha_sums -
          ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
        diff_ir[which(is.na(diff_ir))] <- 0
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        diff_rf[which(is.na(diff_rf))] <- 0
        # Update available space for rainfed
        tmp_suit <- pmin(
          country_ir_suit - cellsum(tmp_ir_ha),
          country_rf_suit_ir,
          na.rm = TRUE
        )
        tmp_rf_suit <- country_rf_suit_rf + tmp_suit
        rm(tmp_suit)
      }

      # Iterative approach may not have been able to match country sums exactly.
      # The following "emergency redistribution" algorithms make sure that
      # country sums are matched.

      # Rainfed harvested areas definitely not on rainfed cropland do not
      # leave enough space for irrigated harvested areas.
      tmp_rf_ha_on_irrigated <- pmax(cellsum(tmp_rf_ha) - country_rf_suit_rf, 0)
      if (
        sum(tmp_rf_ha_on_irrigated) + sum(ir_ha_year[country, ts_crops]) -
          sum(tmp_ir_suit) >= 1e-4
      ) {
        message(
          "Emergency re-distribution of rainfed harvested areas because ",
          "default algorithm was not fully successful."
        )
        message(
          "Need to remove ",
          round(
            sum(tmp_rf_ha_on_irrigated) + sum(ir_ha_year[country, ts_crops]) -
              sum(tmp_ir_suit),
            5
          ),
          " ", fao_area_units,
          " of rainfed harvested areas from irrigated cropland to ensure ",
          "enough space for irrigated harvested areas."
        )
        # Determine crops in cells with rainfed harvested areas on irrigated
        # cropland, sort by decreasing area.
        tmp_rf_ha_red <- tmp_rf_ha
        if (length(which(tmp_rf_ha_on_irrigated > 0)) > 1) {
          crops <- names(
            sort(
              countrysum(tmp_rf_ha[which(tmp_rf_ha_on_irrigated > 0), ]),
              decreasing = TRUE
            )
          )
        } else {
          crops <- names(
            sort(
              tmp_rf_ha[which(tmp_rf_ha_on_irrigated > 0), ],
              decreasing = TRUE
            )
          )
        }
        for (crop in crops) {
          tmp_rf_ha_on_irrigated <- pmax(
            cellsum(tmp_rf_ha_red) - country_rf_suit_rf, 0
          )
          # Sum of rainfed harvested areas that need to be removed from
          # irrigated cropland
          excess <- sum(tmp_rf_ha_on_irrigated) +
            sum(ir_ha_year[country, ts_crops]) - sum(tmp_ir_suit)
          if (
            sum(tmp_rf_ha[which(tmp_rf_ha_on_irrigated > 0), crop]) > 0 &&
            excess >= 1e-4
          ) {
            # Share of excess assigned to this crop
            crop_share_excess <- excess *
              sum(tmp_rf_ha[which(tmp_rf_ha_on_irrigated > 0), crop]) /
              sum(tmp_rf_ha[which(tmp_rf_ha_on_irrigated > 0), crops])
            # Distribution of crop_share_excess among individual cells
            pattern_share_excess <- ifelse(
              tmp_rf_ha_on_irrigated > 0,
              tmp_rf_ha[, crop],
              0
            ) / sum(tmp_rf_ha[which(tmp_rf_ha_on_irrigated > 0), crop])
            tmp_rf_ha_red[, crop] <- tmp_rf_ha_red[, crop] -
              pattern_share_excess * crop_share_excess
            rm(crop_share_excess, pattern_share_excess)
            # Remove crop from list of crops to which excess can be distributed.
            crops <- setdiff(crops, crop)
            tmp_rf_ha_red[which(tmp_rf_ha_red[, crop] < 0), crop] <- 0
            if (sum(tmp_rf_ha_red[, crop]) < sum(tmp_rf_ha[, crop])) {
              message(
                "Rainfed ", sQuote(crop), ": ",
                round(sum(abs(tmp_rf_ha[, crop] - tmp_rf_ha_red[, crop])), 5),
                " ", fao_area_units, ", ",
                round(
                  sum(abs(tmp_rf_ha[, crop] - tmp_rf_ha_red[, crop])) /
                    sum(tmp_rf_ha[, crop]) * 100,
                3),
                " % of crop-specific harvested area removed from ",
                "irrigated cropland"
              )
            }
          }
        }
        tmp_rf_ha <- tmp_rf_ha_red
        tmp_rf_ha_sums <- countrysum(tmp_rf_ha)
        rm(tmp_rf_ha_red)
      }
      rm(tmp_rf_ha_on_irrigated)
      # Update missing areas per crop
      diff_rf <- tmp_rf_ha_sums -
        rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
      diff_rf[which(is.na(diff_rf))] <- 0

      # If any rainfed crops are missing harvested areas, expand by filling
      # up available space linearly.
      if (
        any(diff_rf < -1e-4) || any(cellsum(tmp_rf_ha) - tmp_rf_suit > 1e-4)
      ) {
        message(
          "Emergency re-distribution of rainfed harvested areas because ",
          "default algorithm was not fully successful."
        )
        if (sum(tmp_rf_suit) >
          sum(rf_ha_year[country, dimnames(country_rf_ha)[[2]]])
        ) {
          # Enough space given current irrigated harvested areas.
          tmp_suit <- pmax(cellsum(tmp_rf_ha) / tmp_rf_suit, 1, na.rm = TRUE)
          tmp_rf_ha <- tmp_rf_ha / tmp_suit
          rm(tmp_suit)
        } else {
          # Use maximum possible rainfed harvested areas on rainfed and
          # irrigated cropland.
          tot_rf_suit <- country_rf_suit_ir + country_rf_suit_rf
          tmp_suit <- pmax(cellsum(tmp_rf_ha) / tot_rf_suit, 1, na.rm = TRUE)
          tmp_rf_ha <- tmp_rf_ha / tmp_suit
          rm(tmp_suit, tot_rf_suit)
        }
        gc()
        tmp_rf_ha[which(!is.finite(tmp_rf_ha))] <- 0
        tmp_rf_ha_sums <- countrysum(tmp_rf_ha)
        # Update missing areas per crop
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        diff_rf[which(is.na(diff_rf))] <- 0
        # First fill up rainfed cropland.
        for (crop in names(which(sort(diff_rf) < -1e-4))) {
          avail <- country_rf_suit_rf - cellsum(tmp_rf_ha)
          avail[which(avail < 0)] <- 0
          if (sum(avail) <= 0) {
            break
          }
          mismatch_rf <- rf_ha_year[country, crop] - sum(tmp_rf_ha[, crop])
          tmp_rf_ha[, crop] <- tmp_rf_ha[, crop] + avail / sum(avail) *
            mismatch_rf * min(sum(avail) / mismatch_rf, 1)
          message(
            "Rainfed ", sQuote(crop), ": ",
            round(sum(abs(tmp_rf_ha[, crop] - country_rf_ha[, crop])), 5),
            " ", fao_area_units, ", ",
            round(
              sum(abs(tmp_rf_ha[, crop] - country_rf_ha[, crop])) /
                sum(country_rf_ha[, crop]) * 100,
              3
            ),
            " % of crop-specific harvested area added on rainfed cropland"
          )
          print_stats <- TRUE
        }
        tmp_rf_ha_sums <- countrysum(tmp_rf_ha)
        # Update missing areas per crop
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        diff_rf[which(is.na(diff_rf))] <- 0
        tmp_rf_ha_exp <- tmp_rf_ha - country_rf_ha
        # Second allow also irrigated cropland if rainfed cropland was not
        # sufficient.
        for (crop in names(which(sort(diff_rf) < -1e-4))) {
          avail <- country_rf_suit_rf + country_rf_suit_ir - cellsum(tmp_rf_ha)
          avail[which(avail < 0)] <- 0
          if (sum(avail) <= 0) {
            rm(avail)
            break
          }
          mismatch_rf <- (rf_ha_year[country, crop] - sum(tmp_rf_ha[, crop]))
          tmp_rf_ha[, crop] <- tmp_rf_ha[, crop] + avail / sum(avail) *
            mismatch_rf * min(sum(avail) / mismatch_rf, 1)
          message(
            "Rainfed ", sQuote(crop), ": ",
            round(
              sum(
                abs(tmp_rf_ha[, crop] - tmp_rf_ha_exp[, crop] -
                  country_rf_ha[, crop])
              ),
              5
            ),
            " ", fao_area_units, ", ",
            round(
              sum(
                abs(tmp_rf_ha[, crop] - tmp_rf_ha_exp[, crop] -
                    country_rf_ha[, crop])
              ) / sum(country_rf_ha[, crop]) * 100,
              3
            ),
            " % of crop-specific harvested area added on irrigated cropland"
          )
          print_stats <- TRUE
          rm(avail)
        }
        tmp_rf_ha_sums <- countrysum(tmp_rf_ha)
        # Update missing areas per crop
        diff_rf <- tmp_rf_ha_sums -
          rf_ha_year[country, dimnames(country_rf_ha)[[2]]]
        diff_rf[which(is.na(diff_rf))] <- 0

        # Derive remaining irrigated space given final rainfed harvested areas.
        tmp_ir_suit <- country_rf_suit_rf + country_ir_suit - cellsum(tmp_rf_ha)
        mismatch_ir <- which(tmp_ir_suit > country_ir_suit)
        tmp_ir_suit[mismatch_ir] <- country_ir_suit[mismatch_ir]

        # Available irrigated space may have changed due to emergency
        # re-distribution of rainfed areas, so check.
        tmp_suit <- pmax(cellsum(tmp_ir_ha) / tmp_ir_suit, 1)
        tmp_ir_ha <- tmp_ir_ha / tmp_suit
        # Fix division by zero
        index <- which(tmp_ir_suit == 0)
        tmp_ir_ha[index, ] <- 0
        rm(index, tmp_suit)
        tmp_ir_ha_sums <- countrysum(tmp_ir_ha)
        # Update missing areas per crop
        diff_ir <- tmp_ir_ha_sums -
          ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
        index <- which(is.na(diff_ir))
        diff_ir[index] <- 0
        rm(index)
      }
      gc()

      # If any irrigated crops are missing harvested areas, expand by filling
      # up available space linearly.
      if (
        any(diff_ir < -1e-4) || any(cellsum(tmp_ir_ha) - tmp_ir_suit > 1e-4)
      ) {
        message(
          "Emergency re-distribution of irrigated harvested areas because ",
          "default algorithm was not fully successful."
        )
        # Fill up irrigated cropland
        for (crop in names(which(sort(diff_ir) < -1e-4))) {
          avail <- tmp_ir_suit - cellsum(tmp_ir_ha)
          avail[which(avail < 0)] <- 0
          if (sum(avail) <= 0) {
            rm(avail)
            break
          }
          tmp_ir_ha[, crop] <- tmp_ir_ha[, crop] + avail / sum(avail) *
            (ir_ha_year[country, crop] - sum(tmp_ir_ha[, crop]))
          message(
            "Irrigated ", sQuote(crop), ": ",
            round(sum(abs(tmp_ir_ha[, crop] - country_ir_ha[, crop])), 5),
            " ", fao_area_units, ", ",
            round(
              sum(abs(tmp_ir_ha[, crop] - country_ir_ha[, crop])) /
                sum(country_ir_ha[, crop]) * 100,
              3
            ),
            " % of crop-specific harvested area added on irrigated cropland"
          )
          rm(avail)
          print_stats <- TRUE
        }
        tmp_ir_ha_sums <- countrysum(tmp_ir_ha)
        # Update missing areas per crop
        diff_ir <- tmp_ir_ha_sums -
          ir_ha_year[country, dimnames(country_ir_ha)[[2]]]
        index <- which(is.na(diff_ir))
        diff_ir[index] <- 0
        rm(index)
      }
      gc()

      # All redistribution attempts finished. Final check that assigned areas
      # match FAOSTAT country sums
      if (any(abs(diff_rf) > 1e-4) || any(abs(diff_ir) > 1e-4)) {
        stop(
          "Failure to disaggregate country-level values to grid in year ",
          year, " and country ", sQuote(country),
          ". This should not happen.\n",
          "Please make sure that country-level processing enforces ",
          "country-level harvested area sums that are consistent with ",
          "cropland data used."
        )
      }
      if (print_stats) {
        year_country_stats <- data.frame(
          total_rainfed = NA,
          redistributed_rainfed = NA,
          emergency_redistribution_rainfed = NA,
          out_of_pattern_rainfed = NA,
          iterations_rainfed = NA,
          total_irrigated = NA,
          redistributed_irrigated = NA,
          emergency_redistribution_irrigated = NA,
          out_of_pattern_irrigated = NA,
          iterations_irrigated = NA,
          has_crop_pattern = apply(
            country_pattern,
            2,
            function(indata, na.rm) (any(indata > 0, na.rm = na.rm)),
            na.rm = TRUE
          )
        )
        rownames(year_country_stats) <- dimnames(country_pattern)[[2]]

        if (country_rf_dim[2] > 0) {
          crops <- dimnames(country_rf_ha)[[2]]
          year_country_stats[crops, "total_rainfed"] <- tmp_rf_ha_sums
          year_country_stats[crops, "redistributed_rainfed"] <- countrysum(
            abs(tmp_rf_ha - get(paste0("redist_country_rf_ha", cindex)))
          ) / 2
          year_country_stats[crops, "emergency_redistribution_rainfed"] <-
            countrysum(abs(tmp_rf_ha - country_rf_ha))
          if (length(crops) > 1) {
            year_country_stats[crops, "out_of_pattern_rainfed"] <- countrysum(
              tmp_rf_ha * ifelse(
                rep(
                  apply(
                    country_pattern[, crops],
                    2,
                    function(indata, na.rm) (any(indata > 0, na.rm = na.rm)),
                    na.rm = TRUE
                  ),
                  each = dim(country_pattern)[1]
                ),
                ifelse(
                  !is.na(country_pattern[, crops]) &
                    country_pattern[, crops] > 0,
                  0,
                  1
                ),
                0
              ),
              na.rm = TRUE
            )
          } else {
            year_country_stats[crops, "out_of_pattern_rainfed"] <- countrysum(
              tmp_rf_ha * ifelse(
                rep(
                  any(country_pattern[, crops] > 0, na.rm = TRUE),
                  dim(country_pattern)[1]
                ),
                ifelse(
                  !is.na(country_pattern[, crops]) &
                    country_pattern[, crops] > 0,
                  0,
                  1
                ),
                0
              ),
              na.rm = TRUE
            )
          }
          year_country_stats[crops, "iterations_rainfed"] <- rainfed_i - 1
        }
        if (country_ir_dim[2] > 0) {
          crops <- dimnames(country_ir_ha)[[2]]
          year_country_stats[crops, "total_irrigated"] <- tmp_ir_ha_sums
          year_country_stats[crops, "redistributed_irrigated"] <- countrysum(
            abs(tmp_ir_ha - get(paste0("redist_country_ir_ha", cindex)))
          ) / 2
          year_country_stats[crops, "emergency_redistribution_irrigated"] <-
            countrysum(abs(tmp_ir_ha - country_ir_ha))
          if (length(crops) > 1) {
            year_country_stats[crops, "out_of_pattern_irrigated"] <- countrysum(
              tmp_ir_ha * ifelse(
                rep(
                  apply(
                    country_pattern[, crops],
                    2,
                    function(indata, na.rm) (any(indata > 0, na.rm = na.rm)),
                    na.rm = TRUE
                  ),
                  each = dim(country_pattern)[1]
                ),
                ifelse(
                  !is.na(country_pattern[, crops]) &
                    country_pattern[, crops] > 0,
                  0,
                  1
                ),
                0
              ),
              na.rm = TRUE
            )
          } else {
            year_country_stats[crops, "out_of_pattern_irrigated"] <- countrysum(
              tmp_ir_ha * ifelse(
                rep(
                  any(country_pattern[, crops] > 0, na.rm = TRUE),
                  dim(country_pattern)[1]
                ),
                ifelse(
                  !is.na(country_pattern[, crops]) &
                    country_pattern[, crops] > 0,
                  0,
                  1
                ),
                0
              ),
              na.rm = TRUE
            )
          }
          year_country_stats[crops, "iterations_irrigated"] <- irrigated_i - 1
        }
        redist_stats[[as.character(year)]][[country]] <- year_country_stats
        rm(year_country_stats)
      }
      # Save back to country-specific variables
      assign(paste0("redist_country_rf_ha", cindex), tmp_rf_ha)
      assign(paste0("redist_country_ir_ha", cindex), tmp_ir_ha)
      message(
        i - 1, " iterations to redistribute harvested areas in ",
        sQuote(country)
      )
      rm(
        country_ir_ha,
        country_ir_suit,
        tmp_ir_ha,
        country_rf_ha,
        country_rf_suit_ir,
        country_rf_suit_rf,
        tmp_rf_ha,
        tmp_rf_suit,
        country_pattern,
        ccells,
        ccells_crop
      )
      gc()
    } # End country loop over redist_countries

    # Write back corrected country-specific data to global fields
    message(
      "Writing updated harvested areas back to ",
      sQuote(ha_timeseries_file$filename)
    )
    # Arrays for harvested area sum across all crops in each cell, used to check
    # cropping factor at the end.
    for (var in c("irrigated_area_sum", "rainfed_area_sum", "total_area_sum")) {
      vals <- double(length(hyde_cropland))
      vals[which(is.na(gadm_raster[]))] <- NA
      assign(var, vals)
    }
    # Crops that have been subjected to redistribution
    croplist <- names(
      which(apply(tot_ha_year[redist_countries, ] > 0, 2, any, na.rm = TRUE))
    )
    for (crop in ts_crops) {
      tmp_rf_ha <- ncvar_get(
        ha_timeseries_file,
        rainfed_output_name,
        start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
        count = c(-1, -1, 1, 1))
      tmp_ir_ha <- ncvar_get(
        ha_timeseries_file,
        irrigated_output_name,
        start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
        count = c(-1, -1, 1, 1)
      )
      if (!crop %in% croplist) {
        # Add unchanged harvested areas to sum over all crops
        rainfed_area_sum <- c(
          rainfed_area_sum + pmax(tmp_rf_ha, 0, na.rm = TRUE)
        )
        irrigated_area_sum <- c(
          irrigated_area_sum + pmax(tmp_ir_ha, 0, na.rm = TRUE)
        )
      } else {
        # Write back harvested areas from country-specific arrays to global
        # data fields
        for (country in redist_countries) {
          cindex <- which(redist_countries == country)
          ccells <- fao_gadm_country_cells[[country]]
          ccells_crop <- intersect(ccells, crop_cells)
          country_rf_ha <- get(paste0("redist_country_rf_ha", cindex))
          if (crop %in% dimnames(country_rf_ha)[[2]]) {
            tmp_rf_ha[ccells_crop] <- country_rf_ha[, crop]
          }
          rm(country_rf_ha)
          country_ir_ha <- get(paste0("redist_country_ir_ha", cindex))
          if (crop %in% dimnames(country_ir_ha)[[2]]) {
            tmp_ir_ha[ccells_crop] <- country_ir_ha[, crop]
          }
          rm(country_ir_ha)
          rm(ccells, ccells_crop)
        }
        # Sum over all crops
        rainfed_area_sum <- c(
          rainfed_area_sum + pmax(tmp_rf_ha, 0, na.rm = TRUE)
        )
        irrigated_area_sum <- c(
          irrigated_area_sum + pmax(tmp_ir_ha, 0, na.rm = TRUE)
        )
        # 7) Save final patterns to NetCDF file.
        ncvar_put(
          ha_timeseries_file,
          rainfed_output_name,
          c(tmp_rf_ha),
          start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
          count = c(-1, -1, 1, 1)
        )
        ncvar_put(
          ha_timeseries_file,
          irrigated_output_name,
          c(tmp_ir_ha),
          start = c(1, 1, which(ts_crops == crop), year - min_fileyear + 1),
          count = c(-1, -1, 1, 1)
        )
      }
      rm(tmp_ir_ha, tmp_rf_ha)
      gc(full = (which(ts_crops == crop) %% 5 == 0))
    }
    total_area_sum <- rainfed_area_sum + irrigated_area_sum

    # Clean up of country-specific variables before next year.
    rm(list = paste0("redist_country_rf_ha", seq_along(redist_countries)))
    rm(list = paste0("redist_country_ir_ha", seq_along(redist_countries)))
    rm(list = paste0("redist_country_pattern", seq_along(redist_countries)))
    gc()
    # Cropping intensity range
    cf <- total_area_sum / ifelse(hyde_cropland > 0, hyde_cropland, NA)
    cat(
      "Range of cropping factor this year after redistribution:",
      paste(round(range(cf, na.rm = TRUE), 2), collapse = " - "), "\n"
    )
    rm(cf)
  } # End of spatial redistribution

  # Save harvested area sums to NetCDF file.
  ncvar_put(
    ha_timeseries_file,
    rainfed_output_sum_name,
    c(rainfed_area_sum),
    start = c(1, 1, year - min_fileyear + 1),
    count = c(-1, -1, 1)
  )
  ncvar_put(
    ha_timeseries_file,
    irrigated_output_sum_name,
    c(irrigated_area_sum),
    start = c(1, 1, year - min_fileyear + 1),
    count = c(-1, -1, 1)
  )
  ncvar_put(
    ha_timeseries_file,
    total_output_sum_name,
    c(total_area_sum),
    start = c(1, 1, year - min_fileyear + 1),
    count = c(-1, -1, 1)
  )
  rm(irrigated_area_sum, rainfed_area_sum, total_area_sum)
  # Reload year_processed before saving in case several jobs might be
  # accessing the same file.
  year_processed <- ncvar_get(ha_timeseries_file, "year_processed")
  year_processed[year - min_fileyear + 1] <- 1
  # 8) Mark year as processed in NetCDF file.
  ncvar_put(ha_timeseries_file, "year_processed", year_processed)

  # Before saving stats make sure redist_stats has not been changed by other
  # jobs accessing the same file.
  if (file.exists(redist_stats_file)) {
    tmpstats <- redist_stats
    load(redist_stats_file)
    for (ystats in which(!names(redist_stats) %in% names(tmpstats))) {
      tmpstats[[ystats]] <- redist_stats[[ystats]]
    }
    redist_stats <- tmpstats
  }
  save(redist_stats, file = redist_stats_file)

  # Finish processing this year.
  if (year %in% chunk_end) {
    cat("Finalizing output file:", ha_timeseries_file$filename, "\n")
    nc_close(ha_timeseries_file)
    rm(ha_timeseries_file)
  } else {
    nc_sync(ha_timeseries_file)
  }
  # Track runtime.
  runtime_stop <- proc.time()
  runtime_year <- (runtime_stop - runtime_start)["elapsed"]
  cat(
    "*** Year ", year, " processed in ",
    runtime_year %/% 3600, ":",
    formatC((runtime_year %% 3600) %/% 60, width = 2, flag = "0"), ":",
    formatC(round((runtime_year %% 3600) %% 60), width = 2, flag = "0"),
    " ***\n",
    sep = ""
  )
  gc()
  runtime_start <- runtime_stop
}
# Close any NetCDF files that might still be open.
if (exists("ha_timeseries_file")) {
  nc_close(ha_timeseries_file)
}
nc_close(harvested_fraction_file)
