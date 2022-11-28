################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script defines common settings, directories etc. for use by the other ##
## scripts in this directory.                                                 ##
## The target resolution of the derived fertilizer dataset is determined by   ##
## the spatial resolution of the GADM mask of administrative units and cannot ##
## be finer than the source resolution of any of the gridded source datasets. ##
## We suggest to process the data at the native resolution of the gridded     ##
## source data and only aggregate to a coarser resolution as a final          ##
## processing step.
## This directory contains scripts to derive inputs for                       ##
## - mineral fertilizer application rate                                      ##
## - manure application rate                                                  ##
## You may either create separate inputs for these two or combine them into   ##
## one input.                                                                 ##
################################################################################

################################################################################
## Basic setup:                                                               ##
## Base directory. Landuse source datasets and scripts are generally located  ##
## under this base directory.                                                 ##
fertilizer_dir <- ""
if (nchar(fertilizer_dir) > 0) {
  setwd(fertilizer_dir)
}
## Temporal range is generally limited by availability of fertilizer and      ##
## manure time series data but can also be shorter.                           ##
## Period to process:                                                         ##
output_period <- c(1860, 2015)
## If output_period starts before or ends after the period covered by the     ##
## source datasets, how should the source datasets be extended? Either        ##
## "replicate" (which simply replicates the first/last year for which data is ##
## available in the source dataset) or "zero" (which sets values to zero      ##
## outside of the period covered by the source data.                          ##
extend_source_period <- "replicate"
## Should mineral fertilizer and manure be combined into one input (for       ##
## models that do not distinguish between organic and mineral fertilizer)     ##
combine_fertilizer_manure <- FALSE
## Nutrients from manure are generally less available to plants than mineral  ##
## fertilizers. If mineral fertilizer and manure are combined into one input  ##
## manure rates may optionally be scaled by a factor to account for this      ##
## reduced plant availability. Default: 0.6 following Elliott et al., Geosci. ##
## Model Dev., 8, 261–277, 2015 (doi: 10.5194/gmd-8-261-2015)                 ##
manure_availability_scalar <- 0.6
## Mapping table between crops in different source datasets (incl. datasets   ##
## used for weighted aggregation).
mapping_file <- "crop_types_fert_Monfreda_FAOSTAT_LUH2.csv"
## Column in mapping table giving LPJmL CFT names. Note: The values in this   ##
## column should be consistent with the aggregation table used to process     ##
## land use patterns in ../landuse.                                           ##
cft_map_col <- "LPJmL_name"
## Column in mapping table giving Monfreda crop names (used for harvested     ##
## area time series)                                                          ##
monfreda_map_col <- "Monfreda_name"
## Column in mapping table giving FAOSTAT item names (used for harvested area ##
## time series)                                                               ##
fao_name_map_col <- "FAOSTAT_item_name"
## Column in mapping table giving FAOSTAT item code (currently not used)      ##
fao_code_map_col <- "FAOSTAT_item_code"
################################################################################

################################################################################
## Dataset-specific setup                                                     ##
################################################################################

################################################################################
## Spatial patterns of mineral fertilizer application rates                   ##
## By default, uses rates from Mueller et al., Nature, 490, 254–257, 2012     ##
## (doi: 10.1038/nature11420)                                                 ##
## Directory that contains this dataset (relative to fertilizer_dir if not    ##
## supplied as absolute path)                                                 ##
fertilizer_pattern_dir <- "muellerNPKfertilizer"
## Does this dataset only provide national data? Information used for         ##
## gap-filling. Set to FALSE if source data includes sub-national data.       ##
fertilizer_pattern_is_national <- FALSE
## Which nutrients to process. Mueller et al. provide K2O, N, and P2O5 while  ##
## LUH2 used to derive temporal trends of fertilizer application only includes##
## nitrogen. By default, process only nitrogen                                ##
fertilizer_pattern_nutrients <- "N"
## Unit of fertilizer data                                                    ##
fertilizer_pattern_unit <- "kg/ha"
## Download base URL for https://doi.org/10.5281/zenodo.5260732 (Mueller et   ##
## al. 2012 data at Zenodo                                                    ##
fertilizer_pattern_base_url <- "https://zenodo.org/record/5260732/files/"
## Corresponding column in mapping_file                                       ##
fertilizer_pattern_map_col <- "fert_name"
## Which administrative unit dataset to use, currently implemented:           ##
## "gadm", "luh2"                                                             ##
fertilizer_pattern_admin <- "gadm"
## Does the administrative unit dataset have the same spatial resolution?     ##
fertilizer_pattern_admin_resmatch <- TRUE
## Data band and band with source information. Set to NULL if missing.        ##
fertilizer_pattern_fert_band <- 1
fertilizer_pattern_source_band <- 2
## If data uses 0 instead of fill value in cells with missing values set to   ##
## TRUE                                                                       ##
fertilizer_pattern_strip_zero <- FALSE
## Reference year(s) for spatial patterns. Note: number of years must match   ##
## number of data bands above.                                                ##
fertilizer_pattern_refyear <- 2000
## Minimum number of countries required in group to use said group value in   ##
## gap-filling.                                                               ##
fertilizer_pattern_assign_country_threshold <- 1
## Minimum number of source cells required to use value for admin unit in     ##
## gap-filling. Accounts for possible inconsistencies between admin masks     ##
## used. Set to 1 if you use a matching mask. GADM is known not to match      ##
## Mueller et al. dataset fully.                                              ##
fertilizer_pattern_assign_grid_threshold <- 5
################################################################################


################################################################################
## Temporal trends of fertilizer use                                          ##
## By default, uses rates from historical LUH2 dataset                        ##
## Directory that contains this dataset. Put downloaded data here.            ##
fertilizer_trend_dir <- "LUH2"
## Does this dataset only provide national data? Information used for         ##
## gap-filling. Set to FALSE if source data included sub-national data.       ##
fertilizer_trend_is_national <- TRUE
## Included nutrients                                                         ##
fertilizer_trend_nutrients <- "N"
## Unit of fertilizer data                                                    ##
fertilizer_trend_unit <- "kg/ha"
## Corresponding column in mapping_file                                       ##
fertilizer_trend_map_col <- "LUH2_crop_type"
## Which administrative unit dataset to use, currently implemented:           ##
## "gadm", "luh2"                                                             ##
fertilizer_trend_admin <- "luh2"
## Does the administrative unit dataset have the same spatial resolution      ##
fertilizer_trend_admin_resmatch <- TRUE
## Data band and band with source information. Set to NULL if missing.        ##
fertilizer_trend_fert_band <- 1
fertilizer_trend_source_band <- NULL
## If data uses 0 instead of fill value in cells with missing values set to   ##
## TRUE                                                                       ##
fertilizer_trend_strip_zero <- TRUE
## Source files                                                               ##
# Fertilizer data. If time series for crops are in separate files set to NULL
# and add logic to gapfill_fertilizer_trend.R to determine filename.
fertilizer_trend_src_name <- file.path(fertilizer_trend_dir, "management.nc")
# Optional matching landuse patterns. If fertilizer_trend_strip_zero == TRUE and
# you have matching landuse patterns specify file here to use landuse mask to
# distinguish between cells with 0 fertilizer application and cells where the
# crop is not grown. Set to NULL if not available or if landuse patterns are in
# multiple files.
fertilizer_trend_lu_name <- file.path(fertilizer_trend_dir, "states.nc")
## Minimum number of countries required in group to use said group value in   ##
## gap-filling.                                                               ##
fertilizer_trend_assign_country_threshold <- 1
## Minimum number of source cells required to use value for admin unit in     ##
## gap-filling. Accounts for possible inconsistencies between admin masks     ##
## used. Set to 1 if you use a matching mask, e.g. using LUH2 country mask    ##
## with LUH2 fertilizer time series.                                          ##
fertilizer_trend_assign_grid_threshold <- 1
################################################################################


################################################################################
## Manure application rates                                                   ##
## By default, uses rates from Zhang et al., Earth System Science Data, 9,    ##
## 667-678, 2017 (doi:10.5194/essd-9-667-2017)                                ##
## Data can be downloaded from: https://doi.org/10.1594/PANGAEA.871980
manure_dir <- "manure"
## Included nutrients                                                         ##
manure_nutrients <- "N"
## Unit of manure data                                                        ##
manure_src_unit <- "kg/km2"
## Unit to use for manure rate after conversion. Especially relevant if       ##
## data represents absolute amounts but also allows any unit change from      ##
## manure_src_unit.
manure_rate_unit <- "kg/ha"
## Optional maximum manure application rate. Values above threshold are cut   ##
## off and accounted for in separate file. Set to Inf in order not to impose  ##
## maximum application rate. Supply in unit manure_rate_unit.                 ##
manure_threshold <- 100
## Where to apply manure application threshold: either at the "source" or     ##
## "target" output resolution. When aggregating source data to a coarser
## output resolution, source cells above and below threshold may average out  ##
## at the target resolution. As such, total applied manure amount is          ##
## potentially higher if threshold is applied at the target resolution.       ##
## Default: "target"                                                          ##
manure_threshold_res <- "target"
## Spatial reference area. Does the rate refer to the full grid area or the   ##
## area to which manure is applied. Either "grid" or "cropland". Setting to   ##
## "grid" requires rescaling of rates to the cropland area in each cell. Set  ##
## to NULL if unit is total amount, not rate per unit area.                   ##
manure_ref_area <- "grid"
## Optional file providing grid area for manure dataset. Used to convert      ##
## between absolute amounts and rates. If set to NULL, uses internal function ##
## to estimate grid area. Suggestion: Use HYDE area if also using HYDE        ##
## cropland for manure_cropland_name below.                                   ##
manure_area_file <- file.path(
  "..", "landuse", "HYDE", "general_files",
  "garea_cr.asc"
)
## Area unit used in manure_area_file.                                        ##
manure_area_unit <- "km2"
## Manure time series filename (either created by preprocess_manure.R or      ##
## source filename if data source provides time series NetCDF file directly.  ##
## Note: If using preprocess_manure.R make sure this matches the naming       ##
## scheme used in that script. If more than one nutrient, vector with one     ##
## named entry per nutrient.                                                  ##
manure_src_name <- file.path(manure_dir, "manure_N_\\d{4}-\\d{4}.nc")
## If manure_ref_area == "grid" or if you plan to apply a manure_threshold    ##
## you need to provide a matching cropland dataset, which is used to scale    ##
## rates per unit area grid cell to rates per unit area and to convert        ##
## absolute amounts to application rates. Use HYDE cropland from landuse      ##
## processing by default. Code expects a NetCDF time series. Cropland time    ##
## series is also used to aggregate manure data spatially if output           ##
## resolution does not match source resolution.                               ##
manure_cropland_name <- file.path(
  "..", "landuse", "HYDE", "path", "to",
  "hyde_cropland_annual_startyear_endyear.nc4"
)
## Unit of cropland dataset
manure_cropland_unit <- "km2"
## Variable name used in manure_cropland_name
manure_cropland_varname <- "cropland"
## Optional file providing grid area for cropland dataset. Set to NULL if not ##
## used. Required if manure_cropland_unit is fractional instead of an area.   ##
manure_cropland_area_file <- file.path(
  "..", "landuse", "HYDE", "general_files",
  "garea_cr.asc"
)
## Area unit in manure_cropland_area_file.                                    ##
manure_cropland_area_file_unit <- "km2"
################################################################################


################################################################################
## Administrative units used by LUH2 dataset                                  ##
## Supply this if using LUH2 as dataset providing historical trends.          ##
# File containing admin units
luh2country_file <- file.path(fertilizer_trend_dir, "staticData_quarterdeg.nc")
# Variable name in file (if file contains more than one, otherwise set to NA)
luh2country_variable <- "ccode"
# Layer to use if file contains one variable but several bands (set to NA if
# not applicable)
luh2country_layer <- NA
################################################################################


################################################################################
## GADM administrative units mask                                             ##
## GADM data is used for country and region delineation                       ##
## These files should have been created using scripts in ../gadm/             ##
## Make sure that you create gridded administrative areas in the desired      ##
## target resolution of the fertilizer dataset created by scripts in this     ##
## directory.                                                                 ##
## Administrative levels 0-2; codes only, by default called something like    ##
## gadm_level0_1_2_*.nc.                                                      ##
gadmlevel_file <- "../gadm/gadm_level0_1_2_[spatial_resolution].nc"
## List linking codes in gadmlevel_file to names (CSV expected)               ##
## By default called something like "gadm_level0_1_2_*_indices.csv"           ##
## This is used to translate numeric codes used in gadmlevel_file to 3-letter ##
## ISO codes used with UNSD regions.                                          ##
gadmlevel_names_file <- "../gadm/gadm_level0_1_2_[spatial_resolution]_indices.csv"
## Raster with number of unique administrative level 0 shapes in each cell    ##
## (as opposed to gadmlevel_file which always returns unit with largest area  ##
## in cell).                                                                  ##
gadmborder_file <- "../gadm/ncountry_gadm_[spatial_resolution].nc"
################################################################################


################################################################################
## UNSD Regions                                                               ##
## File containing UN standard country or area codes for statistical use      ##
## (M49). Can be downloaded from                                              ##
## https://unstats.un.org/unsd/methodology/m49/overview/                      ##
## Note: CSV direct download from website does not quote strings, leading to  ##
## problems with country names containing a comma. Either edit CSV file       ##
## manually or download Excel file from website and convert into CSV.         ##
country_group_file <- "UNSD-Methodology.csv"
## Column in country table corresponding to codes used in GADM                ##
gadm_country_col <- "ISO.alpha3.Code"
## Column in country table corresponding to codes used in LUH2 country dataset##
luh2_country_col <- "M49.Code"
################################################################################


################################################################################
## Load required R packages. These may need to be installed first.            ##
library(tools)
library(ncdf4)
library(raster)
library(udunits2)
library(abind)
library(stringi)
library(stringdist)
################################################################################


################################################################################
## Helper functions for LPJmL input format                                    ##
## The script lpjml_format_helper_functions.R is saved in the parent          ##
## directory by default.                                                      ##
if (file.exists("../lpjml_format_helper_functions.R")) {
  source("../lpjml_format_helper_functions.R")
} else {
  stop("Please update path to script with LPJmL input format helper function")
}
## Raster helpers                                                             ##
source(file.path("helper", "raster_helpers.R"))
## Country names and corresponding ISO codes; also includes some non-official ##
## codes used in GADM.                                                        ##
source(file.path("helper", "fao_gadm_country_mapping.R"))
## Spatial gap-fill algorithm for fertilizer patterns                         ##
source(file.path("helper", "gapfill_pattern.R"))
## Gap-fill algorithm for fertilizer trend time series at country level       ##
source(file.path("helper", "gapfill_country_trend.R"))
## Function to derive file years in NetCDF file                               ##
source(file.path("helper", "nc_file_years.R"))
## Function to add country to UNSD Regions                                    ##
source(file.path("helper", "add_to_country_list.R"))
## Various fixes related to admin unit masks (for GADM and LUH2 as set up     ##
## above). Check if these are still valid for the datasets you are using.     ##
source(file.path("helper", "fix_admin_masks.R"))
## Function to match admin units to spatial resolution of spatial patterns    ##
source(file.path("helper", "match_admin_to_data.R"))
## Utility functions to use modal() with different "ties" parameters          ##
source(file.path("helper", "modal_ties.R"))
## Utility function to find national band in multi-band admin unit dataset    ##
source(file.path("helper", "find_national_band.R"))
## Utility function to load and gap-fill (HYDE) area file                     ##
source(file.path("helper", "load_hyde_area.R"))
## Spatial aggregation function that uses C library loaded below              ##
source(file.path("helper", "array_aggregate.R"))
################################################################################

################################################################################
## C library offering raster aggregation functions; included version is       ##
## compiled for 64-bit Linux. Source code available in helper directory to    ##
## compile for other OS using R CMD SHLIB                                     ##
if (file.exists(file.path("helper", paste0("rescale", .Platform$dynlib.ext)))) {
  dyn.load(file.path("helper", paste0("rescale", .Platform$dynlib.ext)))
} else {
  stop(
    paste(
      "Please compile", file.path("helper", "rescale.c"),
      "for your operating system", Sys.info()$sysname,
      "and update dyn.load() call in landuse_setup.R"
    )
  )
}
################################################################################

# Use simple quotation marks in print statements
options(useFancyQuotes = FALSE)
# Do not convert strings to factors in data.frames.
options(stringsAsFactors = FALSE)

if (!all(manure_threshold_res %in% c("source", "target"))) {
  stop(
    paste(
      "Invalid option(s)",
      toString(
        sQuote(unique(setdiff(manure_threshold_res, c("source", "target"))))
      ),
      "in manure_threshold_res. Valid options:",
      toString(sQuote(c("source", "target")))
    )
  )
}

if (!extend_source_period %in% c("replicate", "zero")) {
  stop(
    paste(
      "Invalid option(s)",
      toString(sQuote(setdiff(extend_source_period, c("replicate", "zero")))),
      "in extend_source_period. Valid options:",
      toString(sQuote(c("replicate", "zero")))
    )
  )
}
