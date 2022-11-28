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
## The target resolution of the derived land use dataset is determined by the ##
## spatial resolution of the GADM mask of administrative units and cannot be  ##
## finer than the source resolution of any of the gridded source datasets.    ##
################################################################################

################################################################################
## Basic setup:                                                               ##
## Base directory. Landuse source datasets and scripts are generally located  ##
## under this base directory.                                                 ##
landuse_dir <- ""
if (nchar(landuse_dir) > 0) {
  setwd(landuse_dir)
}
## General note: Spatial resolution of final dataset is determined by         ##
## spatial resolution of GADM mask (grid-to-country association).             ##
## Temporal range is limited by availability of HYDE cropland data but can    ##
## also be shorter.                                                           ##
## Period to process:                                                         ##
output_period <- c(1500, 2017)
## Optional version strings to use in automatically created filenames (leave  ##
## empty in order not to use any version string)                              ##
## This allows to distinguish different versions of the various source        ##
## datasets. In order to limit the length of filenames provide only for       ##
## datasets where you intend to use more than one version.                    ##
aquastat_version_string <- ""
fao_version_string <- ""
gadm_version_string <- ""
gaez_version_string <- ""
hyde_version_string <- ""
mirca_version_string <- ""
monfreda_version_string <- ""
ramankutty_version_string <- ""
################################################################################


################################################################################
## Dataset-specific setup                                                     ##
################################################################################

################################################################################
## AQUASTAT setup                                                             ##
## AQUASTAT provides time series of irrigated harvested areas for a number of ##
## crops/crop groups at the country scale. These are combined with harvested  ##
## area data from MIRCA2000 (setup further below).                            ##
## CSV file downloaded from AQUASTAT website                                  ##
aquastat_file <- stop("Set 'aquastat_file' in landuse_setup.R")
## If file has empty rows at the beginning indicate here, otherwise set to 0  ##
aquastat_file_empty_rows <- 2
## Are all columns named? In versions downloaded over a period of several     ##
## months the last column is missing a name. Check your version.              ##
aquastat_file_all_column_names <- FALSE
## Define for each column in the AQUASTAT file the  data type (make sure this ##
## matches the file you downloaded).                                          ##
aquastat_file_column_classes <-  c(
  "character",
  "integer",
  "character",
  "integer",
  "integer",
  "numeric",
  "character",
  "character",
  "character"
)
## Index of the column which contains Metadata Confirm in your download.      ##
aquastat_file_metadata_col <- 8
## Depending on when you downloaded AQUASTAT data the country codes either    ##
## correspond to country codes or M49 codes used in FAOSTAT. Check manually   ##
## and set which codes to use for naming consistency checks.                  ##
## Options: "Country.Code" or "M49.Code"                                      ##
aquastat_file_use_FAOSTAT_country_col <- "M49.Code"
## The script read_AQUASTAT.R can either run interactively and ask the user   ##
## how to handle data points with additional metadata or apply a set of       ##
## rules default. See read_AQUASTAT.R for more details.                       ##
aquastat_file_run_interactively <- TRUE
## Area unit used in aquastat_file. Confirm on AQUASTAT website. Not included ##
## in downloaded file.                                                        ##
aquastat_area_source_units <- "1000 ha"
## Mapping between AQUASTAT and MIRCA2000 crops, usually included in toolbox  ##
## distribution. Update by hand if names have changed in either dataset.      ##
aquastat_mapping <- read.csv(
  "crop_types_aquastat.csv",
  stringsAsFactors = FALSE,
  comment.char = "#"
)
## Area unit to be used in further processing. Usually "ha" to be consistent  ##
## with other datasets. Set to aquastat_area_source_units if you want no unit ##
## conversion.                                                                ##
aquastat_area_units <- "ha"
################################################################################

################################################################################
## FAOSTAT setup                                                              ##
## FAOSTAT provides time series harvested areas for many crops/crop groups at ##
## the country scale.
## Directory where FAOSTAT data are saved. You may add a version number or    ##
## download date to distinguish different data versions.                      ##
faostat_dir <- "Faostat"
## Full datasets downloaded from FAOSTAT website and extracted from ZIP       ##
## Confirm filenames with your download.                                      ##
fao_production_file <- file.path(
  faostat_dir,
  "Production_Crops_Livestock_E_All_Data.csv"
)
fao_landuse_file <- file.path(faostat_dir, "Inputs_LandUse_E_All_Data.csv")
## Does the FAOSTAT production data include livestock? Structure of FAOSTAT   ##
## database was changed recently. If TRUE, read_FAOSTAT.R will attempt to     ##
## filter only crop-related data.
fao_production_file_includes_livestock <- TRUE
## The following tables can be downloaded from the FAOSTAT website under      ##
## "Definitions and standards". You should download them when downloading the ##
## main FAOSTAT data used in read_FAOSTAT.R                                   ##
## Production Item list:                                                      ##
fao_production_item_file <-
  stop("Set 'fao_production_item_file' in landuse_setup.R")
## Production Item Group list:                                                ##
fao_production_item_group_file <-
  stop("Set 'fao_production_item_group_file' in landuse_setup.R")
## Production Country list:                                                   ##
fao_production_country_file <-
  stop("Set 'fao_production_country_file' in landuse_setup.R")
## Production Country Group list:                                             ##
fao_production_country_group_file <-
  stop("Set 'fao_production_country_group_file' in landuse_setup.R")
## Land use Country list:                                                     ##
fao_landuse_country_file <-
  stop("Set 'fao_landuse_country_file' in landuse_setup.R")
## Land use Country Group list:                                               ##
fao_landuse_country_group_file <-
  stop("Set 'fao_landuse_country_group_file' in landuse_setup.R")
## Units to be used for FAOSTAT data (data are converted from their source    ##
## unit automatically by read_FAOSTAT.R. fao_area_units should be identical   ##
## to aquastat_area_units and monfreda_area_units.                            ##
fao_area_units <- "ha"
fao_yield_units <- "t/ha"
fao_production_units <- "t"
## Fix for Cyprus:                                                            ##
## FAOSTAT only has one Cyprus, which seems to cover the whole of Cyprus      ##
## until 1974 and then only refer to Southern Cyprus starting in 1975.        ##
## Agricultural statistics for Northern Cyprus seem to be missing.            ##
## MIRCA only has 1 spatial unit covering all of Cyprus.                      ##
## GADM (v3.6) distinguishes between Southern and Northern Cyprus.            ##
## If you use a newer version of FAOSTAT check if this is still true.         ##
## HYDE currently uses total cropland from FAOSTAT to scale gridded cropland  ##
## in all of Cyprus even after 1975, so using FAOSTAT harvested areas just    ##
## for Southern Cyprus leads to a bias -> for the moment, don't fix FAOSTAT.  ##
fix_cyprus <- FALSE
## Some FAOSTAT crops are products of other crops; only assign areas to       ##
## primary crops and drop areas for products.                                 ##
## Crops in this list will not be disaggregated to the grid.                  ##
## Use FAOSTAT names or a Monfreda name added to FAOSTAT country-level data   ##
## by country_level_data.R.                                                   ##
fao_drop_crops <- c(
  # Use Seed cotton, drop:
  "Cotton lint",
  "Cottonseed",
  # Use Cassava (the root), drop:
  "Cassava leaves",
  # Use Oil palm fruit, drop:
  "Oil, palm",
  "Palm kernels",
  # Use "Rice, paddy", drop:
  "Rice, paddy (rice milled equivalent)",
  # Use Kapok fruit, drop:
  "Kapok fibre",
  "Kapokseed in shell",
  "kapokseed",
  "kapokfiber"
)
################################################################################

################################################################################
## GADM administrative units mask                                             ##
## GADM data is used for country and region delineation                       ##
## These files should have been created using scripts in ../gadm/             ##
## Make sure that you create gridded administrative areas in the desired      ##
## target resolution of the land use dataset created by scripts in this       ##
## directory.                                                                 ##
## Administrative levels 0-2; codes only, by default called something like    ##
## gadm_level0_1_2_*.nc.                                                      ##
gadmlevel_file <- stop("Set 'gadmlevel_file' in landuse_setup.R")
## List linking codes in gadmlevel_file to names (CSV expected)               ##
## By default called something like "gadm_level0_1_2_*_indices.csv"           ##
gadmlevel_names_file <- stop("Set 'gadmlevel_names_file' in landuse_setup.R")
## Raster with number of unique administrative level 0 shapes in each cell    ##
## (as opposed to gadm_file and gadmlevel_file which always return unit with  ##
## largest area in cell). By default called something like ncountry_*.nc      ##
gadmborder_file <- stop("Set 'gadmborder_file' in landuse_setup.R")
## Mapping between FAOSTAT countries and GADM codes
## fao_gadm_country_mapping.R matches FAOSTAT countries with GADM codes
## fao_compound_countries.R lists FAOSTAT countries that split into other     ##
## FAOSTAT countries at some point in time, e.g. USSR
fao_gadm_mapping_file <- file.path(
  "helper",
  c("fao_gadm_country_mapping.R", "fao_compound_countries.R")
)
## fao_mirca_country_mapping.R matches MIRCA unit codes with FAOSTAT          ##
## countries and GADM level 1 units for select countries                      ##
fao_mirca_mapping_file <- "helper/fao_mirca_country_mapping.R"
################################################################################

################################################################################
## GAEZ multicropping suitability                                             ##
## Multicropping suitability is used in combination with irrigated and        ##
## rainfed cropland extent (HYDE) to set an upper threshold for harvested     ##
## area in each cell.                                                         ##
## Note: multi_cropping_suitability_GAEZ.R uses GAEZ version 3 to calculate   ##
## multicropping suitability. GAEZ version 4 also allows direct download of   ##
## multicropping suitability, but distinguishes more classes. If switching to ##
## version 4 scripts need updating.                                           ##
## Directory where climatic indicators from GAEZ v3 have been downloaded      ##
gaez_dir <- "GAEZ"
## GAEZ climatic indicator variables used.                                    ##
## Download data to subdirectory gaez_dir and decompress each ZIP file to a   ##
## subdirectory corresponding to its variable name.                           ##
gaez_variables <- c(
  "frost_free_period",
  "reference_length_growing_period",
  "temperature_growing_period",
  "Tsum_frost_free_period",
  "Tsum_growing_temperature_period",
  "thermal_climates"
)
################################################################################

################################################################################
## HYDE cropland setup                                                        ##
## HYDE provides time series of gridded cropland with a distinction into      ##
## rainfed and irrigated cropland.                                            ##
## Target resolution cannot be finer than HYDE, but can be coarser.           ##
## hyde_period: first and last year covered by HYDE time series. Must match   ##
## STARTYEAR and LASTYEAR in shell scripts used to process the HYDE download. ##
hyde_period <- c(1500, 2017)
## By default, NetCDF subdirectory created by shell scripts should include    ##
## years covered by data.                                                     ##
hyde_netcdf <- file.path(
  "HYDE",
  paste0("NetCDF_full", hyde_period[1], "-", hyde_period[2])
)
## Expected unit in gridded HYDE data                                         ##
hyde_area_units <- "km2"
## Raster file containing areas of HYDE grid cells. Part of general_files.zip ##
## from HYDE download repository.                                             ##
hyde_area_file <- "HYDE/general_files/garea_cr.asc"
## Unit of hyde_area_file. hyde_area_file is used if main HYDE NetCDFs are    ##
## not in absolute unit but a fraction.                                       ##
hyde_area_file_units <- "km2"
## Set up file names and NetCDF variable names for main HYDE variables used.  ##
## Names below should be correct if running shell scripts in HYDE with        ##
## default settings.                                                          ##
if (exists("hyde_version_string") && nchar(hyde_version_string) > 0) {
  hyde_cropland_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_cropland_annual_",
      hyde_period[1], "_", hyde_period[2], "_",
      hyde_version_string,
      ".nc4"
    )
  )
  hyde_irrigated_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_tot_irri_annual_",
      min(hyde_period), "_", max(hyde_period), "_",
      hyde_version_string,
      ".nc4"
    )
  )
  hyde_rainfed_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_tot_rainfed_annual_",
      min(hyde_period), "_", max(hyde_period), "_",
      hyde_version_string,
      ".nc4"
    )
  )
  hyde_grazing_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_grazing_annual_",
      min(hyde_period), "_", max(hyde_period), "_",
      hyde_version_string,
      ".nc4"
    )
  )
} else {
  hyde_cropland_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_cropland_annual_",
      hyde_period[1], "_", hyde_period[2],
      ".nc4"
    )
  )
  hyde_irrigated_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_tot_irri_annual_",
      min(hyde_period), "_", max(hyde_period),
      ".nc4"
    )
  )
  hyde_rainfed_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_tot_rainfed_annual_",
      min(hyde_period), "_", max(hyde_period),
      ".nc4"
    )
  )
  hyde_grazing_filename <- file.path(
    hyde_netcdf,
    paste0(
      "hyde_grazing_annual_",
      min(hyde_period), "_", max(hyde_period),
      ".nc4"
    )
  )
}
hyde_cropland_varname <- "cropland"
hyde_irrigated_varname <- "tot_irri"
hyde_rainfed_varname <- "tot_rainfed"
hyde_grazing_varname <- "grazing"
################################################################################

################################################################################
## MIRCA setup                                                                ##
## MIRCA condensed cropping calendars provide irrigated harvested areas for   ##
## several crops/crop groups per country/region (in some large countries)     ##
## These are combined with AQUASTAT data (above) and used to distinguish      ##
## total harvested areas (FAOSTAT) into into rainfed and irrigated.           ##
## Download and decompress datasets listed in MIRCA2000/README before running ##
## any R scripts in this directory.                                           ##
## Base directory containing MIRCA data                                       ##
mirca_base <- "MIRCA2000"
## Spatial area unit used in MIRCA2000 data                                   ##
mirca_area_units <- "ha"
## Reference year                                                             ##
mirca_refyear <- 2000
## Read names of crops included in MIRCA2000 (file provided by toolbox)       ##
mirca_names <- readLines(file.path(mirca_base, "mirca_names.txt"))
## Filenames of condensed cropping calendars, one for each year in            ##
## mirca_refyear (only irrigated cropping calendar is used in current         ##
## implementation)                                                            ##
rainfed_cropping_calendar_file <- c(
  "2000" = file.path(
    mirca_base,
    "condensed_cropping_calendars/cropping_calendar_rainfed.txt"
  )
)
irrigated_cropping_calendar_file <- c(
  "2000" = file.path(
    mirca_base,
    "condensed_cropping_calendars/cropping_calendar_irrigated.txt"
  )
)
## Read unit code lists                                                       ##
mirca_unit_names <- read.table(
  file.path(mirca_base, "unit_code_grid", "unit_name.txt"),
  sep = "\t",
  header = TRUE,
  strip.white = TRUE,
  quote = "",
  flush = TRUE,
  colClasses = c("integer", "character")
)
## Raster with cell areas used by the gridded MIRCA2000 datasets
mirca_area_file <- file.path(
  mirca_base,
  "cell_area_grid",
  "cell_area_ha_05mn.asc"
)
## Spatial area unit used in mirca_area_file
mirca_area_file_units <- "ha"
################################################################################

################################################################################
## Monfreda harvested areas                                                   ##
## Monfreda provides gridded harvested areas for 175 crops (no distinction    ##
## between rainfed and irrigated).                                            ##
## Spatial patterns from this dataset are used as base patterns to            ##
## disaggregate country sums.                                                 ##
## Base directory containing Monfreda data                                    ##
monfreda_base <-  "Monfreda"
## Monfreda data have been released in either Geotiff or NetCDF formats.      ##
## Select your version.                                                       ##
monfreda_format <- "Geotiff"
## NetCDFs include a total of 6 layers within one file; these variables       ##
## define the layers for harvested area and quality flag.                     ##
monfreda_area_layer <- 5
monfreda_area_quality_layer <- 3
## Variable names: For Geotiff first name is for harvested area file, second  ##
## name is for corresponding data quality file. Used to construct file names. ##
monfreda_file_var <- list(
  Geotiff = c("HarvestedAreaHectares", "DataQuality_HarvestedArea"),
  NetCDF = "AreaYieldProduction"
)
## File extension based on monfreda_format selected above.                    ##
monfreda_file_ext <- c(Geotiff = "tif", NetCDF = "nc")
## Area unit used in files                                                    ##
monfreda_area_units <- "ha"
## Reference year covered by Monfreda data                                    ##
monfreda_refyear <- 2000
## Read Monfreda crops names (file provided by toolbox)                       ##
monfreda_names <- readLines(file.path(monfreda_base, "monfreda_names.txt"))
## Data directory structure depending monfreda_format. Confirm with your      ##
## download.                                                                  ##
monfreda_datadir <- c(
  Geotiff = file.path(
    "HarvestedAreaYield175Crops_Geotiff",
    "HarvestedAreaYield175Crops_Geotiff"
  ),
  NetCDF = "HarvestedAreaYield175Crops_NetCDF"
)
## Monfreda sometimes has extremely small areas. To avoid flagging such crop  ##
## areas as inconsistencies that need to be dealt with, remove entries below  ##
## cutoff value; Note: MIRCA2000 has accuracy of 1e-6                         ##
monfreda_cutoff <- 1e-6
## Due to different country definitions used here and by Monfreda cells may   ##
## be assigned to the wrong country along borders. Do not assign a Monfreda   ##
## crop to a country if more than the threshold of cells are border cells as  ##
## defined by gadmborder_file                                                 ##
monfreda_threshold_border <- 0.6
################################################################################

################################################################################
## Ramankutty cropland                                                        ##
## This is the cropland dataset used by Monfreda. Needed for consistency      ##
## check.                                                                     ##
## Directory containing decompressed Ramankutty data                          ##
ramankutty_base <- stop("Set 'ramankutty_base' in landuse_setup.R")
## Raster file providing gridded cropland extent
ramankutty_cropland_file <- file.path(ramankutty_base, "Cropland2000_5m.tif")
## Spatial area unit used in ramankutty_cropland_file
ramankutty_units <- ""
#############################################################################

################################################################################
## crop type mapping between FAOSTAT, MIRCA, and Monfreda                     ##
## This file provides mapping between Monfreda crops, FAOSTAT crops and MIRCA ##
## crops. It has been put together by hand and may need to be updated for     ##
## new versions of any of the three datasets.                                 ##
mapping_file <- "crop_types_Monfreda_FAOSTAT_MIRCA.csv"
crop_type_mapping <- read.csv(
  mapping_file,
  stringsAsFactors = FALSE,
  comment.char = "#"
)
################################################################################


################################################################################
## Load required R packages. These may need to be installed first.            ##
library(ncdf4)
library(raster)
library(udunits2)
library(stringi)
library(abind)
library(data.table)
################################################################################


################################################################################
## Helper functions for LPJmL input format                                    ##
## The script lpjml_format_helper_functions.R is saved in the parent          ##
## directory by default.                                                      ##
if (file.exists(file.path("..", "lpjml_format_helper_functions.R"))) {
  source(file.path("..", "lpjml_format_helper_functions.R"))
} else {
  stop("Please update path to script with LPJmL input format helper function")
}
## Further helper functions:                                                  ##
## Gap-filling function for FAOSTAT time series data:                         ##
source(file.path("helper", "fill_timeseries.R"))
## Function to load Monfreda data:                                            ##
source(file.path("helper", "get_crop_monfreda.R"))
## Funtion to load and gap-fill HYDE area data                                ##
source(file.path("helper", "load_hyde_area.R"))
## Function to load one year of HYDE data and aggregate/crop to target        ##
## resolution and extent                                                      ##
source(file.path("helper", "load_hyde_yeardata.R"))
## Function that takes HYDE time series NetCDF and creates a file with the    ##
## maximum value across time in each cell; requires CDO tools                 ##
source(file.path("helper", "create_hyde_timeseries_max.R"))
## Function to load one year of gridded crop-specific harvested area time     ##
## series data from NetCDF file and aggregate/crop to target resolution and   ##
## extent
source(file.path("helper", "load_ha_yeardata.R"))
## Spatial aggregation function that uses C library loaded below              ##
source(file.path("helper", "array_aggregate.R"))
## Transformation functions                                                   ##
source(file.path("helper", "transformation_functions.R"))
## Raster helpers                                                             ##
source(file.path("helper", "raster_helpers.R"))
## Utility functions to use modal() with different "ties" parameters          ##
source(file.path("helper", "modal_ties.R"))
## Function to set up "nes" groups in FAOSTAT production data
source(file.path("helper", "setup_nes_groups.R"))
################################################################################


################################################################################
## C library offering raster aggregation functions; included version is       ##
## compiled for 64-bit Linux. Source code available in helper directory to    ##
## compile for other OS using R CMD SHLIB                                     ##
if (file.exists(file.path("helper", paste0("rescale", .Platform$dynlib.ext)))) {
  dyn.load(file.path("helper", paste0("rescale", .Platform$dynlib.ext)))
} else {
  stop(
    "Please compile ", file.path("helper", "rescale.c"),
    " for your operating system ", Sys.info()["sysname"],
    " and update dyn.load() call in landuse_setup.R"
  )
}
################################################################################

################################################################################
## Set up working directory with intermediate results, name depends on        ##
## spatial resolution of GADM mask.                                           ##
## "tmp" directory should only contain intermediate results that are          ##
## independent of spatial resolution/country mask.
gadm_raster <- raster(gadmlevel_file, layer = 0)
tmp_res <- unique(res(gadm_raster) * ifelse(res(gadm_raster) >= 1/60, 60, 3600))
tmp_string <- paste(
  round(tmp_res),
  unique(ifelse(res(gadm_raster) >= 1/60, "min", "sec")),
  sep = "",
  collapse = "_by_"
)
working_dir <- file.path("tmp", paste0("work_", tmp_string))
if (!dir.exists(working_dir)) {
  dir.create(working_dir, recursive = TRUE)
}
################################################################################


################################################################################
## Intermediate results generated by various scripts                          ##
##                                                                            ##
## Data prepared by read_AQUASTAT.R                                           ##
## aquastat_rdata contains data read in by read_AQUASTAT.R. You may want to   ##
## to add a download date or version number to filename to distinguish more   ##
## than one version of data. By default, saved to tmp subdirectory.           ##
if (exists("aquastat_version_string") && nchar(aquastat_version_string) > 0) {
  aquastat_rdata <- file.path(
    "tmp",
    paste0(
      "AQUASTAT_irrigated_harvested_areas_",
      aquastat_version_string,
      ".RData"
    )
  )
} else {
  aquastat_rdata <- file.path("tmp", "AQUASTAT_irrigated_harvested_areas.RData")
}
##                                                                            ##
## RData files with FAOSTAT landuse and production data                       ##
## Created by read_FAOSTAT.R to be used in further processing by other        ##
## scripts. Independent of spatial resolution, saved in tmp directory.        ##
## You may want to add a version number or download date to filenames to      ##
## distinguish different versions of FAOSTAT data.                            ##
if (exists("fao_version_string") && nchar(fao_version_string) > 0) {
  fao_production_RData <- file.path(
    "tmp",
    paste0("FAOSTAT_production_", fao_version_string, ".RData")
  )
  fao_landuse_RData <- file.path(
    "tmp",
    paste0("FAOSTAT_landuse_", fao_version_string, ".RData")
  )
} else {
  fao_production_RData <- file.path("tmp", "FAOSTAT_production.RData")
  fao_landuse_RData <- file.path("tmp", "FAOSTAT_landuse.RData")
}
## RData file with gap-filled production (by default, attaches "_gapfilled"   ##
## to name of fao_production_RData.                                           ##
fao_production_gapfilled_RData <- gsub(
  ".RData",
  "_gapfilled.RData",
  fao_production_RData
)
##                                                                            ##
## File containing Monfreda harvested areas aggregated to GADM country mask.  ##
## File is resolution-specific so save in working_dir. The list of countries  ##
## varies between FAOSTAT versions so this depends on the specific version of ##
## FAOSTAT, GADM and Monfreda used.                                           ##
## Generated by country_level_data.R                                          ##
monfreda_gadm_sums_RData <- file.path(
  working_dir,
  paste0(
    "monfreda_GADM_country_sums",
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
      exists("monfreda_version_string") && nchar(monfreda_version_string) > 0,
      paste0("_", monfreda_version_string),
      ""
    ),
    ".RData"
  )
)
##                                                                            ##
## File containing table with matching Monfreda and FAOSTAT crops in each     ##
## country. File is resolution-specific so save in working_dir. This depends  ##
## on the specific version of FAOSTAT, GADM and Monfreda used.                ##
## Generated by country_level_data.R and used by subsequent scripts           ##
fao_monfreda_country_RData <- file.path(
  working_dir,
  paste0(
    "fao_monfreda_GADM_country_production",
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
      exists("monfreda_version_string") && nchar(monfreda_version_string) > 0,
      paste0("_", monfreda_version_string),
      ""
    ),
    ".RData"
  )
)
##                                                                            ##
## Crop-specific harvested area base patterns and gapfilling statistics       ##
## NetCDF and RData created by harvested_fraction.R. This depends on the      ##
## version of Monfreda and Ramankutty used. Gapfilling also depends on the    ##
## version of HYDE used. Additionally, uses information from FAOSTAT about    ##
## which crops are present in which countries.                                ##
harvested_fraction_filename <- file.path(
  working_dir,
  paste0(
    "harvested_fraction_GADM",
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
      exists("hyde_version_string") && nchar(hyde_version_string) > 0,
      paste0("_", hyde_version_string),
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
    ".nc"
  )
)
harvested_fraction_fill_statistics_RData <- file.path(
  working_dir,
  paste0(
    "harvested_fraction_GADM_fill_statistics",
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
      exists("hyde_version_string") && nchar(hyde_version_string) > 0,
      paste0("_", hyde_version_string),
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
    ".RData"
  )
)
##                                                                            ##
## Maximum cropland extent over whole period                                  ##
## Used/generated by harvested_fraction.R                                     ##
if (exists("hyde_version_string") && nchar(hyde_version_string) > 0) {
  hyde_max_cropland_filename <- file.path(
    working_dir,
    paste0(
      "hyde_", hyde_cropland_varname, "_max_",
      hyde_period[1], "_", hyde_period[2], "_",
      hyde_version_string,
      ".nc4"
    )
  )
  hyde_max_irrigated_filename <- file.path(
    working_dir,
    paste0(
      "hyde_", hyde_irrigated_varname, "_max_",
      hyde_period[1], "_", hyde_period[2], "_",
      hyde_version_string,
      ".nc4"
    )
  )
  hyde_max_rainfed_filename <- file.path(
    working_dir,
    paste0(
      "hyde_", hyde_rainfed_varname, "_max_",
      hyde_period[1], "_", hyde_period[2], "_",
      hyde_version_string,
      ".nc4"
    )
  )
} else {
  hyde_max_cropland_filename <- file.path(
    working_dir,
    paste0(
      "hyde_", hyde_cropland_varname, "_max_",
      hyde_period[1], "_", hyde_period[2],
      ".nc4"
    )
  )
  hyde_max_irrigated_filename <- file.path(
    working_dir,
    paste0(
      "hyde_", hyde_irrigated_varname, "_max_",
      hyde_period[1], "_", hyde_period[2],
      ".nc4"
    )
  )
  hyde_max_rainfed_filename <- file.path(
    working_dir,
    paste0(
      "hyde_", hyde_rainfed_varname, "_max_",
      hyde_period[1], "_", hyde_period[2],
      ".nc4"
    )
  )
}
##                                                                            ##
## GAEZ multicropping suitability files at GADM resolution                    ##
## Generated by multi_cropping_suitability_GAEZ.R                             ##
## This can be any format supported by the R raster package, e.g. ".asc" or   ##
## ".nc" for ASCII grid or NetCDF.                                            ##
if (exists("gaez_version_string") && nchar(gaez_version_string) > 0) {
  gaez_multicropping_suit_rf_file <- file.path(
    working_dir,
    paste0(
      "multicropping_suitability_GAEZ_rainfed_",
      gaez_version_string,
      ".asc"
    )
  )
  gaez_multicropping_suit_ir_file <- file.path(
    working_dir,
    paste0(
      "multicropping_suitability_GAEZ_irrigated_",
      gaez_version_string,
      ".asc"
    )
  )
} else {
  gaez_multicropping_suit_rf_file <- file.path(
    working_dir,
    "multicropping_suitability_GAEZ_rainfed.asc"
  )
  gaez_multicropping_suit_ir_file <- file.path(
    working_dir,
    "multicropping_suitability_GAEZ_irrigated.asc"
  )
}
##                                                                            ##
## Country-scale time series of aggregated HYDE cropland and HYDE cropland    ##
## multiplied with multicropping suitability generated by                     ##
## split_global_harvested_areas_into_rainfed_irrigated.R. Country list        ##
## depends on FAOSTAT version used.                                           ##
hyde_country_sums_RData <- file.path(
  working_dir,
  paste0(
    "hyde_country_sums_",
    max(min(output_period), min(hyde_period)), "-",
    min(max(output_period), max(hyde_period)),
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
    ".RData"
  )
)
## Country-scale time series of crop-specific irrigated and total harvested   ##
## areas. Generated by split_global_harvested_areas_into_rainfed_irrigated.R  ##
## The script allows different options for consistency scaling. See details   ##
## in split_global_harvested_areas_into_rainfed_irrigated.R. Set here which   ##
## version to use for rainfed and irrigated crops.                            ##
irrigated_version_to_use <- "irr_ha_timeseries_unconstrained"
total_version_to_use <- "tot_ha_timeseries"
## Main versions are saved to this file for further use:                      ##
ha_country_timeseries_RData <- file.path(
  working_dir,
  paste0(
    "GADM_country_timeseries_ha_croparea_maxsuit",
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
    ".RData"
  )
)
## Alternative versions are saved to this file:                               ##
alt_ha_country_timeseries_RData <- file.path(
  working_dir,
  paste0(
    "GADM_country_timeseries_ha_alternatives",
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
    ".RData"
  )
)
##                                                                            ##
## Variable names for crop-specific time series of harvested areas in NetCDF  ##
rainfed_output_name <- "rainfed_harvested_area"
irrigated_output_name <- "irrigated_harvested_area"
rainfed_output_sum_name <- "rainfed_harvested_area_sum"
irrigated_output_sum_name <- "irrigated_harvested_area_sum"
total_output_sum_name <- "total_harvested_area_sum"
## The timeseries NetCDF is split into blocks to limit filesize of individual ##
## files. These files are saved in working_dir by default.                    ##
## Filename base (years covered and optional version strings are added        ##
## automatically)                                                             ##
ha_timeseries_filename_base <- file.path(
  working_dir,
  "harvested_area_GADM_timeseries"
)
## Split whole time series into chunks of this number of years.               ##
ha_timeseries_chunk_length <- 10
################################################################################


################################################################################
## Crop aggregation from full list of FAOSTAT/Monfreda types to LPJmL CFTs.   ##
## This will depend on the CFTs implemented in your version of LPJmL. The     ##
## file that comes with the toolbox provides a standard list of LPJmL CFTs at ##
## the time of release.                                                       ##
## You can use other values for 'aggregation_name' for other lists of CFTs.   ##
crop_aggregation_file <- paste0(
  "crop_types_FAOSTAT_LPJmL_",
  ifelse(
    exists("aggregation_name") && nchar(aggregation_name) > 0,
    aggregation_name,
    "default"
  ),
  ".csv"
)
crop_aggregation_types <- read.csv(
  crop_aggregation_file,
  stringsAsFactors = FALSE,
  comment.char = "#"
)
## Filename base for aggregated timeseries generated by aggregate_timeseries.R##
## These files are saved in working_dir by default. Years covered and         ##
## optional version strings are added automatically.                          ##
aggregated_timeseries_filename_base <- file.path(
  working_dir,
  paste0(
    "harvested_area_GADM_timeseries_",
    ifelse(
      exists("aggregation_name") && nchar(aggregation_name) > 0,
      paste0(aggregation_name, "_"),
      ""
    ),
    "cft_aggregation"
  )
)
################################################################################

## Use simple quotes in all print statements
options(useFancyQuotes = FALSE)
## Allow longer warning messages
options(warning.length = 8000)
