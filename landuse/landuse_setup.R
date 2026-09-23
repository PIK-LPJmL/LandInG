################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Helper functions for LPJmL input format and basic LandInG setup.           ##
## The script landing_setup.R is saved in the parent directory by default.    ##
################################################################################
if (file.exists("../landing_setup.R")) {
  source("../landing_setup.R", chdir = TRUE)
  LandInG_setup$landuse <- list()
} else if (!exists("LandInG_setup") || !is.environment(LandInG_setup)) {
  stop("Please update path to script with LandInG setup script")
}
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
LandInG_setup$landuse$landuse_dir <- ""
if (nchar(LandInG_setup$landuse$landuse_dir) > 0) {
  setwd(LandInG_setup$landuse$landuse_dir)
}
## General note: Spatial resolution of final dataset is determined by         ##
## spatial resolution of GADM mask (grid-to-country association).             ##
## Temporal range is limited by availability of HYDE cropland data but can    ##
## also be shorter.                                                           ##
## Period to process:                                                         ##
LandInG_setup$landuse$output_period <- c(1500, 2017)
## Optional version strings to use in automatically created filenames (leave  ##
## empty in order not to use any version string)                              ##
## This allows to distinguish different versions of the various source        ##
## datasets. In order to limit the length of filenames provide only for       ##
## datasets where you intend to use more than one version.                    ##
LandInG_setup$landuse$aquastat_version_string <- ""
LandInG_setup$landuse$fao_version_string <- ""
LandInG_setup$landuse$gadm_version_string <- ""
LandInG_setup$landuse$gaez_version_string <- ""
LandInG_setup$landuse$hyde_version_string <- ""
LandInG_setup$landuse$mirca_version_string <- ""
LandInG_setup$landuse$mon_version_string <- ""
LandInG_setup$landuse$ram_version_string <- ""
################################################################################


################################################################################
## Helper functions used across multiple scripts:                             ##
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
## Transformation functions                                                   ##
source(file.path("helper", "transformation_functions.R"))
## Terra helpers                                                              ##
source(file.path("helper", "terra_helpers.R"))
## Utility functions to use modal() with different "ties" parameters          ##
source(file.path("helper", "modal_ties.R"))
## Function to set up "nes" groups in FAOSTAT production data                 ##
source(file.path("helper", "setup_nes_groups.R"))
## Function to add optional version strings defined below to file names       ##
source(file.path("helper", "add_version_string.R"))
## Function to load harvested fraction pattern for one crop                   ##
source(file.path("helper", "load_ha_fraction.R"))
## Function to update country-level data generated by earlier LandInG version ##
source(file.path("helper", "update_country_data.R"))
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
LandInG_setup$landuse$aquastat_file <- stop("Set 'aquastat_file' in landuse_setup.R")
## If file has empty rows at the beginning indicate here, otherwise set to 0  ##
LandInG_setup$landuse$aquastat_file_empty_rows <- 2
## Are all columns named? In versions downloaded over a period of several     ##
## months the last column is missing a name. Check your version.              ##
LandInG_setup$landuse$aquastat_file_all_column_names <- FALSE
## Define for each column in the AQUASTAT file the  data type (make sure this ##
## matches the file you downloaded).                                          ##
LandInG_setup$landuse$aquastat_file_column_classes <-  c(
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
LandInG_setup$landuse$aquastat_file_metadata_col <- 8
## Depending on when you downloaded AQUASTAT data the country codes either    ##
## correspond to country codes or M49 codes used in FAOSTAT. Check manually   ##
## and set which codes to use for naming consistency checks.                  ##
## Options: "Country.Code" or "M49.Code"                                      ##
LandInG_setup$landuse$aquastat_file_use_FAOSTAT_country_col <- "M49.Code"
## The script read_AQUASTAT_legacy.R can either run interactively and ask the ##
## user how to handle data points with additional metadata or apply a set of  ##
## rules default. See read_AQUASTAT_legacy.R for more details.                ##
LandInG_setup$landuse$aquastat_file_run_interactively <- TRUE
## Area unit used in aquastat_file. Confirm on AQUASTAT website. Not included ##
## in downloaded file.                                                        ##
LandInG_setup$landuse$aquastat_area_source_units <- "1000 ha"
## Mapping between AQUASTAT and MIRCA2000 crops, usually included in toolbox  ##
## distribution. Update by hand if names have changed in either dataset.      ##
LandInG_setup$landuse$aquastat_mapping <- read.csv(
  "crop_types_aquastat.csv",
  stringsAsFactors = FALSE,
  comment.char = "#"
)
## Area unit to be used in further processing. Usually "ha" to be consistent  ##
## with other datasets. Set to aquastat_area_source_units if you want no unit ##
## conversion.                                                                ##
LandInG_setup$landuse$aquastat_area_units <- "ha"
################################################################################

################################################################################
## FAOSTAT setup                                                              ##
## FAOSTAT provides time series harvested areas for many crops/crop groups at ##
## the country scale.
## Directory where FAOSTAT data are saved. You may add a version number or    ##
## download date to distinguish different data versions.                      ##
LandInG_setup$landuse$faostat_dir <- "Faostat"
## Full datasets downloaded from FAOSTAT website and extracted from ZIP       ##
## Confirm filenames with your download.                                      ##
LandInG_setup$landuse$fao_production_file <- file.path(
  LandInG_setup$landuse$faostat_dir,
  "Production_Crops_Livestock_E_All_Data.csv"
)
LandInG_setup$landuse$fao_landuse_file <- file.path(
  LandInG_setup$landuse$faostat_dir,
  "Inputs_LandUse_E_All_Data.csv"
)
## Does the FAOSTAT production data include livestock? Structure of FAOSTAT   ##
## database was changed recently. If TRUE, read_FAOSTAT.R will attempt to     ##
## filter only crop-related data.
LandInG_setup$landuse$fao_production_file_includes_livestock <- TRUE
## The following tables can be downloaded from the FAOSTAT website under      ##
## "Definitions and standards". You should download them when downloading the ##
## main FAOSTAT data used in read_FAOSTAT.R                                   ##
## Production Item list:                                                      ##
LandInG_setup$landuse$fao_production_item_file <-
  stop("Set 'fao_production_item_file' in landuse_setup.R")
## Production Item Group list:                                                ##
LandInG_setup$landuse$fao_production_item_group_file <-
  stop("Set 'fao_production_item_group_file' in landuse_setup.R")
## Production Country list:                                                   ##
LandInG_setup$landuse$fao_production_country_file <-
  stop("Set 'fao_production_country_file' in landuse_setup.R")
## Production Country Group list:                                             ##
LandInG_setup$landuse$fao_production_country_group_file <-
  stop("Set 'fao_production_country_group_file' in landuse_setup.R")
## Land use Country list:                                                     ##
LandInG_setup$landuse$fao_landuse_country_file <-
  stop("Set 'fao_landuse_country_file' in landuse_setup.R")
## Land use Country Group list:                                               ##
LandInG_setup$landuse$fao_landuse_country_group_file <-
  stop("Set 'fao_landuse_country_group_file' in landuse_setup.R")
## Units to be used for FAOSTAT data (data are converted from their source    ##
## unit automatically by read_FAOSTAT.R. fao_area_units should be identical   ##
## to aquastat_area_units and mon_area_units.                            ##
LandInG_setup$landuse$fao_area_units <- "ha"
LandInG_setup$landuse$fao_yield_units <- "t/ha"
LandInG_setup$landuse$fao_production_units <- "t"
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
LandInG_setup$landuse$fix_cyprus <- FALSE
## Some FAOSTAT crops are products of other crops; only assign areas to       ##
## primary crops and drop areas for products.                                 ##
## Crops in this list will not be disaggregated to the grid.                  ##
## Use FAOSTAT names or a Monfreda name added to FAOSTAT country-level data   ##
## by country_level_data.R.                                                   ##
LandInG_setup$landuse$fao_drop_crops <- c(
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
LandInG_setup$landuse$gadmlevel_file <-
  stop("Set 'gadmlevel_file' in landuse_setup.R")
## List linking codes in gadmlevel_file to names (CSV expected)               ##
## By default called something like "gadm_level0_1_2_*_indices.csv"           ##
LandInG_setup$landuse$gadmlevel_names_file <-
  stop("Set 'gadmlevel_names_file' in landuse_setup.R")
## Raster with number of unique administrative level 0 shapes in each cell    ##
## (as opposed to gadm_file and gadmlevel_file which always return unit with  ##
## largest area in cell). By default called something like ncountry_*.nc      ##
LandInG_setup$landuse$gadmborder_file <-
  stop("Set 'gadmborder_file' in landuse_setup.R")
## Mapping between FAOSTAT countries and GADM codes
## fao_gadm_country_mapping.R matches FAOSTAT countries with GADM codes
## fao_compound_countries.R lists FAOSTAT countries that split into other     ##
## FAOSTAT countries at some point in time, e.g. USSR
LandInG_setup$landuse$fao_gadm_mapping_file <- file.path(
  "helper",
  c("fao_gadm_country_mapping.R", "fao_compound_countries.R")
)
## fao_mirca_country_mapping.R matches MIRCA unit codes with FAOSTAT          ##
## countries and GADM level 1 units for select countries                      ##
LandInG_setup$landuse$fao_mirca_mapping_file <- file.path(
  "helper",
  "fao_mirca_country_mapping.R"
)
################################################################################

################################################################################
## GAEZ multicropping suitability                                             ##
## Multicropping suitability is used in combination with irrigated and        ##
## rainfed cropland extent (HYDE) to set an upper threshold for harvested     ##
## area in each cell.                                                         ##
## Directory where data from GAEZ have been downloaded                        ##
LandInG_setup$landuse$gaez_dir <- "GAEZ"
## Settings for GAEZ version 3:                                               ##
## Script multi_cropping_suitability_GAEZ_v3.R uses GAEZ version 3 to         ##
## calculate multicropping suitability from climatic indicators.              ##
## GAEZ climatic indicator variables used.                                    ##
## Download data to subdirectory gaez_dir and decompress each ZIP file to a   ##
## subdirectory corresponding to its variable name.                           ##
LandInG_setup$landuse$gaez_v3_variables <- c(
  "frost_free_period",
  "reference_length_growing_period",
  "temperature_growing_period",
  "Tsum_frost_free_period",
  "Tsum_growing_temperature_period",
  "thermal_climates"
)
## Settings for GAEZ version 4:                                               ##
## GAEZ v4 also allows direct download of Multi-cropping class, but           ##
## distinguishes more classes. If output resolution equals GAEZ resolution    ##
## the script multi_cropping_suitability_GAEZ_v4.R will attempt to convert    ##
## Multi-cropping class directly. Otherwise, multicropping suitability is     ##
## derived from agroclimatic data similar to GAEZ version 3.                  ##
# List multiple cropping classes before agroclimatic variables
LandInG_setup$landuse$gaez_v4_variables <- c(
  "mci", # Multi-cropping class, irrigated
  "mcr", # Multi-cropping class, rainfed
  "lt3", # Temperature growing period LGPt10: Number of days when Ta >= 10°C
  "lgd", # Total number of growing period days
  "lt2", # Temperature growing period LGPt5: Number of days when Ta >= 5°C
  "ts3", # Annual accumulated temperature sum for days with Ta >= 10°C
  "ts2", # Annual accumulated temperature sum for days with Ta >= 5°C
  "mcl"  # Thermal Climate class
)
################################################################################

################################################################################
## HYDE cropland setup                                                        ##
## HYDE provides time series of gridded cropland with a distinction into      ##
## rainfed and irrigated cropland.                                            ##
## Target resolution cannot be finer than HYDE, but can be coarser.           ##
## hyde_period: first and last year covered by HYDE time series. Must match   ##
## STARTYEAR and LASTYEAR in shell scripts used to process the HYDE download. ##
LandInG_setup$landuse$hyde_period <- c(1500, 2017)
## By default, NetCDF subdirectory created by shell scripts should include    ##
## years covered by data.                                                     ##
LandInG_setup$landuse$hyde_netcdf <- file.path(
  "HYDE",
  paste0(
    "NetCDF_full", LandInG_setup$landuse$hyde_period[1],
    "-", LandInG_setup$landuse$hyde_period[2]
  )
)
## Expected unit in gridded HYDE data                                         ##
LandInG_setup$landuse$hyde_area_units <- "km2"
## Raster file containing areas of HYDE grid cells. Part of general_files.zip ##
## from HYDE download repository.                                             ##
LandInG_setup$landuse$hyde_area_file <- "HYDE/general_files/garea_cr.asc"
## Unit of hyde_area_file. hyde_area_file is used if main HYDE NetCDFs are    ##
## not in absolute unit but a fraction.                                       ##
LandInG_setup$landuse$hyde_area_file_units <- "km2"
## Set up file names and NetCDF variable names for main HYDE variables used.  ##
## Names below should be correct if running shell scripts in HYDE with        ##
## default settings.                                                          ##
LandInG_setup$landuse$hyde_cropland_filename <- file.path(
  LandInG_setup$landuse$hyde_netcdf,
  paste0(
    "hyde_cropland_annual_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)
LandInG_setup$landuse$hyde_irrigated_filename <- file.path(
  LandInG_setup$landuse$hyde_netcdf,
  paste0(
    "hyde_tot_irri_annual_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)
LandInG_setup$landuse$hyde_rainfed_filename <- file.path(
  LandInG_setup$landuse$hyde_netcdf,
  paste0(
    "hyde_tot_rainfed_annual_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)
LandInG_setup$landuse$hyde_grazing_filename <- file.path(
  LandInG_setup$landuse$hyde_netcdf,
  paste0(
    "hyde_grazing_annual_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)

LandInG_setup$landuse$hyde_cropland_varname <- "cropland"
LandInG_setup$landuse$hyde_irrigated_varname <- "tot_irri"
LandInG_setup$landuse$hyde_rainfed_varname <- "tot_rainfed"
LandInG_setup$landuse$hyde_grazing_varname <- "grazing"
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
LandInG_setup$landuse$mirca_dir <- "MIRCA2000"
## Spatial area unit used in MIRCA2000 data                                   ##
LandInG_setup$landuse$mirca_area_units <- "ha"
## Reference year                                                             ##
LandInG_setup$landuse$mirca_refyear <- 2000
## Read names of crops included in MIRCA2000 (file provided by toolbox)       ##
LandInG_setup$landuse$mirca_names <- readLines(
  file.path(LandInG_setup$landuse$mirca_dir, "mirca_names.txt")
)
## Filenames of condensed cropping calendars, one for each year in            ##
## mirca_refyear (only irrigated cropping calendar is used in current         ##
## implementation)                                                            ##
LandInG_setup$landuse$rainfed_cropping_calendar_file <- c(
  "2000" = file.path(
    LandInG_setup$landuse$mirca_dir,
    "condensed_cropping_calendars/cropping_calendar_rainfed.txt"
  )
)
LandInG_setup$landuse$irrigated_cropping_calendar_file <- c(
  "2000" = file.path(
    LandInG_setup$landuse$mirca_dir,
    "condensed_cropping_calendars/cropping_calendar_irrigated.txt"
  )
)
## Read unit code lists                                                       ##
LandInG_setup$landuse$mirca_unit_names <- read.table(
  file.path(LandInG_setup$landuse$mirca_dir, "unit_code_grid", "unit_name.txt"),
  sep = "\t",
  header = TRUE,
  strip.white = TRUE,
  quote = "",
  flush = TRUE,
  colClasses = c("integer", "character")
)
## Raster with cell areas used by the gridded MIRCA2000 datasets
LandInG_setup$landuse$mirca_area_file <- file.path(
  LandInG_setup$landuse$mirca_dir,
  "cell_area_grid",
  "cell_area_ha_05mn.asc"
)
## Spatial area unit used in mirca_area_file
LandInG_setup$landuse$mirca_area_file_units <- "ha"
################################################################################

################################################################################
## Monfreda harvested areas                                                   ##
## Monfreda provides gridded harvested areas for 175 crops (no distinction    ##
## between rainfed and irrigated).                                            ##
## Spatial patterns from this dataset are used as base patterns to            ##
## disaggregate country sums.                                                 ##
## Base directory containing Monfreda data                                    ##
LandInG_setup$landuse$mon_dir <-  "Monfreda"
## Monfreda data have been released in either Geotiff or NetCDF formats.      ##
## Select your version.                                                       ##
LandInG_setup$landuse$mon_fmt <- "Geotiff"
## NetCDFs include a total of 6 layers within one file; these variables       ##
## define the layers for harvested area and quality flag.                     ##
LandInG_setup$landuse$mon_area_layer <- 5
LandInG_setup$landuse$mon_area_quality_layer <- 3
## Variable names: For Geotiff first name is for harvested area file, second  ##
## name is for corresponding data quality file. Used to construct file names. ##
LandInG_setup$landuse$mon_file_var <- list(
  Geotiff = c("HarvestedAreaHectares", "DataQuality_HarvestedArea"),
  NetCDF = "AreaYieldProduction"
)
## File extension based on mon_fmt selected above.                    ##
LandInG_setup$landuse$mon_file_ext <- c(Geotiff = "tif", NetCDF = "nc")
## Area unit used in files                                                    ##
LandInG_setup$landuse$mon_area_units <- "ha"
## Reference year covered by Monfreda data                                    ##
LandInG_setup$landuse$mon_refyear <- 2000
## Read Monfreda crops names (file provided by toolbox)                       ##
LandInG_setup$landuse$mon_names <- readLines(
  file.path(LandInG_setup$landuse$mon_dir, "monfreda_names.txt")
)
## Data directory structure depending mon_fmt. Confirm with your      ##
## download.                                                                  ##
LandInG_setup$landuse$mon_datadir <- c(
  Geotiff = file.path(
    LandInG_setup$landuse$mon_dir,
    "HarvestedAreaYield175Crops_Geotiff",
    "HarvestedAreaYield175Crops_Geotiff"
  ),
  NetCDF = file.path(
    LandInG_setup$landuse$mon_dir,
    "HarvestedAreaYield175Crops_NetCDF"
  )
)
## Monfreda sometimes has extremely small areas. To avoid flagging such crop  ##
## areas as inconsistencies that need to be dealt with, remove entries below  ##
## cutoff value; Note: MIRCA2000 has accuracy of 1e-6                         ##
LandInG_setup$landuse$mon_cutoff <- 1e-6
## Due to different country definitions used here and by Monfreda cells may   ##
## be assigned to the wrong country along borders. Do not assign a Monfreda   ##
## crop to a country if more than the threshold of cells are border cells as  ##
## defined by gadmborder_file                                                 ##
LandInG_setup$landuse$mon_threshold_border <- 0.6
################################################################################

################################################################################
## Ramankutty cropland                                                        ##
## This is the cropland dataset used by Monfreda. Needed for consistency      ##
## check.                                                                     ##
## Note: cropland dataset must cover exactly the same year(s) as Monfreda     ##
## data as specified by mon_refyear above.                                    ##
## Directory containing decompressed Ramankutty data                          ##
LandInG_setup$landuse$ram_dir <-
  stop("Set 'ram_dir' in landuse_setup.R")
## Raster file providing gridded cropland extent
LandInG_setup$landuse$ram_cropland_file <- file.path(
  LandInG_setup$landuse$ram_dir,
  "Cropland2000_5m.tif"
)
## Spatial area unit used in ram_cropland_file
LandInG_setup$landuse$ram_units <- ""
#############################################################################

################################################################################
## crop type mapping between FAOSTAT, MIRCA, and Monfreda                     ##
## This file provides mapping between Monfreda crops, FAOSTAT crops and MIRCA ##
## crops. It has been put together by hand and may need to be updated for     ##
## new versions of any of the three datasets.                                 ##
LandInG_setup$landuse$mapping_file <- "crop_types_Monfreda_FAOSTAT_MIRCA.csv"
################################################################################


################################################################################
## Check for availability of required packages. These may need to be          ##
## installed first.                                                           ##
required_packages <- c(
  "ncdf4", "terra", "units", "stringi", "abind", "data.table", "collapse"
)
if (!all(required_packages %in% .packages(all.available = TRUE))) {
  stop(
    "Please install missing package(s): ",
    toString(
      sQuote(
        setdiff(required_packages, .packages(all.available = TRUE)),
        q = FALSE
      )
    )
  )
}
## Optionally set maximum number of threads used by data.table package. This  ##
## can be useful in an environment with shared hardware resources where       ##
## data.table should not over-extent its resource usage. If not specified,    ##
## the number of threads is determined automatically (see ?setDTthreads).     ##
data.table::setDTthreads(1)
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
## Spatial aggregation function that uses C library loaded above              ##
source(file.path("helper", "array_aggregate.R"))
################################################################################

################################################################################
## Set up working directory with intermediate results, name depends on        ##
## spatial resolution of GADM mask.                                           ##
## "tmp" directory should only contain intermediate results that are          ##
## independent of spatial resolution/country mask.
LandInG_setup$landuse$gadm_raster <- terra::rast(
  LandInG_setup$landuse$gadmlevel_file,
  lyrs = 1
)
tmp_res <- unique(
  terra::res(LandInG_setup$landuse$gadm_raster) * ifelse(
    terra::res(LandInG_setup$landuse$gadm_raster) >= 1 / 60, 60, 3600
  )
)
tmp_string <- paste(
  round(tmp_res),
  unique(
    ifelse(terra::res(LandInG_setup$landuse$gadm_raster) >= 1 / 60, "min", "sec")
  ),
  sep = "",
  collapse = "_by_"
)
LandInG_setup$landuse$working_dir <-
  file.path("tmp", paste0("work_", tmp_string))
if (!dir.exists(LandInG_setup$landuse$working_dir)) {
  dir.create(LandInG_setup$landuse$working_dir, recursive = TRUE)
}
################################################################################


################################################################################
## Intermediate results generated by various scripts                          ##
##                                                                            ##
## Data prepared by read_AQUASTAT_legacy.R                                    ##
## aquastat_rdata contains data read in by read_AQUASTAT_legacy.R. You may    ##
## want to add a download date or version number to filename to distinguish   ##
## more than one version of data. By default, saved to tmp subdirectory.      ##
LandInG_setup$landuse$aquastat_rdata <- file.path(
  "tmp",
  paste0(
    "AQUASTAT_irrigated_harvested_areas",
    add_version_string(LandInG_setup$landuse$aquastat_version_string),
    ".RData"
  )
)
##                                                                            ##
## RData files with FAOSTAT landuse and production data                       ##
## Created by read_FAOSTAT.R to be used in further processing by other        ##
## scripts. Independent of spatial resolution, saved in tmp directory.        ##
## You may want to add a version number or download date to filenames to      ##
## distinguish different versions of FAOSTAT data.                            ##
LandInG_setup$landuse$fao_production_RData <- file.path(
  "tmp",
  paste0(
    "FAOSTAT_production",
    add_version_string(LandInG_setup$landuse$fao_version_string),
    ".RData"
  )
)
LandInG_setup$landuse$fao_landuse_RData <- file.path(
  "tmp",
  paste0(
    "FAOSTAT_landuse",
    add_version_string(LandInG_setup$landuse$fao_version_string),
    ".RData"
  )
)
## RData file with gap-filled production (by default, attaches "_gapfilled"   ##
## to name of fao_production_RData.                                           ##
LandInG_setup$landuse$fao_production_gapfilled_RData <- gsub(
  ".RData",
  "_gapfilled.RData",
  LandInG_setup$landuse$fao_production_RData
)
##                                                                            ##
## File containing Monfreda harvested areas aggregated to GADM country mask.  ##
## File is resolution-specific so save in working_dir. The list of countries  ##
## varies between FAOSTAT versions so this depends on the specific version of ##
## FAOSTAT, GADM and Monfreda used.                                           ##
## Generated by country_level_data.R                                          ##
LandInG_setup$landuse$mon_gadm_sums_RData <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "monfreda_GADM_country_sums",
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$mon_version_string),
    ".RData"
  )
)
##                                                                            ##
## File containing table with matching Monfreda and FAOSTAT crops in each     ##
## country. File is resolution-specific so save in working_dir. This depends  ##
## on the specific version of FAOSTAT, GADM and Monfreda used.                ##
## Generated by country_level_data.R and used by subsequent scripts           ##
LandInG_setup$landuse$fao_mon_country_RData <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "fao_monfreda_GADM_country_production",
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$mon_version_string),
    ".RData"
  )
)
##                                                                            ##
## Crop-specific harvested area base patterns and gapfilling statistics       ##
## NetCDF and RData created by harvested_fraction.R. This depends on the      ##
## version of Monfreda and Ramankutty used. Gapfilling also depends on the    ##
## version of HYDE used. Additionally, uses information from FAOSTAT about    ##
## which crops are present in which countries.                                ##
LandInG_setup$landuse$harvested_fraction_filename <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "harvested_fraction_GADM",
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    add_version_string(LandInG_setup$landuse$mon_version_string),
    add_version_string(LandInG_setup$landuse$ram_version_string),
    ".nc"
  )
)
LandInG_setup$landuse$harvested_fraction_fill_statistics_RData <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "harvested_fraction_GADM_fill_statistics",
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    add_version_string(LandInG_setup$landuse$mon_version_string),
    add_version_string(LandInG_setup$landuse$ram_version_string),
    ".RData"
  )
)
##                                                                            ##
## Maximum cropland extent over whole period                                  ##
## Used/generated by harvested_fraction.R                                     ##
LandInG_setup$landuse$hyde_max_cropland_filename <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "hyde_", LandInG_setup$landuse$hyde_cropland_varname, "_max_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)
LandInG_setup$landuse$hyde_max_irrigated_filename <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "hyde_", LandInG_setup$landuse$hyde_irrigated_varname, "_max_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)
LandInG_setup$landuse$hyde_max_rainfed_filename <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "hyde_", LandInG_setup$landuse$hyde_rainfed_varname, "_max_",
    min(LandInG_setup$landuse$hyde_period), "_",
    max(LandInG_setup$landuse$hyde_period),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".nc4"
  )
)
##                                                                            ##
## GAEZ multicropping suitability files at GADM resolution                    ##
## Generated by multi_cropping_suitability_GAEZ.R                             ##
## This can be any format supported by the R terra package, e.g. ".asc" or    ##
## ".nc" for ASCII grid or NetCDF.                                            ##
LandInG_setup$landuse$gaez_multicropping_suit_rf_file <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "multicropping_suitability_GAEZ_rainfed",
    add_version_string(LandInG_setup$landuse$gaez_version_string),
    ".asc"
  )
)
LandInG_setup$landuse$gaez_multicropping_suit_ir_file <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "multicropping_suitability_GAEZ_irrigated",
    add_version_string(LandInG_setup$landuse$gaez_version_string),
    ".asc"
  )
)
##                                                                            ##
## Country-scale time series of aggregated HYDE cropland and HYDE cropland    ##
## multiplied with multicropping suitability generated by                     ##
## split_global_harvested_areas_into_rainfed_irrigated.R. Country list        ##
## depends on FAOSTAT version used.                                           ##
LandInG_setup$landuse$hyde_country_sums_RData <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "hyde_country_sums_",
    max(
      min(LandInG_setup$landuse$output_period),
      min(LandInG_setup$landuse$hyde_period)
    ),
    "-",
    min(
      max(LandInG_setup$landuse$output_period),
      max(LandInG_setup$landuse$hyde_period)
    ),
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$gaez_version_string),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    ".RData"
  )
)
## Country-scale time series of crop-specific irrigated and total harvested   ##
## areas. Generated by split_global_harvested_areas_into_rainfed_irrigated.R  ##
## The script allows different options for consistency scaling. See details   ##
## in split_global_harvested_areas_into_rainfed_irrigated.R. Set here which   ##
## version to use for rainfed and irrigated crops.                            ##
LandInG_setup$landuse$irrigated_version_to_use <-
  "irr_ha_timeseries_unconstrained"
LandInG_setup$landuse$total_version_to_use <- "tot_ha_timeseries"
## Main versions are saved to this file for further use:                      ##
LandInG_setup$landuse$ha_country_timeseries_RData <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "GADM_country_timeseries_ha_croparea_maxsuit",
    add_version_string(LandInG_setup$landuse$aquastat_version_string),
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$gaez_version_string),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    add_version_string(LandInG_setup$landuse$mirca_version_string),
    add_version_string(LandInG_setup$landuse$mon_version_string),
    add_version_string(LandInG_setup$landuse$ram_version_string),
    ".RData"
  )
)
## Alternative versions are saved to this file:                               ##
LandInG_setup$landuse$alt_ha_country_timeseries_RData <- file.path(
  LandInG_setup$landuse$working_dir,
  paste0(
    "GADM_country_timeseries_ha_alternatives",
    add_version_string(LandInG_setup$landuse$aquastat_version_string),
    add_version_string(LandInG_setup$landuse$fao_version_string),
    add_version_string(LandInG_setup$landuse$gadm_version_string),
    add_version_string(LandInG_setup$landuse$gaez_version_string),
    add_version_string(LandInG_setup$landuse$hyde_version_string),
    add_version_string(LandInG_setup$landuse$mirca_version_string),
    add_version_string(LandInG_setup$landuse$mon_version_string),
    add_version_string(LandInG_setup$landuse$ram_version_string),
    ".RData"
  )
)
##                                                                            ##
## Variable names for crop-specific time series of harvested areas in NetCDF  ##
LandInG_setup$landuse$rainfed_output_name <- "rainfed_harvested_area"
LandInG_setup$landuse$irrigated_output_name <- "irrigated_harvested_area"
LandInG_setup$landuse$rainfed_output_sum_name <- "rainfed_harvested_area_sum"
LandInG_setup$landuse$irrigated_output_sum_name <- "irrigated_harvested_area_sum"
LandInG_setup$landuse$total_output_sum_name <- "total_harvested_area_sum"
## The timeseries NetCDF is split into blocks to limit filesize of individual ##
## files. These files are saved in working_dir by default.                    ##
## Filename base (years covered and optional version strings are added        ##
## automatically)                                                             ##
LandInG_setup$landuse$ha_timeseries_filename_base <- file.path(
  LandInG_setup$landuse$working_dir,
  "harvested_area_GADM_timeseries"
)
## Split whole time series into chunks of this number of years.               ##
LandInG_setup$landuse$ha_timeseries_chunk_length <- 10
################################################################################


################################################################################
## Crop aggregation from full list of FAOSTAT/Monfreda types to LPJmL CFTs.   ##
## This will depend on the CFTs implemented in your version of LPJmL. The     ##
## file that comes with the toolbox provides a standard list of LPJmL CFTs at ##
## the time of release.                                                       ##
## You can use other values for 'aggregation_name' for other lists of CFTs.   ##
LandInG_setup$landuse$crop_aggregation_file <- paste0(
  "crop_types_FAOSTAT_LPJmL_",
  ifelse(
    exists("aggregation_name") && nchar(aggregation_name) > 0,
    aggregation_name,
    "default"
  ),
  ".csv"
)
LandInG_setup$landuse$crop_aggregation_types <- read.csv(
  LandInG_setup$landuse$crop_aggregation_file,
  stringsAsFactors = FALSE,
  comment.char = "#"
)
## Filename base for aggregated timeseries generated by aggregate_timeseries.R##
## These files are saved in working_dir by default. Years covered and         ##
## optional version strings are added automatically.                          ##
LandInG_setup$landuse$aggregated_timeseries_filename_base <- file.path(
  LandInG_setup$landuse$working_dir,
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
## Use fewer line breaks
options(width = 200)
## Ensure converting numbers to strings does not use scientific notation
options(scipen = 999)
