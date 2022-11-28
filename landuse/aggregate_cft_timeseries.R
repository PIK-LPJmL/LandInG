################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script aggregates crop-specific harvested area time series to a list  ##
## of crop types. Usually, these should be CFTs implemented in LPJmL.         ##
## Lists of CFTs and allocated crops are defined in file                      ##
## "crop_aggregation_file", which is set in landuse_setup.R                   ##
## If you plan to create more than one crop aggregation you may give it a     ##
## short name, set below as variable "aggregation_name" and observed in       ##
## landuse_setup.R.                                                           ##
## While this script is not parallelized explicitly you can provide           ##
## start_year and end_year as command line parameters to split processing the ##
## whole timeseries into several tasks.                                       ##
################################################################################

# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Basic setup of aggregation                                                 ##
## (Optional) name of crop aggregation                                        ##
aggregation_name <- "default"
################################################################################


################################################################################
## Setup of variables valid across all scripts related to land use data       ##
## processing.                                                                ##
## - sets many directories and file names                                     ##
## - also loads several helper functions used by various land use processing  ##
##   scripts                                                                  ##
source("landuse_setup.R")
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
      # Cannot exceed output_period
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
      # Cannot exceed output_period
      end_year <- max(output_period)
    }
    if (end_year < start_year) {
      stop(
        "Parameter end_year (", end_year,
        ") must be larger than start_year (", start_year, ")"
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
## Find available harvested area time series files (created by                ##
## harvested_area_timeseries.R)                                               ##
## Needs to match the filename pattern used in harvested_area_timeseries.R    ##
# Concatenate all possible version strings
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
input_timeseries_filenames <- list.files(
  dirname(ha_timeseries_filename_base),
  pattern = paste0(
    basename(ha_timeseries_filename_base), "_",
    "[0-9]{4}-[0-9]{4}", # chunk_start, chunk_end in harvested_area_timeseries.R
    file_version_string,
    ".nc"
  )
)
if (length(input_timeseries_filenames) == 0) {
  stop(
    "No harvested area timeseries files found in ",
    sQuote(dirname(ha_timeseries_filename_base)), ".\n",
    "Check that filename pattern used for search matches filename pattern ",
    "used in harvested_area_timeseries.R"
  )
}
# Extract chunk_start and chunk_end from filenames
input_timeseries_fileyears <- t(
  sapply(
    regmatches(
      input_timeseries_filenames,
      regexec(
        "harvested_area_GADM_timeseries_([0-9]{4})-([0-9]{4})",
        input_timeseries_filenames
      )
    ),
    function(indata) as.integer(indata[-1])
  )
)
colnames(input_timeseries_fileyears) <- c("start", "end")
cat(
  "*** Found", length(input_timeseries_filenames),
  "files with crop-specific harvested areas covering the time period",
  paste(
    min(input_timeseries_fileyears[, "start"]),
    max(input_timeseries_fileyears[, "end"]),
    sep = "-"
  ),
  "***\n"
)
if (start_year < min(input_timeseries_fileyears[, "start"])) {
  cat(
    "Parameter start_year", start_year,
    "outside of range of available files\n"
  )
  start_year <- min(input_timeseries_fileyears[, "start"])
}
if (end_year > max(input_timeseries_fileyears[, "end"])) {
  cat(
    "Parameter end_year", end_year,
    "outside of range of available files\n"
  )
  end_year <- max(input_timeseries_fileyears[, "end"])
}
cat(
  "This script is set to process harvested areas for ",
  start_year, "-", end_year, "\n", sep = ""
)
cat("Crop aggregation table used:", sQuote(crop_aggregation_file), "\n")
cat(
  "This aggregation table aggregates",
  length(unique(crop_aggregation_types$FAOSTAT_name)), "crops to",
  length(unique(crop_aggregation_types$LPJmL_name)),
  "aggregate crops and crop groups:\n"
)
for (cft in sort(unique(crop_aggregation_types$LPJmL_name))) {
  cat("Aggregate:", sQuote(cft), "\n")
  index <- which(crop_aggregation_types$LPJmL_name == cft)
  cat(toString(sQuote(unique(crop_aggregation_types$FAOSTAT_name)[index])), "\n")
}
################################################################################


################################################################################
## Check files if crops from aggregation table are present and if files have  ##
## actually finished processing.                                              ##
year_processed <- integer(0)
fileyears <- integer(0)
for (fileindex in seq_along(input_timeseries_filenames)) {
  input_timeseries_file <- nc_open(
    filename = file.path(
      dirname(ha_timeseries_filename_base),
      input_timeseries_filenames[fileindex]
    )
  )
  # Check which years have finished processing
  year_processed <- c(
    year_processed,
    ncvar_get(input_timeseries_file, "year_processed")
  )
  fileyears <- c(
    fileyears,
    min(output_period) + input_timeseries_file$dim$time$vals
  )
  # Check that crops match
  nccrops <- ncvar_get(input_timeseries_file, "crop")
  if (any(!nccrops %in% crop_aggregation_types$FAOSTAT_name)) {
    warning(
      "Crop(s) ",
      toString(sQuote(setdiff(nccrops, crop_aggregation_types$FAOSTAT_name))),
      " from ", sQuote(input_timeseries_file$filename),
      " missing in crop aggregation table.\n",
      "Your aggregated crops will be incomplete.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  if (any(!crop_aggregation_types$FAOSTAT_name %in% nccrops)) {
    message(
      "Error: Crop(s) ",
      toString(sQuote(setdiff(crop_aggregation_types$FAOSTAT_name, nccrops))),
      " from crop aggregation table missing in ",
      sQuote(input_timeseries_file$filename)
    )
    if (any(seq(start_year, end_year) %in% seq.int(
      input_timeseries_fileyears[fileindex, "start"],
      input_timeseries_fileyears[fileindex, "end"]
    ))) {
      # Only stop if this file belongs to period start_year to end_year
      stop(
        "Mismatch between harvested area time series NetCDF and crop ",
        "aggregation table"
      )
    }
  }
  # Check NetCDF dimensions
  if (exists("lon_dim")) {
    if (!identical(lon_dim, input_timeseries_file$dim$lon) &&
      any(seq(start_year, end_year) %in% seq.int(
        input_timeseries_fileyears[fileindex, "start"],
        input_timeseries_fileyears[fileindex, "end"]
      ))
    ) {
      stop(
        "Longitude dimension differs in ",
        sQuote(input_timeseries_file$filename)
      )
    }
  } else {
    lon_dim <- input_timeseries_file$dim$lon
  }
  if (exists("lat_dim")) {
    if (!identical(lat_dim, input_timeseries_file$dim$lat) &&
      any(seq(start_year, end_year) %in% seq.int(
        input_timeseries_fileyears[fileindex, "start"],
        input_timeseries_fileyears[fileindex, "end"]
      ))
    ) {
      stop(
        "Latitude dimension differs in ",
        sQuote(input_timeseries_file$filename)
      )
    }
  } else {
    lat_dim <- input_timeseries_file$dim$lat
  }
  nc_close(input_timeseries_file)
}
if (!any(
  year_processed == 1 & fileyears >= start_year & fileyears <= end_year
)) {
  # No data available
  in_period <- which(
    input_timeseries_fileyears[, "start"] >= start_year &
    input_timeseries_fileyears[, "end"] <= end_year
  )
  stop(
    "All years in ",
    length(in_period),
    " harvested area timeseries ",
    ifelse(
      length(in_period) == 1,
      paste("file", sQuote(input_timeseries_filenames[in_period])),
      paste(
        "files",
        sQuote(input_timeseries_filenames[min(in_period)]), "-",
        sQuote(input_timeseries_filenames[max(in_period)])
      )
    ),
    " are missing.\nMake sure that harvested_area_timeseries.R has finished",
    " processing all years."
  )
}
if (any(
  year_processed != 1 & fileyears >= start_year & fileyears <= end_year
)) {
  in_period <- which(
    input_timeseries_fileyears[, "start"] >= start_year &
    input_timeseries_fileyears[, "end"] <= end_year
  )
  missing <- which(
    year_processed != 1 &
    fileyears >= start_year &
    fileyears <= end_year
  )
  warning(
    "Year(s) ", toString(fileyears[missing]),
    " missing in ", length(in_period), " found harvested area timeseries ",
    ifelse(length(in_period) > 1, "files", "file"),
    ". Adjusting start_year and end_year. Make sure that ",
    "harvested_area_timeseries.R has finished processing all years.",
    call. = FALSE,
    immediate. = TRUE
  )
  valid <- which(
    year_processed == 1 &
    fileyears >= start_year &
    fileyears <= end_year
  )
  start_year <- min(fileyears[valid])
  end_year <- min(fileyears[missing]) - 1
  cat("Adjusted processing period:", start_year, "-", end_year, "\n")
}
################################################################################

################################################################################
## Generate NetCDF files for aggregated crop list                             ##
## Chunk length (number of years per file) depends on number of aggregated    ##
## crop types.
cfts <- sort(unique(crop_aggregation_types$LPJmL_name))
if (length(cfts) <= 10) {
  chunklength <- 200
} else if (length(cfts) <= 20) {
  chunklength <- 100
} else if (length(cfts) <= 40) {
  chunklength <- 50
} else {
  chunklength <- 10
}
# Variables included in NetCDF files
nc_vars <- c(
  rainfed_output_name,
  irrigated_output_name,
  rainfed_output_sum_name,
  irrigated_output_sum_name,
  total_output_sum_name,
  "year_processed"
)
# Start and end years of chunks
chunk_start <- seq(min(output_period), max(output_period), by = chunklength)
chunk_end <- chunk_start + chunklength - 1
chunk_end[which(chunk_end > max(output_period))] <- max(output_period)
for (chunk in seq_along(chunk_start)) {
  # NetCDF dimensions (lon_dim and lat_dim are copied from harvested area
  # timeseries files)
  time_dim <- ncdim_def(
    name = "time",
    units = paste0("years since ", min(output_period), "-01-01"),
    vals = (chunk_start[chunk]:chunk_end[chunk]) - min(output_period),
    unlim = TRUE
  )
  nchar_dim <- ncdim_def(
    name = "nchar",
    units = "",
    vals = seq_len(max(nchar(crop_aggregation_types$LPJmL_name))),
    create_dimvar = FALSE
  )
  crop_dim <- ncdim_def(
    name = "crop",
    units = "",
    vals = seq_along(unique(crop_aggregation_types$LPJmL_name)),
    create_dimvar = FALSE
  )
  crop_var <- ncvar_def(
    name = "crop",
    units = "",
    dim = list(nchar_dim, crop_dim),
    longname = "crop name",
    prec = "char"
  )
  # Crop-group-specfic variables
  rainfed_area_var <- ncvar_def(
    name = rainfed_output_name,
    units = fao_area_units,
    dim = list(lon_dim, lat_dim, crop_dim, time_dim),
    longname = "rainfed harvested area",
    missval = 1e30,
    chunksizes = c(lon_dim$len, lat_dim$len, 1, 1),
    prec = "double",
    compression = 5 # NetCDF compression to reduce file size
  )
  irrigated_area_var <- ncvar_def(
    name = irrigated_output_name,
    units = fao_area_units,
    dim = list(lon_dim, lat_dim, crop_dim, time_dim),
    longname = "irrigated harvested area",
    missval = 1e30,
    chunksizes = c(lon_dim$len, lat_dim$len, 1, 1),
    prec = "double",
    compression = 5 # NetCDF compression to reduce file size
  )
  # Sum over all crops
  rainfed_sum_var <- ncvar_def(
    name = rainfed_output_sum_name,
    units = fao_area_units,
    dim = list(lon_dim, lat_dim, time_dim),
    longname = "sum of rainfed harvested areas over all crops",
    missval = 1e30,
    chunksizes = c(lon_dim$len, lat_dim$len, 1),
    prec = "double",
    compression = 5 # NetCDF compression to reduce file size
  )
  irrigated_sum_var <- ncvar_def(
    name = irrigated_output_sum_name,
    units = fao_area_units,
    dim = list(lon_dim, lat_dim, time_dim),
    longname = "sum of irrigated harvested areas over all crops",
    missval = 1e30,
    chunksizes = c(lon_dim$len, lat_dim$len, 1),
    prec = "double",
    compression = 5 # NetCDF compression to reduce file size
  )
  total_sum_var <- ncvar_def(
    name = total_output_sum_name,
    units = fao_area_units,
    dim = list(lon_dim, lat_dim, time_dim),
    longname = c(
      "sum of total (rainfed + irrigated) harvested areas over all crops"
    ),
    missval = 1e30,
    chunksizes = c(lon_dim$len, lat_dim$len, 1),
    prec = "double",
    compression = 5 # NetCDF compression to reduce file size
  )
  # Filename of newly generated aggregated time series file
  harvested_area_group_filename <- paste0(
    basename(aggregated_timeseries_filename_base), "_",
    chunk_start[chunk], "-", chunk_end[chunk],
    file_version_string,
    ".nc"
  )
  # Create file if it does not exist yet
  if (!file.exists(
    file.path(
      dirname(aggregated_timeseries_filename_base),
      harvested_area_group_filename
    ))
  ) {
    # Status variable
    year_processed_var <- ncvar_def(
      name = "year_processed",
      units = "",
      dim = time_dim,
      longname = "year already fully processed",
      prec = "short",
      missval = -9
    )
    cat(
      "Creating",
      sQuote(
        file.path(
          dirname(aggregated_timeseries_filename_base),
          harvested_area_group_filename
        )
      ),
      "\n"
    )
    harvested_area_group_file <- nc_create(
      filename = file.path(
        dirname(aggregated_timeseries_filename_base),
        harvested_area_group_filename
      ),
      vars = list(# list of variables created in file
        rainfed_area_var,
        irrigated_area_var,
        rainfed_sum_var,
        irrigated_sum_var,
        total_sum_var,
        crop_var,
        year_processed_var
      ),
      force_v4 = TRUE
    )
    # Set initial values
    # Set years that have finished processing
    year_processed <- rep(0, time_dim$len)
    ncvar_put(
      nc = harvested_area_group_file,
      varid = "year_processed",
      vals = year_processed,
      count = length(year_processed)
    )
    # Set crop names
    ncvar_put(
      nc = harvested_area_group_file,
      varid = "crop",
      vals = sort(unique(crop_aggregation_types$LPJmL_name))
    )
    nc_close(harvested_area_group_file)
  } else if (any(
    (start_year:end_year) %in% (chunk_start[chunk]:chunk_end[chunk])
  )) {
    # Re-use existing file but check first
    cat(
      "Trying to re-use previously created",
      sQuote(
        file.path(
          dirname(aggregated_timeseries_filename_base),
          harvested_area_group_filename
        )
      ),
      "\n"
    )
    harvested_area_group_file <- nc_open(
      filename = file.path(
        dirname(aggregated_timeseries_filename_base),
        harvested_area_group_filename
      )
    )
    # Check that all variables exist in file
    if (any(!nc_vars %in% names(harvested_area_group_file$var))) {
      stop(
        "Variable(s) ",
        toString(
          sQuote(setdiff(nc_vars, names(harvested_area_group_file$var)))
        ),
        " missing in ", sQuote(harvested_area_group_file$filename), "\n",
        "File will be recreated if you delete the existing one."
      )
    }
    if (lon_dim$len != harvested_area_group_file$dim$lon$len ||
      lat_dim$len != harvested_area_group_file$dim$lat$len ||
      crop_dim$len != harvested_area_group_file$dim$crop$len
    ) {
      stop(
        "Dimensions of ", sQuote(harvested_area_group_file$filename),
        " do not match script run.\n",
        "File will be recreated if you delete the existing one."
      )
    }
    if (any(
      ncvar_get(harvested_area_group_file, "crop") !=
      sort(unique(crop_aggregation_types$LPJmL_name))
    )) {
      stop(
        "Crop names in ", sQuote(harvested_area_group_file$filename),
        " do not match script run.\n",
        "File will be recreated if you delete the existing one."
      )
    }
    if (harvested_area_group_file$dim$time$units != time_dim$units ||
      any(harvested_area_group_file$dim$time$vals != time_dim$vals)
    ) {
      stop(
        "Time axis in ", sQuote(harvested_area_group_file$filename),
        " does not match script run.\n",
        "File will be recreated if you delete the existing one."
      )
    }
    nc_close(harvested_area_group_file)
  }
}
rm(input_timeseries_file, harvested_area_group_file)
################################################################################

################################################################################
## Process timeseries from start_year to end_year                             ##
all_crops <- TRUE
double_crops <- FALSE
for (year in start_year:end_year) {
  if (year %in% input_timeseries_fileyears[, "start"] ||
    !exists("input_timeseries_file")
  ) {
    # Open one of the input files
    fileindex <- which(
      input_timeseries_fileyears[, "start"] <= year &
      input_timeseries_fileyears[, "end"] >= year
    )
    input_timeseries_file <- nc_open(
      filename = file.path(
        dirname(ha_timeseries_filename_base),
        input_timeseries_filenames[fileindex]
      )
    )
    year_processed_input <- ncvar_get(input_timeseries_file, "year_processed")
    year_input <- input_timeseries_file$dim$time$vals + min(output_period)
    crops_input <- ncvar_get(input_timeseries_file, "crop")
    cat("Input file:", input_timeseries_file$filename, "\n")
    if (!all(crops_input %in% crop_aggregation_types[, "FAOSTAT_name"])) {
      # Print this message only once
      if (all_crops) {
        warning(
          length(
            which(!crops_input %in% crop_aggregation_types[, "FAOSTAT_name"])
          ),
          " crop(s) from ", sQuote(input_timeseries_file$filename), "(",
          toString(
            sQuote(
              setdiff(
                crops_input,
                crop_aggregation_types[, "FAOSTAT_name"]
              )
            )
          ),
          ") not included in aggregated crop list.\n",
          "Some harvested areas may be lost in aggregated dataset.",
          call. = FALSE,
          immediate. = TRUE
        )
        # Do not print again
        all_crops <- FALSE
      }
    }
  }
  if (year %in% chunk_start || !exists("harvested_area_group_file")) {
    # Open one of the output files
    chunk <- which(chunk_start <= year & chunk_end >= year)
    harvested_area_group_filename <- paste0(
      basename(aggregated_timeseries_filename_base), "_",
      chunk_start[chunk], "-", chunk_end[chunk],
      file_version_string,
      ".nc"
    )
    harvested_area_group_file <- nc_open(
      filename = file.path(
        dirname(aggregated_timeseries_filename_base),
        harvested_area_group_filename
      ),
      write = TRUE
    )
    year_processed_output <- ncvar_get(
      harvested_area_group_file,
      "year_processed"
    )
    group_crops <- ncvar_get(harvested_area_group_file, "crop")
    year_output <- harvested_area_group_file$dim$time$vals + min(output_period)
    cat("Output file:", harvested_area_group_file$filename, "\n")
  }
  # Skip any years that may have been processed in previous script run
  index <- which(year_output == year)
  if (year_processed_output[index] == 1) {
    message(
      "Skipping year ", year,
      " because it is marked as already processed in ",
      sQuote(harvested_area_group_file$filename)
    )
    if ((year %in% input_timeseries_fileyears[, "end"]) ||
      year == end_year
    ) {
      nc_close(input_timeseries_file)
      rm(input_timeseries_file)
    }
    if ((year %in% chunk_end) || year == end_year) {
      nc_close(harvested_area_group_file)
      rm(harvested_area_group_file)
    }
    # Skip to next year
    next
  }
  cat("*** Year", year, "***\n")
  # Variables for sums across all CFTs
  year_irrigated_sum <- double(lon_dim$len * lat_dim$len)
  year_rainfed_sum <- double(lon_dim$len * lat_dim$len)
  year_total_sum <- double(lon_dim$len * lat_dim$len)
  crops_summed <- character(0)
  # Sum individual crops to aggregated crop groups
  for (cft in cfts) {
    cat(cft, "\n")
    # Variables for sums across crops grouped under that CFT
    year_rainfed_cft <- year_irrigated_cft <- double(lon_dim$len * lat_dim$len)
    # Indices of crops grouped under that CFT
    index <- which(crop_aggregation_types[, "LPJmL_name"] == cft)
    for (crop in crop_aggregation_types[index, "FAOSTAT_name"]) {
      crop_rainfed <- ncvar_get(
        nc = input_timeseries_file,
        varid = rainfed_output_name,
        start = c(1, 1, which(crops_input == crop), which(year_input == year)),
        count = c(-1, -1, 1, 1)
      )
      crop_irrigated <- ncvar_get(
        nc = input_timeseries_file,
        varid = irrigated_output_name,
        start = c(1, 1, which(crops_input == crop), which(year_input == year)),
        count = c(-1, -1, 1, 1)
      )
      year_rainfed_cft <- year_rainfed_cft + crop_rainfed
      year_irrigated_cft <- year_irrigated_cft + crop_irrigated
      crops_summed <- c(crops_summed, crop)
    }
    # Aggregate into total sums
    year_rainfed_sum <- year_rainfed_sum + year_rainfed_cft
    year_irrigated_sum <- year_irrigated_sum + year_irrigated_cft
    # Write CFT to NetCDF
    ncvar_put(
      nc = harvested_area_group_file,
      varid = rainfed_output_name,
      vals = c(year_rainfed_cft),
      start = c(1, 1, which(group_crops == cft), which(year_output == year)),
      count = c(-1, -1, 1, 1)
    )
    ncvar_put(
      nc = harvested_area_group_file,
      varid = irrigated_output_name,
      vals = c(year_irrigated_cft),
      start = c(1, 1, which(group_crops == cft), which(year_output == year)),
      count = c(-1, -1, 1, 1)
    )
  }
  if (!double_crops && length(crops_summed) > length(unique(crops_summed))) {
    # Print this message only once
    warning(
      "Source crop(s) ",
      toString(sQuote(names(which(table(crops_summed) > 1)))),
      " is/are part of more than one aggregated group crop.\n",
      "Sums across all crops will probably deviate from sums in source data.",
      call. = FALSE,
      immediate. = TRUE
    )
    # Do not print again
    double_crops <- TRUE
  }
  year_total_sum <- year_rainfed_sum + year_irrigated_sum
  # Write sums across all crops to file
  for (sums in c("rainfed", "irrigated", "total")) {
    if (!double_crops) {
      # Compare with sums from source file only if !double_crops
      tmpsum <- ncvar_get(
        nc = input_timeseries_file,
        varid = get(paste0(sums, "_output_sum_name")),
        start = c(1, 1, which(year_input == year)),
        count = c(-1, -1, 1)
      )
      if (max(
        abs(tmpsum - get(paste0("year_", sums, "_sum"))),
        na.rm = TRUE
      ) > 1e-4) {
        warning(
          get(paste0(sums, "_output_sum_name")),
          " from ", sQuote(input_timeseries_file$filename),
          " differs from ", paste0("year_", sums, "_sum"),
          " computed from aggregated crop groups.",
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }
    # Write sum to file
    ncvar_put(
      nc = harvested_area_group_file,
      varid = get(paste0(sums, "_output_sum_name")),
      vals = c(get(paste0("year_", sums, "_sum"))),
      start = c(1, 1, which(year_output == year)),
      count = c(-1, -1, 1)
    )
  }
  # Set year as fully processed
  ncvar_put(
    nc = harvested_area_group_file,
    varid = "year_processed",
    vals = 1,
    start = which(year_output == year),
    count = 1
  )
  # Make sure that all data is written to file; clear buffers
  nc_sync(harvested_area_group_file)
  # Read year_processed_output from file in case of several processes accessing
  # the same file. Note: We advise against running several tasks with
  # overlapping time periods at the same time.
  year_processed_output <- ncvar_get(
    nc = harvested_area_group_file,
    varid = "year_processed"
  )

  # Clean up
  if (year %in% input_timeseries_fileyears[, "end"] ||
    year == end_year
  ) {
    nc_close(input_timeseries_file)
    rm(input_timeseries_file)
  }
  if (year %in% chunk_end || year == end_year) {
    nc_close(harvested_area_group_file)
    rm(harvested_area_group_file)
  }
}
################################################################################
