################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script can be used to download fertilizer application patterns by     ##
## Mueller et al., Nature, 490, 254–257, 2012 (doi: 10.1038/nature11420) from ##
## Zenodo (https://doi.org/10.5281/zenodo.5260732)                            ##
################################################################################

# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Setup of variables valid across all scripts related to fertilizer data     ##
## processing.                                                                ##
## - sets many directories and file names                                     ##
source("fertilizer_setup.R")
################################################################################

if (!"R.utils" %in% .packages(all.available = TRUE)) {
  stop("Please install missing package 'R.utils'")
}

################################################################################
## Download data for the nutrients defined in 'fertilizer_pattern_nutrients'  ##
## for all crops listed in 'mapping_file' to                                  ##
## 'directory fertilizer_pattern_dir'                                         ##
if (file.exists(LandInG_setup$fertilizer$mapping_file)) {
  cat(
    "Crop type mapping loaded from",
    sQuote(LandInG_setup$fertilizer$mapping_file), "\n"
  )
  crop_type_mapping <- read.csv(
    LandInG_setup$fertilizer$mapping_file,
    stringsAsFactors = FALSE
  )
} else {
  stop(
    "Mapping file ", LandInG_setup$fertilizer$mapping_file, " does not exist.",
    "\nPlease check fertilizer_setup.R"
  )
}
if (!LandInG_setup$fertilizer$fertilizer_pattern_map_col %in%
    colnames(crop_type_mapping)
) {
  stop(
    "Column ", sQuote(LandInG_setup$fertilizer$fertilizer_pattern_map_col),
    " not found in mapping_file.",
    "\nPlease check 'fertilizer_pattern_map_col' in fertilizer_setup.R"
  )
}
# Determine crop types for which to download data
crops <- na.omit(
  crop_type_mapping[, LandInG_setup$fertilizer$fertilizer_pattern_map_col]
)
crops <- crops[which(nchar(crops) > 0)]
# Remove any special characters from crops list.
if (!all(stringi::stri_enc_isascii(crops), na.rm = TRUE)) {
  # String has non-ASCII characters
  if (!all(stringi::stri_enc_isutf8(crops), na.rm = TRUE)) {
    # String has non-UTF8 characters -> assume windows-1252 encoding and
    # convert to UTF-8
    message(
      "Converting column ",
      sQuote(LandInG_setup$fertilizer$fertilizer_pattern_map_col),
      " from windows-1252 to UTF-8 encoding"
    )
    crops <- stringi::stri_encode(crops, "windows-1252", "UTF-8")
  }
  # Convert UTF-8 strings to ASCII strings, if necessary translating
  # special characters
  message(
    "Converting column ",
    sQuote(LandInG_setup$fertilizer$fertilizer_pattern_map_col),
    " from UTF-8 to ASCII encoding"
  )
  crops <- stringi::stri_encode(crops, "UTF-8", "UTF-8")
  crops <- stringi::stri_trans_general(crops, "latin-ascii")
}

cat(
  "Attempting to download data for", length(crops), "crop(s)",
  "and", length(LandInG_setup$fertilizer$fertilizer_pattern_nutrients),
  "nutrient(s)\n"
)
if (!dir.exists(LandInG_setup$fertilizer$fertilizer_pattern_dir)) {
  dir.create(LandInG_setup$fertilizer$fertilizer_pattern_dir, recursive = TRUE)
}
progress_step <- round(
  seq(
    0,
    length(crops),
    length.out = ifelse(
      length(crops) > 100,
      21,
      ifelse(length(crops) > 50, 11, 6)
    )
  )
)
# Set timeout. Do not decrease below existing limit. Also set warning level so
# that all warnings are displayed immediately, incl. possible server response
# warnings.
timeout <- options(
  timeout = max(300, getOption("timeout")),
  warn = max(1, getOption("warn"))
)
for (nut in LandInG_setup$fertilizer$fertilizer_pattern_nutrients) {
  cat("Nutrient:", nut, "\n")
  for (crop in crops) {
    # Expected filename pattern: [CROP][NUTRIENT]apprate.nc.gz
    filename <- paste0(crop, nut, "apprate.nc.gz")
    # Download URL on Zenodo server. Valid at the time of writing this script.
    download_url <- paste0(
      LandInG_setup$fertilizer$fertilizer_pattern_base_url, filename,
      "?download=1"
    )
    status <- 1
    tryCatch(
      status <- download.file(
        url = download_url,
        destfile = file.path(
          LandInG_setup$fertilizer$fertilizer_pattern_dir,
          filename
        ),
        quiet = TRUE, mode = "wb"
      ),
      error = function(e) message(e$message)
    )
    if (status != 0) {
      message("Non-success exit code by download.file() for file ", filename)
    }
    if (file.exists(
      file.path(LandInG_setup$fertilizer$fertilizer_pattern_dir, filename)
    ) &&
        R.utils::isGzipped(
          file.path(LandInG_setup$fertilizer$fertilizer_pattern_dir, filename),
          method = "content"
        )
    ) {
      # Decompress file. This removes the downloaded gzip file unless remove is
      # set to FALSE.
      R.utils::gunzip(
        file.path(LandInG_setup$fertilizer$fertilizer_pattern_dir, filename),
        remove = TRUE,
        overwrite = TRUE
      )
    } else {
      message(
        "Cannot decompress ",
        file.path(LandInG_setup$fertilizer$fertilizer_pattern_dir, filename)
      )
    }
    if (which(crops == crop) %in% progress_step) {
      cat(round(which(crops == crop) / length(crops) * 100), "% finished\n")
      # Wait for 1 second to avoid rate limit for downloads from Zenodo.
      Sys.sleep(1)
    }
  }
}
# Reset timeout and warning level
options(timeout)
