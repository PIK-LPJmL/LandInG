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
rm(list=ls(all=TRUE))

################################################################################
## Setup of variables valid across all scripts related to fertilizer data     ##
## processing.                                                                ##
## - sets many directories and file names                                     ##
source("fertilizer_setup.R")
################################################################################

library(R.utils)

################################################################################
## Download data for the nutrients defined in 'fertilizer_pattern_nutrients'  ##
## for all crops listed in 'mapping_file' to                                  ##
## 'directory fertilizer_pattern_dir'                                         ##
if (file.exists(mapping_file)) {
  cat("Crop type mapping loaded from", sQuote(mapping_file), "\n")
  crop_type_mapping <- read.csv(mapping_file, stringsAsFactors = FALSE)
} else {
  stop(
    paste(
      "Mapping file", mapping_file, "does not exist.",
      "\nPlease check fertilizer_setup.R"
    )
  )
}
if (!fertilizer_pattern_map_col %in% colnames(crop_type_mapping)) {
  stop(
    paste(
      "Column", sQuote(fertilizer_pattern_map_col), "not found in mapping_file",
      "\nPlease check 'fertilizer_pattern_map_col' in fertilizer_setup.R"
    )
  )
}
# Determine crop types for which to download data
crops <- na.omit(crop_type_mapping[, fertilizer_pattern_map_col])
crops <- crops[which(nchar(crops) > 0)]
# Remove any special characters from crops list.
if (!all(stri_enc_isascii(crops), na.rm = TRUE)) {
  # String has non-ASCII characters
  if (!all(stri_enc_isutf8(crops), na.rm = TRUE)) {
    # String has non-UTF8 characters -> assume windows-1252 encoding and
    # convert to UTF-8
    message(
      "Converting column ", sQuote(fertilizer_pattern_map_col),
      " from windows-1252 to UTF-8 encoding"
    )
    crops <- stri_encode(crops, "windows-1252", "UTF-8")
  }
  # Convert UTF-8 strings to ASCII strings, if necessary translating
  # special characters
  message(
    "Converting column ", sQuote(fertilizer_pattern_map_col),
    " from UTF-8 to ASCII encoding"
  )
  crops <- stri_encode(crops, "UTF-8", "UTF-8")
  crops <- stri_trans_general(crops, "latin-ascii")
}

cat(
  "Attempting to download data for", length(crops), "crop(s)",
  "and", length(fertilizer_pattern_nutrients), "nutrient(s)\n"
)
if (!dir.exists(fertilizer_pattern_dir)) {
  dir.create(fertilizer_pattern_dir, recursive = TRUE)
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
for (nut in fertilizer_pattern_nutrients) {
  cat("Nutrient:", nut, "\n")
  for (crop in crops) {
    # Expected filename pattern: [CROP][NUTRIENT]apprate.nc.gz
    filename <- paste0(crop, nut, "apprate.nc.gz")
    # Download URL on Zenodo server. Valid at the time of writing this script.
    download_url <- paste0(fertilizer_pattern_base_url, filename, "?download=1")
    tryCatch(
      download.file(
        url = download_url,
        destfile = file.path(fertilizer_pattern_dir, filename),
        method = "libcurl",
        quiet = TRUE
      ),
      error = function(e) cat(e$message, "\n")
    )
    if (file.exists(file.path(fertilizer_pattern_dir, filename)) &&
      isGzipped(file.path(fertilizer_pattern_dir, filename), method = "content")
    ) {
      # Decompress file. This removes the downloaded gzip file unless remove is
      # set to FALSE.
      gunzip(
        file.path(fertilizer_pattern_dir, filename),
        remove = TRUE,
        overwrite = TRUE
      )
    } else {
      cat("Cannot decompress", file.path(fertilizer_pattern_dir, filename), "\n")
    }
    if (which(crops == crop) %in% progress_step) {
      cat(round(which(crops == crop) / length(crops) * 100), "% finished\n")
    }
  }
}
