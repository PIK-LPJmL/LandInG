################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This scripts reads AQUASTAT data and reformats for use in further scripts. ##
## The script can run in interactive or non-interactive mode depending on     ##
## setting aquastat_file_run_interactively in landuse_setup.R                 ##
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
## Load Aquastat CSV flat file                                                ##
cat("Loading Aquastat data from", sQuote(aquastat_file), "\n")
if (aquastat_file_all_column_names) {
  aquastat_rawdata <- read.csv(
    aquastat_file,
    strip.white = TRUE,
    quote = "\"",
    flush = TRUE,
    colClasses = aquastat_file_column_classes,
    stringsAsFactors = FALSE,
    skip = aquastat_file_empty_rows
  )
} else {
  column_names <- scan(
    aquastat_file,
    what = character(),
    sep = ",",
    skip = aquastat_file_empty_rows,
    nlines = 1,
    quote = "\"",
    quiet = TRUE
  )
  if (length(column_names) < length(aquastat_file_column_classes)) {
    cat(
      "Adding dummy column name(s)",
      toString(
        paste0(
          "Unnamed",
          seq(1, length.out = length(aquastat_file_column_classes) -
              length(column_names))
        )
      ),
      "\n"
    )
    column_names <- c(
      column_names,
      paste0(
        "Unnamed",
        seq(1, length.out = length(aquastat_file_column_classes) -
            length(column_names))
      )
    )
  } else if (length(column_names) > length(aquastat_file_column_classes)) {
    stop(
      "Number of elements in aquastat_file_column_classes (",
      length(aquastat_file_column_classes),
      ") does not match column_names read from ", sQuote(aquastat_file), ": ",
      toString(sQuote(column_names)),
      " (", length(column_names), " names read in total)"
    )
  }
  aquastat_rawdata <- read.csv(
    aquastat_file,
    strip.white = TRUE,
    quote = "\"",
    flush = TRUE,
    colClasses = aquastat_file_column_classes,
    stringsAsFactors = FALSE,
    skip = aquastat_file_empty_rows + 1,
    header = FALSE
  )
  colnames(aquastat_rawdata) <- column_names
}
# Split into data part and Metadata
# Data rows: First 4 columns cannot be empty
aquastat_datarows <- which(
  !apply(aquastat_rawdata, 1, function(indata) any(is.na(indata[1:4])))
)
# Footnote rows: Row starts with [#] in first column
aquastat_footrows <- grep("^\\[[0-9]+\\]", aquastat_rawdata[, 1])
# Extract footnote numbers from footnote rows
aquastat_footindex <- as.numeric(
  unlist(
    regmatches(
      aquastat_rawdata[aquastat_footrows, 1],
      # Search pattern: only numbers
      regexec("[0-9]+", aquastat_rawdata[aquastat_footrows, 1])
    )
  )
)
cat(
  "Detected",
  length(aquastat_datarows), "rows with data and",
  length(aquastat_footrows), "footnotes\n"
)
################################################################################

################################################################################
## FAOSTAT definitions and standards                                          ##
fao_production_country_def <- fread(
  fao_production_country_file,
  na.strings = "...",
  # country code for Namibia is "NA", which would normally be converted into NA
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)

# Clean up encoding
message("Converting special characters if necessary.")
for (table in c("fao_production_country_def", "aquastat_rawdata")) {
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
## Consistenxy checks                                                         ##
## Get country names and codes assuming that those are named either           ##
## "country *" or "area *"                                                    ##
index <- grep(
  "area|country",
  colnames(aquastat_rawdata),
  ignore.case = TRUE,
  value = TRUE
)
aquastat_countries <- unique(aquastat_rawdata[aquastat_datarows, index])
cat(
  "Columns containing country information:",
  toString(sQuote(colnames(aquastat_countries))), "\n"
)
# Get variables assuming that those columns are named "variable *"
index <- grep("variable", colnames(aquastat_rawdata), ignore.case = TRUE)
aquastat_vars <- unique(aquastat_rawdata[aquastat_datarows, index])
val_col <- grep(
  "value",
  colnames(aquastat_rawdata),
  ignore.case = TRUE,
  value = TRUE
)
if (length(val_col) != 1) {
  stop("Cannot determine value column automatically in ", sQuote(aquastat_file))
} else {
  cat("Column containing values:", sQuote(val_col), "\n")
}
cat(
  "Columns containing variable information:",
  toString(sQuote(colnames(aquastat_vars))), "\n"
)
# Get year range assuming that column name includes "year"
year_col <- grep(
  "year",
  colnames(aquastat_rawdata),
  ignore.case = TRUE,
  value = TRUE
)
if (length(year_col) != 1) {
  stop("Cannot determine year column automatically in ", sQuote(aquastat_file))
} else {
  cat("Column containing year information:", sQuote(year_col), "\n")
}
aquastat_years <- range(
  aquastat_rawdata[aquastat_datarows, year_col],
  na.rm = TRUE
)
# Metadata column. Index is defined in landuse_setup.R
cat(
  "Column(s) containing Metadata:",
  toString(sQuote(colnames(aquastat_rawdata)[aquastat_file_metadata_col])), "\n"
)
metadata_col <- colnames(aquastat_rawdata)[aquastat_file_metadata_col]

# Consistency check for names
if (!aquastat_file_use_FAOSTAT_country_col %in%
  colnames(fao_production_country_def)
) {
  stop(
    "aquastat_file_use_FAOSTAT_country_col ",
    sQuote(aquastat_file_use_FAOSTAT_country_col),
    " not found in fao_production_country_def.\n",
    "Check setting in landuse_setup.R"
  )
}
# Country ID column
index1 <- grep("id", colnames(aquastat_countries), ignore.case = TRUE)
# Country column
index2 <- grep(
  "id",
  colnames(aquastat_countries),
  ignore.case = TRUE,
  invert = TRUE,
  value = TRUE
)
mismatch <- which(!aquastat_countries[, index1] %in%
  fao_production_country_def[, aquastat_file_use_FAOSTAT_country_col]
)
if (length(mismatch) > 0) {
  stop(
    "Aquastat contains countries not included in ",
    "FAOSTAT country definitions: ",
    toString(sQuote(aquastat_countries[mismatch, index2])), "\n",
    "Please confirm that you have set the correct value for ",
    "aquastat_file_use_FAOSTAT_country_col in landuse_setup.R"
  )
  # If this error continues to occur even if the correct
  # aquastat_file_use_FAOSTAT_country_col is set you will have to change this
  # to a warning instead of an error.
}
index <- match(
  aquastat_countries[, index1],
  fao_production_country_def[, aquastat_file_use_FAOSTAT_country_col]
)
newnames <- fao_production_country_def$Country[index]
for (n in which(newnames != aquastat_countries[, index2])) {
  cat(
    "Renaming", sQuote(aquastat_countries[n, index2]),
    "in Aquastat data to match name in FAOSTAT data:",
    sQuote(newnames[n]), "\n"
  )
  index <- which(aquastat_rawdata[, index2] == aquastat_countries[n, index2])
  aquastat_rawdata[index, index2] <- newnames[n]
  aquastat_countries[n, index2] <- newnames[n]
}
# Clean up crop names
# Extract crop names and permanence (temporary/permanent) from variable names
# ID column
index1 <- grep("id", colnames(aquastat_vars), ignore.case = TRUE, invert = TRUE)
varname_table <- data.frame(
  aquastat_vars,
  permanence = unlist(
    regmatches(
      aquastat_vars[, index1],
      regexec("temporary|permanent", aquastat_vars[, index1])
    )
    # Matches "temporary" or "permanent"
  ),
  cropname = unlist(
    regmatches(
      aquastat_vars[, index1],
      regexec("[A-Za-z]+[[:alnum:]|[:space:]]*$", aquastat_vars[, index1])
    )
    # Matches the remaining string after ": "
  ),
  stringsAsFactors = FALSE
)
# Count how often each crop name exists (there are permanent and temporary group
# categories)
varname_table <- data.frame(
  varname_table,
  rep = c(table(varname_table$cropname)[varname_table$cropname])
)
# Create new names for crops. Add permanence for those that occur more than
# once. All others just get the crop name without permanence.
varname_table <- data.frame(
  varname_table,
  newname = paste0(
    varname_table$cropname,
    ifelse(
      varname_table$rep > 1,
      paste0(" (", varname_table$permanence, ")"),
      ""
    )
  ),
  stringsAsFactors = FALSE
)
# ID column
index1 <- grep("id", colnames(varname_table)[1:2], ignore.case = TRUE)
# Value column
index2 <- grep(
  "id", colnames(varname_table)[1:2],
  ignore.case = TRUE,
  invert = TRUE
)
# Order by ID
index3 <- order(varname_table[, index1])
# Prints:
# ID: original AQUASTAT name -> new name
cat("Variable names are renamed as:\n")
print(
  paste(
    paste(
      varname_table[index3, index1],
      sQuote(varname_table[index3, index2]),
      sep = ": "
    ),
    sQuote(varname_table$newname[index3]),
    sep = " -> "
  )
)
if (!ud.are.convertible(aquastat_area_source_units, aquastat_area_units)) {
  stop(
    "aquastat_area_source_units ", sQuote(aquastat_area_source_units),
    " cannot be converted to aquastat_area_units ", sQuote(aquastat_area_units)
  )
} else {
  cat(
    "Areas in AQUASTAT data will be converted from",
    sQuote(aquastat_area_source_units), "to", sQuote(aquastat_area_units), "\n"
  )
}
################################################################################

################################################################################
## Convert data into array with dimensions country, crop, years               ##
## If aquastat_file_run_interactively == TRUE, user is given choice how to    ##
## handle rows with metadata entries. Otherwise, default choices are made.    ##
# Country column name
index <- grep(
  "id",
  colnames(aquastat_countries),
  ignore.case = TRUE,
  invert = TRUE,
  value = TRUE
)
aquastat_array <- aquastat_metadata <- array(
  dim = c(
    nrow(aquastat_countries),
    nrow(aquastat_vars),
    length(min(aquastat_years):max(aquastat_years))
  ),
  dimnames = list(
    aquastat_countries[, index],
    varname_table$newname,
    format(min(aquastat_years):max(aquastat_years), scientific = FALSE)
  )
)

metadata_decision <- list()
if (file.exists(aquastat_rdata)) {
  warning("Output file ", sQuote(aquastat_rdata), " exists already.",
          call. = FALSE, immediate. = TRUE)
  load(aquastat_rdata)
  if (aquastat_file_run_interactively) {
    choice <- ""
    if (aquastat_aquastat_file_run_interactively !=
        aquastat_file_run_interactively
    ) {
      message(
        "Additional warning: Existing outfile ", sQuote(aquastat_rdata),
        " used non-interactive mode while current run uses interactive mode",
        " which may lead to different results."
      )
    }
    if (aquastat_file_used != aquastat_file) {
      message(
        "Additional warning: Existing outfile ", sQuote(aquastat_rdata),
        " was based on ", sQuote(aquastat_file_used),
        " whereas aquastat_file is set to ", sQuote(aquastat_file),
        " for this run."
      )
    }
    # Ask user how to proceed
    while (!choice %in% c("p", "c")) {
      choice <- readline(
        paste(
          "You may either \"p\"roceed and overwrite existing outfile or",
          "\"c\"ancel this operation: "
        )
      )
    }
    if (choice == "c") {
      # End run
      stop("Run canceled by user")
    }
  } else {
    if (aquastat_aquastat_file_run_interactively !=
        aquastat_file_run_interactively
    ) {
      stop(
        "Outfile ", sQuote(aquastat_rdata),
        " used interactive mode while current run uses non-interactive mode",
        " which may lead to different results.\n",
        "Change aquastat_rdata or remove existing file to force re-run."
      )
    }
    if (aquastat_file_used != aquastat_file) {
      stop(
        "Existing outfile ", sQuote(aquastat_rdata),
        " was based on ", sQuote(aquastat_file_used),
        " whereas aquastat_file is set to ", sQuote(aquastat_file),
        " for this run.\n",
        "Change aquastat_rdata or remove existing file to force re-run."
      )
    }
  }
  cat("Existing outfile", sQuote(aquastat_rdata), "will be overwritten.\n")
}

if (aquastat_file_run_interactively) {
  cat(
    "Running in interactive mode. You will be asked to decide on how to",
    "handle values with additional metadata.\n"
  )
} else {
  cat(
    "Running in non-interactive mode.",
    "Default decision will be taken automatically.",
    "Set aquastat_file_run_interactively to TRUE to take decisions yourself.\n"
  )
}
for (r in aquastat_datarows) {
  # ID column name
  index <- grep("id", colnames(aquastat_vars), ignore.case = TRUE, value = TRUE)
  # Crop ID in raw data
  cropid <- aquastat_rawdata[r, index]
  # ID column name in varname_table
  index <- grep("id", colnames(varname_table), ignore.case = TRUE, value = TRUE)
  # New crop name
  crop <- varname_table$newname[match(cropid, varname_table[, index])]
  # Country column name
  index <- grep(
    "id",
    colnames(aquastat_countries),
    ignore.case = TRUE,
    invert = TRUE,
    value = TRUE
  )
  # Country name
  country <- aquastat_rawdata[r, index]
  # Placeholder for new crop name
  newcrop <- ""
  year <- aquastat_rawdata[r, year_col]

  # Initiate choice variable
  choice <- ""
  if (any(nchar(aquastat_rawdata[r, metadata_col]) > 0, na.rm = TRUE)) {
    # Row has metadata
    # Extract footnote numbers; expects string in the form of [##] or [##,##]
    # or more elements
    footnotes <- as.numeric(
      unlist(
        regmatches(
          # Split metadata by comma to distinguish multiple footnote numbers
          unlist(strsplit(toString(aquastat_rawdata[r, metadata_col]), ",")),
          regexec(
            # Search pattern: only numbers
            pattern = "[0-9]+",
            # Split metadata by comma to distinguish multiple footnote numbers
            unlist(strsplit(toString(aquastat_rawdata[r, metadata_col]), ","))
          )
        )
      )
    )
    # Find corresponding footnotes in aquastat_footrow and remove [##] from
    # beginning of string
    index <- aquastat_footrows[match(footnotes, aquastat_footindex)]
    footnotes <- gsub("\\[[0-9]+\\] ", "", aquastat_rawdata[index, 1])
    # Determine type of footnote, given by [***] at beginning of footnote string
    footnotetypes <- unlist(
      regmatches(footnotes, regexec("\\[[^]]+\\]", footnotes))
    )
    # Extract value of footnote
    footnotevalues <- sapply(
      regmatches(footnotes, regexec("[|] (.*)", footnotes)),
      function(indata) indata[-1]
    )
    cat("==========================================================\n")
    cat(
      "Data value",
      sQuote(
        paste(
          format(
            ud.convert(
              aquastat_rawdata[r, val_col],
              aquastat_area_source_units,
              aquastat_area_units
            ),
            scientific = FALSE
          ),
          aquastat_area_units
        )
      ),
      "for", sQuote(crop), "in", sQuote(country), "and year", year,
      "includes additional metadata:\n"
    )
    # Check for footnotes of type "reference area"
    index <- grep("reference area", footnotetypes, ignore.case = TRUE)
    if (length(index) > 0) {
      # Data has limited reference area
      cat(
        "Limited reference area:",
        toString(footnotevalues[index]), "\n"
      )
      if (aquastat_file_run_interactively) {
        # Check if user has made a choice for the same footnote before, if so
        # tell them
        for (c in intersect(footnotevalues[index], names(metadata_decision))) {
          cat(
            "Info: You have previously chosen to",
            switch(
              metadata_decision[[c]],
              "k" = "'k'eep a value with this metadata",
              "r" = paste("'r'ename a value to", sQuote(c)),
              "d" = "'d'rop a value with this metadata"
            ),
            "\n"
          )
        }
        choice <- readline(
          paste(
            "Your options: \"d\"rop data point (suggested default action) or",
            "\"k\"eep data point despite limitation: "
          )
        )
        # Check for valid choice
        while (!choice %in% c("d", "k")) {
          cat(
            "Invalid choice ", sQuote(choice), ". Please try again.\n",
            sep = ""
          )
          choice <- readline(
            paste(
              "Your options: \"d\"rop data point (suggested default action) or",
              "\"k\"eep data point despite limitation: "
            )
          )
        }
      } else {
        # Default in non-interactive mode to drop values with limited reference
        # area
        choice <- "d"
      }
      metadata_decision[footnotevalues[index]] <- choice
    }
    # Check for footnotes of type "reference period"
    # Do not process footnote if user has already decided to drop value
    index <- grep("reference period", footnotetypes, ignore.case = TRUE)
    if (length(index) > 0 && choice != "d") {
      cat(
        "Reference period information:",
        toString(footnotevalues[index]), "\n"
      )
      if (aquastat_file_run_interactively) {
        # Check if user has made a choice for the same footnote before, if so
        # tell them
        for (c in intersect(footnotevalues[index], names(metadata_decision))) {
          cat(
            "Info: You have previously chosen to",
            switch(
              metadata_decision[[c]],
              "k" = "'k'eep a value with this metadata",
              "r" = paste("'r'ename a value to", sQuote(c)),
              "d" = "'d'rop a value with this metadata"
            ),
            "\n"
          )
        }
        choice <- readline(
          paste(
            "Your options: \"k\"eep data point (suggested default action) or",
            "\"d\"rop data point: "
          )
        )
        # Check for valid choice
        while (!choice %in% c("d", "k")) {
          cat(
            "Invalid choice ", sQuote(choice), ". Please try again.\n",
            sep = ""
          )
          choice <- readline(
            paste(
              "Your options: \"k\"eep data point (suggested default action) or",
              "\"d\"rop data point: "
            )
          )
        }
      } else {
        # Default in non-interactive mode to keep values with reference period
        choice <- "k"
      }
      metadata_decision[footnotevalues[index]] <- choice
    }
    # Check for footnotes of type "observation" or "component"
    # Do not process footnote if user has already decided to drop value
    index <- which(
      grepl("observation", footnotetypes, ignore.case = TRUE) |
      grepl("component", footnotetypes, ignore.case = TRUE)
    )
    if (length(index) > 0 && choice != "d") {
      cat(
        ifelse(
          grepl("observation", footnotetypes, ignore.case = TRUE),
          "Observations:",
          "Components information:"
        ),
        toString(footnotevalues[index]), "\n"
      )
      if (aquastat_file_run_interactively) {
        textonly <- which(!grepl("[0-9]+", footnotevalues[index]))
        if (length(textonly) > 0) {
          # Only text in footnote
          # The text could refer to a different (more specific) crop name
          newcrop <- footnotevalues[index][textonly]
          cat(
            "You may choose to replace crop type ", sQuote(crop),
            " with ", sQuote(newcrop), ". Do this only if the replacement is",
            " a unique crop type that you want to use.\n",
            sep = ""
          )
          # Check if user has made a choice for the same footnote before, if so
          # tell them
          for (c in intersect(footnotevalues[index], names(metadata_decision))) {
            cat(
              "Info: You have previously chosen to",
              switch(
                metadata_decision[[c]],
                "k" = "'k'eep a value with this metadata",
                "r" = paste("'r'ename a value to", sQuote(c)),
                "d" = "'d'rop a value with this metadata"
              ),
              "\n"
            )
          }
          choice <- readline(
            paste(
              "Your options: \"k\"eep value for", sQuote(crop),
              "(suggested default action) or",
              "\"r\"ename value to crop", sQuote(newcrop), "or",
              "\"d\"rop this data point completely: "
            )
          )
          # Check for valid choice
          while (!choice %in% c("k", "r", "d")) {
            cat(
              "Invalid choice ", sQuote(choice), ". Please try again.\n",
              sep = ""
            )
            choice <- readline(
              paste(
                "Your options: \"k\"eep value for", sQuote(crop),
                "(suggested default action) or",
                "\"r\"ename value to crop", sQuote(newcrop), "or",
                "\"d\"rop this data point completely: "
              )
            )
          }
        } else {
          # Combination of text and numbers in footnote
          # Check if user has made a choice for the same footnote before, if so
          # tell them
          for (c in intersect(footnotevalues[index], names(metadata_decision))) {
            cat(
              "Info: You have previously chosen to",
              switch(
                metadata_decision[[c]],
                "k" = "'k'eep a value with this metadata",
                "r" = paste("'r'ename a value to", sQuote(c)),
                "d" = "'d'rop a value with this metadata"
              ),
              "\n"
            )
          }
          choice <- readline(
            paste(
              "Your options: \"k\"eep value (suggested default action) or",
              "\"d\"rop value: "
            )
          )
          # Check for valid choice
          while (!choice %in% c("d", "k")) {
            cat(
              "Invalid choice ", sQuote(choice), ". Please try again.\n",
              sep = ""
            )
            choice <- readline(
              paste(
                "Your options: \"k\"eep value (suggested default action) or",
                "\"d\"rop value: "
              )
            )
          }
        }
      } else {
        # Default in non-interactive mode to keep values as is
        choice <- "k"
      }
      metadata_decision[footnotevalues[index]] <- choice
    }
    # Check for footnotes of type "accuracy"
    # Do not process footnote if user has already decided to drop value
    index <- grep("accuracy", footnotetypes, ignore.case = TRUE)
    if (length(index) > 0 && choice != "d") {
      cat("Accuracy information:", toString(footnotevalues[index]), "\n")
      if (aquastat_file_run_interactively) {
        # Check if user has made a choice for the same footnote before, if so
        # tell them
        for (c in intersect(footnotevalues[index], names(metadata_decision))) {
          cat(
            "Info: You have previously chosen to",
            switch(
              metadata_decision[[c]],
              "k" = "'k'eep a value with this metadata",
              "r" = paste("'r'ename a value to", sQuote(c)),
              "d" = "'d'rop a value with this metadata"
            ),
            "\n"
          )
        }
        choice <- readline(
          paste(
            "Your options: \"k\"eep value (suggested default action) or",
            "\"d\"rop value: "
          )
        )
        # Check for valid choice
        while (!choice %in% c("d", "k")) {
          cat(
            "Invalid choice ", sQuote(choice), ". Please try again.\n",
            sep = ""
          )
          choice <- readline(
            paste(
              "Your options: \"k\"eep value (suggested default action) or",
              "\"d\"rop value: "
            )
          )
        }
      } else {
        # Default in non-interactive mode to keep values as is
        choice <- "k"
      }
      metadata_decision[footnotevalues[index]] <- choice
    }
    # Check for footnotes of type "adjust"
    # Do not process footnote if user has already decided to drop value
    index <- grep("adjust", footnotetypes, ignore.case = TRUE)
    if (length(index) > 0 && choice != "d") {
      cat("Adjustment information:", toString(footnotevalues[index]), "\n")
      if (aquastat_file_run_interactively) {
        # Check if user has made a choice for the same footnote before, if so
        # tell them
        for (c in intersect(footnotevalues[index], names(metadata_decision))) {
          cat(
            "Info: You have previously chosen to",
            switch(
              metadata_decision[[c]],
              "k" = "'k'eep a value with this metadata",
              "r" = paste("'r'ename a value to", sQuote(c)),
              "d" = "'d'rop a value with this metadata"
            ),
            "\n"
          )
        }
        choice <- readline(
          paste(
            "Your options: \"k\"eep value (suggested default action) or",
            "\"d\"rop value: "
          )
        )
        # Check for valid choice
        while (!choice %in% c("d", "k")) {
          cat(
            "Invalid choice ", sQuote(choice), ". Please try again.\n",
            sep = ""
          )
          choice <- readline(
            paste(
              "Your options: \"k\"eep value (suggested default action) or",
              "\"d\"rop value: "
            )
          )
        }
      } else {
        # Default in non-interactive mode to keep values as is
        choice <- "k"
      }
      metadata_decision[footnotevalues[index]] <- choice
    }
    # Check for footnotes of type "comparison"
    # Do not process footnote if user has already decided to drop value
    index <- grep("compar", footnotetypes, ignore.case = TRUE)
    index2 <- grep("compar", footnotetypes, ignore.case = TRUE, value = TRUE)
    if (length(index) > 0 && choice != "d") {
      cat(
        sapply(
          regmatches(index2, regexec("-([^-]*)", index2)),
          function(indata) indata[-1]
        ),
        "information:",
        toString(footnotevalues[index]), "\n"
      )
      if (aquastat_file_run_interactively) {
        # Check if user has made a choice for the same footnote before, if so
        # tell them
        for (c in intersect(footnotevalues[index], names(metadata_decision))) {
          cat(
            "Info: You have previously chosen to",
            switch(
              metadata_decision[[c]],
              "k" = "'k'eep a value with this metadata",
              "r" = paste("'r'ename a value to", sQuote(c)),
              "d" = "'d'rop a value with this metadata"
            ),
            "\n"
          )
        }
        choice <- readline(
          paste(
            "Your options: \"k\"eep value (suggested default action) or",
            "\"d\"rop value: "
          )
        )
        # Check for valid choice
        while (!choice %in% c("d", "k")) {
          cat(
            "Invalid choice ", sQuote(choice), ". Please try again.\n",
            sep = ""
          )
          choice <- readline(
            paste(
              "Your options: \"k\"eep value (suggested default action) or",
              "\"d\"rop value: "
            )
          )
        }
      } else {
        # Default in non-interactive mode to keep values as is
        choice <- "k"
      }
      metadata_decision[footnotevalues[index]] <- choice
    }
    # Still no choice, not a predefined footnote type
    if (choice == "") {
      cat(
        "Unknown metadata information:",
        footnotetypes, footnotevalues, "\n"
      )
      if (aquastat_file_run_interactively) {
        choice <- readline(
          paste(
            "Your options: \"k\"eep value (suggested default action) or",
            "\"d\"rop value: "
          )
        )
        # Check for valid choice
        while (!choice %in% c("d", "k")) {
          cat(
            "Invalid choice ", sQuote(choice), ". Please try again.\n",
            sep = ""
          )
          choice <- readline(
            paste(
              "Your options: \"k\"eep value (suggested default action) or",
              "\"d\"rop value: "
            )
          )
        }
      } else {
        # Default in non-interactive mode to keep values as is
        choice <- "k"
      }
    }
    if (choice == "r") {
      if (newcrop == "") {
        # This should not happen since "rename" is only valid choice for
        # footnote type where newcrop is determined
        stop("Empty newcrop")
      }
      # Check if new name is already in aquastat_array, otherwise add crop
      if (!newcrop %in% dimnames(aquastat_array)[[2]]) {
        cat("Adding crop type", sQuote(newcrop), "to aquastat_array\n")
        aquastat_array <- abind(
          aquastat_array,
          array(
            dim = c(dim(aquastat_array)[1], 1, dim(aquastat_array)[3]),
            dimnames = list(
              dimnames(aquastat_array)[[1]],
              newcrop,
              dimnames(aquastat_array)[[3]]
            )
          ),
          along = 2
        )
        aquastat_metadata <- abind(
          aquastat_metadata,
          array(
            dim = c(dim(aquastat_array)[1], 1, dim(aquastat_array)[3]),
            dimnames = list(
              dimnames(aquastat_array)[[1]],
              newcrop,
              dimnames(aquastat_array)[[3]]
            )
          ),
          along = 2
        )
      }
      # Set new crop name
      crop <- newcrop
      newcrop <- ""
      # Change choice from rename to keep (but for new crop name)
      choice <- "k"
    }
    aquastat_metadata[country, crop, format(year, scientific = FALSE)] <- choice
    if (choice == "d") {
      cat("Dropping value ")
    }
    if (choice == "k") {
      cat("Saving value ")
    }
    cat(
      sQuote(
        paste(
          ud.convert(
            aquastat_rawdata[r, val_col],
            aquastat_area_source_units,
            aquastat_area_units
          ),
          aquastat_area_units
        )
      ),
      "for crop", sQuote(crop), "in", sQuote(country), "and year", year, "\n"
    )
  }
  if (choice != "d") {
    # Put value from aquastat_rawdata into aquastat_array
    aquastat_array[country, crop, format(year, scientific = FALSE)] <-
      ud.convert(
        aquastat_rawdata[r, val_col],
        aquastat_area_source_units,
        aquastat_area_units
      )
  }
}
################################################################################


################################################################################
## Save data to aquastat_rdata (defined in landuse_setup.R) for further use   ##
aquastat_aquastat_file_run_interactively <- aquastat_file_run_interactively
aquastat_file_used <- aquastat_file
cat(
  "Saving Aquastat data 'aquastat_array', interactive vs. automatic mode,",
  "and information about choices made based on additional metadata",
  "'aquastat_metadata' to file", sQuote(aquastat_rdata), "\n"
)
save(
  aquastat_array,
  aquastat_metadata,
  aquastat_aquastat_file_run_interactively,
  aquastat_file_used,
  aquastat_countries,
  file = aquastat_rdata
)
################################################################################
