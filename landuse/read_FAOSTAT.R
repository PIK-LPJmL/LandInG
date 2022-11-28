################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script reads in FAOSTAT production and land use data and reformats    ##
## for further use in other scripts.                                          ##
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
## Unit conversion rules; name refers to origin, values refers to target unit ##
## This uses unit conversion functionality of udunits2 package                ##
## fao_yield_units, fao_production_units, fao_area_units defined in           ##
## landuse_setup.R.                                                           ##
unit_conversion <- list(
  "hg/ha" = fao_yield_units,
  "tonnes" = fao_production_units,
  "1000 ha" = fao_area_units
)
# Check validity of unit conversion rules
for (conv in names(unit_conversion)) {
  if (!ud.are.convertible(conv, unit_conversion[[conv]])) {
    stop("Cannot convert [", conv, "] to [", unit_conversion[[conv]], "]")
  }
}
################################################################################

################################################################################
## List of land use variables to load. Provide either a vector of Item names  ##
## or character(0) to load all                                                ##
landuse_vars <- c(
  "Cropland",
  "Land with temporary fallow",
  "Land under temp. meadows and pastures",
  "Land under perm. meadows and pastures"
)
################################################################################

################################################################################
## Processing of production data                                              ##
cat("Production data from", sQuote(fao_production_file), "\n")
cat(
  "Definitions and standards from",
  toString(
    sQuote(
      c(fao_production_country_file, fao_production_country_group_file,
        fao_production_item_file, fao_production_item_group_file)
    )
  ),
  "\n"
)

production_data <- fread(
  fao_production_file,
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
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
fao_production_item_def <- fread(
  fao_production_item_file,
  na.strings = "...",
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
fao_production_item_group_def <- fread(
  fao_production_item_group_file,
  na.strings = "...",
  check.names = TRUE,
  data.table = FALSE,
  header = TRUE
)
# Clean encoding
message("Converting special characters if necessary.")
for (table in c(
  "production_data",
  "fao_production_country_def",
  "fao_production_country_group_def",
  "fao_production_item_def",
  "fao_production_item_group_def"
)) {
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
# Determine year columns and flag columns
production_yearcols <- grep("Y[[:digit:]]{4}$", colnames(production_data))
# looks for columns with "Y####"
production_flagcols <- grep("Y[[:digit:]]{4}F$", colnames(production_data))
# looks for columns with "Y####F"
production_years <- as.integer(
  regmatches(
    colnames(production_data)[production_yearcols],
    regexpr("[[:digit:]]{4}$", colnames(production_data)[production_yearcols])
  )
)

# List of available countries, items and elements
production_countries <- unique(production_data[, c("Area.Code", "Area")])
colnames(production_countries) <- c("CODE", "NAME")
production_items <- unique(production_data[, c("Item.Code", "Item")])
colnames(production_items) <- c("CODE", "NAME")
# FAOSTAT have recently changed their data domains and merged crop and
# livestock data. If fao_production_file_includes_livestock is set to TRUE in
# landuse_setup.R, try to filter only crop data.
if (fao_production_file_includes_livestock) {
  cat(
    "** Note: You have selected that your FAOSTAT production data includes",
    "livestock data. **\n"
  )
  cat(
    "** Using only items belonging to the item group(s)",
    toString(
      sQuote(
        unique(
          grep(
            "^crop.*primary",
            fao_production_item_group_def$Item.Group,
            ignore.case = TRUE,
            value = TRUE
          )
        )
      )
    ),
    "**\n"
  )
  # Crop item codes
  index <- grep(
    "^crop.*primary",
    fao_production_item_group_def$Item.Group,
    ignore.case = TRUE
  )
  # Non-crops
  remove <- which(!production_items$CODE %in%
    fao_production_item_group_def$Item.Code[index]
  )
  # Crops
  keep <- which(production_items$CODE %in%
    fao_production_item_group_def$Item.Code[index]
  )
  cat("** Data for", length(remove), "items removed **\n")
  production_items <- production_items[keep, ]
  # Adjust production_data to updated production_items
  keep2 <- which(production_data$Item.Code %in% production_items$CODE)
  production_data <- production_data[keep2, ]
} else if (any(
  grepl(
    "meat|animal|milk",
    fao_production_item_group_def$Item.Group,
    ignore.case = TRUE
  )
)) {
  warning(
    "** You have selected that your FAOSTAT production data does ",
    "not include livestock data, but the following item groups have been ",
    "detected in the data: ",
    toString(
      sQuote(
        unique(
          grep(
            "meat|animal|milk",
            fao_production_item_group_def$Item.Group,
            ignore.case = TRUE,
            value = TRUE
          )
        )
      )
    ),
    ".\nPlease check setting 'fao_production_file_includes_livestock' in ",
    "landuse_setup.R",
    call. = FALSE,
    immediate. = TRUE
  )
}
production_elements <- unique(production_data[, c("Element.Code", "Element")])
colnames(production_elements) <- c("CODE", "NAME")

options(useFancyQuotes = FALSE, width = 200)
# Check if country names in production_data match country lists
print("Checking naming consistency")
# Missing codes
index <- which(!production_countries$CODE %in%
  fao_production_country_def$Country.Code
)
if (length(index) > 0) {
  warning(
    "production_data includes countries not found in ",
    sQuote(fao_production_country_file), ": ",
    toString(sQuote(production_countries$NAME[index])),
    call. = FALSE,
    immediate. = TRUE
  )
}
# Different names for same country code
index <- match(
  production_countries$CODE,
  fao_production_country_def$Country.Code
)
newnames <- fao_production_country_def$Country[index]
if (any(production_countries$NAME != newnames)) {
  mismatch <- which(production_countries$NAME != newnames)
  cat(
    "The following", length(mismatch),
    "country names do not match between production_data and",
    sQuote(fao_production_country_file), "\n"
  )
  print(
    cbind(
      production_data = production_countries$NAME[mismatch],
      fao_production_country_def = newnames[mismatch]
    )
  )
  cat("Renaming in production_data\n")
  for (n in mismatch) {
    index <- which(production_data$Area == production_countries$NAME[n])
    production_data[index, "Area"] <- newnames[n]
    production_countries[n, "NAME"] <-  newnames[n]
  }
}
# Check country group names with respective definitions file
# Country groups have code > 1000
# Missing codes
index <- which(production_countries$CODE > 1000 &
  !production_countries$CODE %in%
  fao_production_country_group_def$Country.Group.Code
)
if (length(index) > 0) {
  warning(
    "production_data includes country groups not found in ",
    sQuote(fao_production_country_group_file), ": ",
    toString(sQuote(production_countries$NAME[index])),
    call. = FALSE,
    immediate. = TRUE
  )
}
# Different names for group with same code
groupids <- which(production_countries$CODE > 1000)
index <- match(
  production_countries$CODE[groupids],
  fao_production_country_group_def$Country.Group.Code
)
newnames <- fao_production_country_group_def$Country.Group[index]
mismatch <- which(production_countries$NAME[groupids] != newnames)
if (any(production_countries$NAME[groupids] != newnames)) {
  cat(
    "The following", length(mismatch),
    "country names do not match between production_data and",
    sQuote(fao_production_country_group_file), "\n"
  )
  print(
    cbind(
      production_data = production_countries$NAME[groupids][mismatch],
      fao_production_country_def = newnames[mismatch]
    )
  )
  cat("Renaming in production_data\n")
  for (n in mismatch) {
    index <- which(
      production_data$Area == production_countries$NAME[groupids[n]]
    )
    production_data[index, "Area"] <- newnames[n]
    production_countries[groupids[n], "NAME"] <-  newnames[n]
  }
}

# Check if item names in production_data match item lists
# Missing codes
mismatch <- which(!production_items$CODE %in% fao_production_item_def$Item.Code)
if (length(mismatch) > 0) {
  warning(
    "production_data includes items not found in ",
    sQuote(fao_production_item_file), ": ",
    toString(sQuote(production_items$NAME[mismatch])),
    call. = FALSE,
    immediate. = TRUE
  )
}
# Different names for same item code
index <- match(production_items$CODE, fao_production_item_def$Item.Code)
newnames <- fao_production_item_def$Item[index]
mismatch <- which(production_items$NAME != newnames)
if (length(mismatch) > 0) {
  cat(
    "The following", length(mismatch),
    "item names do not match between production_data and",
    sQuote(fao_production_item_file), "\n"
  )
  print(
    cbind(
      production_data = production_items$NAME[mismatch],
      fao_production_item_def = newnames[mismatch]
    )
  )
  cat("Renaming items in production_data\n")
  for (n in mismatch) {
    index <- which(production_data$Item == production_items$NAME[n])
    production_data[index, "Item"] <- newnames[n]
    production_items[n, "NAME"] <-  newnames[n]
  }
}
# Check item group names with respective definitions file
# Item groups have code > 1000
# Missing code
index <- which(production_items$CODE > 1000 &
  !production_items$CODE %in% fao_production_item_group_def$Item.Group.Code
)
if (length(index) > 0) {
  warning(
    "production_data includes item groups not found in ",
    sQuote(fao_production_item_group_file), ": ",
    toString(sQuote(production_items$NAME[index])),
    call. = FALSE,
    immediate. = TRUE
  )
}
# Different names for group with same code
groupids <- which(production_items$CODE > 1000)
index <- match(
  production_items$CODE[groupids],
  fao_production_item_group_def$Item.Group.Code
)
newnames <- fao_production_item_group_def$Item.Group[index]
mismatch <- which(production_items$NAME[groupids] != newnames)
if (length(mismatch) > 0) {
  cat(
    "The following", length(mismatch),
    "item names do not match between production_data and",
    sQuote(fao_production_item_group_file), "\n"
  )
  print(
    cbind(
      production_data = production_items$NAME[groupids][mismatch],
      fao_production_item_group_def = newnames[mismatch]
    )
  )
  cat("Renaming items in production_data\n")
  for (code in production_items$CODE[groupids][mismatch]) {
    index <- match(code, fao_production_item_group_def$Item.Group.Code)
    production_data[which(production_data$Item.Code == code), "Item"] <-
      fao_production_item_group_def[index, "Item.Group"]
    production_items[which(production_items$CODE == code), "NAME"] <-
      fao_production_item_group_def[index, "Item.Group"]
  }
}
# Also check item names because there inconsistencies in naming between FAOSTAT
# item list and FAOSTAT item group list
ids <- which(production_items$CODE <= 1000)
index <- match(
  production_items$CODE[ids],
  fao_production_item_group_def$Item.Code
)
newnames <- fao_production_item_group_def$Item[index]
mismatch <- which(production_items$NAME[ids] != newnames)
if (length(mismatch) > 0) {
  cat(
    "The following", length(mismatch),
    "item names do not match between production_data and",
    sQuote(fao_production_item_group_file), "\n"
  )
  print(
    cbind(
      production_data = production_items$NAME[ids][mismatch],
      fao_production_item_group_def = newnames[mismatch]
    )
  )
  cat("Renaming items in production_data\n")
  for (n in mismatch) {
    index <- which(production_data$Item == production_items$NAME[ids][n])
    production_data[index, "Item"] <- newnames[n]
    production_items[ids[n], "NAME"] <- newnames[n]
  }
}

# Convert data into array with dimensions country, item, element, years
production_array <- array(
  dim = c(
    nrow(production_countries),
    nrow(production_items),
    nrow(production_elements),
    length(production_years)
  ),
  dimnames = list(
    production_countries$NAME,
    production_items$NAME,
    production_elements$NAME,
    format(production_years, scientific = FALSE)
  )
)
conv_msg <- list() # dummy variable keeping track of unit conversion messages
cat(
  "Parsing FAO production data for",
  dim(production_array)[1], "countries/country groups from",
  sQuote(fao_production_file), "\n"
)
for (country in production_countries$NAME) {
  country_data <- production_data[which(production_data$Area == country), ]
  for (item in unique(country_data$Item)) {
    item_data <- country_data[which(country_data$Item == item), ]
    for (element in unique(item_data$Element)) {
      index <- which(item_data$Element == element)
      if (length(index) != 1) {
        stop(
          length(index),
          " lines for Element ", sQuote(element),
          " Item ", sQuote(item),
          " in country ", sQuote(country), "\n",
          "Expecting exactly one line."
        )
      }
      if (item_data[index, "Unit"] %in% names(unit_conversion)) {
        conv_factor <- ud.convert(1,
          item_data[index, "Unit"],
          unit_conversion[[item_data[index, "Unit"]]]
        )
        if (is.null(conv_msg[[element]])) {
          message(
            "Converting ", sQuote(element),
            " from ", item_data[index, "Unit"],
            " to ", unit_conversion[[item_data[index, "Unit"]]]
          )
          conv_msg[[element]] <- TRUE
        }
      } else {
        conv_factor <- 1
      }
      if (grepl("harvested", element, ignore.case = TRUE)) {
        if (ud.convert(1, item_data[index, "Unit"], fao_area_units) != 1 &&
          is.null(conv_msg[[element]])
        ) {
          warning(
            "Unit of element ", sQuote(element),
            " [", item_data[index, "Unit"], "] ",
            "differs from defined fao_area_units [", fao_area_units, "]. ",
            "All further processing scripts expect harvested areas to be in",
            " [", fao_area_units, "].\n",
            "Please add conversion rule from",
            " [", item_data[index, "Unit"], "] to [", fao_area_units, "].",
            call. = FALSE,
            immediate. = TRUE
          )
          conv_msg[[element]] <- TRUE
        }
      }
      production_array[country, item, element, ] <-
        as.numeric(item_data[index, production_yearcols]) * conv_factor
    }
  }
}
################################################################################


################################################################################
## Processing of landuse data                                                 ##
cat("Landuse data from", sQuote(fao_landuse_file), "\n")
cat(
  "Definitions and standards from",
  toString(sQuote(c(fao_landuse_country_file, fao_landuse_country_group_file))),
  "\n"
)
landuse_data <- fread(
  fao_landuse_file,
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
# Clean encoding
for (table in c(
  "landuse_data",
  "fao_landuse_country_def",
  "fao_landuse_country_group_def"
)) {
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

# Determine year columns and flag columns
landuse_yearcols <- grep("Y[[:digit:]]{4}$", colnames(landuse_data))
# looks for columns with "Y####"
landuse_flagcols <- grep("Y[[:digit:]]{4}F$", colnames(landuse_data))
# looks for columns with "Y####F"
landuse_years <- as.integer(
  regmatches(
    colnames(landuse_data)[landuse_yearcols],
    regexpr("[[:digit:]]{4}$", colnames(landuse_data)[landuse_yearcols])
  )
)

# List of available countries, items and elements
landuse_countries <- unique(landuse_data[, c("Area.Code", "Area")])
colnames(landuse_countries) <- c("CODE", "NAME")
landuse_items <- unique(landuse_data[, c("Item.Code", "Item")])
colnames(landuse_items) <- c("CODE", "NAME")
if (length(landuse_vars) > 0) {
  landuse_items <- landuse_items[which(landuse_items$NAME %in% landuse_vars), ]
  if (nrow(landuse_items) < length(landuse_vars)) {
    stop(
      "Desired landuse_vars ",
      toString(sQuote(landuse_vars)),
      " are not available in list of FAO items ",
      toString(sQuote(unique(landuse_data$Item)))
    )
  }
}
index <- which(landuse_data$Item %in% landuse_items$NAME)
landuse_elements <- unique(landuse_data[index, c("Element.Code", "Element")])
colnames(landuse_elements) <- c("CODE", "NAME")

# Check if country names in landuse_data match country lists
mismatch <- which(!landuse_countries$CODE %in%
  fao_landuse_country_def$Country.Code
)
if (length(mismatch) > 0) {
  warning(
    "landuse_data includes countries not found in ",
    sQuote(fao_landuse_country_file), ": ",
    toString(sQuote(landuse_countries$NAME[mismatch])), ".",
    call. = FALSE,
    immediate. = TRUE
  )
  message(
    "This points to inconsistencies between the data and its metadata. ",
    "Removing data to avoid problems in further processing. ",
    "Otherwise, update metadata."
  )
  rem <- which(!landuse_data$Area.Code %in%
    fao_landuse_country_def$Country.Code)
  landuse_data <- landuse_data[-rem, ]
  landuse_countries <- landuse_countries[-mismatch, ]
}
index <- match(landuse_countries$CODE, fao_landuse_country_def$Country.Code)
newnames <- fao_landuse_country_def$Country[index]
mismatch <- which(landuse_countries$NAME != newnames)
if (length(mismatch) > 0) {
  cat(
    "The following", length(mismatch),
    "country names do not match between landuse_data and",
    sQuote(fao_landuse_country_file), "\n"
  )
  print(
    cbind(
      landuse_data = landuse_countries$NAME[mismatch],
      fao_landuse_country_def = newnames[mismatch]
    )
  )
  cat("Renaming in landuse_data\n")
  for (n in mismatch) {
    index <- which(landuse_data$Area == landuse_countries$NAME[n])
    landuse_data[index, "Area"] <- newnames[n]
    landuse_countries[n, "NAME"] <-  newnames[n]
  }
}
# Check if country names in landuse_data match country group lists
mismatch <- which(
  landuse_countries$CODE > 1000 &
  !landuse_countries$CODE %in% fao_landuse_country_group_def$Country.Group.Code
)
if (length(mismatch) > 0) {
  warning(
    "landuse_data includes country groups not found in ",
    sQuote(fao_landuse_country_group_file), ": ",
    toString(sQuote(landuse_countries$NAME[mismatch])),
    call. = FALSE,
    immediate. = TRUE
  )
}
groupids <- which(landuse_countries$CODE > 1000)
index <- match(
  landuse_countries$CODE[groupids],
  fao_landuse_country_group_def$Country.Group.Code
)
newnames <- fao_landuse_country_group_def$Country.Group[index]
mismatch <- which(landuse_countries$NAME[groupids] != newnames)
if (length(mismatch) > 0) {
  cat(
    "The following", length(mismatch),
    "country names do not match between landuse_data and",
    sQuote(fao_landuse_country_file), "\n"
  )
  print(
    cbind(
      landuse_data = landuse_countries$NAME[groupids][mismatch],
      fao_landuse_country_def = newnames[mismatch]
    )
  )
  cat("Renaming in landuse_data\n")
  for (n in mismatch) {
    index <- which(landuse_data$Area == landuse_countries$NAME[groupids[n]])
    landuse_data[index, "Area"] <- newnames[n]
    landuse_countries[groupids[n], "NAME"] <-  newnames[n]
  }
}

# Check if country names match between production_data and landuse_data
mismatch <- which(!landuse_countries$CODE %in% production_countries$CODE)
if (length(mismatch) > 0) {
  cat(
    "landuse_data has", length(mismatch),
    "countries not included in production_data:",
    toString(sQuote(landuse_countries$NAME[mismatch])), "\n"
  )
  index <- match(production_countries$CODE, landuse_countries$CODE)
  newnames <- landuse_countries$NAME[index]
  mismatch <- which(production_countries$NAME != newnames)
  if (length(mismatch) > 0) {
    cat(
      "The following", length(mismatch),
      "country names do not match between production_data and landuse_data\n"
    )
    print(
      cbind(
        production_data = production_countries$NAME[mismatch],
        landuse_data = newnames[mismatch]
      )
    )
    cat("Renaming in production_data\n")
    for (n in mismatch) {
      index <- which(production_data$Area == production_countries$NAME[n])
      production_data[index, "Area"] <- newnames[n]
      production_countries[n, "NAME"] <-  newnames[n]
      dimnames(production_array)[[1]][n] <- newnames[n]
    }
  }
}
mismatch <- which(!production_countries$CODE %in% landuse_countries$CODE)
if (length(mismatch) > 0) {
  cat(
    "production_data has", length(mismatch),
    "countries not included in landuse_data:",
    toString(sQuote(production_countries$NAME[mismatch])), "\n"
  )
  index <- match(landuse_countries$CODE, production_countries$CODE)
  newnames <- production_countries$NAME[index]
  mismatch <- which(landuse_countries$NAME != newnames)
  if (length(mismatch) > 0) {
    cat(
      "The following", length(mismatch),
      "country names do not match between production_data and landuse_data\n"
    )
    print(
      cbind(
        production_data = production_countries$NAME[mismatch],
        landuse_data = newnames[mismatch]
      )
    )
    cat("Renaming in landuse_data\n")
    for (n in mismatch) {
      index <- which(landuse_data$Area == landuse_countries$NAME[n])
      landuse_data[index, "Area"] <- newnames[n]
      landuse_countries[n, "NAME"] <-  newnames[n]
    }
  }
}
# Convert data into array with dimensions country, item, element, years
landuse_array <- array(
  dim = c(
    nrow(landuse_countries),
    nrow(landuse_items),
    nrow(landuse_elements),
    length(landuse_years)
  ),
  dimnames = list(
    landuse_countries$NAME,
    landuse_items$NAME,
    landuse_elements$NAME,
    format(landuse_years, scientific = FALSE)
  )
)
conv_msg <- list() # dummy variable keeping track of unit conversion messages
cat(
  "Parsing FAO landuse data for", dim(landuse_array)[1],
  "countries/country groups from",
  fao_landuse_file, "\n"
)
for (country in landuse_countries$NAME) {
  country_data <- landuse_data[which(landuse_data$Area == country), ]
  for (item in unique(intersect(landuse_items$NAME, country_data$Item))) {
    item_data <- country_data[which(country_data$Item == item), ]
    for (element in unique(item_data$Element)) {
      index <- which(item_data$Element == element)
      if (length(index) != 1) {
        stop(
          length(index),
          " lines for Element ", sQuote(element),
          " Item ", sQuote(item),
          " in country ", sQuote(country), "\n",
          "Expecting exactly one line."
        )
      }
      if (item_data[index, "Unit"] %in% names(unit_conversion)) {
        conv_factor <- ud.convert(1,
          item_data[index, "Unit"],
          unit_conversion[[item_data[index, "Unit"]]]
        )
        if (is.null(conv_msg[[item]])) {
          message(
            "Converting ", sQuote(item), " ", sQuote(element),
            " from ", item_data[index, "Unit"],
            " to ", unit_conversion[[item_data[index, "Unit"]]]
          )
          conv_msg[[item]] <- TRUE
        }
      } else {
        conv_factor <- 1
      }
      landuse_array[country, item, element, ] <-
        as.numeric(item_data[index, landuse_yearcols]) * conv_factor
    }
  }
}
################################################################################


################################################################################
## Save data to RData files for use in other scripts.                         ##
cat(
  "Outputs saved to",
  toString(sQuote(c(fao_production_RData, fao_landuse_RData))), "\n"
)
save(
  production_countries,
  production_items,
  production_elements,
  production_array,
  file = fao_production_RData
)
save(
  landuse_countries,
  landuse_items,
  landuse_elements,
  landuse_array,
  file = fao_landuse_RData
)
################################################################################
