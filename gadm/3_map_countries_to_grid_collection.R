################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script takes results from shapefile intersection and determines:      ##
## - all cells with land (unless force_grid == TRUE)                          ##
## - fraction of each grid cell covered by land                               ##
## - dominant country in each cell                                            ##
## - dominant state/region in each cell for countries included in             ##
##   include_regions                                                          ##
## - number of countries in each grid cell                                    ##
################################################################################

# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Load variables and helper functions used across several scripts.           ##
## You should always set up gadm_setup.R first and make sure it is in the     ##
## same directory.                                                            ##
source("gadm_setup.R")
source("gadm_helper.R")
################################################################################

cat("*** Script run in", getwd(), "***\n")
cat("Spatial resolution:", lpj_res_string, "\n")
if (force_grid) {
  cat(
    "Info: Using predefined grid with", nrow(griddata),
    "cells and a spatial extent:", toString(lpjgrid_extent), "\n"
  )
}
if (!force_grid && threshold_grid > 0) {
  cat(
    "Enforcing a minimum grid cell area of", threshold_grid, "m2.",
    "All cells below that threshold will be filtered.\n"
  )
}

################################################################################
## Load country and state/region layer                                        ##
## gadm_load() is defined in gadm_helper.R                                    ##
## gadm_dir is defined in gadm_setup.R                                        ##
cat(
  "Loading GADM shapes from",
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()), "\n"
)
# Create a list of all countries (ISO code and name)
load_gadm(gadm_dir, levels = 0)
country_list <- as.list(unique(as.character(gadm_countries$GID_0)))
names(country_list) <- country_list
# Replace codes with country names
country_list <- sapply(
  country_list,
  function(indata) {
    as.character(gadm_countries$NAME_0[match(indata, gadm_countries$GID_0)])
  }
)
# Clean-up
rm(gadm_countries)
# Create a list of all regions (ISO code and name)
load_gadm(gadm_dir, levels = 1)
# Find all unique GADM level 1 codes
# Region names may not all be unique but codes should be
region_list <- as.list(unique(as.character(gadm_regions$GID_1)))
names(region_list) <- region_list
# replace codes with region/state names
region_list <- sapply(
  region_list,
  function(indata) {
    as.character(gadm_regions$NAME_1[match(indata, gadm_regions$GID_1)])
  }
)
# Clean-up
rm(gadm_regions)
################################################################################

################################################################################
## Loop over shapefile intersections and create list with information         ##
## cell_list: a list containing a vector for each cell:                       ##
## - GridID (cell ID in raster)                                               ##
## - Gridarea (total cell area)                                               ##
## - Shapearea (land area of all polygons in cell)                            ##
## - Area covered by each country present in cell                             ##
## - Area covered by each region present in cell (for countries included in   ##
##   include_regions)                                                         ##
################################################################################
if (file.exists(cell_list_RData)) {
  # Skip processing if RData file exists already from previous script run.
  # Delete RData file to force processing.
  cat("Reloading preprocessed cell list from", cell_list_RData, "\n")
  load(cell_list_RData)
} else {
  cat("Reading in cell list from shapefiles in", intersect_directory, "\n")
  cell_list <- list()
  ISO_list <- character()
  # Find shapefiles in intersect_directory
  intersect_shapes <- list.files(intersect_directory, ".shp")
  # Loop over shapefiles (countries or parts of countries)
  for (country in intersect_shapes) {
    cat(country, "\n")
    # Read shapefile
    country_shape <- st_read(
      file.path(intersect_directory, country),
      stringsAsFactors = FALSE,
      quiet = TRUE
    )
    # List of ISO codes in all shapefiles
    ISO_list <- unique(c(ISO_list, country_shape$GID_0))
    # Find all GridIDs (unique grid cells)
    cloop <- unique(country_shape$GridID)
    # Loop over grid cells
    for (GridID in cloop) {
      if (length(cell_list) < GridID) {
        # GridIDs from shapefiles are not consecutive,  make sure that list has
        # index
        cell_list[[GridID]] <- numeric(0)
      }
      # Subset of polygons belonging to GridID
      grid_shape <- country_shape[which(country_shape$GridID == GridID), ]
      if (length(cell_list[[GridID]]) == 0) {
        # GridID not yet in cell_list, create empty entry
        cell_list[[GridID]] <- c(
          GridID = GridID,
          Gridarea = grid_shape$Gridarea[1],
          Landarea = 0
        )
      }
      # Loop over unique country codes GID_0
      for (c in unique(grid_shape$GID_0)) {
        if (c %in% skip_countries) {
          # Do not use data from countries in skip_countries
          next
        }
        if (!c %in% names(country_list)) {
          # Invalid country code
          stop(
            "Country code ", sQuote(c), " found in ", country,
            " is missing in country_list. Data in ", intersect_directory,
            " appear to be inconsistent with GADM source data."
          )
        }
        # Area of all polygons in grid cell with same GID_0 (may be be more than
        # one if regions/states are present)
        carea <- sum(grid_shape[which(grid_shape$GID_0 == c), ]$Shapearea)
        if (c %in% names(cell_list[[GridID]])) {
          # For countries split into several shapefiles, some polygons may have
          # been counted already; add new carea
          cell_list[[GridID]][c] <- cell_list[[GridID]][c] + carea
        } else {
          # If country does not have entry in grid cell yet, add as new entry
          cell_list[[GridID]] <- c(cell_list[[GridID]], carea)
          names(cell_list[[GridID]])[length(cell_list[[GridID]])] <- c
        }
        # Also add carea to total land area in cell
        cell_list[[GridID]]["Landarea"] <- cell_list[[GridID]]["Landarea"] +
          carea
        # Processing of regions/states for countries from include_regions
        if (c %in% include_regions) {
          if (!"GID_1" %in% colnames(grid_shape)) {
            # Check that include_regions was not expanded after running grid
            # intersection
            stop(
              "Country ", sQuote(c),
              " is included in include_regions but 'GID_1' column is missing ",
              "in grid_shape.\nThis suggests shape intersection was performed",
              "using different include_regions."
            )
          }
          for (r in unique(grid_shape[which(grid_shape$GID_0 == c), ]$GID_1)) {
            index <- which(grid_shape$GID_1 == r)
            if (!r %in% names(region_list)) {
              cat(
                "Adding region", sQuote(r),
                sQuote(
                  stri_trans_general(grid_shape[index, ]$NAME_1, "latin-ascii")
                ),
                "to region_list. This does not seem to be part of",
                "the original GADM source data.\n"
              )
              region_list[[r]] <- grid_shape[index, ]$NAME_1
            }
            if (r %in% names(cell_list[[GridID]])) {
              cell_list[[GridID]][r] <- cell_list[[GridID]][r] +
                sum(grid_shape[index, ]$Shapearea)
            } else {
              # If region does not have entry in grid cell yet, add as new entry
              cell_list[[GridID]] <- c(
                cell_list[[GridID]],
                sum(grid_shape[index, ]$Shapearea)
              )
              names(cell_list[[GridID]])[length(cell_list[[GridID]])] <- r
            }
          }
        }
      }
    }
  }
  # Since processing of shapefiles takes quite long save results to RData file
  # which will be reused on next script run
  save(cell_list, region_list, ISO_list, country_list, file = cell_list_RData)
}

################################################################################
## Files created below:                                                       ##
## - Grid (list of coordinates), filename: gridname                           ##
## - Land fraction, file: landfracname; raster: landfracraster                ##
## - Country and region codes, filename: cowname; meta information:           ##
##   cowmetaname, regmetaname; raster version: cowraster, regraster           ##
## - Number of countries per cell, filename: ncountryname;                    ##
##   raster: ncountryraster                                                   ##
## You may switch off the creation of any of these files by either commenting ##
## the respective filename variable in gadm_setup.R or by deleting the        ##
## filename variable here.                                                    ##
################################################################################


################################################################################
## Determine land fraction in each cell included in cell_list.                ##
## Function landfrac() defined in gadm_helper.R                               ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and land fraction.                                              ##
cat("Determining land fraction in each cell.\n")
frac_list <- lapply(cell_list, landfrac)
# Reduce list to a matrix containing only those cells with land according to
# GADM.
frac_cells <- matrix(
  unlist(frac_list[which(!sapply(frac_list, is.null))]),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("GridID", "Landfraction"))
)
# If force_grid == TRUE check if all cells in griddata have a land fraction
# assigned. If not, set to 0.
if (force_grid) {
  # Coordinates from griddata have been assigned GridIDs from 1 to
  # nrow(griddata), check if any missing
  if (any(!seq_len(nrow(griddata)) %in% frac_cells[, "GridID"])) {
    warning(
      length(setdiff(seq_len(nrow(griddata)), frac_cells[, "GridID"])),
      " cells in your predefined grid are not covered by GADM.",
      " Assigning a land fraction of 0.",
      call. = FALSE,
      immediate. = TRUE
    )
    frac_cells <- rbind(
      frac_cells,
      cbind(setdiff(seq_len(nrow(griddata)), frac_cells[, "GridID"]), 0)
    )
  }
} else if (threshold_grid > 0) {
  # Return land area in addition to land fraction to apply threshold of minimum
  # grid cell area.
  landarea <- matrix(
    unlist(
      sapply(
        cell_list[which(sapply(cell_list, length) > 0)],
        function(indata) indata[c("GridID", "Landarea")]
      )
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("GridID", "Landarea"))
  )
  if (any(landarea[, "Landarea"] < threshold_grid)) {
    message(
      "Info: Removing ",
      length(which(landarea[, "Landarea"] < threshold_grid)),
      " cells from grid because their cell area is below threshold_grid of ",
      threshold_grid, " m2."
    )
    valid_cells <- match(
      landarea[which(landarea[, "Landarea"] >= threshold_grid), "GridID"],
      frac_cells[, "GridID"]
    )
    frac_cells <- frac_cells[valid_cells, ]
  }
}
################################################################################


################################################################################
## Derive coordinates of all land cells which will be included in grid.       ##
## If (force_grid==TRUE) use predefined grid.                                 ##
## Otherwise use all cells included in frac_cells.                            ##
if (force_grid) {
  # Grid corresponds to predefined list of coordinates
  lpjgrid_cells <- cbind(as.matrix(griddata), GridID = seq_len(nrow(griddata)))
} else {
  # Copy lpjgrid_raster
  lpjfound_raster <- raster(lpjgrid_raster)
  # Assign all cells with country code using GridID
  grid_index <- match(frac_cells[, "GridID"], values(lpjgrid_raster))
  lpjfound_raster[grid_index] <- frac_cells[, "GridID"]
  # Extract coordinates of all cells with values
  lpjgrid_cells <- rasterToPoints(lpjfound_raster)
  colnames(lpjgrid_cells) <- c("lon", "lat", "GridID")
  rm(lpjfound_raster, grid_index)
}
################################################################################

################################################################################
## Create grid file                                                           ##
## File name gridname defined in gadm_setup.R                                 ##
## Format gridformat defined in gadm_setup.R                                  ##
## Version for LPJmL input format bintype defined in gadm_setup.R             ##
## If the format is "BIN" this uses a number of helper functions for the      ##
## LPJmL input format defined in "../helper_functions.R"                      ##
if (exists("gridname")) {
  cat("Creating grid file", gridname, "\n")
}
if (exists("gridname") && file.exists(gridname)) {
  stop(gridname, " exists already. Delete file to create a new one")
}
if (gridformat == "BIN" && exists("gridname")) {
  scalar <- 1.0
  # Variable type depends on bintype (version of LPJmL input format)
  if (bintype < 3) {
    # Check if coordinates can be expressed as integer values by multiplying
    # with a scalar
    scaled_coords <- lpjgrid_cells[, c("lon", "lat")] * scalar
    while (any(scaled_coords != as.integer(scaled_coords))) {
      scalar <- scalar * 10
      # bintype 1 and 2 allow only data type short (2-byte integer), check if
      # valid range
      scaled_coords <- lpjgrid_cells[, c("lon", "lat")] * scalar
      if (max(abs(scaled_coords)) >= 2^15) {
        stop(
          "Grid coordinates cannot be saved as data type short.\n",
          "Set bintype to 3 to allow saving as floating point values."
        )
      }
    }
    # bintype 1 needs a default scaling factor of 100
    if (bintype < 2 && scalar != 100) {
      stop(
        "Grid coordinates cannot be saved as data type short using as the ",
        "default scaling factor.\n",
        "Set bintype at least to 2 to allow flexible scaling factors.\n",
        "Or set bintype to 3 to allow saving as floating point values."
      )
    }
    # bintype 1 does not support saving resolution, only default of 0.5
    if (bintype < 2 && lpj_res["lon"] != 0.5) {
      stop("bintype 1 only supports resolution of 0.5 degree")
    }
  }
  # Create header of grid file
  gridheader <- create_header(
    name = grid_headername,
    version = bintype,
    nyear = 1,
    ncell = nrow(lpjgrid_cells),
    nbands = 2,
    cellsize_lon = lpj_res["lon"],
    scalar = 1 / scalar,
    datatype = ifelse(bintype < 3, 1, 3),
    cellsize_lat = lpj_res["lat"]
  )
  # Write header to file
  write_header(gridname, gridheader)
  # Open file in binary appending mode to add data
  gridfile <- file(gridname, "ab")
  # Data type to be written depends on data type set in header
  # get_datatype() returns type and size of data to be written
  if (typeof(get_datatype(gridheader)$type) == "integer") {
    writeBin(
      as.integer(
        t(lpjgrid_cells[, c("lon", "lat")]) / gridheader[["header"]]["scalar"]
      ),
      gridfile,
      size = get_datatype(gridheader)$size,
      endian = gridheader[["endian"]]
    )
  } else if (typeof(get_datatype(gridheader)$type) == "double") {
    writeBin(
      as.double(
        t(lpjgrid_cells[, c("lon", "lat")]) / gridheader[["header"]]["scalar"]
      ),
      gridfile,
      size = get_datatype(gridheader)$size,
      endian = gridheader[["endian"]]
    )
  } else {
    stop(
      "Invalid datatype ", gridheader[["header"]]["datatype"],
      " in gridheader"
    )
  }
  close(gridfile)
} else if (gridformat == "CSV" && exists("gridname")) {
  write.csv(
    lpjgrid_cells[, c("lon", "lat")],
    file = gridname,
    row.names = FALSE
  )
} else if (exists("gridname")) {
  stop("Invalid gridformat ", sQuote(gridformat))
}
################################################################################


################################################################################
## Create land fraction file                                                  ##
## File name landfracname defined in gadm_setup.R                             ##
## Format landfracformat defined in gadm_setup.R                              ##
## Version for LPJmL input format bintype defined in gadm_setup.R             ##
## If the format is "BIN" this uses a number of helper functions for the      ##
## LPJmL input format defined in "../helper_functions.R"                      ##
if (exists("landfracname")) {
  cat("Creating land fraction file", landfracname, "\n")
}
if (exists("landfracname") && file.exists(landfracname)) {
  stop(landfracname, " exists already. Delete file to create a new one")
}
# Prepare data
frac_data <- numeric(nrow(lpjgrid_cells))
grid_index <- match(lpjgrid_cells[, "GridID"], frac_cells[, "GridID"])
frac_data <- frac_cells[grid_index, "Landfraction"]
if (anyNA(frac_data)) {
  stop("frac_data could not be filled correctly.")
}
# Check that land fraction does not 100%, allow for some numerical inaccuracy
if (any(frac_data > 1.001)) {
  stop(
    "There are ", length(which(frac_data > 1.001)),
    " cells in frac_data with land fraction exceeding 1.001"
  )
}
if (any(frac_data > 1)) {
  message(
    "Info: There are ", length(which(frac_data > 1)),
    " cells exceeding land fraction of 1 by a maximum of ",
    format(max(frac_data) - 1, digits = 3),
    " which will be cut to 1"
  )
  frac_data[which(frac_data > 1)] <- 1
}
# Write out data
if (landfracformat == "BIN" && exists("landfracname")) {
  scalar <- 1.0
  # Variable type depends on bintype (version of LPJmL input format)
  if (bintype == 2) {
    # Check for maximum scaling factor that still allows values to be
    # expressed as 2-byte integer
    while (max(frac_cells[, "Landfraction"]) * scalar * 10 < 2^15) {
      scalar <- scalar * 10
    }
    message(
      "Info: land fraction will be saved as a 2-byte integer using a scaling ",
      "factor of ", scalar, ", which reduces precision. ",
      "Use bintype 3 to save as floating point value."
    )
  } else if (bintype == 1) {
    # There is currently no default scalar in the LPJmL code so it must be
    # provided by the input file
    stop(
      "No default scalar for land fraction defined. ",
      "Use at least bintype 2 or even bintype 3."
    )
  }
  # Create header of land fraction file
  landfracheader <- create_header(
    name = landfrac_headername,
    version = bintype,
    nyear = 1,
    ncell = length(frac_data),
    nbands = 1,
    cellsize_lon = lpj_res["lon"],
    scalar = 1 / scalar,
    datatype = ifelse(bintype < 3, 1, 3),
    cellsize_lat = lpj_res["lat"]
  )
  # Write header to file
  write_header(landfracname, landfracheader)
  # Open file in binary appending mode to add data
  landfracfile <- file(landfracname, "ab")
  # Data type to be written depends on data type set in header
  # get_datatype() returns type and size of data to be written
  if (typeof(get_datatype(landfracheader)$type) == "integer") {
    writeBin(
      as.integer(round(frac_data / landfracheader[["header"]]["scalar"])),
      landfracfile,
      size = get_datatype(landfracheader)$size,
      endian = landfracheader[["endian"]]
    )
  } else if (typeof(get_datatype(landfracheader)$type) == "double") {
    writeBin(
      as.double(frac_data / landfracheader[["header"]]["scalar"]),
      landfracfile,
      size = get_datatype(landfracheader)$size,
      endian = landfracheader[["endian"]]
    )
  } else {
    stop(
      "Invalid datatype ",
      landfracheader[["header"]]["datatype"],
      " in landfracheader"
    )
  }
  close(landfracfile)
} else if (landfracformat == "CSV" && exists("landfracname")) {
  write.csv(
    matrix(frac_data, ncol = 1, dimnames = list(NULL, "Landfraction")),
    file = landfracname,
    row.names = FALSE
  )
} else if (exists("landfracname")) {
  stop("Invalid landfracformat ", sQuote(landfracformat))
}
################################################################################


################################################################################
## Determine dominant country in each cell included in cell_list.             ##
## Function dominant_country() defined in gadm_helper.R                       ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and ISO code of the dominant country.                           ##
cat("Determining dominant country in each cell.\n")
cow_list <- lapply(
  cell_list,
  dominant_country,
  ISO_list = ISO_list,
  water_bodies = water_bodies
)
# Reduce list to a data frame containing only those cells with assigned
# countries.
cow_cells <- data.frame(
  matrix(
    unlist(cow_list[which(!sapply(cow_list, is.null))]),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("GridID", "country"))
  ),
  stringsAsFactors = FALSE
)
# Set GridID column from character to integer
cow_cells$GridID <- as.integer(cow_cells$GridID)
# If force_grid == TRUE check if all cells in griddata have a country
# assigned, if not assign gadm_no_land defined in gadm_setup.R
if (force_grid) {
  # Coordinates from griddata have been assigned GridIDs from 1 to
  # nrow(griddata), check if any missing.
  if (any(!seq_len(nrow(griddata)) %in% cow_cells$GridID)) {
    warning(
      length(setdiff(seq_len(nrow(griddata)), cow_cells$GridID)),
      " cells in your predefined grid are not covered by GADM. Assigning ",
      sQuote(gadm_no_land),
      " (", names(gadm_no_land), ")",
      call. = FALSE,
      immediate. = TRUE
    )
    message(
      "Please check created grid and country code files manually whether ",
      "these cells can be assigned to an adjacent country."
    )
    # Append entries for missing cells with gadm_no_land.
    cow_cells <- rbind(
      cow_cells,
      data.frame(
        GridID = setdiff(seq_len(nrow(griddata)), cow_cells$GridID),
        country = names(gadm_no_land),
        stringsAsFactors = FALSE
      )
    )
    # Add gadm_no_land to country_list
    country_list[[names(gadm_no_land)]] <- gadm_no_land
  }
  # Check if GADM includes cells not included in predefined grid
  if (any(! cow_cells$GridID > nrow(griddata))) {
    warning(
      "GADM includes land in ",
      length(which(cow_cells$GridID > nrow(griddata))),
      " cells not included in predefined grid. ",
      "These will be dropped from all created files.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}
# Check consistency with country_list
if (any(!cow_cells[, "country"] %in% names(country_list))) {
  stop(
    "Country code(s) ",
    toString(unique(setdiff(cow_cells[, "country"], names(country_list)))),
    " from cell_list are missing in country_list."
  )
}
################################################################################


################################################################################
## Determine dominant region/state in each cell included in cell_list.        ##
## Function dominant_region() defined in gadm_helper.R                        ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and code of the dominant region/state.                          ##
cat(
  "Determining dominant region/state in countries included in include_regions\n"
)
reg_list <- lapply(
  cell_list,
  dominant_region,
  region_list = region_list,
  ISO_list = ISO_list,
  water_bodies = water_bodies
)
# Reduce list to a data frame containing only those cells with assigned
# regions/states.
reg_cells <- data.frame(
  matrix(
    unlist(reg_list[which(!sapply(reg_list, is.null))]),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("GridID", "region"))
  ),
  stringsAsFactors = FALSE
)
# Set GridID column from character to integer
reg_cells$GridID <- as.integer(reg_cells$GridID)
# If force_grid == TRUE check if any cells are not included in predefined grid
if (force_grid) {
  if (any(!reg_cells$GridID > nrow(griddata))) {
    warning(
      "GADM includes region/state information in ",
      length(which(reg_cells$GridID > nrow(griddata))),
      " cells not included in predefined grid. ",
      "These will be dropped from country code file ",
      cowname,
      call. = FALSE,
      immediate. = TRUE
    )
  }
}
# Check consistency with region_list
if (any(!reg_cells[, "region"] %in% names(region_list))) {
  stop(
    "Region code(s) ",
    toString(unique(setdiff(reg_cells[, "region"], names(region_list)))),
    " from cell_list are missing in region_list."
  )
}
################################################################################


################################################################################
## Derive indices for countries and regions/states.                           ##
## LPJmL requires that countries are numbered starting at 0. Countries that   ##
## include regions/state are listed at the end. The code for "No land" will   ##
## be added between normal countries and countries with regions.              ##
## Region indices start after the index assigned to "No land".                ##
## To avoid country and region/state indices from being resolution-dependent  ##
## all countries in country_list and all regions of countries in              ##
## include_regions are assigned even if they are not assigned as dominant     ##
## country or region to any cell.                                             ##
##                                                                            ##
# Countries first ordered alphabetically by ISO code (excluding ISO codes
# starting with "X")
sort_order <- sort(
  setdiff(
    names(country_list),
    c(
      include_regions,
      names(gadm_no_land),
      grep("^X", names(country_list), value = TRUE)
    )
  )
)
level0 <- unlist(country_list[sort_order])
# Add countries whose ISO code starts with "X" (which are not part of ISO-3166)
sort_order <- sort(
  setdiff(
    grep("^X", names(country_list), value = TRUE),
    c(include_regions, names(gadm_no_land))
  )
)
level0 <- c(level0, unlist(country_list[sort_order]))
# Add gadm_no_land
level0 <- c(level0, gadm_no_land)
# Add countries from include_regions
level0 <- c(
  level0,
  unlist(country_list[sort(intersect(names(country_list), include_regions))])
)
# To avoid problems with special characters if not running in a UTF8
# environment convert names to ASCII characters only.
level0_ascii <- stri_trans_general(level0, "latin-ascii")
names(level0_ascii) <- names(level0)
# Indices to be used
level0_index <- seq(0, length.out = length(level0_ascii))
names(level0_index) <- names(level0)
# Extract all regions of countries in include_regions
level1 <- character()
for (c in sort(intersect(substr(names(region_list), 1, 3), include_regions))) {
  level1 <- c(level1, region_list[grep(c, names(region_list))])
}
# To avoid problems with special characters if not running in a UTF8
# environment convert names to ASCII characters only.
level1_ascii <- stri_trans_general(level1, "latin-ascii")
names(level1_ascii) <- names(level1)
# Indices to be used (start after gadm_no_land)
level1_index <- seq(
  level0_index[max(match(names(gadm_no_land), names(level0_index)))] + 1,
  length.out = length(level1)
)
names(level1_index) <- names(level1)
################################################################################


################################################################################
## Preparation of country code file                                           ##
## This is a matrix with two columns:                                         ##
## 1) country index                                                           ##
## 2) region index for countries in include_regions, otherwise same as 1)     ##
## A third column with the GridID is added for convenience.                   ##
cow_data <- array(
  dim = c(nrow(lpjgrid_cells), 3),
  dimnames = list(NULL, c("country", "region", "GridID"))
)
# GridID
cow_data[, "GridID"] <- lpjgrid_cells[, "GridID"]
# Country
grid_index <- match(cow_data[, "GridID"], cow_cells[, "GridID"])
cow_data[, "country"] <- level0_index[cow_cells[grid_index, "country"]]
# Region (not available in all cells)
grid_index1 <- match(
  intersect(cow_data[, "GridID"], reg_cells[, "GridID"]),
  cow_data[, "GridID"]
)
grid_index2 <- match(
  intersect(cow_data[, "GridID"], reg_cells[, "GridID"]),
  reg_cells[, "GridID"]
)
cow_data[grid_index1, "region"] <-
  level1_index[reg_cells[grid_index2, "region"]]
# Fill empty rows in region column with country column
grid_index <- which(is.na(cow_data[, "region"]))
cow_data[grid_index, "region"] <- cow_data[grid_index, "country"]
# Check for errors
if (anyNA(cow_data)) {
  stop("cow_data could not be filled correctly")
}
# Meta information: data frame with names and corresponding codes
cowmetatable <- data.frame(
  ISO = names(level0_ascii),
  country = level0_ascii[names(level0_index)],
  ID = level0_index
)
regmetatable <- data.frame(
  ISO = names(level1_ascii),
  region = level1_ascii[names(level1_index)],
  ID = level1_index
)
################################################################################


################################################################################
## Create country code file                                                   ##
## File name cowname defined in gadm_setup.R                                  ##
## Format gridformat defined in gadm_setup.R                                  ##
## Version for LPJmL input format bintype defined in gadm_setup.R             ##
## If the format is "BIN" this uses a number of helper functions for the      ##
## LPJmL input format defined in "../helper_functions.R"                      ##
if (exists("cowname")) {
  cat("Creating country code file", cowname, "\n")
}
for (filecheck in c("cowname", "cowmetaname", "regmetaname")) {
  if (exists(filecheck) && file.exists(get(filecheck))) {
    stop(get(filecheck), " exists already. Delete file to create a new one.")
  }
}
clist <- setdiff(names(country_list), skip_countries)
cindex1 <- match(cow_data[, "country"], cowmetatable$ID)
if (length(setdiff(clist, cow_data[, "country"])) > 0) {
  cat(
    "The following",
    length(setdiff(clist, cowmetatable$ISO[cindex1])),
    "GADM countries are not assigned to any cell:\n"
  )
  cindex2 <- match(
    setdiff(clist, cowmetatable$ISO[cindex1]),
    cowmetatable$ISO
  )
  cindex3 <- match(
    setdiff(
      setdiff(names(country_list), skip_countries),
      cowmetatable$ISO[cindex1]
    ),
    names(level0_index)
  )
  print(
    data.frame(
      country = cowmetatable$country[cindex2],
      ID = level0_index[cindex3]
    )
  )
}
if (cowformat == "BIN" && exists("cowname")) {
  scalar <- 1.0
  # Variable type depends on bintype (version of LPJmL input format)
  if (bintype < 3) {
    # Check if country/region indices can be expressed as short
    if (any(abs(cow_data[, c("country", "region")]) >= 2^15)) {
      stop(
        "Range of country/region codes [",
        toString(range(cow_data[, c("country", "region")])),
        "] cannot be saved as 2-byte integer.\n",
        "Set bintype to 3 to allow 4-byte integer"
      )
    }
    if (bintype < 2 && lpj_res["lon"] != 0.5) {
      stop("bintype 1 only supports resolution of 0.5 degree")
    }
  }
  if (any(abs(cow_data[, c("country", "region")]) >= 2^31)) {
    stop(
      "Range of country/region codes [",
      toString(range(cow_data[, c("country", "region")])),
      "] cannot be saved as 4-byte integer.\n",
      "Reduce number of countries/regions."
    )
  }
  # Create header of cow file
  cowheader <- create_header(
    name = cow_headername,
    version = bintype,
    nyear = 1,
    ncell = nrow(cow_data),
    nbands = 2,
    cellsize_lon = lpj_res["lon"],
    scalar = 1 / scalar,
    datatype = ifelse(max(cow_data[, c("country", "region")]) >= 2^15, 2, 1),
    cellsize_lat = lpj_res["lat"]
  )
  # Write header to file
  write_header(cowname, cowheader)
  # Open file in binary appending mode to add data
  cowfile <- file(cowname, "ab")
  # Data type to be written depends on data type set in header
  # get_datatype() returns type and size of data to be written
  if (typeof(get_datatype(cowheader)$type) == "integer") {
    writeBin(
      as.integer(
        t(cow_data[, c("country", "region")]) / cowheader[["header"]]["scalar"]
      ),
      cowfile,
      size = get_datatype(cowheader)$size,
      endian = cowheader[["endian"]]
    )
  } else if (typeof(get_datatype(cowheader)$type) == "double") {
    writeBin(
      as.double(
        t(cow_data[, c("country", "region")]) / cowheader[["header"]]["scalar"]
      ),
      cowfile,
      size = get_datatype(cowheader)$size,
      endian = cowheader[["endian"]]
    )
  } else {
    stop("Invalid datatype ", cowheader[["header"]]["datatype"], " in cowheader")
  }
  close(cowfile)
} else if (cowformat == "CSV" && exists("cowname")) {
  write.csv(
    cow_data[, c("country", "region")],
    file = cowname,
    row.names = FALSE
  )
} else if (exists("cowname")) {
  stop("Invalid cowformat ", sQuote(cowformat))
}
# Also write out meta information
if (exists("cowmetaname")) {
  cat("Meta information for country codes saved to", cowmetaname, "\n")
  write.csv(
    cowmetatable,
    file = cowmetaname,
    fileEncoding = "ASCII",
    row.names = FALSE
  )
}
if (exists("regmetaname")) {
  cat("Meta information for region codes saved to", regmetaname, "\n")
  write.csv(
    regmetatable,
    file = regmetaname,
    fileEncoding = "ASCII",
    row.names = FALSE
  )
}
################################################################################


################################################################################
## Determine the number of countries in each cell.                            ##
## dominant_country() above only returns the dominant country. The number of  ##
## countries per cell is used in the creation of landuse data to detect       ##
## border cells.                                                              ##
## Function number_country() defined in gadm_helper.R                         ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and number of unique GID_0 codes.                               ##
cat("Determining number of countries in each cell.\n")
ncountry_list <- lapply(cell_list, number_country, ISO_list = ISO_list)
# Reduce list to a matrix containing only those cells with land according to
# GADM.
ncountry_cells <- matrix(
  unlist(ncountry_list[which(!sapply(ncountry_list, is.null))]),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("GridID", "ncountry"))
)
if (force_grid) {
  # Coordinates from griddata have been assigned GridIDs from 1 to
  # nrow(griddata), check if any missing
  if (any(!seq_len(nrow(griddata)) %in% ncountry_cells[, "GridID"])) {
    warning(
      length(setdiff(seq_len(nrow(griddata)), ncountry_cells[, "GridID"])),
      " cells in your predefined grid are not covered by GADM. ",
      "Assigning ncountry of 0.",
      call. = FALSE,
      immediate. = TRUE
    )
    ncountry_cells <- rbind(
      ncountry_cells,
      cbind(setdiff(seq_len(nrow(griddata)), ncountry_cells[, "GridID"]), 0)
    )
  }
}
################################################################################


################################################################################
## Create file containing number of countries in each cell.                   ##
## File name ncountryname and format ncountryformat defined in gadm_setup.R   ##
## Prepare data                                                               ##
ncountry_data <- integer(nrow(lpjgrid_cells))
grid_index <- match(lpjgrid_cells[, "GridID"], ncountry_cells[, "GridID"])
ncountry_data <- ncountry_cells[grid_index, "ncountry"]
# Check for errors
if (anyNA(ncountry_data)) {
  stop("ncountry_data could not be filled correctly")
}
# Write out data
if (exists("ncountryname") && file.exists(ncountryname)) {
  stop(ncountryname, " exists already. Delete file to create a new one.")
}
if (exists("ncountryname")) {
  cat(
    "Writing file containing number of countries in each cell to",
    ncountryname, "\n"
  )
}
if (ncountryformat == "BIN" && exists("ncountryname")) {
  scalar <- 1.0
  # Variable type depends on bintype (version of LPJmL input format)
  if (bintype < 3) {
    # check if number of countries can be expressed as short
    if (any(abs(ncountry_data) >= 2^15)) {
      stop(
        "Maximum number of countries per cell ",
        max(ncountry_data),
        " exceeds value range of 2-byte integer.\n",
        "Set bintype to 3 to allow 4-byte integer."
      )
    }
    if (bintype < 2 && lpj_res["lon"] != 0.5) {
      stop("bintype 1 only supports resolution of 0.5 degree.")
    }
  }
  if (any(ncountry_data >= 2^31)) {
    stop(
      "Maximum number of countries per cell ",
      max(ncountry_data),
      " cannot be saved as 4-byte integer."
    )
  }
  # Create header of file
  ncountryheader <- create_header(
    name = ncountry_headername,
    version = bintype,
    nyear = 1,
    ncell = length(ncountry_data),
    nbands = 1,
    cellsize_lon = lpj_res["lon"],
    scalar = 1 / scalar,
    datatype = ifelse(max(ncountry_data) >= 2^15, 2, 1),
    cellsize_lat = lpj_res["lat"]
  )
  # Write header to file
  write_header(ncountryname, ncountryheader)
  # Open file in binary appending mode to add data
  ncountryfile <- file(ncountryname, "ab")
  # Data type to be written depends on data type set in header
  # get_datatype() returns type and size of data to be written
  if (typeof(get_datatype(ncountryheader)$type) == "integer") {
    writeBin(
      as.integer(ncountry_data / ncountryheader[["header"]]["scalar"]),
      ncountryfile,
      size = get_datatype(ncountryheader)$size,
      endian = ncountryheader[["endian"]]
    )
  } else if (typeof(get_datatype(ncountryheader)$type) == "double") {
    writeBin(
      as.double(ncountry_data / ncountryheader[["header"]]["scalar"]),
      ncountryfile,
      size = get_datatype(ncountryheader)$size,
      endian = ncountryheader[["endian"]]
    )
  } else {
    stop(
      "Invalid datatype ",
      ncountryheader[["header"]]["datatype"],
      " in ncountryheader."
    )
  }
  close(ncountryfile)
} else if (ncountryformat == "CSV" && exists("ncountryname")) {
  write.csv(
    matrix(ncountry_data, ncol = 1, dimnames = list(NULL, "ncountry")),
    file = ncountryname,
    row.names = FALSE
  )
} else if (exists("ncountryname")) {
  stop("Invalid ncountryformat ", sQuote(ncountryformat))
}
################################################################################


################################################################################
## Create raster objects of inputs generated above.                           ##
## - Country code (filename cowraster defined in gadm_setup.R)                ##
## - Region code (for include_regions, otherwise same as country code)        ##
##   (filename regraster defined in gadm_setup.R)                             ##
## - Number of countries (filename ncountryraster defined in gadm_setup.R)    ##
## - Land fraction (filename landfracraster defined in gadm_setup.R)          ##
##                                                                            ##
# Empty raster
outputraster <- raster(lpjgrid_raster)
# Country code
if (exists("cowraster")) {
  cat("Writing raster of country codes to", cowraster, "\n")
  grid_index1 <- match(lpjgrid_cells[, "GridID"], values(lpjgrid_raster))
  grid_index2 <- match(lpjgrid_cells[, "GridID"], cow_data[, "GridID"])
  outputraster[grid_index1] <- cow_data[grid_index2, "country"]
  names(outputraster) <- "country"
  writeRaster(
    outputraster,
    filename = cowraster,
    NAflag = -99,
    datatype = ifelse(
      max(cellStats(outputraster, max)) < 2^15,
      "INT2S",
      "INT4S"
    ),
    overwrite = TRUE
  )
  if (tolower(rev(unlist(strsplit(cowraster, ".", fixed = TRUE)))[1]) == "nc") {
    # NetCDF format
    # Try to add variable containing country names
    if (require(ncdf4)) {
      # Open file for writing
      nc <- nc_open(cowraster, write = TRUE)
      # Generate required variable dimensions
      nchar_dim <- ncdim_def(
        "nchar",
        units = "",
        vals = seq_len(
          max(
            nchar(
              paste(cowmetatable[, "ID"], cowmetatable[, "country"], sep = ": ")
            )
          )
        ),
        create_dimvar = FALSE
      )
      cow_dim <- ncdim_def(
        "country",
        units = "",
        vals = seq_len(nrow(cowmetatable)),
        create_dimvar = FALSE
      )
      # Generate variable
      cow_var <- ncvar_def(
        "country_name",
        units = "",
        dim = list(nchar_dim, cow_dim),
        longname = "country name",
        prec = "char"
      )
      # Add variable to file
      nc <- ncvar_add(nc, cow_var)
      # Write values to file
      ncvar_put(
        nc,
        "country_name",
        paste(cowmetatable[, "ID"], cowmetatable[, "country"], sep = ": ")
      )
      nc_close(nc)
      rm(nchar_dim, cow_dim, cow_var, nc)
    }
  }
  # Reset raster values
  outputraster[] <- NA
}
# Region code
if (exists("regraster")) {
  cat("Writing raster of region codes to", regraster, "\n")
  grid_index1 <- match(lpjgrid_cells[, "GridID"], values(lpjgrid_raster))
  grid_index2 <- match(lpjgrid_cells[, "GridID"], cow_data[, "GridID"])
  outputraster[grid_index1] <- cow_data[grid_index2, "region"]
  names(outputraster) <- "region"
  writeRaster(
    outputraster,
    filename = regraster,
    NAflag = -99,
    datatype = ifelse(
      max(cellStats(outputraster, max)) < 2^15,
      "INT2S",
      "INT4S"
    ),
    overwrite = TRUE
  )
  if (tolower(rev(unlist(strsplit(regraster, ".", fixed = TRUE)))[1]) == "nc") {
    # NetCDF format
    # Try to add variable containing region names
    # Region names include countries not included in include_regions
    if (require(ncdf4)) {
      # Open file for writing
      nc <- nc_open(regraster, write = TRUE)
      # Generate required variable dimensions
      nchar_dim <- ncdim_def(
        "nchar",
        units = "",
        vals = seq_len(
          max(
            nchar(
              paste(regmetatable[, "ID"], regmetatable[, "region"], sep = ": ")
            ),
            nchar(
              paste(cowmetatable[, "ID"], cowmetatable[, "country"], sep = ": ")
            )
          )
        ),
        create_dimvar = FALSE
      )
      index <- which(cowmetatable[, "ID"] < min(regmetatable[, "ID"]))
      r <- nrow(regmetatable) + length(index)
      reg_dim <- ncdim_def(
        "region",
        units = "",
        vals = seq_len(r),
        create_dimvar = FALSE
      )
      # Generate variable
      reg_var <- ncvar_def(
        "region_name",
        units = "",
        dim = list(nchar_dim, reg_dim),
        longname = "region name",
        prec = "char"
      )
      # Add variable to file
      nc <- ncvar_add(nc, reg_var)
      # Write values to file
      ncvar_put(
        nc,
        "region_name",
        paste(
          c(
            cowmetatable[index, "ID"],
            regmetatable[, "ID"]
          ),
          c(
            cowmetatable[index, "country"],
            regmetatable[, "region"]
          ),
          sep = ": "
        )
      )
      nc_close(nc)
      rm(nchar_dim, reg_dim, reg_var, nc)
    }
  }
  # Reset raster values
  outputraster[] <- NA
}

# Number of countries
if (exists("ncountryraster")) {
  cat("Writing raster of number of countries in cell to", ncountryraster, "\n")
  grid_index <- match(lpjgrid_cells[, "GridID"], values(lpjgrid_raster))
  outputraster[grid_index] <- ncountry_data
  names(outputraster) <- "ncountry"
  writeRaster(
    outputraster,
    filename = ncountryraster,
    NAflag = -99,
    datatype = ifelse(
      max(cellStats(outputraster, max)) < 2^7,
      "INT1S",
      ifelse(
        max(cellStats(outputraster, max)) < 2^15,
        "INT2S",
        "INT4S"
      )
    ),
    overwrite = TRUE
  )
  # Reset raster values
  outputraster[] <- NA
}

# Land fraction
if (exists("landfracraster")) {
  cat("Writing raster of land fraction to", landfracraster, "\n")
  grid_index <- match(lpjgrid_cells[, "GridID"], values(lpjgrid_raster))
  outputraster[grid_index] <- frac_data
  names(outputraster) <- "Landfraction"
  writeRaster(
    outputraster,
    filename = landfracraster,
    NAflag = -99,
    compression = 9
  )
  # Reset raster values
  outputraster[] <- NA
}
################################################################################
