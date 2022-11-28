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
## - dominant country in each cell                                            ##
## - dominant region/state belonging to dominant country in each cell         ##
## - dominant district/country belonging to dominant region/state in each cell##
################################################################################

# Clean up memory
rm(list = ls(all = TRUE))
# Time execution
process_start <- proc.time()["elapsed"]

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

################################################################################
## Load country, region/state and district/country layer                      ##
## gadm_load() is defined in gadm_helper.R                                    ##
## gadm_dir is defined in gadm_setup.R                                        ##
cat(
  "Loading GADM shapes from",
  ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()), "\n"
)
##                                                                            ##
# Create a list of all countries (ISO code and name)
load_gadm(gadm_dir, levels = c(0))
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
load_gadm(gadm_dir, levels = c(1))
# Find all unique GADM level 1 codes
# Region names may not all be unique but codes should be
region_list <- as.list(unique(as.character(gadm_regions$GID_1)))
names(region_list) <- region_list
# Replace codes with region/state names
region_list <- sapply(
  region_list,
  function(indata) {
    as.character(gadm_regions$NAME_1[match(indata, gadm_regions$GID_1)])
  }
)
# Clean-up
rm(gadm_regions)
# Create a list of all districts (ISO code and name)
load_gadm(gadm_dir, levels = c(2))
# Find all unique GADM level 2 codes
# District names may not all be unique but codes should be
district_list <- as.list(unique(as.character(gadm_districts$GID_2)))
names(district_list) <- district_list
# Replace codes with district/county names
district_list <- sapply(
  district_list,
  function(indata) {
    as.character(gadm_districts$NAME_2[match(indata, gadm_districts$GID_2)])
  }
)
# Clean-up
rm(gadm_districts)
################################################################################


################################################################################
## Loop over shapefile intersections and create list with information         ##
## cell_list: a list containing a vector for each cell:                       ##
## - GridID (cell ID in raster)                                               ##
## - Gridarea (total cell area)                                               ##
## - Shapearea (land area of all polygons in cell)                            ##
## - Area covered by each GADM level 0 unit in cell                           ##
## - Area covered by each GADM level 1 unit in cell                           ##
## - Area covered by each GADM level 2 unit in cell                           ##
################################################################################
if (file.exists(cell_list_districts_RData)) {
  # Skip processing if RData file exists already from previous script run.
  # Delete RData file to force processing.
  cat("Reloading preprocessed cell list from", cell_list_districts_RData, "\n")
  load(cell_list_districts_RData)
} else {
  cat(
    "Reading in cell list from shapefiles in",
    intersect_directory_districts, "\n"
  )
  cell_list <- list()
  ISO_list <- character()
  # Find shapefiles in intersect_directory_districts
  intersect_shapes <- list.files(intersect_directory_districts, ".shp")
  # Loop over shapefiles (each shape file contains information for (a part of)
  # one country)
  for (country in intersect_shapes) {
    cat(country, "\n")
    # Read shapefile
    country_shape <- st_read(
      file.path(intersect_directory_districts, country),
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
            " is missing in country_list. Data in ",
            intersect_directory_districts,
            " appear to be inconsistent with GADM source data."
          )
        }
        # Area of all polygons in grid cell with same GID_0 (may be be more
        # than one if regions/states are present)
        carea <- sum(grid_shape[which(grid_shape$GID_0 == c), ]$Shapearea)
        if (c %in% names(cell_list[[GridID]])) {
          # For countries split into several shapefiles, some polygons may
          # have been counted already; add new carea
          cell_list[[GridID]][c] <- cell_list[[GridID]][c] + carea
        } else {
          # If country does not have entry in grid cell yet, add as new entry
          cell_list[[GridID]] <- c(cell_list[[GridID]], carea)
          names(cell_list[[GridID]])[length(cell_list[[GridID]])] <- c
        }
        # Also add carea to total land area in cell
        cell_list[[GridID]]["Landarea"] <- cell_list[[GridID]]["Landarea"] +
          carea

        # Processing of regions/states (level 1) belonging to country c
        # First check if level 1 information is available
        # (not for all countries)
        if (!"GID_1" %in% names(grid_shape)) {
          # Administrative level missing, add dummy
          grid_shape <- cbind(
            grid_shape,
            GID_1 = paste0(grid_shape$GID_0, ".1_1"),
            NAME_1 = paste0(grid_shape$NAME_0, " (dummy region)")
          )
        }
        # Now loop over regions; each region can have several district polygons
        for (r in unique(grid_shape[which(grid_shape$GID_0 == c), ]$GID_1)) {
          rindex <- which(grid_shape$GID_1 == r)
          if (!r %in% names(region_list)) {
              cat(
                "Adding region", sQuote(r),
                sQuote(
                  stri_trans_general(grid_shape[rindex, ]$NAME_1, "latin-ascii")
                ),
                "to region_list. This does not seem to be part of the",
                "original GADM source data.\n"
              )
              region_list[[r]] <- grid_shape[rindex, ]$NAME_1
          }
          # Determine area covered by each region
          if (r %in% names(cell_list[[GridID]])) {
            cell_list[[GridID]][r] <- cell_list[[GridID]][r] +
              sum(grid_shape[rindex, ]$Shapearea)
          } else {
            cell_list[[GridID]] <- c(
              cell_list[[GridID]],
              sum(grid_shape[rindex, ]$Shapearea)
            )
            names(cell_list[[GridID]])[length(cell_list[[GridID]])] <- r
          }
          # Processing of districts/counties (level 2) belonging to region r
          # First check if level 2 information is available
          # (not for all countries)
          if (!"GID_2" %in% names(grid_shape)) {
            # Administrative level missing, add dummy
            grid_shape <- cbind(
              grid_shape,
              GID_2 = paste0(
                unlist(
                  regmatches(
                    grid_shape$GID_1,
                    gregexpr("([A-Z]{3}).([0-9]+)", grid_shape$GID_1)
                  )
                ),
                ".1_1"
              ),
              NAME_2 = paste0(grid_shape$NAME_1, " (dummy district)")
            )
          }
          # Now loop over districts
          for (d in unique(grid_shape[rindex, ]$GID_2)) {
            dindex <- which(grid_shape$GID_2 == d)
            if (!d %in% names(district_list)) {
              cat(
                "Adding district", sQuote(d),
                sQuote(
                  stri_trans_general(grid_shape[dindex, ]$NAME_2, "latin-ascii")
                ),
                "to district_list. This does not seem to be part of the",
                "original GADM source data.\n"
              )
              district_list[[d]] <- grid_shape[dindex, ]$NAME_2
            }
            # Determine area covered by each district
            if (d %in% names(cell_list[[GridID]])) {
              cell_list[[GridID]][d] <- cell_list[[GridID]][d] +
                sum(grid_shape[dindex, ]$Shapearea)
            } else {
              cell_list[[GridID]] <- c(
                cell_list[[GridID]],
                sum(grid_shape[dindex, ]$Shapearea)
              )
              names(cell_list[[GridID]])[length(cell_list[[GridID]])] <- d
            }
          }
        }
      }
    }
  }
  # Since processing of shapefiles takes quite long save results to RData file
  # which will be reused on next script run
  save(
    cell_list,
    region_list,
    district_list,
    ISO_list,
    country_list,
    file = cell_list_districts_RData
  )
}


################################################################################
## Check for existence of gridname and cowname created by step 3.             ##
## Read data if available.                                                    ##
##                                                                            ##
## Load grid data generated by step 3 to ensure GADM level 0-2 information is ##
## available for all cells.                                                   ##
if (exists("gridname") && file.exists(gridname)) {
  if (gridformat == "BIN") {
    gridheader <- read_header(gridname)
    gridres <- gridheader$header[c("cellsize_lon", "cellsize_lat")]
    if (any(gridres / lpj_res[c("lon", "lat")] < 0.99999 |
        gridres / lpj_res[c("lon", "lat")] > 1.00001)) {
      stop(
        "Resolution in ", gridname,
        " [", toString(format(gridres, digits = 4)), "] ",
        "does not match lpj_res defined in gadm_setup.R ",
        "[", toString(format(lpj_res[c("lon", "lat")], digits = 4)), "]."
      )
    }
    gridfile <- file(gridname, "rb")
    seek(gridfile, get_headersize(gridheader))
    griddata_tmp <- matrix(
      readBin(
        gridfile,
        what = get_datatype(gridheader)$type,
        size = get_datatype(gridheader)$size,
        n = gridheader$header["ncell"] * gridheader$header["nbands"]
      ) * gridheader$header["scalar"],
      ncol = gridheader$header["nbands"],
      byrow = TRUE,
      dimnames = list(NULL, c("lon", "lat"))
    )
    close(gridfile)
  } else if (gridformat == "CSV") {
    griddata_tmp <- read.csv(gridname)
  } else {
    stop("Invalid gridformat ", gridformat)
  }
  if (force_grid && any(griddata != griddata_tmp)) {
    stop(
      "There seems to be a difference between the grid file ", gridname,
      " and the predefined grid supplied with force_grid"
    )
  } else {
    griddata <- griddata_tmp
    rm(griddata_tmp)
  }
} else if (exists("gridname")) {
  stop("Grid file ", gridname, " does not exist.")
} else {
  stop("Variable gridname does not exist")
}
# Load country code file generated by step 3.
if (exists("cowname") && file.exists(cowname)) {
  if (gridformat == "BIN") {
    cowheader <- read_header(cowname)
    cowres <- cowheader$header[c("cellsize_lon", "cellsize_lat")]
    if (any(cowres / lpj_res[c("lon", "lat")] < 0.99999 |
        cowres / lpj_res[c("lon", "lat")] > 1.00001)) {
      stop(
        "Resolution in ", cowname,
        " [", toString(format(cowres, digits = 4)), "] ",
        "does not match lpj_res defined in gadm_setup.R ",
        "[", toString(format(lpj_res[c("lon", "lat")], digits = 4)), "]."
      )
    }
    cowfile <- file(cowname, "rb")
    seek(cowfile, get_headersize(cowheader))
    cowdata <- matrix(
      readBin(
        cowfile,
        what = get_datatype(cowheader)$type,
        size = get_datatype(cowheader)$size,
        n = cowheader$header["ncell"] * cowheader$header["nbands"]
      ) * cowheader$header["scalar"],
      ncol = cowheader$header["nbands"],
      byrow = TRUE,
      dimnames = list(NULL, c("country", "region"))
    )
    close(cowfile)
  } else if (cowformat == "CSV") {
    cowdata <- read.csv(cowname)
  } else {
    stop("Invalid cowformat ", cowformat)
  }
} else if (exists("cowname")) {
  warning(
    "Country file ", cowname,
    " does not exist.\n",
    "It is generated by step 3. Please provide it as a consistency check.",
    call. = FALSE,
    immediate. = TRUE
  )
} else {
  stop("Variable cowname does not exist")
}
# Load meta information for country code file
if (exists("cowmetaname") && file.exists(cowmetaname)) {
  cowmetatable <- read.csv(cowmetaname)
} else if (exists("cowmetaname")) {
  warning(
    "Country file meta information ", cowmetaname,
    " does not exist.\n",
    "It is generated by step 3. Please provide it as a consistency check.",
    call. = FALSE,
    immediate. = TRUE
  )
}
################################################################################

################################################################################
## Consistency check between cell_list and griddata.                          ##
grid_index <- cellFromXY(lpjgrid_raster, griddata)
if (any(sapply(cell_list[lpjgrid_raster[grid_index]], is.null))) {
  if (exists("cowdata")) {
    # Check which country code missing cells from cell_list have in cowdata
    mindex <- which(sapply(cell_list[lpjgrid_raster[grid_index]], is.null))
    missing_cow <- cowdata[mindex, "country"]
    if (exists("cowmetatable")) {
      # Check that missing cells have "No land" code
      cindex <- match(missing_cow, cowmetatable[, "ID"])
      mismatch <- which(!cowmetatable[cindex, "ISO"] %in% names(gadm_no_land))
      if (length(mismatch) > 0) {
        # GADM country processing in steps 1-3 should have assigned
        # gadm_no_land to cells from grid that are outside of GADM land
        stop(
          length(mismatch),
          " cells in ", gridname, " have no GADM information in cell_list.",
          "\nThey are not associated to gadm_no_land in ", cowname, " either."
        )
      }
    }
  }
  # Set missing cells to gadm_no_land
  # Add gadm_no_land to country_list
  missing_cow <- setdiff(names(gadm_no_land), names(country_list))
  if (length(missing_cow) > 0) {
    country_list[missing_cow] <- gadm_no_land[missing_cow]
  }
  # Add dummy region to region_list
  reg <- paste0(names(gadm_no_land), ".1_1")
  reg_names <- paste(gadm_no_land, "(dummy region)")
  if (any(!reg %in% names(region_list))) {
    region_list[reg] <- reg_names
  }
  # Add dummy district to district_list
  dis <- paste0(
    unlist(
      regmatches(reg,  gregexpr("([A-Z]{3}).([0-9]+)", reg))
    ),
    ".1_1"
  )
  dis_names <- paste(reg_names, "(dummy district)")
  if (any(!dis %in% names(district_list))) {
    district_list[dis] <- dis_names
  }
  # Add gadm_no_land to ISO_list
  ISO_list <- unique(c(ISO_list, names(gadm_no_land)))
  if (force_grid) {
    # May have missing cells because grid is predefined
    warning(
      "Assigning gadm_no_land ", names(gadm_no_land),
      " (", sQuote(gadm_no_land), ") to ", length(mindex),
      " cells included in grid that have no land according to GADM.\n",
      "Please check created GADM level 0-2 files manually whether ",
      "these cells can be assigned to an adjacent GADM unit.",
      call. = FALSE,
      immediate. = TRUE
    )
  } else {
    # Should not have any cells outside GADM cover
    warning(
      length(mindex), " cells in ", gridname,
      " have no land according to GADM level 0-2 data. ",
      "This should not normally happen.\n",
      "Assigning them to gadm_no_land ",
      names(gadm_no_land), " (", sQuote(gadm_no_land), ").",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  for (c in mindex) {
    # Add dummy entry for No land with 0 area
    # cellarea() defined in helper_functions.R
    cell_list[[c]] <- c(
      GridID = c,
      Gridarea = cellarea(griddata[c, "lat"], lpj_res["lon"], lpj_res["lat"]),
      Landarea = 0,
      0, 0, 0
    )
    # Name country, region and district
    nindex <- seq(to = length(cell_list[[c]]), length.out = 3)
    names(cell_list[[c]])[nindex] <- c(names(gadm_no_land), reg, dis)
  }
}
# Check if GADM includes cells that are not used if a pre-defined grid is used.
if (force_grid) {
  unused <- seq(nrow(griddata) + 1, length(cell_list))
  if (any(sapply(cell_list[unused], length) > 0)) {
    warning(
      "Cell list contains data for ",
      length(which((sapply(cell_list[unused], length) > 0))),
      " cells which are not part of the predefined grid. ",
      "These cells will be dropped.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}
################################################################################

################################################################################
## Assign dominant GADM levels 0-2 to each cell.                              ##
cat("Determining dominant GADM level 0-2 in each cell.\n")
leveldata <- lapply(
  cell_list[lpjgrid_raster[grid_index]],
  dominant_levels,
  ISO_list = ISO_list,
  region_list = region_list,
  water_bodies = water_bodies
)
# Remove empty entries and combine in data.frame
level_df <- data.frame(
  matrix(
    unlist(leveldata[which(!sapply(leveldata, is.null))]),
    ncol = 4,
    byrow = TRUE,
    dimnames = list(NULL, c("GridID", "country", "region", "district"))
  )
)
level_df$GridID <- as.integer(level_df$GridID)
# Replace country/region/district codes by index number
level_index <- cbind(
  GridID = level_df$GridID,
  country = match(level_df$country, names(country_list)),
  region = match(level_df$region, names(region_list)),
  district = match(level_df$district, names(district_list))
)
# Consistency checks
if (anyNA(level_index)) {
  for (c in c("country", "region", "district")) {
    if (anyNA(level_index[, c])) {
      message(
        "The following ", c, " codes in level_df could not be found in ",
        c, "_list: ",
        toString(sQuote(level_df[which(is.na(level_index[, c])), c])), "\n",
      )
    }
  }
  stop("Code assignment")
}
if (exists("cowdata")) {
  # Consistency check between countries in level_index and countries in cowdata
  if (exists("cowmetatable")) {
    # Check that assigned countries match
    cindex <- match(cowdata[, "country"], cowmetatable[, "ID"])
    if (any(level_df$country != cowmetatable[cindex, "ISO"])) {
      stop(
        "There is a difference in assigned country between cell_list and ",
        cowname, " in ",
        length(which(level_df$country != cowmetatable[cindex, "ISO"])),
        " cells.\n",
        "Make sure you used the same GADM version for both."
      )
    }
  } else {
    # Check that the number of unique countries assigned is the same
    if (length(unique(level_index[, "country"])) !=
        length(unique(cowdata[, "country"]))) {
      stop(
        "Number of unique countries in level_index differs from ",
        "country file ", cowname,
        ".\nMake sure you used the same GADM version for both."
      )
    }
  }
}
# Meta information: names and assigned codes
gadmmetatable <- data.frame(
  level0_ID = c(
    seq_along(country_list),
    rep(NA, length(district_list) - length(country_list))
  ),
  level0_code = c(
    names(country_list),
    rep(NA, length(district_list) - length(country_list))
  ),
  country = c(
    as.character(country_list),
    rep(NA, length(district_list) - length(country_list))
  ),
  level1_ID = c(
    seq_along(region_list),
    rep(NA, length(district_list) - length(region_list))
  ),
  level1_code = c(
    names(region_list),
    rep(NA, length(district_list) - length(region_list))
  ),
  region = c(
    as.character(region_list),
    rep(NA, length(district_list) - length(region_list))
  ),
  level2_ID = seq_along(district_list),
  level2_code = names(district_list),
  district = as.character(district_list)
)
# To avoid problems with special characters if not running in a UTF8
# environment convert names to ASCII characters only.
for (c in c("country", "region", "district")) {
  gadmmetatable[, c] <- stri_trans_general(gadmmetatable[, c], "latin-ascii")
}
# Check if all admin units from GADM have been assigned to cells.
# Note: some units may be too small for a single cell
for (l in c("country", "region", "district")) {
  llist <- setdiff(names(get(paste0(l, "_list"))), skip_countries)
  if (any(!llist %in% level_df[, l])) {
    if (length(which(!llist %in% level_df[, l])) < 50) {
      cat(
        "The following", length(which(!llist %in% level_df[, l])), "out of",
        length(llist), l, "codes are not assigned to any cell:\n"
      )
      c <- switch(l,
        country = "level0_code",
        region = "level1_code",
        district = "level2_code",
        stop("Invalid level ", l)
      )
      unused <- match(setdiff(llist, level_df[, l]), gadmmetatable[, c])
      print(
        data.frame(
          ISO = setdiff(llist, level_df[, l]),
          name = gadmmetatable[unused, l],
          index = match(
            setdiff(llist, level_df[, l]),
            names(get(paste0(l, "_list")))
          )
        )
      )
    } else {
      cat(
        length(which(!llist %in% level_df[, l])), "out of a total of",
        length(llist), l, "codes are not assigned to any cell.\n"
      )
    }
  }
}
# Write GADM level 0-2 to file.
if (exists("gadmname")) {
  cat("Creating GADM level 0-2 file", gadmname, "\n")
}
for (filecheck in c("gadmname", "gadmmetaname")) {
  if (exists(filecheck) && file.exists(get(filecheck))) {
    stop(get(filecheck), " exists already. Delete file to create a new one.")
  }
}
if (gadmformat == "BIN" && exists("gadmname")) {
  scalar <- 1.0
  ## Variable type depends on bintype (version of LPJmL input format)
  if (bintype < 3) {
    ## Check if country/region/district indices can be expressed as short
    if (any(abs(level_index[, c("country", "region", "district")]) >= 2^15)) {
      stop(
        "Range of country/region/district codes [",
        toString(range(level_index[, c("country", "region", "district")])),
        "] cannot be saved as 2-byte integer.\n",
        "Set bintype to 3 to allow 4-byte integer"
      )
    }
    if (bintype < 2 && lpj_res["lon"] != 0.5) {
      stop("bintype 1 only supports resolution of 0.5 degree.")
    }
  }
  if (any(abs(level_index[, c("country", "region", "district")]) >= 2^31)) {
    stop(
      "Range of country/region/district codes [",
      toString(range(level_index[, c("country", "region", "district")])),
      "] cannot be saved as 4-byte integer.\n",
      "Reduce number of countries/regions/districts."
    )
  }
  # Create header of GADM level file
  gadmheader <- create_header(
    name = gadm_headername,
    version = bintype,
    nyear = 1,
    ncell = nrow(level_index),
    nbands = 2,
    cellsize_lon = lpj_res["lon"],
    scalar = 1 / scalar,
    datatype = ifelse(
      max(level_index[, c("country", "region", "district")]) >= 2^15,
      2,
      1
    ),
    cellsize_lat = lpj_res["lat"]
  )
  # Write header to file
  write_header(gadmname, gadmheader)
  # Open file in binary appending mode to add data
  gadmfile <- file(gadmname, "ab")
  # Data type to be written depends on data type set in header
  # get_datatype() return type and size of data to be written
  if (typeof(get_datatype(gadmheader)$type) == "integer") {
    writeBin(
      as.integer(
        t(level_index[, c("country", "region", "district")]) /
          gadmheader[["header"]]["scalar"]
      ),
      gadmfile,
      size = get_datatype(gadmheader)$size,
      endian = gadmheader[["endian"]]
    )
  } else if (typeof(get_datatype(gadmheader)$type) == "double") {
    writeBin(
      as.double(
        t(level_index[, c("country", "region", "district")]) /
          gadmheader[["header"]]["scalar"]
      ),
      gadmfile,
      size = get_datatype(gadmheader)$size,
      endian = gadmheader[["endian"]]
    )
  } else {
    stop(
      "Invalid datatype ",
      gadmheader[["header"]]["datatype"],
      " in gadmheader"
    )
  }
  close(gadmfile)
} else if (gadmformat == "CSV" && exists("gadmname")) {
  write.csv(
    level_index[, c("country", "region", "district")],
    file = gadmname,
    row.names = FALSE
  )
} else if (exists("gadmname")) {
  stop("Invalid gadmformat ", sQuote(gadmformat))
}
# Write meta information to file.
if (exists("gadmmetaname")) {
  cat("Meta information for GADM level 0-2 codes saved to", gadmmetaname, "\n")
  write.csv(
    gadmmetatable,
    file = gadmmetaname,
    fileEncoding = "ASCII",
    row.names = FALSE
  )
}
################################################################################


################################################################################
## Create raster brick of GADM level 0-2.                                     ##
## The brick has three layers:                                                ##
## 1) country index                                                           ##
## 2) region/state index                                                      ##
## 3) district/county index                                                   ##
## Make sure that gadmraster is set to a format that supports multiband       ##
## raster objects.                                                            ##
## If saved to a NetCDF file, names of GADM units are saved as well.          ##
if (exists("gadmraster")) {
  cat("Writing raster of GADM level 0-2 codes to", gadmraster, "\n")
  outputbrick <- brick(lpjgrid_raster, values = FALSE, nl = 3)
  names(outputbrick) <- c("country", "region", "district")
  grid_index <- match(level_index[, "GridID"], values(lpjgrid_raster))
  for (l in names(outputbrick)) {
    layervals <- integer(ncell(outputbrick))
    layervals[] <- NA
    layervals[grid_index] <- level_index[, l]
    outputbrick <- setValues(
      outputbrick,
      values = layervals,
      layer = match(l, names(outputbrick))
    )
  }
  writeRaster(
    outputbrick,
    filename = gadmraster,
    varname = "gadm",
    xname = "lon",
    yname = "lat",
    zname = "level",
    zunit = "",
    NAflag = -99,
    datatype = ifelse(
      max(cellStats(outputbrick, max)) < 2^15,
      "INT2S",
      "INT4S"
    ),
    overwrite = TRUE,
    compression = 9
  )
  if (tolower(
    rev(unlist(strsplit(gadmraster, ".", fixed = TRUE)))[1]
  ) == "nc" ||
    tolower(rev(unlist(strsplit(gadmraster, ".", fixed = TRUE)))[1]) == "nc4"
  ) {
    # NetCDF format
    # Try to add variables containing GADM names
    if (require(ncdf4)) {
      nc <- nc_open(gadmraster, write = TRUE)
      for (l in names(outputbrick)) {
        c <- switch(l,
          country = "level0_ID",
          region = "level1_ID",
          district = "level2_ID",
          stop("Invalid level ", l)
        )
        nchar_dim <- ncdim_def(
          paste0("nchar_", l),
          units = "",
          vals = 1:max(
            nchar(
              paste(
                unlist(gadmmetatable[, c]),
                unlist(gadmmetatable[, names(outputbrick)]),
                sep = ": "
              )
            )
          ),
          create_dimvar = FALSE
        )
        nvar_dim <- ncdim_def(
          paste0("n", l),
          units = "",
          vals = seq_along(which(!is.na(gadmmetatable[, l]))),
          create_dimvar = FALSE
        )
        names_var <- ncvar_def(
          paste(l, "name", sep = "_"),
          units = "",
          dim = list(nchar_dim, nvar_dim),
          longname = paste(l, "name"),
          prec = "char",
          compression = 9
        )
        nc <- ncvar_add(nc, names_var)
        ncvar_put(
          nc,
          varid = paste(l, "name", sep = "_"),
          vals = paste(
            gadmmetatable[, c],
            gadmmetatable[, l],
            sep = ": "
          )[which(!is.na(gadmmetatable[, l]))],
          count = c(-1, length(which(!is.na(gadmmetatable[, l]))))
        )
      }
      nc_close(nc)
    }
  }
}
################################################################################

# Time execution
cat("Script run took", proc.time()["elapsed"] - process_start, "seconds\n")
