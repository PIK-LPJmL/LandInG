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
## - all cells with land (unless force_grid == TRUE) to use as grid           ##
## - fraction of each grid cell covered by land                               ##
## - dominant country in each cell                                            ##
## - dominant state/region in each cell for countries included in             ##
##   include_regions                                                          ##
## - number of countries in each grid cell                                    ##
## In addition, it parses the full GADM level 0-2 to create an administrative ##
## hierarchy of:
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
cat("Spatial resolution:", LandInG_setup$gadm$lpj_res_string, "\n")
if (LandInG_setup$gadm$force_grid) {
  cat(
    "Info: Using predefined grid with", nrow(LandInG_setup$gadm$griddata),
    "cells and a spatial extent:", toString(LandInG_setup$gadm$lpjgrid_extent),
    "\n"
  )
}

################################################################################
## Load country, region/state and district/country layer                      ##
## load_gadm() is defined in gadm_helper.R                                    ##
## gadm_dir is defined in gadm_setup.R                                        ##
cat(
  "Loading GADM shapes from",
  ifelse(
    nchar(LandInG_setup$gadm$gadm_dir) > 0,
    LandInG_setup$gadm$gadm_dir,
    getwd()
  ),
  "\n"
)
##                                                                            ##
# Create a list of all countries (ISO code and name)
load_gadm(
  LandInG_setup$gadm$gadm_dir,
  version = LandInG_setup$gadm$gadm_data_version,
  levels = c(0),
  format = LandInG_setup$gadm$gadm_format,
  return_env = LandInG_setup
)
country_list <- unique(as.character(LandInG_setup$gadm$gadm_countries$GID_0))
names(country_list) <- country_list
# Replace codes with country names
# Different versions of GADM use different names for the country column. Try to
# detect.
country_col <- grep(
  "NAME_0|Country",
  colnames(LandInG_setup$gadm$gadm_countries),
  ignore.case = TRUE,
  value = TRUE
)
if (length(country_col) != 1) {
  stop("Cannot detect country column in GADM data")
}
# Replace codes with names
index <- match(country_list, LandInG_setup$gadm$gadm_countries$GID_0)
country_list[] <- unlist(
  sf::st_drop_geometry(LandInG_setup$gadm$gadm_countries[index, country_col])
)
# Clean-up
rm(index)
# Check for NA values in GADM data
if (anyNA(country_list)) {
  stop(
    length(which(is.na(country_list))),
    " NA values found in country_list derived from GADM level 0"
  )
}
LandInG_setup$gadm$gadm_countries <- NULL
# Create a list of all regions (ISO code and name)
load_gadm(
  LandInG_setup$gadm$gadm_dir,
  version = LandInG_setup$gadm$gadm_data_version,
  levels = c(1),
  format = LandInG_setup$gadm$gadm_format,
  return_env = LandInG_setup
)
# Find all unique GADM level 1 codes
# Region names may not all be unique but codes should be
region_list <- unique(as.character(LandInG_setup$gadm$gadm_regions$GID_1))
names(region_list) <- region_list
# Replace codes with region/state names
index <- match(region_list, LandInG_setup$gadm$gadm_regions$GID_1)
region_list[] <- unlist(LandInG_setup$gadm$gadm_regions$NAME_1[index])
# Clean-up
rm(index)
# Check for NA values in GADM data
if (anyNA(region_list)) {
  missing_names <- names(which(is.na(region_list)))
  r <- match(missing_names, LandInG_setup$gadm$gadm_regions$GID_1)
  region_list[missing_names] <- paste(
    sf::st_drop_geometry(LandInG_setup$gadm$gadm_regions[r, country_col]),
    "(dummy region)"
  )
  if (anyNA(region_list)) {
    stop(
      length(missing_names), " NA values in region_list derived from GADM",
      " level 1 could not be filled with dummy values."
    )
  } else {
    warning(
      length(missing_names), " NA values in region_list derived from GADM",
      " level 1 filled with dummy names.",
      immediate. = TRUE
    )
  }
}
LandInG_setup$gadm$gadm_regions <- NULL
# Create a list of all districts (ISO code and name)
load_gadm(
  LandInG_setup$gadm$gadm_dir,
  version = LandInG_setup$gadm$gadm_data_version,
  levels = c(2),
  format = LandInG_setup$gadm$gadm_format,
  return_env = LandInG_setup
)
# Find all unique GADM level 2 codes
# District names may not all be unique but codes should be
district_list <- unique(as.character(LandInG_setup$gadm$gadm_districts$GID_2))
names(district_list) <- district_list
# Replace codes with district/county names
index <- match(district_list, LandInG_setup$gadm$gadm_districts$GID_2)
district_list[] <- unlist(LandInG_setup$gadm$gadm_districts$NAME_2[index])
# Clean-up
rm(index)
# Check for NA values in GADM data
if (anyNA(district_list)) {
  missing_names <- names(which(is.na(district_list)))
  r <- match(missing_names, LandInG_setup$gadm$gadm_districts$GID_2)
  district_list[missing_names] <- paste(
    LandInG_setup$gadm$gadm_districts$NAME_1[r],
    "(dummy district)"
  )
  if (anyNA(district_list)) {
    missing_names <- names(which(is.na(district_list)))
    r <- match(missing_names, LandInG_setup$gadm$gadm_districts$GID_2)
    district_list[missing_names] <- paste(
      sf::st_drop_geometry(LandInG_setup$gadm$gadm_districts[r, country_col]),
      "(dummy region) (dummy district)"
    )
  }
  if (anyNA(district_list)) {
    stop(
      length(missing_names), " NA values in district_list derived from GADM",
      " level 2 could not be filled with dummy values."
    )
  } else {
    warning(
      length(missing_names), " NA values in district_list derived from GADM",
      " level 2 filled with dummy names.",
      immediate. = TRUE
    )
  }
}
LandInG_setup$gadm$gadm_districts <- NULL
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
if (file.exists(LandInG_setup$gadm$cell_list_RData)) {
  # Skip processing if RData file exists already from previous script run.
  # Delete RData file to force processing.
  cat(
    "Reloading preprocessed cell list from",
    LandInG_setup$gadm$cell_list_RData, "\n"
  )
  load(LandInG_setup$gadm$cell_list_RData)
  # Consistency check
  if (
    !identical(
      intersect_shapes,
      list.files(LandInG_setup$gadm$intersect_directory, ".shp$")
    )
  ) {
    stop(
      "Preprocessed cell list ", LandInG_setup$gadm$cell_list_RData,
      " does not match shapefiles in ",
      LandInG_setup$gadm$intersect_directory,
      ".\nDelete cell list and run this script again."
    )
  }
} else {
  cat(
    "Reading in cell list from shapefiles in",
    LandInG_setup$gadm$intersect_directory, "\n"
  )
  cell_list <- list()
  ISO_list <- character()
  # Find shapefiles in intersect_directory
  intersect_shapes <- list.files(
    LandInG_setup$gadm$intersect_directory,
    ".shp$"
  )
  # Consistency check with split_directory
  country_shapes <- list.files(
    LandInG_setup$gadm$split_directory,
    ".shp$"
  )
  if (!identical(intersect_shapes, country_shapes)) {
    stop(
      "Inconsistency between shapefiles in split_directory ",
      LandInG_setup$gadm$split_directory,
      " and intersect_directory ",
      LandInG_setup$gadm$intersect_directory,
      ", suggesting that step 1 and step 2 may have run with inconsistent",
      " settings.\nPlease check and run again if necessary."
    )
  }
  rm(country_shapes)
  # Loop over shapefiles (each shapefile contains information for (a part of)
  # one country)
  for (country in intersect_shapes) {
    cat(country, "\n")
    # Read shapefile
    country_shape <- sf::st_read(
      file.path(LandInG_setup$gadm$intersect_directory, country),
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
        if (c %in% LandInG_setup$gadm$skip_countries) {
          # Do not use data from countries in skip_countries
          next
        }
        if (!c %in% names(country_list)) {
          # Invalid country code
          stop(
            "Country code ", sQuote(c), " found in ", country,
            " is missing in country_list. Data in ",
            LandInG_setup$gadm$intersect_directory,
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
          if (c %in% LandInG_setup$gadm$include_regions) {
            warning(
              "No region data available for ", country,
              " included in include_regions",
              call. = FALSE, immediate. = TRUE
            )
          }
          # Administrative level missing, add dummy
          grid_shape <- cbind(
            grid_shape,
            GID_1 = paste0(grid_shape$GID_0, ".1_1"),
            NAME_1 = paste(
              sf::st_drop_geometry(grid_shape[, country_col]),
              "(dummy region)"
            )
          )
        }
        # Now loop over regions; each region can have several district polygons
        for (r in unique(grid_shape[which(grid_shape$GID_0 == c), ]$GID_1)) {
          rindex <- which(grid_shape$GID_1 == r)
          if (!r %in% names(region_list)) {
            cat(
              "Adding region", sQuote(r),
              sQuote(
                stringi::stri_trans_general(
                  grid_shape[rindex, ]$NAME_1,
                  "latin-ascii"
                )
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
              NAME_2 = paste(grid_shape$NAME_1, "(dummy district)")
            )
          }
          # Now loop over districts
          for (d in unique(grid_shape[rindex, ]$GID_2)) {
            dindex <- which(grid_shape$GID_2 == d)
            if (!d %in% names(district_list)) {
              cat(
                "Adding district", sQuote(d),
                sQuote(
                  stringi::stri_trans_general(
                    grid_shape[dindex, ]$NAME_2,
                    "latin-ascii"
                  )
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
    intersect_shapes,
    file = LandInG_setup$gadm$cell_list_RData
  )
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
rm(frac_list)
# If force_grid == TRUE check if all cells in griddata have a land fraction
# assigned. If not, set to 0.
if (LandInG_setup$gadm$force_grid) {
  # Coordinates from griddata have been assigned GridIDs from 1 to
  # nrow(griddata), check if any missing
  if (
    any(!seq_len(nrow(LandInG_setup$gadm$griddata)) %in% frac_cells[, "GridID"])
  ) {
    warning(
      length(
        setdiff(
          seq_len(nrow(LandInG_setup$gadm$griddata)),
          frac_cells[, "GridID"]
        )
      ),
      " cells in your predefined grid are not covered by GADM.",
      " Assigning a land fraction of 0.",
      call. = FALSE,
      immediate. = TRUE
    )
    frac_cells <- rbind(
      frac_cells,
      cbind(
        setdiff(
          seq_len(nrow(LandInG_setup$gadm$griddata)),
          frac_cells[, "GridID"]
        ),
        0
      )
    )
  }
} else if (LandInG_setup$gadm$threshold_grid > 0) {
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
  if (any(landarea[, "Landarea"] < LandInG_setup$gadm$threshold_grid)) {
    message(
      "Info: Removing ",
      length(which(landarea[, "Landarea"] < LandInG_setup$gadm$threshold_grid)),
      " cells from grid because their cell area is below threshold_grid of ",
      LandInG_setup$gadm$threshold_grid, " m2."
    )
    select <- which(landarea[, "Landarea"] >= LandInG_setup$gadm$threshold_grid)
    valid_cells <- match(landarea[select, "GridID"], frac_cells[, "GridID"])
    frac_cells <- frac_cells[valid_cells, ]
    rm(select, valid_cells)
  }
}
################################################################################


################################################################################
## Derive coordinates of all land cells which will be included in grid.       ##
## If (force_grid==TRUE) use predefined grid.                                 ##
## Otherwise use all cells included in frac_cells.                            ##
if (LandInG_setup$gadm$force_grid) {
  # Grid corresponds to predefined list of coordinates
  lpjgrid_cells <- cbind(
    as.matrix(LandInG_setup$gadm$griddata),
    GridID = seq_len(nrow(LandInG_setup$gadm$griddata))
  )
} else {
  # Copy lpjgrid_raster
  lpjfound_raster <- terra::rast(LandInG_setup$gadm$lpjgrid_raster)
  # Assign all cells with country code using GridID
  grid_index <- match(
    frac_cells[, "GridID"],
    terra::values(LandInG_setup$gadm$lpjgrid_raster)
  )
  lpjfound_raster[grid_index] <- frac_cells[, "GridID"]
  # Extract coordinates of all cells with values
  lpjgrid_cells <- as.matrix(terra::as.data.frame(lpjfound_raster, xy = TRUE))
  colnames(lpjgrid_cells) <- c("lon", "lat", "GridID")
  rm(lpjfound_raster, grid_index)
  LandInG_setup$gadm$griddata <- lpjgrid_cells[, c("lon", "lat")]
}
################################################################################


################################################################################
## Create grid file                                                           ##
## File name gridname defined in gadm_setup.R                                 ##
## Format gridformat defined in gadm_setup.R                                  ##
## Version for LPJmL input format bintype defined in gadm_setup.R             ##
## If the format is "BIN" this uses a number of helper functions for the      ##
## LPJmL file format from the lpjmlkit package.                               ##
if (!is.null(LandInG_setup$gadm$gridname)) {
  cat("Creating grid file", LandInG_setup$gadm$gridname, "\n")
  if (file.exists(LandInG_setup$gadm$gridname)) {
    # Check whether existing file matches coordinates derived here
    if (LandInG_setup$gadm$gridformat == "BIN") {
      gridheader <- lpjmlkit::read_header(LandInG_setup$gadm$gridname)
      griddata_tmp <- lpjmlkit::read_grid(
        LandInG_setup$gadm$gridname,
        silent = TRUE
      )$data
      if (
        gridheader$header["ncell"] != nrow(lpjgrid_cells) ||
          gridheader$header["datatype"] != ifelse(
            LandInG_setup$gadm$bintype < 3,
            1,
            3
          ) ||
          !isTRUE(
            all.equal(
              gridheader$header[c("cellsize_lon", "cellsize_lat")],
              LandInG_setup$gadm$lpj_res,
              check.attributes = FALSE,
              tolerance = LandInG_setup$single.eps
            )
          )
      ) {
        stop(
          LandInG_setup$gadm$gridname, " exists already and is inconsistent",
          " with current script run. Delete file to create a new one."
        )
      }
    } else if (LandInG_setup$gadm$gridformat == "CSV") {
      griddata_tmp <- read.csv(LandInG_setup$gadm$gridname)
    } else {
      stop("Invalid gridformat ", sQuote(LandInG_setup$gadm$gridformat))
    }
    if (
      (
        LandInG_setup$gadm$gridformat == "BIN" &&
          lpjmlkit::get_datatype(gridheader)$type == "integer" &&
          any(
            abs(
              griddata_tmp[, c("lon", "lat")] - lpjgrid_cells[, c("lon", "lat")]
            ) > gridheader$header["scalar"] * 0.5
          )
      ) ||
        !isTRUE(
          all.equal(
            griddata_tmp[, c("lon", "lat")],
            lpjgrid_cells[, c("lon", "lat")],
            check.attributes = FALSE,
            tolerance = LandInG_setup$single.eps
          )
        )
    ) {
      stop(
        LandInG_setup$gadm$gridname, " exists already and is inconsistent with",
        " derived coordinates. Delete file to create a new one."
      )
    } else {
      cat(
        LandInG_setup$gadm$gridname,
        "exists already and is not created again.\n"
      )
    }
  } else {
    if (LandInG_setup$gadm$gridformat == "BIN") {
      scalar <- 1.0
      # Variable type depends on bintype (version of LPJmL input format)
      if (LandInG_setup$gadm$bintype < 3) {
        # Check if coordinates can be expressed as integer values by multiplying
        # with a scalar
        scaled_coords <- lpjgrid_cells[, c("lon", "lat")] * scalar
        while (any(scaled_coords != as.integer(scaled_coords))) {
          scalar <- scalar * 10
          # Bintype 1 and 2 allow only data type short (2-byte integer), check
          # if valid range.
          scaled_coords <- lpjgrid_cells[, c("lon", "lat")] * scalar
          if (max(abs(scaled_coords)) >= 2^15) {
            stop(
              "Grid coordinates cannot be saved as data type short.\n",
              "Set bintype to 3 to allow saving as floating point values."
            )
          }
        }
        # Bintype 1 needs a default scaling factor of 100.
        if (LandInG_setup$gadm$bintype < 2 && scalar != 100) {
          stop(
            "Grid coordinates cannot be saved as data type short using as the ",
            "default scaling factor.\n",
            "Set bintype at least to 2 to allow flexible scaling factors.\n",
            "Or set bintype to 3 to allow saving as floating point values."
          )
        }
        # Bintype 1 does not support saving resolution, only default of 0.5.
        if (LandInG_setup$gadm$bintype < 2 &&
            LandInG_setup$gadm$lpj_res["lon"] != 0.5
        ) {
          stop("bintype 1 only supports resolution of 0.5 degree")
        }
      }
      # Create header of grid file
      gridheader <- lpjmlkit::create_header(
        name = LandInG_setup$gadm$grid_headername,
        version = LandInG_setup$gadm$bintype,
        nyear = 1,
        ncell = nrow(lpjgrid_cells),
        nbands = 2,
        cellsize_lon = LandInG_setup$gadm$lpj_res["lon"],
        scalar = 1 / scalar,
        datatype = ifelse(LandInG_setup$gadm$bintype < 3, 1, 3),
        cellsize_lat = LandInG_setup$gadm$lpj_res["lat"]
      )
      # Write header to file
      lpjmlkit::write_header(LandInG_setup$gadm$gridname, gridheader)
      # Open file in binary appending mode to add data
      gridfile <- file(LandInG_setup$gadm$gridname, "ab")
      # Data type to be written depends on data type set in header
      # get_datatype() returns type and size of data to be written
      if (typeof(lpjmlkit::get_datatype(gridheader)$type) == "integer") {
        writeBin(
          as.integer(
            round(
              t(lpjgrid_cells[, c("lon", "lat")]) /
                gridheader[["header"]]["scalar"]
            )
          ),
          gridfile,
          size = lpjmlkit::get_datatype(gridheader)$size,
          endian = gridheader[["endian"]]
        )
      } else if (typeof(lpjmlkit::get_datatype(gridheader)$type) == "double") {
        writeBin(
          as.double(
            t(lpjgrid_cells[, c("lon", "lat")]) /
              gridheader[["header"]]["scalar"]
          ),
          gridfile,
          size = lpjmlkit::get_datatype(gridheader)$size,
          endian = gridheader[["endian"]]
        )
      } else {
        stop(
          "Invalid datatype ", gridheader[["header"]]["datatype"],
          " in gridheader"
        )
      }
      close(gridfile)
    } else if (LandInG_setup$gadm$gridformat == "CSV") {
      write.csv(
        lpjgrid_cells[, c("lon", "lat")],
        file = LandInG_setup$gadm$gridname,
        row.names = FALSE
      )
    } else {
      stop("Invalid gridformat ", sQuote(LandInG_setup$gadm$gridformat))
    }
  }
} else {
  warning(
    "Grid data is not written to file because 'gridname' is not defined in ",
    "gadm_setup.R",
    call. = FALSE, immediate. = TRUE
  )
}
################################################################################


################################################################################
## Create land fraction file                                                  ##
## File name landfracname defined in gadm_setup.R                             ##
## Format landfracformat defined in gadm_setup.R                              ##
## Version for LPJmL input format bintype defined in gadm_setup.R             ##
## If the format is "BIN" this uses a number of helper functions for the      ##
## LPJmL file format from the lpjmlkit package.                               ##
if (!is.null(LandInG_setup$gadm$landfracname) ||
    !is.null(LandInG_setup$gadm$landfracraster)
) {
  # Prepare data
  frac_data <- numeric(nrow(lpjgrid_cells))
  grid_index <- match(lpjgrid_cells[, "GridID"], frac_cells[, "GridID"])
  frac_data <- frac_cells[grid_index, "Landfraction"]
  if (anyNA(frac_data)) {
    stop("frac_data could not be filled correctly.")
  }
  # Check that land fraction does not exceed 100%, allow for some numerical
  # inaccuracy. Constrain maximum value to 1.0.
  # Additional note: There are some erroneous, overlapping polygons in GADM
  # (tested for version 3.6) that can lead to total land area exceeding grid
  # area.
  tolerance <- 1.002
  if (any(frac_data > tolerance)) {
    stop(
      "There are ", length(which(frac_data > tolerance)),
      " cells in frac_data with land fraction exceeding ", tolerance
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
}
if (!is.null(LandInG_setup$gadm$landfracname)) {
  cat("Creating land fraction file", LandInG_setup$gadm$landfracname, "\n")
  if (file.exists(LandInG_setup$gadm$landfracname)) {
    if (LandInG_setup$gadm$landfracformat == "BIN") {
      landfracheader <- lpjmlkit::read_header(
        LandInG_setup$gadm$landfracname,
        verbose = FALSE
      )
      if (
        landfracheader$header["ncell"] != nrow(lpjgrid_cells) ||
          !isTRUE(
            all.equal(
              landfracheader$header[c("cellsize_lon", "cellsize_lat")],
              LandInG_setup$gadm$lpj_res,
              check.attributes = FALSE,
              tolerance = LandInG_setup$single.eps
            )
          )
      ) {
        stop(
          LandInG_setup$gadm$landfracname,
          " exists already and is inconsistent with current script run. ",
          "Delete file to create a new one."
        )
      } else {
        landfrac_tmp <- matrix(
          drop(
            lpjmlkit::read_io(
              LandInG_setup$gadm$landfracname,
              silent = TRUE
            )$data
          ),
          ncol = 1, dimnames = list(NULL, "Landfraction")
        )
      }
    } else if (LandInG_setup$gadm$landfracformat == "CSV") {
      landfrac_tmp <- read.csv(LandInG_setup$gadm$landfracname)
    } else {
      stop("Invalid landfracformat ", sQuote(LandInG_setup$gadm$landfracformat))
    }
    if (
      (
        LandInG_setup$gadm$landfracformat == "BIN" &&
          lpjmlkit::get_datatype(landfracheader)$type == "integer" &&
          any(
            abs(landfrac_tmp[, "Landfraction"] - frac_data) >
              landfracheader$header["scalar"] * 0.5
          )
      ) ||
        !isTRUE(
          all.equal(
            landfrac_tmp[, "Landfraction"],
            frac_data,
            tolerance = LandInG_setup$single.eps,
            check.attributes = FALSE
          )
        )
    ) {
      stop(
        LandInG_setup$gadm$landfracname,
        " exists already and is inconsistent with current script run. ",
        "Delete file to create a new one."
      )
    } else {
      cat(
        LandInG_setup$gadm$landfracname,
        "exists already and is not created again.\n"
      )
    }
    rm(landfrac_tmp)
  } else {
    # Write out data
    if (LandInG_setup$gadm$landfracformat == "BIN") {
      scalar <- 1.0
      # Variable type depends on bintype (version of LPJmL input format)
      if (LandInG_setup$gadm$bintype == 2) {
        # Check for maximum scaling factor that still allows values to be
        # expressed as 2-byte integer
        while (max(frac_cells[, "Landfraction"]) * scalar * 10 < 2^15) {
          scalar <- scalar * 10
        }
        message(
          "Info: land fraction will be saved as a 2-byte integer using a ",
          "scaling factor of ", scalar, ", which reduces precision. ",
          "Use bintype 3 to save as floating point value."
        )
      } else if (LandInG_setup$gadm$bintype == 1) {
        # There is currently no default scalar in the LPJmL code so it must be
        # provided by the input file
        stop(
          "No default scalar for land fraction defined. ",
          "Use at least bintype 2 or even bintype 3."
        )
      }
      # Create header of land fraction file
      landfracheader <- lpjmlkit::create_header(
        name = LandInG_setup$gadm$landfrac_headername,
        version = LandInG_setup$gadm$bintype,
        nyear = 1,
        ncell = length(frac_data),
        nbands = 1,
        cellsize_lon = LandInG_setup$gadm$lpj_res["lon"],
        scalar = 1 / scalar,
        datatype = ifelse(LandInG_setup$gadm$bintype < 3, 1, 3),
        cellsize_lat = LandInG_setup$gadm$lpj_res["lat"]
      )
      # Write header to file
      lpjmlkit::write_header(LandInG_setup$gadm$landfracname, landfracheader)
      # Open file in binary appending mode to add data
      landfracfile <- file(LandInG_setup$gadm$landfracname, "ab")
      # Data type to be written depends on data type set in header
      # get_datatype() returns type and size of data to be written
      if (typeof(lpjmlkit::get_datatype(landfracheader)$type) == "integer") {
        writeBin(
          as.integer(round(frac_data / landfracheader[["header"]]["scalar"])),
          landfracfile,
          size = lpjmlkit::get_datatype(landfracheader)$size,
          endian = landfracheader[["endian"]]
        )
      } else if (
        typeof(lpjmlkit::get_datatype(landfracheader)$type) == "double"
      ) {
        writeBin(
          as.double(frac_data / landfracheader[["header"]]["scalar"]),
          landfracfile,
          size = lpjmlkit::get_datatype(landfracheader)$size,
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
    } else if (LandInG_setup$gadm$landfracformat == "CSV") {
      write.csv(
        matrix(frac_data, ncol = 1, dimnames = list(NULL, "Landfraction")),
        file = LandInG_setup$gadm$landfracname,
        row.names = FALSE
      )
    } else {
      stop("Invalid landfracformat ", sQuote(LandInG_setup$gadm$landfracformat))
    }
  }
} else {
  message(
    "Land fraction data is not written to file because 'landfracname' is not ",
    "defined in gadm_setup.R"
  )
}
if (!is.null(LandInG_setup$gadm$landfracraster)) {
  cat(
    "Writing raster of land fraction to",
    LandInG_setup$gadm$landfracraster, "\n"
  )
  # Empty raster
  outputraster <- terra::rast(LandInG_setup$gadm$lpjgrid_raster)
  grid_index <- match(
    lpjgrid_cells[, "GridID"],
    terra::values(LandInG_setup$gadm$lpjgrid_raster)
  )
  outputraster[grid_index] <- frac_data
  names(outputraster) <- "Landfraction"
  if (
    grepl(".nc[0-9]*$", LandInG_setup$gadm$landfracraster, ignore.case = TRUE)
  ) {
    terra::writeCDF(
      outputraster,
      filename = LandInG_setup$gadm$landfracraster,
      varname = names(outputraster),
      compression = 9,
      overwrite = TRUE
    )
  } else {
    terra::writeRaster(
      outputraster,
      filename = LandInG_setup$gadm$landfracraster,
      overwrite = TRUE
    )
  }
  rm(outputraster)
}
################################################################################


################################################################################
## Determine dominant country in each cell included in cell_list.             ##
## Function dominant_country() defined in gadm_helper.R                       ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and ISO code of the dominant country.                           ##
if (!is.null(LandInG_setup$gadm$cowname) ||
    !is.null(LandInG_setup$gadm$cowmetaname) ||
    !is.null(LandInG_setup$gadm$cowraster)
) {
  cat("Determining dominant country in each cell.\n")
  cow_list <- lapply(
    cell_list,
    dominant_country,
    ISO_list = ISO_list,
    water_bodies = LandInG_setup$gadm$water_bodies
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
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fix.empty.names = FALSE
  )
  rm(cow_list)
  # Set GridID column from character to integer
  cow_cells$GridID <- as.integer(cow_cells$GridID)
  # If force_grid == TRUE check if all cells in griddata have a country
  # assigned, if not assign gadm_no_land defined in gadm_setup.R
  if (LandInG_setup$gadm$force_grid) {
    # Coordinates from griddata have been assigned GridIDs from 1 to
    # nrow(griddata), check if any missing.
    if (
      any(!seq_len(nrow(LandInG_setup$gadm$griddata)) %in% cow_cells$GridID)
    ) {
      warning(
        length(
          setdiff(seq_len(nrow(LandInG_setup$gadm$griddata)), cow_cells$GridID)
        ),
        " cells in your predefined grid are not covered by GADM. Assigning ",
        sQuote(LandInG_setup$gadm$gadm_no_land),
        " (", names(LandInG_setup$gadm$gadm_no_land), ")",
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
          GridID = setdiff(
            seq_len(nrow(LandInG_setup$gadm$griddata)),
            cow_cells$GridID
          ),
          country = names(LandInG_setup$gadm$gadm_no_land),
          stringsAsFactors = FALSE,
          check.names = FALSE,
          fix.empty.names = FALSE
        )
      )
      # Add gadm_no_land to country_list
      country_list[[names(LandInG_setup$gadm$gadm_no_land)]] <-
        LandInG_setup$gadm$gadm_no_land
    }
    # Check if GADM includes cells not included in predefined grid
    if (any(cow_cells$GridID > nrow(LandInG_setup$gadm$griddata))) {
      warning(
        "GADM includes land in ",
        length(which(cow_cells$GridID > nrow(LandInG_setup$gadm$griddata))),
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
}
################################################################################


################################################################################
## Determine dominant region/state in each cell included in cell_list.        ##
## Function dominant_region() defined in gadm_helper.R                        ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and code of the dominant region/state.                          ##
if (
  length(LandInG_setup$gadm$include_regions) > 0 &&
    (!is.null(LandInG_setup$gadm$cowname) ||
        !is.null(LandInG_setup$gadm$regmetaname) ||
        !is.null(LandInG_setup$gadm$regraster)
    )
) {
  cat(
    "Determining dominant region/state in countries included in",
    "include_regions.\n"
  )
  reg_list <- lapply(
    cell_list,
    dominant_region,
    region_list = region_list,
    ISO_list = ISO_list,
    include_regions = LandInG_setup$gadm$include_regions,
    water_bodies = LandInG_setup$gadm$water_bodies
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
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fix.empty.names = FALSE
  )
  rm(reg_list)
  # Set GridID column from character to integer
  reg_cells$GridID <- as.integer(reg_cells$GridID)
  # If force_grid == TRUE check if any cells are not included in predefined grid
  if (LandInG_setup$gadm$force_grid) {
    if (any(reg_cells$GridID > nrow(LandInG_setup$gadm$griddata))) {
      warning(
        "GADM includes region/state information in ",
        length(which(reg_cells$GridID > nrow(LandInG_setup$gadm$griddata))),
        " cells not included in predefined grid. ",
        "These will be dropped from country code file ",
        LandInG_setup$gadm$cowname,
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
## Note: The LPJmL code has country and region codes and corresponding names  ##
## hard-coded. These may require updates to be consistent with GADM.          ##
if (!is.null(LandInG_setup$gadm$cowname) ||
    !is.null(LandInG_setup$gadm$cowmetaname) ||
    !is.null(LandInG_setup$gadm$cowraster) ||
    !is.null(LandInG_setup$gadm$regmetaname) ||
    !is.null(LandInG_setup$gadm$regraster)
) {
  # Countries first ordered alphabetically by ISO code (excluding ISO codes
  # starting with "X")
  sort_order <- sort(
    setdiff(
      names(country_list),
      c(
        LandInG_setup$gadm$include_regions,
        names(LandInG_setup$gadm$gadm_no_land),
        grep("^X", names(country_list), value = TRUE)
      )
    )
  )
  level0 <- unlist(country_list[sort_order])
  # Add countries whose ISO code starts with "X" (which are not part of
  # ISO-3166)
  sort_order <- sort(
    setdiff(
      grep("^X", names(country_list), value = TRUE),
      c(LandInG_setup$gadm$include_regions,
        names(LandInG_setup$gadm$gadm_no_land))
    )
  )
  level0 <- c(level0, unlist(country_list[sort_order]))
  # Add gadm_no_land
  level0 <- c(level0, LandInG_setup$gadm$gadm_no_land)
  # Add countries from include_regions
  index <-
    sort(intersect(names(country_list), LandInG_setup$gadm$include_regions))
  level0 <- c(level0, unlist(country_list[index]))
  rm(index)
  # To avoid problems with special characters if not running in a UTF-8
  # environment convert names to ASCII characters only.
  level0_ascii <- stringi::stri_trans_general(level0, "latin-ascii")
  names(level0_ascii) <- names(level0)
  # Indices to be used
  level0_index <- seq(0, length.out = length(level0_ascii))
  names(level0_index) <- names(level0)
  # Extract all regions of countries in include_regions
  if (length(LandInG_setup$gadm$include_regions) > 0) {
    level1 <- character()
    for (c in
      sort(
        intersect(
          substr(names(region_list), 1, 3),
          LandInG_setup$gadm$include_regions
        )
      )
    ) {
      level1 <- c(level1, region_list[grep(c, names(region_list))])
    }
    # To avoid problems with special characters if not running in a UTF-8
    # environment convert names to ASCII characters only.
    level1_ascii <- stringi::stri_trans_general(level1, "latin-ascii")
    names(level1_ascii) <- names(level1)
    # Indices to be used (start after gadm_no_land)
    index <- max(
      match(names(LandInG_setup$gadm$gadm_no_land), names(level0_index))
    )
    level1_index <- seq(level0_index[index] + 1, length.out = length(level1))
    rm(index)
    names(level1_index) <- names(level1)
  }
}
################################################################################


################################################################################
## Preparation of country code file                                           ##
## This is a matrix with two columns:                                         ##
## 1) country index                                                           ##
## 2) region index for countries in include_regions, otherwise same as 1)     ##
## A third column with the GridID is added for convenience.                   ##
## If no include_regions are defined the second column is discarded.          ##
if (!is.null(LandInG_setup$gadm$cowname) ||
    !is.null(LandInG_setup$gadm$cowmetaname) ||
    !is.null(LandInG_setup$gadm$cowraster) ||
    !is.null(LandInG_setup$gadm$regmetaname) ||
    !is.null(LandInG_setup$gadm$regraster)
) {
  cow_data <- array(
    dim = c(nrow(lpjgrid_cells), 3),
    dimnames = list(NULL, c("country", "region", "GridID"))
  )
  # GridID
  cow_data[, "GridID"] <- lpjgrid_cells[, "GridID"]
  # Country
  grid_index <- match(cow_data[, "GridID"], cow_cells[, "GridID"])
  cow_data[, "country"] <- level0_index[cow_cells[grid_index, "country"]]
  if (length(LandInG_setup$gadm$include_regions) > 0) {
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
  } else {
    cow_data <- cow_data[, -grep("region", colnames(cow_data))]
  }
  # Check for errors
  if (anyNA(cow_data)) {
    stop("cow_data could not be filled correctly")
  }
}
if (!is.null(LandInG_setup$gadm$cowmetaname) ||
    !is.null(LandInG_setup$gadm$cowraster) ||
    !is.null(LandInG_setup$gadm$regmetaname) ||
    !is.null(LandInG_setup$gadm$regraster)
) {
  # Meta information: data frame with names and corresponding codes
  cowmetatable <- data.frame(
    ISO = names(level0_ascii),
    country = level0_ascii[names(level0_index)],
    ID = level0_index,
    check.names = FALSE,
    fix.empty.names = FALSE
  )
  if (length(LandInG_setup$gadm$include_regions) > 0) {
    regmetatable <- data.frame(
      ISO = names(level1_ascii),
      region = level1_ascii[names(level1_index)],
      ID = level1_index,
      check.names = FALSE,
      fix.empty.names = FALSE
    )
  }
}
################################################################################


################################################################################
## Create country code file                                                   ##
## File name cowname defined in gadm_setup.R                                  ##
## Format cowformat defined in gadm_setup.R                                   ##
## Version for LPJmL input format bintype defined in gadm_setup.R             ##
## If the format is "BIN" this uses a number of helper functions for the      ##
## LPJmL file format from the lpjmlkit package.                               ##
if (!is.null(LandInG_setup$gadm$cowname)) {
  cat("Creating country code file", LandInG_setup$gadm$cowname, "\n")
  if (file.exists(LandInG_setup$gadm$cowname)) {
    if (LandInG_setup$gadm$cowformat == "BIN") {
      cowheader <- lpjmlkit::read_header(
        LandInG_setup$gadm$cowname,
        verbose = FALSE
      )
      if (
        any(
          cowheader$header[c("ncell", "nbands")] !=
            c(nrow(cow_data), ncol(cow_data) - 1)
        ) ||
          !isTRUE(
            all.equal(
              cowheader$header[c("cellsize_lon", "cellsize_lat")],
              LandInG_setup$gadm$lpj_res,
              check.attributes = FALSE,
              tolerance = LandInG_setup$single.eps
            )
          )
      ) {
        stop(
          LandInG_setup$gadm$cowname, " exists already and is inconsistent ",
          "with current script run. Delete file to create a new one."
        )
      }
      cow_data_tmp <- drop(
        lpjmlkit::read_io(LandInG_setup$gadm$cowname, silent = TRUE)$data
      )
      colnames(cow_data_tmp) <- grep(
        "GridID",
        colnames(cow_data),
        invert = TRUE,
        value = TRUE
      )
    } else if (LandInG_setup$gadm$cowformat == "CSV") {
      cow_data_tmp <- read.csv(LandInG_setup$gadm$cowname)
    } else {
      stop("Invalid cowformat ", sQuote(LandInG_setup$gadm$cowformat))
    }
    if (any(cow_data_tmp != cow_data[, -grep("GridID", colnames(cow_data))])) {
      stop(
        LandInG_setup$gadm$cowname, " exists already and is inconsistent with ",
        "current script run. Delete file to create a new one."
      )
    } else {
      cat(
        LandInG_setup$gadm$cowname,
        "exists already and is not created again.\n"
      )
      rm(cow_data_tmp)
    }
  } else {
    if (LandInG_setup$gadm$cowformat == "BIN") {
      scalar <- 1.0
      # Variable type depends on bintype (version of LPJmL input format)
      if (LandInG_setup$gadm$bintype < 3) {
        # Check if country/region indices can be expressed as short
        if (any(abs(cow_data[, -grep("GridID", colnames(cow_data))]) >= 2^15)) {
          stop(
            "Range of country/region codes [",
            toString(range(cow_data[, -grep("GridID", colnames(cow_data))])),
            "] cannot be saved as 2-byte integer.\n",
            "Set bintype to 3 to allow 4-byte integer"
          )
        }
        if (LandInG_setup$gadm$bintype < 2 &&
            LandInG_setup$gadm$lpj_res["lon"] != 0.5
        ) {
          stop("bintype 1 only supports resolution of 0.5 degree")
        }
      }
      if (any(abs(cow_data[, -grep("GridID", colnames(cow_data))]) >= 2^31)) {
        stop(
          "Range of country/region codes [",
          toString(range(cow_data[, -grep("GridID", colnames(cow_data))])),
          "] cannot be saved as 4-byte integer.\n",
          "Reduce number of countries/regions."
        )
      }
      # Create header of cow file
      cowheader <- lpjmlkit::create_header(
        name = LandInG_setup$gadm$cow_headername,
        version = LandInG_setup$gadm$bintype,
        nyear = 1,
        ncell = nrow(cow_data),
        nbands = ncol(cow_data) - 1,
        cellsize_lon = LandInG_setup$gadm$lpj_res["lon"],
        scalar = 1 / scalar,
        datatype = ifelse(
          max(cow_data[, -grep("GridID", colnames(cow_data))]) >= 2^15,
          2,
          1
        ),
        cellsize_lat = LandInG_setup$gadm$lpj_res["lat"]
      )
      # Write header to file
      lpjmlkit::write_header(LandInG_setup$gadm$cowname, cowheader)
      # Open file in binary appending mode to add data
      cowfile <- file(LandInG_setup$gadm$cowname, "ab")
      # Data type to be written depends on data type set in header
      # get_datatype() returns type and size of data to be written
      if (typeof(lpjmlkit::get_datatype(cowheader)$type) == "integer") {
        writeBin(
          as.integer(
            t(cow_data[, -grep("GridID", colnames(cow_data))]) /
              cowheader[["header"]]["scalar"]
          ),
          cowfile,
          size = lpjmlkit::get_datatype(cowheader)$size,
          endian = cowheader[["endian"]]
        )
      } else if (typeof(lpjmlkit::get_datatype(cowheader)$type) == "double") {
        writeBin(
          as.double(
            t(cow_data[, -grep("GridID", colnames(cow_data))]) /
              cowheader[["header"]]["scalar"]
          ),
          cowfile,
          size = lpjmlkit::get_datatype(cowheader)$size,
          endian = cowheader[["endian"]]
        )
      } else {
        stop(
          "Invalid datatype ", cowheader[["header"]]["datatype"],
          " in cowheader"
        )
      }
      close(cowfile)
    } else if (LandInG_setup$gadm$cowformat == "CSV") {
      write.csv(
        cow_data[, -grep("GridID", colnames(cow_data))],
        file = LandInG_setup$gadm$cowname,
        row.names = FALSE
      )
    } else {
      stop("Invalid cowformat ", sQuote(LandInG_setup$gadm$cowformat))
    }
  }
} else {
  message(
    "Country code data is not written to file because 'cowname' is not ",
    "defined in gadm_setup.R"
  )
}
# Also write out meta information
if (!is.null(LandInG_setup$gadm$cowmetaname)) {
  cat(
    "Meta information for country codes saved to",
    LandInG_setup$gadm$cowmetaname, "\n"
  )
  write.csv(
    cowmetatable,
    file = LandInG_setup$gadm$cowmetaname,
    fileEncoding = "UTF-8",
    row.names = FALSE
  )
}
if (!is.null(LandInG_setup$gadm$regmetaname) &&
    length(LandInG_setup$gadm$include_regions) > 0
) {
  cat(
    "Meta information for region codes saved to",
    LandInG_setup$gadm$regmetaname, "\n"
  )
  write.csv(
    regmetatable,
    file = LandInG_setup$gadm$regmetaname,
    fileEncoding = "UTF-8",
    row.names = FALSE
  )
}
# Also write raster version
if (!is.null(LandInG_setup$gadm$cowraster)) {
  cat("Writing raster of country codes to", LandInG_setup$gadm$cowraster, "\n")
  grid_index1 <- match(
    lpjgrid_cells[, "GridID"],
    terra::values(LandInG_setup$gadm$lpjgrid_raster)
  )
  grid_index2 <- match(lpjgrid_cells[, "GridID"], cow_data[, "GridID"])
  outputraster <- terra::rast(LandInG_setup$gadm$lpjgrid_raster)
  outputraster[grid_index1] <- cow_data[grid_index2, "country"]
  names(outputraster) <- "country"
  if (
    grepl(".nc[0-9]*$", LandInG_setup$gadm$cowraster, ignore.case = TRUE) &&
      "ncdf4" %in%  .packages(all.available = TRUE)
  ) {
    # NetCDF format
    terra::writeCDF(
      outputraster,
      filename = LandInG_setup$gadm$cowraster,
      missval = -99,
      prec = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^15,
        "short",
        "integer"
      ),
      varname = names(outputraster),
      unit = "",
      compression = 9,
      overwrite = TRUE
    )
    # Try to add variable containing country names
    # Open file for writing
    nc <- ncdf4::nc_open(LandInG_setup$gadm$cowraster, write = TRUE)
    # Generate required variable dimensions
    nchar_dim <- ncdf4::ncdim_def(
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
    cow_dim <- ncdf4::ncdim_def(
      "country_name",
      units = "",
      vals = seq_len(nrow(cowmetatable)),
      create_dimvar = FALSE
    )
    # Generate variable
    cow_var <- ncdf4::ncvar_def(
      "country_name",
      units = "",
      dim = list(nchar_dim, cow_dim),
      longname = "country name",
      prec = "char"
    )
    # Add variable to file
    nc <- ncdf4::ncvar_add(nc, cow_var)
    # Write values to file
    ncdf4::ncvar_put(
      nc,
      "country_name",
      paste(cowmetatable[, "ID"], cowmetatable[, "country"], sep = ": ")
    )
    ncdf4::nc_close(nc)
    rm(nchar_dim, cow_dim, cow_var, nc)
  } else {
    terra::writeRaster(
      outputraster,
      filename = LandInG_setup$gadm$cowraster,
      NAflag = -99,
      datatype = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^15,
        "INT2S",
        "INT4S"
      ),
      overwrite = TRUE
    )
  }
  rm(outputraster)
}
# Region code
if (!is.null(LandInG_setup$gadm$regraster) &&
    length(LandInG_setup$gadm$include_regions) > 0
) {
  cat("Writing raster of region codes to", LandInG_setup$gadm$regraster, "\n")
  grid_index1 <- match(
    lpjgrid_cells[, "GridID"],
    terra::values(LandInG_setup$gadm$lpjgrid_raster)
  )
  grid_index2 <- match(lpjgrid_cells[, "GridID"], cow_data[, "GridID"])
  outputraster <- terra::rast(LandInG_setup$gadm$lpjgrid_raster)
  outputraster[grid_index1] <- cow_data[grid_index2, "region"]
  names(outputraster) <- "region"
  if (
    grepl(".nc[0-9]*$", LandInG_setup$gadm$regraster, ignore.case = TRUE) &&
      "ncdf4" %in%  .packages(all.available = TRUE)
  ) {
    # NetCDF format
    terra::writeCDF(
      outputraster,
      filename = LandInG_setup$gadm$regraster,
      missval = -99,
      prec = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^15,
        "short",
        "integer"
      ),
      varname = names(outputraster),
      unit = "",
      compression = 9,
      overwrite = TRUE
    )
    # Try to add variable containing region names
    # Region names include countries not included in include_regions
    # Open file for writing
    nc <- ncdf4::nc_open(LandInG_setup$gadm$regraster, write = TRUE)
    # Generate required variable dimensions
    nchar_dim <- ncdf4::ncdim_def(
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
    reg_dim <- ncdf4::ncdim_def(
      "region_name",
      units = "",
      vals = seq_len(r),
      create_dimvar = FALSE
    )
    # Generate variable
    reg_var <- ncdf4::ncvar_def(
      "region_name",
      units = "",
      dim = list(nchar_dim, reg_dim),
      longname = "region name",
      prec = "char"
    )
    # Add variable to file
    nc <- ncdf4::ncvar_add(nc, reg_var)
    # Write values to file
    ncdf4::ncvar_put(
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
    ncdf4::nc_close(nc)
    rm(nchar_dim, reg_dim, reg_var, nc)
  } else {
    terra::writeRaster(
      outputraster,
      filename = LandInG_setup$gadm$regraster,
      NAflag = -99,
      datatype = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^15,
        "INT2S",
        "INT4S"
      ),
      overwrite = TRUE
    )
  }
  rm(outputraster)
}
################################################################################


################################################################################
## Determine the number of countries in each cell.                            ##
## dominant_country() above only returns the dominant country. The number of  ##
## countries per cell is used in the creation of landuse/fertilizer data to   ##
## detect border cells.                                                       ##
## Function number_country() defined in gadm_helper.R                         ##
## This call returns a list with the same length as cell_list but containing  ##
## the GridID and number of unique GID_0 codes.                               ##
if (!is.null(LandInG_setup$gadm$ncountryname) ||
    !is.null(LandInG_setup$gadm$ncountryraster)
) {
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
  if (LandInG_setup$gadm$force_grid) {
    # Coordinates from griddata have been assigned GridIDs from 1 to
    # nrow(griddata), check if any missing
    if (
      any(!seq_len(nrow(LandInG_setup$gadm$griddata)) %in%
            ncountry_cells[, "GridID"])
    ) {
      warning(
        length(
          setdiff(
            seq_len(nrow(LandInG_setup$gadm$griddata)),
            ncountry_cells[, "GridID"]
          )
        ),
        " cells in your predefined grid are not covered by GADM. ",
        "Assigning ncountry of 0.",
        call. = FALSE,
        immediate. = TRUE
      )
      ncountry_cells <- rbind(
        ncountry_cells,
        cbind(
          setdiff(
            seq_len(nrow(LandInG_setup$gadm$griddata)),
            ncountry_cells[, "GridID"]
          ),
          0
        )
      )
    }
  }
  grid_index <- match(lpjgrid_cells[, "GridID"], ncountry_cells[, "GridID"])
  ncountry_data <- ncountry_cells[grid_index, "ncountry"]
  # Check for errors
  if (anyNA(ncountry_data)) {
    stop("ncountry_data could not be filled correctly")
  }
  rm(ncountry_list, ncountry_cells, grid_index)
}
################################################################################


################################################################################
## Create file containing number of countries in each cell.                   ##
## File name ncountryname and format ncountryformat defined in gadm_setup.R   ##
if (!is.null(LandInG_setup$gadm$ncountryname)) {
  cat(
    "Writing file containing number of countries in each cell to",
    LandInG_setup$gadm$ncountryname, "\n"
  )
  if (file.exists(LandInG_setup$gadm$ncountryname)) {
    if (LandInG_setup$gadm$ncountryformat == "BIN") {
      ncountryheader <- lpjmlkit::read_header(
        LandInG_setup$gadm$ncountryname,
        verbose = FALSE
      )
      if (
        ncountryheader$header["ncell"] != length(ncountry_data) ||
          !isTRUE(
            all.equal(
              ncountryheader$header[c("cellsize_lon", "cellsize_lat")],
              LandInG_setup$gadm$lpj_res,
              check.attributes = FALSE,
              tolerance = LandInG_setup$single.eps
            )
          )
      ) {
        stop(
          LandInG_setup$gadm$ncountryname, " exists already and is ",
          "inconsistent with current script run. ",
          "Delete file to create a new one."
        )
      }
      ncountry_data_tmp <- matrix(
        drop(
          lpjmlkit::read_io(LandInG_setup$gadm$ncountryname, silent = TRUE)$data
        ),
        ncol = 1, dimnames = list(NULL, "ncountry")
      )
    } else if (LandInG_setup$gadm$ncountryformat == "CSV") {
      ncountry_data_tmp <- read.csv(LandInG_setup$gadm$ncountryname)
    } else {
      stop(
        "Unsupported ncountryformat ",
        sQuote(LandInG_setup$gadm$ncountryformat)
      )
    }
    if (any(ncountry_data_tmp[, "ncountry"] != ncountry_data)) {
      stop(
        LandInG_setup$gadm$ncountryname, " exists already and is ",
        "inconsistent with current script run. ",
        "Delete file to create a new one."
      )
    } else {
      cat(
        LandInG_setup$gadm$ncountryname,
        "exists already and is not created again.\n"
      )
    }
  } else {
    if (LandInG_setup$gadm$ncountryformat == "BIN") {
      scalar <- 1.0
      # Variable type depends on bintype (version of LPJmL input format)
      if (LandInG_setup$gadm$bintype < 3) {
        # Check if number of countries can be expressed as short
        if (any(abs(ncountry_data) >= 2^15)) {
          stop(
            "Maximum number of countries per cell ", max(ncountry_data),
            " exceeds value range of 2-byte integer.\n",
            "Set bintype to 3 to allow 4-byte integer."
          )
        }
        if (LandInG_setup$gadm$bintype < 2 &&
            LandInG_setup$gadm$lpj_res["lon"] != 0.5
        ) {
          stop("bintype 1 only supports resolution of 0.5 degree.")
        }
      }
      if (any(ncountry_data >= 2^31)) {
        stop(
          "Maximum number of countries per cell ", max(ncountry_data),
          " cannot be saved as 4-byte integer."
        )
      }
      # Create header of file
      ncountryheader <- lpjmlkit::create_header(
        name = LandInG_setup$gadm$ncountry_headername,
        version = LandInG_setup$gadm$bintype,
        nyear = 1,
        ncell = length(ncountry_data),
        nbands = 1,
        cellsize_lon = LandInG_setup$gadm$lpj_res["lon"],
        scalar = 1 / scalar,
        datatype = ifelse(max(ncountry_data) >= 2^15, 2, 1),
        cellsize_lat = LandInG_setup$gadm$lpj_res["lat"]
      )
      # Write header to file
      lpjmlkit::write_header(LandInG_setup$gadm$ncountryname, ncountryheader)
      # Open file in binary appending mode to add data
      ncountryfile <- file(LandInG_setup$gadm$ncountryname, "ab")
      # Data type to be written depends on data type set in header
      # get_datatype() returns type and size of data to be written
      if (typeof(lpjmlkit::get_datatype(ncountryheader)$type) == "integer") {
        writeBin(
          as.integer(ncountry_data / ncountryheader[["header"]]["scalar"]),
          ncountryfile,
          size = lpjmlkit::get_datatype(ncountryheader)$size,
          endian = ncountryheader[["endian"]]
        )
      } else if (
        typeof(lpjmlkit::get_datatype(ncountryheader)$type) == "double"
      ) {
        writeBin(
          as.double(ncountry_data / ncountryheader[["header"]]["scalar"]),
          ncountryfile,
          size = lpjmlkit::get_datatype(ncountryheader)$size,
          endian = ncountryheader[["endian"]]
        )
      } else {
        stop(
          "Invalid datatype ", ncountryheader[["header"]]["datatype"],
          " in ncountryheader."
        )
      }
      close(ncountryfile)
    } else if (LandInG_setup$gadm$ncountryformat == "CSV") {
      write.csv(
        matrix(ncountry_data, ncol = 1, dimnames = list(NULL, "ncountry")),
        file = LandInG_setup$gadm$ncountryname,
        row.names = FALSE
      )
    } else {
      stop("Invalid ncountryformat ", sQuote(LandInG_setup$gadm$ncountryformat))
    }
  }
} else {
  message(
    "Number of countries per cell is not written to file because ",
    "'ncountryname' is not defined in gadm_setup.R"
  )
}
if (!is.null(LandInG_setup$gadm$ncountryraster)) {
  cat(
    "Writing raster of number of countries in cell to",
    LandInG_setup$gadm$ncountryraster, "\n"
  )
  grid_index <- match(
    lpjgrid_cells[, "GridID"],
    terra::values(LandInG_setup$gadm$lpjgrid_raster)
  )
  outputraster <- terra::rast(LandInG_setup$gadm$lpjgrid_raster)
  outputraster[grid_index] <- ncountry_data
  names(outputraster) <- "ncountry"
  if (
    grepl(".nc[0-9]*$", LandInG_setup$gadm$ncountryraster, ignore.case = TRUE)
  ) {
    terra::writeCDF(
      outputraster,
      filename = LandInG_setup$gadm$ncountryraster,
      varname = names(outputraster),
      compression = 9,
      overwrite = TRUE
    )
  } else {
    terra::writeRaster(
      outputraster,
      filename = LandInG_setup$gadm$ncountryraster,
      NAflag = -99,
      datatype = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^7,
        "INT1S",
        ifelse(
          max(terra::values(outputraster), na.rm = TRUE) < 2^15,
          "INT2S",
          "INT4S"
        )
      ),
      overwrite = TRUE
    )
  }
  rm(outputraster, grid_index)
}
################################################################################


################################################################################
## Determine GADM level 0-2 hierarchy in each cell.                           ##
## Check for missing data first.                                              ##
if (!is.null(LandInG_setup$gadm$gadmname) ||
    !is.null(LandInG_setup$gadm$gadmmetaname) ||
    !is.null(LandInG_setup$gadm$gadmraster)
) {
  cat("Determining dominant GADM level 0-2 in each cell.\n")
  # Check consistency check between cell_list and griddata.
  grid_index <- terra::cellFromXY(
    LandInG_setup$gadm$lpjgrid_raster,
    LandInG_setup$gadm$griddata
  )
  if (
    any(
      sapply(
        cell_list[unlist(LandInG_setup$gadm$lpjgrid_raster[grid_index])],
        is.null
      )
    )
  ) {
    mindex <- which(
      sapply(
        cell_list[unlist(LandInG_setup$gadm$lpjgrid_raster[grid_index])],
        is.null
      )
    )
    if (exists("cow_data")) {
      # Check which country code missing cells from cell_list have in cow_data
      missing_cow <- cow_data[mindex, "country"]
      if (exists("cowmetatable")) {
        # Check that missing cells have "No land" code
        cindex <- match(missing_cow, cowmetatable[, "ID"])
        mismatch <- which(
          !cowmetatable[cindex, "ISO"] %in%
            names(LandInG_setup$gadm$gadm_no_land)
        )
        if (length(mismatch) > 0) {
          # GADM country processing above should have assigned gadm_no_land to
          # cells from grid that are outside of GADM land.
          stop(
            length(mismatch),
            " cells in ", LandInG_setup$gadm$gridname,
            " have no GADM information in cell_list.",
            "\nThey are not associated to gadm_no_land in ",
            LandInG_setup$gadm$cowname, " either."
          )
        }
      }
    }
    # Set missing cells to gadm_no_land
    # Add gadm_no_land to country_list
    missing_cow <- setdiff(
      names(LandInG_setup$gadm$gadm_no_land),
      names(country_list)
    )
    if (length(missing_cow) > 0) {
      country_list[missing_cow] <- LandInG_setup$gadm$gadm_no_land[missing_cow]
    }
    # Add dummy region to region_list
    reg <- paste0(names(LandInG_setup$gadm$gadm_no_land), ".1_1")
    reg_names <- paste(LandInG_setup$gadm$gadm_no_land, "(dummy region)")
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
    ISO_list <- unique(c(ISO_list, names(LandInG_setup$gadm$gadm_no_land)))
    if (LandInG_setup$gadm$force_grid) {
      # May have missing cells because grid is predefined
      warning(
        "Assigning gadm_no_land ", names(LandInG_setup$gadm$gadm_no_land),
        " (", sQuote(LandInG_setup$gadm$gadm_no_land), ") to ", length(mindex),
        " cells included in grid that have no land according to GADM.\n",
        "Please check created GADM level 0-2 files manually whether ",
        "these cells can be assigned to an adjacent GADM unit.",
        call. = FALSE,
        immediate. = TRUE
      )
    } else {
      # Should not have any cells outside GADM cover
      stop(
        length(mindex), " cells in ", LandInG_setup$gadm$gridname,
        " have no land according to GADM level 0-2 data. ",
        "This should not happen."
      )
    }
    for (c in mindex) {
      # Add dummy entry for No land with 0 area
      cell_list[[c]] <- c(
        GridID = c,
        Gridarea = lpjmlkit::calc_cellarea(
          LandInG_setup$gadm$griddata[c, "lat"],
          LandInG_setup$gadm$lpj_res["lon"],
          LandInG_setup$gadm$lpj_res["lat"],
          earth_radius = LandInG_setup$earthradius,
          return_unit = "m2"
        ),
        Landarea = 0,
        0, 0, 0
      )
      # Name country, region and district
      nindex <- seq(to = length(cell_list[[c]]), length.out = 3)
      names(cell_list[[c]])[nindex] <-
        c(names(LandInG_setup$gadm$gadm_no_land), reg, dis)
    }
  }
  # Check if GADM includes cells that are not used if a pre-defined grid is
  # used.
  if (LandInG_setup$gadm$force_grid) {
    unused <- seq(nrow(LandInG_setup$gadm$griddata) + 1, length(cell_list))
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
}
################################################################################

################################################################################
## Assign dominant GADM levels 0-2 to each cell.                              ##
if (!is.null(LandInG_setup$gadm$gadmname) ||
    !is.null(LandInG_setup$gadm$gadmmetaname) ||
    !is.null(LandInG_setup$gadm$gadmraster)
) {
  leveldata <- lapply(
    cell_list[unlist(LandInG_setup$gadm$lpjgrid_raster[grid_index])],
    dominant_levels,
    ISO_list = ISO_list,
    region_list = region_list,
    water_bodies = LandInG_setup$gadm$water_bodies
  )
  # Remove empty entries and combine in data.frame
  level_df <- data.frame(
    matrix(
      unlist(leveldata[which(!sapply(leveldata, is.null))]),
      ncol = 4,
      byrow = TRUE,
      dimnames = list(NULL, c("GridID", "country", "region", "district"))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fix.empty.names = FALSE
  )
  rm(leveldata)
  level_df$GridID <- as.integer(level_df$GridID)
  # Replace country/region/district codes by index number
  # Sort lists first, code code alphabetically, region and district codes
  # numerically.
  for (llist in c("country", "region", "district")) {
    pattern <- switch(
      llist,
      country = "([A-Z]+)",
      region = "([A-Z]+)\\.([0-9]+)",
      district = "([A-Z]+)\\.([0-9]+)\\.([0-9]+)"
    )
    # Split code string into components
    in_c <- data.frame(
      matrix(
        unlist(
          regmatches(
            names(get(paste0(llist, "_list"))),
            gregexec(pattern, names(get(paste0(llist, "_list"))))
          ),
          use.names = FALSE
        ),
        ncol = switch(llist, country = 2, region = 3, district = 4),
        byrow = TRUE
      ),
      check.names = FALSE,
      fix.empty.names = FALSE
    )
    if (nrow(in_c) != length(get(paste0(llist, "_list")))) {
      stop("Error splitting ", llist, "_list into components")
    }
    for (col in seq_len(ncol(in_c))) {
      suppressWarnings(num <- as.integer(in_c[[col]]))
      if (!anyNA(num)) {
        in_c[[col]] <- num
      }
    }
    # Reorder lists
    sortorder <- switch(
      llist,
      country = order(in_c[, 2]),
      region = order(in_c[, 2], in_c[, 3]),
      district = order(in_c[, 2], in_c[, 3], in_c[, 4])
    )
    assign(paste0(llist, "_list"), get(paste0(llist, "_list"))[sortorder])
  }
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
          toString(sQuote(level_df[which(is.na(level_index[, c])), c])), "\n"
        )
      }
    }
    stop("Code assignment")
  }
  if (exists("cow_data")) {
    # Consistency check between countries in level_index and countries in
    # cowdata
    if (exists("cowmetatable")) {
      # Check that assigned countries match
      cindex <- match(cow_data[, "country"], cowmetatable[, "ID"])
      if (any(level_df$country != cowmetatable[cindex, "ISO"])) {
        stop(
          "There is a difference in assigned country between cell_list and ",
          LandInG_setup$gadm$cowname, " in ",
          length(which(level_df$country != cowmetatable[cindex, "ISO"])),
          " cells.\n",
          "Make sure you used the same GADM version for both."
        )
      }
    } else {
      # Check that the number of unique countries assigned is the same
      if (length(unique(level_index[, "country"])) !=
            length(unique(cow_data[, "country"]))) {
        stop(
          "Number of unique countries in level_index differs from ",
          "country file ", LandInG_setup$gadm$cowname,
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
    district = as.character(district_list),
    check.names = FALSE,
    fix.empty.names = FALSE
  )
  # To avoid problems with special characters if not running in a UTF8
  # environment convert names to ASCII characters only.
  for (c in c("country", "region", "district")) {
    gadmmetatable[, c] <- stringi::stri_trans_general(
      gadmmetatable[, c],
      "latin-ascii"
    )
  }
  # Check if all admin units from GADM have been assigned to cells.
  # Note: some units may be too small for a single cell
  for (l in c("country", "region", "district")) {
    llist <- setdiff(
      names(get(paste0(l, "_list"))),
      LandInG_setup$gadm$skip_countries
    )
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
  rm(level_df)
  # Write GADM level 0-2 to file.
  if (!is.null(LandInG_setup$gadm$gadmname)) {
    cat("Creating GADM level 0-2 file", LandInG_setup$gadm$gadmname, "\n")
    if (file.exists(LandInG_setup$gadm$gadmname)) {
      if (LandInG_setup$gadm$gadmformat == "BIN") {
        gadmheader <- lpjmlkit::read_header(
          LandInG_setup$gadm$gadmname,
          verbose = FALSE
        )
        if (
          any(
            gadmheader$header[c("ncell", "nbands")] != c(nrow(level_index), 3)
          ) ||
            !isTRUE(
              all.equal(
                gadmheader$header[c("cellsize_lon", "cellsize_lat")],
                LandInG_setup$gadm$lpj_res,
                check.attributes = FALSE,
                tolerance = LandInG_setup$single.eps
              )
            )
        ) {
          stop(
            LandInG_setup$gadm$gadmname, " exists already and is ",
            "inconsistent with current script run. ",
            "Delete file to create a new one."
          )
        }
        gadm_data_tmp <- matrix(
          drop(
            lpjmlkit::read_io(LandInG_setup$gadm$gadmname, silent = TRUE)$data
          ),
          ncol = 3, dimnames = list(NULL, c("country", "region", "district"))
        )
      } else if (LandInG_setup$gadm$gadmformat == "CSV") {
        gadm_data_tmp <- read.csv(LandInG_setup$gadm$gadmname)
      } else {
        stop(
          "Unsupported gadmformat ",
          sQuote(LandInG_setup$gadm$gadmformat)
        )
      }
      if (
        any(gadm_data_tmp != level_index[, c("country", "region", "district")])
      ) {
        stop(
          LandInG_setup$gadm$gadmname, " exists already and is ",
          "inconsistent with current script run. ",
          "Delete file to create a new one."
        )
      }
    } else {
      if (LandInG_setup$gadm$gadmformat == "BIN") {
        scalar <- 1.0
        # Variable type depends on bintype (version of LPJmL input format).
        if (LandInG_setup$gadm$bintype < 3) {
          # Check if country/region/district indices can be expressed as short.
          if (
            any(abs(level_index[, c("country", "region", "district")]) >= 2^15)
          ) {
            stop(
              "Range of country/region/district codes [",
              toString(
                range(level_index[, c("country", "region", "district")])
              ),
              "] cannot be saved as 2-byte integer.\n",
              "Set bintype to 3 to allow 4-byte integer"
            )
          }
          if (LandInG_setup$gadm$bintype < 2 &&
              LandInG_setup$gadm$lpj_res["lon"] != 0.5
          ) {
            stop("bintype 1 only supports resolution of 0.5 degree.")
          }
        }
        if (
          any(abs(level_index[, c("country", "region", "district")]) >= 2^31)
        ) {
          stop(
            "Range of country/region/district codes [",
            toString(range(level_index[, c("country", "region", "district")])),
            "] cannot be saved as 4-byte integer.\n",
            "Reduce number of countries/regions/districts."
          )
        }
        # Create header of GADM level file
        gadmheader <- lpjmlkit::create_header(
          name = LandInG_setup$gadm$gadm_headername,
          version = LandInG_setup$gadm$bintype,
          nyear = 1,
          ncell = nrow(level_index),
          nbands = 3,
          cellsize_lon = LandInG_setup$gadm$lpj_res["lon"],
          scalar = 1 / scalar,
          datatype = ifelse(
            max(level_index[, c("country", "region", "district")]) >= 2^15,
            2,
            1
          ),
          cellsize_lat = LandInG_setup$gadm$lpj_res["lat"]
        )
        # Write header to file
        lpjmlkit::write_header(LandInG_setup$gadm$gadmname, gadmheader)
        # Open file in binary appending mode to add data
        gadmfile <- file(LandInG_setup$gadm$gadmname, "ab")
        # Data type to be written depends on data type set in header
        # get_datatype() return type and size of data to be written
        if (typeof(lpjmlkit::get_datatype(gadmheader)$type) == "integer") {
          writeBin(
            as.integer(
              t(level_index[, c("country", "region", "district")]) /
                gadmheader[["header"]]["scalar"]
            ),
            gadmfile,
            size = lpjmlkit::get_datatype(gadmheader)$size,
            endian = gadmheader[["endian"]]
          )
        } else if (
          typeof(lpjmlkit::get_datatype(gadmheader)$type) == "double"
        ) {
          writeBin(
            as.double(
              t(level_index[, c("country", "region", "district")]) /
                gadmheader[["header"]]["scalar"]
            ),
            gadmfile,
            size = lpjmlkit::get_datatype(gadmheader)$size,
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
      } else if (LandInG_setup$gadm$gadmformat == "CSV") {
        write.csv(
          level_index[, c("country", "region", "district")],
          file = LandInG_setup$gadm$gadmname,
          row.names = FALSE
        )
      } else {
        stop("Invalid gadmformat ", sQuote(LandInG_setup$gadm$gadmformat))
      }
    }
  }
  # Write meta information to file.
  if (!is.null(LandInG_setup$gadm$gadmmetaname)) {
    cat(
      "Meta information for GADM level 0-2 codes saved to",
      LandInG_setup$gadm$gadmmetaname, "\n"
    )
    write.csv(
      gadmmetatable,
      file = LandInG_setup$gadm$gadmmetaname,
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
  }
}
################################################################################


################################################################################
## Create multi-layer raster of GADM level 0-2.                               ##
## The raster has three layers:                                               ##
## 1) country index                                                           ##
## 2) region/state index                                                      ##
## 3) district/county index                                                   ##
## Make sure that gadmraster is set to a format that supports multiband       ##
## raster objects.                                                            ##
## If saved to a NetCDF file, names of GADM units are saved as well.          ##
if (!is.null(LandInG_setup$gadm$gadmraster)) {
  cat(
    "Writing raster of GADM level 0-2 codes to",
    LandInG_setup$gadm$gadmraster, "\n"
  )
  outputraster <- terra::rast(LandInG_setup$gadm$lpjgrid_raster, nlyrs = 3)
  names(outputraster) <- c("country", "region", "district")
  grid_index <- match(
    level_index[, "GridID"],
    terra::values(LandInG_setup$gadm$lpjgrid_raster)
  )
  for (l in names(outputraster)) {
    terra::set.values(
      outputraster,
      cells = grid_index,
      values = level_index[, l],
      layer = match(l, names(outputraster))
    )
  }
  if (
    grepl(".nc[0-9]*$", LandInG_setup$gadm$gadmraster, ignore.case = TRUE)
  ) {
    # NetCDF format
    terra::writeCDF(
      outputraster,
      filename = LandInG_setup$gadm$gadmraster,
      varname = "gadm",
      zname = "level",
      missval = -99,
      prec = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^15,
        "short",
        "integer"
      ),
      compression = 9,
      overwrite = TRUE
    )
    # Try to add variables containing GADM names
    if ("ncdf4" %in%  .packages(all.available = TRUE)) {
      nc <- ncdf4::nc_open(LandInG_setup$gadm$gadmraster, write = TRUE)
      for (l in names(outputraster)) {
        c <- switch(l,
          country = "level0_ID",
          region = "level1_ID",
          district = "level2_ID",
          stop("Invalid level ", l)
        )
        nchar_dim <- ncdf4::ncdim_def(
          paste0("nchar_", l),
          units = "",
          vals = seq_len(
            max(
              nchar(
                paste(
                  unlist(gadmmetatable[, c]),
                  unlist(gadmmetatable[, names(outputraster)]),
                  sep = ": "
                )
              )
            )
          ),
          create_dimvar = FALSE
        )
        nvar_dim <- ncdf4::ncdim_def(
          paste0("n", l),
          units = "",
          vals = seq_along(which(!is.na(gadmmetatable[, l]))),
          create_dimvar = FALSE
        )
        names_var <- ncdf4::ncvar_def(
          paste(l, "name", sep = "_"),
          units = "",
          dim = list(nchar_dim, nvar_dim),
          longname = paste(l, "name"),
          prec = "char",
          compression = 9
        )
        nc <- ncdf4::ncvar_add(nc, names_var)
        ncdf4::ncvar_put(
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
      ncdf4::nc_close(nc)
    }
  } else {
    # Try generic writeRaster(). Note that provided file format must support
    # multiple bands.
    terra::writeRaster(
      outputraster,
      filename = LandInG_setup$gadm$gadmraster,
      names = names(outputraster),
      NAflag = -99,
      datatype = ifelse(
        max(terra::values(outputraster), na.rm = TRUE) < 2^15,
        "INT2S",
        "INT4S"
      ),
      overwrite = TRUE
    )
  }
}
################################################################################

# Time execution
cat("Script run took", proc.time()["elapsed"] - process_start, "seconds\n")
