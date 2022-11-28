################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This script defines a number of utility functions used by other scripts in ##
## this directory.                                                            ##
## Make sure to source gadm_setup.R before this script.                       ##
################################################################################

################################################################################
## Function to load GADM levels into memory                                   ##
## Download and decompress data first.                                        ##
## Preset file names assume default for GADM version 3.6.                     ##
##                                                                            ##
## Parameters:                                                                ##
##   - gadm_dir: directory containing data                                    ##
##   - levels: GADM levels to load (0 = country, 1 = state/region,            ##
##     2 = country/district)                                                  ##
##   - format: data format to use (GeoPackage or ESRI Shapefile are           ##
##     implemented; gadm_format is set in gadm_setup.R)                       ##
################################################################################
load_gadm <- function(gadm_dir, levels = c(0, 1), format = gadm_format) {
  if (!format %in% st_drivers()$name) {
    stop(
      "Format ", sQuote(format),
      " not supported by your installation in load_gadm()"
    )
  }
  if (any(! levels %in% c(0, 1, 2))) {
    stop(
      "Level(s) ", toString(setdiff(levels, c(0, 1, 2))),
      " not supported in load_gadm()."
    )
  }
  if (format == "GPKG") {
    filename <- file.path(
      ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
      "gadm36_levels.gpkg"
    )
    if (0 %in% levels) {
      assign(
        "gadm_countries",
        st_read(filename, layer = "level0"),
        envir = .GlobalEnv
      )
    }
    if (1 %in% levels) {
      assign(
        "gadm_regions",
        st_read(filename, layer = "level1"),
        envir = .GlobalEnv
      )
    }
    if (2 %in% levels) {
      assign(
        "gadm_districts",
        st_read(filename, layer = "level2"),
        envir = .GlobalEnv
      )
    }
  } else if (format == "ESRI Shapefile") {
    if (0 %in% levels) {
      filename <- file.path(
        ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
        "gadm36_0.shp"
      )
      assign("gadm_countries", st_read(filename), envir = .GlobalEnv)
    }
    if (1 %in% levels) {
      filename <- file.path(
        ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
        "gadm36_1.shp"
      )
      assign("gadm_regions", st_read(filename), envir = .GlobalEnv)
    }
    if (2 %in% levels) {
      filename <- file.path(
        ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
        "gadm36_2.shp"
      )
      assign("gadm_districts", st_read(filename), envir = .GlobalEnv)
    }
  } else {
    stop("Format ",  sQuote(format), " not enabled in load_gadm()")
  }
}

################################################################################
## Function to count the number of countries in a cell                        ##
## Parameter indata is usually one element of a cell_list created by scripts  ##
## for step 3 and 6 of processing.                                            ##
## Parameter ISO_list is a character vector of all 3-letter ISO country codes.##
################################################################################
number_country <- function(indata, ISO_list) {
  if (is.null(indata)) {
    # Elements in cell_list can be empty
    return(NULL)
  }
  if (!"GridID" %in% names(indata)) {
    warning("Data needs to have a 'GridID' column")
    indata <- c(indata, GridID = NA)
  }
  data.frame(
    GridID = indata["GridID"],
    ncountry = length(intersect(ISO_list, names(indata)))
  )
}


################################################################################
## Function to calculate land fraction in each cell                           ##
## Parameter indata is usually one element of a cell_list created by scripts  ##
## for step 3 and 6 of processing.                                            ##
################################################################################
landfrac <- function(indata) {
  if (is.null(indata)) {
    # Elements in cell_list can be empty
    return(NULL)
  }
  if (!"Landarea" %in% names(indata) || !"Gridarea" %in% names(indata) ||
    !"GridID" %in% names(indata)) {
    warning(
      "Data needs to have columns named 'GridID', 'Gridarea', and 'Landarea'"
    )
    return(0)
  }
  cbind(
    GridID = indata["GridID"],
    Landfraction = indata["Landarea"] / indata["Gridarea"]
  )
}

################################################################################
## Function to determine the dominant country in each cell (country that      ##
## occupies the most space).                                                  ##
## Parameter indata is usually one element of a cell_list created by scripts  ##
## for step 3 and 6 of processing.                                            ##
## Parameter ISO_list is a character vector of all 3-letter ISO country codes.##
## Parameter water_bodies is a character vector of 3-letter ISO country codes ##
## that are only assigned as dominant country if no other country is present. ##
################################################################################
dominant_country <- function(indata, ISO_list, water_bodies) {
  if (is.null(indata)) {
    # Elements in cell_list can be empty
    return(NULL)
  }
  if (!"GridID" %in% names(indata)) {
    warning("Data needs to have a 'GridID' column")
    indata <- c(indata, GridID = NA)
  }
  if (length(intersect(ISO_list, names(indata))) < 1) {
    warning("Data does not contain valid country codes from ISO_list")
    return(
      data.frame(
        GridID = indata["GridID"],
        country = NA,
        stringsAsFactors = FALSE
      )
    )
  }
  # Country with largest area in cell
  # First try without water bodies like Caspian Sea
  clist <- setdiff(ISO_list, water_bodies)
  if (length(intersect(names(indata), clist)) > 0) {
    # Cell has ISO codes besides water bodies like Caspian Sea, use these
    country <- names(which.max(indata[which(names(indata) %in% clist)])
    )
  } else {
    # Cell has only water bodies
    country <- names(which.max(indata[which(names(indata) %in% ISO_list)]))
  }
  data.frame(
    GridID = indata["GridID"],
    country = country,
    stringsAsFactors = FALSE
  )
}


################################################################################
## Function to determine dominant state/region in each cell                   ##
## The function first determines the dominant country, then the largest       ##
## region/state within the dominant country. This means the result is not     ##
## always the region/state with the largest area overall in the cell.         ##
## Regions are only derived for countries included in include_regions.        ##
## Use function dominant_levels() below to get regions for all countries.     ##
################################################################################
dominant_region <- function(indata, region_list, ISO_list, water_bodies) {
  if (is.null(indata)) {
    # Elements in cell_list can be empty
    return(NULL)
  }
  if (!"GridID" %in% names(indata)) {
    warning("Data needs to have a 'GridID' column")
    indata <- c(indata, GridID = NA)
  }
  # Check if there are any region/state codes in indata
  if (length(intersect(names(region_list), names(indata))) > 0) {
    # Determine dominant country
    country <- dominant_country(indata, ISO_list, water_bodies)$country
    if (country %in% include_regions) {
      # Filter only regions belonging to dominant country (and not country
      # itself)
      reg <- which(grepl(country, names(indata)) & names(indata) != country)
      regiondata <- indata[reg]
      return(
        data.frame(
          GridID = indata["GridID"],
          region = names(which.max(regiondata)),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  # Return NULL if no region/state assigned
  return(NULL)
}

################################################################################
## Function to determine level 0 - 2 association from cell_list               ##
## This function first determines the biggest country, then determines the    ##
## biggest region belonging to the country determined in step 1, then         ##
## determines the biggest district belonging to the region determined in      ##
## step 2. This means the returned district is not always the district with   ##
## the biggest area overall in the cell. Instead, administrative hierarchies  ##
## are preserved.                                                             ##
################################################################################
dominant_levels <- function(indata, region_list, ISO_list, water_bodies) {
  if (is.null(indata)) {
    # Elements in cell_list can be empty
    return(NULL)
  }
  if (!"GridID" %in% names(indata)) {
    warning("Data needs to have a 'GridID' column")
    indata <- c(indata, GridID = NA)
  }
  # Determine dominant country
  level0 <- dominant_country(indata, ISO_list, water_bodies)
  if (!is.null(level0)) {
    country <- level0$country
    if (!is.na(country)) {
      # Find regions in indata that belong to the country determined in
      # first step
      reg <- which(names(indata) %in% names(region_list) &
        grepl(country, names(indata)))
      regiondata <- indata[reg]
      if (length(regiondata) > 0) {
        level1 <- data.frame(
          level0,
          region = names(which.max(regiondata)),
          stringsAsFactors = FALSE
        )
      } else {
        level1 <- data.frame(level0, region = NA, stringsAsFactors = FALSE)
      }
    } else {
      level1 <- data.frame(level0, region = NA, stringsAsFactors = FALSE)
    }
    region <- level1$region
    if (!is.na(region)) {
      # Find districts in indata that belong to region determined in previous
      # step
      dis <- grep(
        paste0(
          regmatches(region, gregexpr("([A-Z]{3}).([0-9]+)", region))[[1]],
          "."
        ),
        names(indata),
        fixed = TRUE
      )
      districtdata <- indata[dis]
      if (length(districtdata) > 0) {
        level2 <- data.frame(
          level1,
          district = names(which.max(districtdata)),
          stringsAsFactors = FALSE
        )
      } else {
        level2 <- data.frame(level1, district = NA, stringsAsFactors = FALSE)
      }
    } else {
      level2 <- data.frame(level1, district = NA, stringsAsFactors = FALSE)
    }
    return(level2)
  } else {
    return(NULL)
  }
}
