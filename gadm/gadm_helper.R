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
##   - version: GADM version to load                                          ##
##   - levels: GADM levels to load (0 = country, 1 = state/region,            ##
##     2 = country/district)                                                  ##
##   - format: data format to use (GeoPackage or ESRI Shapefile are           ##
##     implemented                                                            ##
##   - return_env: environment where data are attached to                     ##
##   - filename: provide filename manually for non-supported version          ##
################################################################################
load_gadm <- function(
  gadm_dir,
  version,
  levels,
  format,
  return_env,
  filename = NULL
) {
  if (!format %in% sf::st_drivers()$name) {
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
  stopifnot(version %in% c("2.8", "3.6", "4.0", "4.1") || !is.null(filename))
  # Note: code below technically supports version 4.0 but version available on
  # server currently does not include level 0. Therefore, trying to load level 0
  # for version 4.0 will fail unless the version is updated on the server.
  stopifnot(format %in% c("ESRI Shapefile", "GPKG") || !is.null(filename))
  if (is.null(filename)) {
    filename <- switch(
      format,
      `GPKG` = switch(
        version,
        `3.6` = "gadm36_levels.gpkg",
        `4.0` = "gadm404-levels.gpkg",
        `4.1` = "gadm_410-levels.gpkg",
        stop(
          "Automatic file name not supported for version ",
          version, " for format ", format
        )
      ),
      `ESRI Shapefile` = switch(
        version,
        `2.8` = paste0("gadm28_adm", levels, ".shp"),
        `3.6` = paste0("gadm36_", levels, ".shp"),
        stop(
          "Automatic file name not supported for version ",
          version, " for format ", format
        )
      ),
      stop(
        "Automatic file name not supported for format ", format,
        "\nSpecify 'filename' argument."
      )
    )
    filename <- file.path(
      ifelse(nchar(gadm_dir) > 0 & gadm_dir != ".", gadm_dir, getwd()),
      filename
    )
    if (any(!file.exists(filename))) {
      # Attempt to download data
      download_gadm(version, gadm_dir, format)
    }
  } else if (length(filename) != length(levels) && format != "GPKG") {
    stop(
      "Extraction of several levels from a single file only supported for",
      " GPKG format"
    )
  }
  if (length(levels) > 1 && length(filename) == 1) {
    filename <- rep(filename, length(levels))
  }
  stopifnot(length(filename) == length(levels))

  if (format == "GPKG") {
    if (0 %in% levels) {
      return_env$gadm$gadm_countries <- sf::st_read(
        filename[match(0, levels)],
        layer = switch(
          version,
          `3.6` = "level0",
          `4.0` = "level0",
          `4.1` = "ADM_0",
          "level0" # fallback assumption for unsupported versions
        )
      )
    }
    if (1 %in% levels) {
      return_env$gadm$gadm_regions <- sf::st_read(
        filename[match(1, levels)],
        layer = switch(
          version,
          `3.6` = "level1",
          `4.0` = "level1",
          `4.1` = "ADM_1",
          "level1" # fallback assumption for unsupported versions
        )
      )
    }
    if (2 %in% levels) {
      return_env$gadm$gadm_districts <- sf::st_read(
        filename[match(2, levels)],
        layer = switch(
          version,
          `3.6` = "level2",
          `4.0` = "level2",
          `4.1` = "ADM_2",
          "level2" # fallback assumption for unsupported versions
        )
      )
    }
  } else {
    # Assume that level can be directly extracted from source file without
    # specifying a layer to load.
    if (0 %in% levels) {
      return_env$gadm$gadm_countries <- sf::st_read(filename[match(0, levels)])
    }
    if (1 %in% levels) {
      return_env$gadm$gadm_regions <- sf::st_read(filename[match(1, levels)])
    }
    if (2 %in% levels) {
      return_env$gadm$gadm_districts <- sf::st_read(filename[match(2, levels)])
    }
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
  c(
    GridID = unname(indata["GridID"]),
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
  c(
    GridID = unname(indata["GridID"]),
    Landfraction = unname(indata["Landarea"] / indata["Gridarea"])
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
  cell_iso <- intersect(ISO_list, names(indata))
  if (length(cell_iso) < 1) {
    warning("Data does not contain valid country codes from ISO_list")
    return(
      list(
        GridID = unname(indata["GridID"]),
        country = NA
      )
    )
  }
  # Country with largest area in cell
  # First try without water bodies like Caspian Sea
  clist <- setdiff(cell_iso, water_bodies)
  if (length(intersect(names(indata), clist)) > 0) {
    # Cell has ISO codes besides water bodies like Caspian Sea, use these
    country <- names(which.max(indata[which(names(indata) %in% clist)]))
  } else {
    # Cell has only water bodies
    country <- names(which.max(indata[which(names(indata) %in% cell_iso)]))
  }
  list(
    GridID = unname(indata["GridID"]),
    country = country
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
dominant_region <- function(
  indata,
  region_list,
  ISO_list,
  include_regions,
  water_bodies
) {
  if (is.null(indata)) {
    # Elements in cell_list can be empty
    return(NULL)
  }
  if (!"GridID" %in% names(indata)) {
    warning("Data needs to have a 'GridID' column")
    indata <- c(indata, GridID = NA)
  }
  # Check if there are any region/state codes in indata
  if (any(names(indata) %in% names(region_list))) {
    # Determine dominant country
    country <- dominant_country(indata, ISO_list, water_bodies)$country
    if (country %in% include_regions) {
      # Filter only regions belonging to dominant country (and not country
      # itself)
      reg <- which(
        names(indata) %in% names(region_list) & grepl(country, names(indata))
      )
      regiondata <- indata[reg]
      return(
        list(
          GridID = unname(indata["GridID"]),
          region = names(which.max(regiondata))
        )
      )
    }
  }
  # Return NULL if no region/state assigned
  NULL
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
      reg <- which(
        names(indata) %in% names(region_list) & grepl(country, names(indata))
      )
      regiondata <- indata[reg]
      if (length(regiondata) > 0) {
        level1 <- c(
          level0,
          region = names(which.max(regiondata))
        )
      } else {
        level1 <- c(level0, region = NA)
      }
    } else {
      level1 <- c(level0, region = NA)
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
        level2 <- c(
          level1,
          district = names(which.max(districtdata))
        )
      } else {
        level2 <- c(level1, district = NA)
      }
    } else {
      level2 <- c(level1, district = NA)
    }
    level2
  } else {
    NULL
  }
}

################################################################################
## Function to download GDAL data from official server                        ##
##                                                                            ##
## Parameters:                                                                ##
##   - version: GADM version to download from web server                      ##
##   - gadm_dir: target directory for GADM data                               ##
##   - format: data format to use (GeoPackage or ESRI Shapefile are           ##
##     implemented                                                            ##
##   - baseurl: Web server address to retrieve GADM data (without version)    ##
##   - timeout: maximum allowed time in seconds for download, increase in     ##
##     case of slow connection                                                ##
################################################################################
download_gadm <- function(
  version,
  gadm_dir,
  format,
  baseurl = "https://geodata.ucdavis.edu/gadm",
  timeout = 1800
) {
  stopifnot(version %in% c("2.8", "3.6", "4.0", "4.1"))
  stopifnot(format %in% c("ESRI Shapefile", "GPKG"))
  stopifnot(length(timeout) == 1 && is.numeric(timeout))
  if (format == "ESRI Shapefile" && !version %in% c("2.8", "3.6")) {
    stop(
      "Format 'ESRI Shapefile' is only supported for GADM version 2.8 or 3.6"
    )
  }
  if (format == "GPKG" && version == "2.8") {
    stop("Format 'GPKG' is not available for version 2.8")
  }
  # Set timeout. Do not decrease below existing limit
  timeout <- options(timeout = max(timeout, getOption("timeout")))
  # Make sure timeout is reset after function
  on.exit(options(timeout))

  gadm_file <- ifelse(
    format == "ESRI Shapefile",
    switch(
      version,
      `2.8` = "gadm28_levels.shp.zip",
      `3.6` = "gadm36_levels_shp.zip"
    ),
    switch(
      version,
      `3.6` = "gadm36_levels_gpkg.zip",
      `4.0` = "gadm404-levels.zip",
      `4.1` = "gadm_410-levels.zip"
    )
  )
  local_filename <- file.path(
    ifelse(nchar(gadm_dir) > 0, gadm_dir, getwd()),
    gadm_file
  )
  remote_filename <- paste0(baseurl, "/", "gadm", version, "/", gadm_file)
  if (file.exists(local_filename) && file.size(local_filename) > 1e8) {
    warning(
      "Target file ", local_filename, " exists already. Skipping download.",
      immediate. = TRUE
    )
  } else {
    cat(
      "Attempting to download", remote_filename, "to", dirname(local_filename),
      "\n"
    )
    rc <- try(
      download.file(
        url = remote_filename,
        destfile = local_filename,
        quiet = TRUE, mode = "wb"
      )
    )
    if (inherits(rc, "try-error") || rc != 0) {
      if (file.exists(local_filename)) {
        file.remove(local_filename)
      }
      stop("Download unsuccessful")
    }
  }
  # Unzip downloaded file
  filelist <- unzip(local_filename, list = TRUE)
  if (format == "ESRI Shapefile") {
    # Reduce to level 0-2
    filelist <-
      filelist[grep("gadm(28|36)_(adm)*[0-2]\\.[a-z]{3}", filelist$Name), ]
  } else {
    # Exclude potential license file included in zip.
    filelist <- filelist[grep("gadm", filelist$Name), ]
  }
  cat("Decompress", nrow(filelist), "file(s) from", local_filename, "\n")
  unzip(
    zipfile = local_filename,
    files = filelist$Name,
    exdir = dirname(local_filename)
  )
  # Check file sizes of decompressed files
  sizes <- file.size(file.path(dirname(local_filename), filelist$Name))
  if (any(sizes != filelist$Length)) {
    stop(
      "Error decompressing ",
      toString(filelist$Name[which(sizes != filelist$Length)]),
      ".\nDelete files in ",
      toString(unique(dirname(filelist$Name[which(sizes != filelist$Length)])))
    )
  }
  if (any(dirname(filelist$Name) != ".")) {
    # Move files to correct destination directory
    movefiles <- filelist$Name[which(dirname(filelist$Name) != ".")]
    file.rename(
      from = file.path(dirname(local_filename), movefiles),
      to = file.path(dirname(local_filename), basename(movefiles))
    )
    # Delete directory if it is empty
    for (dir in unique(dirname(movefiles))) {
      remaining <- list.files(
        file.path(dirname(local_filename), dir)
      )
      if (length(remaining) == 0) {
        file.remove(file.path(dirname(local_filename), dir))
      }
    }
  }
  invisible(local_filename)
}
