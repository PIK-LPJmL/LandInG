################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Helper functions for LPJmL input format and basic LandInG setup.           ##
## The script landing_setup.R is saved in the parent directory by default.    ##
################################################################################
if (file.exists("../landing_setup.R")) {
  source("../landing_setup.R", chdir = TRUE)
  LandInG_setup$river_routing <- list()
} else if (!exists("LandInG_setup") || !is.environment(LandInG_setup)) {
  stop("Please update path to script with LandInG setup script")
}
################################################################################

################################################################################
## Basic setup for all R scripts in this directory:                           ##
## Working directory:                                                         ##
## This is where outputs from this script will be saved.                      ##
LandInG_setup$river_routing$drainagedir <- ""
if (nchar(LandInG_setup$river_routing$drainagedir) > 0) {
  setwd(LandInG_setup$river_routing$drainagedir)
}
## Grid:                                                                      ##
LandInG_setup$river_routing$gridname <- "ADD PATH TO GRID INPUT FILE HERE"
## River routing (such as created by create_river_routing_input.sh):          ##
LandInG_setup$river_routing$drainname <- "ADD PATH TO DRAINAGE INPUT FILE HERE"
## Land fraction (optional, comment line to run without):                     ##
# LandInG_setup$river_routing$landfracname <- "ADD_PATH_TO_LANDFRACTION_FILE_HERE_OR_COMMENT_THIS_LINE"
## Version string (optional string added to filenames of files created by     ##
## this script, e.g. if you have more than one routing network (spatial       ##
## resolution is added automatically).                                        ##
LandInG_setup$river_routing$version_string <- ""
################################################################################

################################################################################
## Additional setup for neighbour_irrigation.R                                ##
## Search area, either "adjacent" for direct neighbour cells or "region" for  ##
## all cells within search radius (default: "region"):                        ##
LandInG_setup$river_routing$search_area <- "region"
## Search radius in case of using "region" as search_area. Default: 75000 m   ##
LandInG_setup$river_routing$search_radius <- 75000
## Exclude upstream cells as neighbour cells since they are on the same river ##
LandInG_setup$river_routing$exclude_upstream <- TRUE
## Exclude downstream cells as neighbour cells since they are on the same     ##
## river (which may join with another tributary though).                      ##
LandInG_setup$river_routing$exclude_downstream <- TRUE
## Inverse distance weighting (IDW) of neighbour cell upstream areas. This    ##
## allows to introduce a "penalty" for transporting water from further away.  ##
## Higher values of IDW power parameter result in bigger penalty.             ##
## Set idw_power_par to 0 to use no inverse distance weighting.               ##
## Only used with search_area "region".                                       ##
LandInG_setup$river_routing$idw_power_par <- 2
## Format specifications for output file created:                             ##
## Format must be either LPJmL input format ("BIN", default) or "CSV" table.  ##
LandInG_setup$river_routing$neighbour_format <- "BIN"
## LPJmL input format version, 2 or 3. Only version 3 allows longitude and    ##
## latitude resolution to differ.                                             ##
LandInG_setup$river_routing$bintype <- 3
## Header name: Headers of LPJmL input files include a name. Header names are ##
## defined in /include/header.h of the LPJmL source code and do not usually   ##
## need to be changed.                                                        ##
LandInG_setup$river_routing$neighbour_headername <- "LPJNIRR"
################################################################################


################################################################################
## Load grid data, river routing data and landfrac, which is used by R scripts##
## in this directory.                                                         ##
## Grid:                                                                      ##
if (file.exists(LandInG_setup$river_routing$gridname)) {
  cat(
    "Loading grid file",
    sQuote(LandInG_setup$river_routing$gridname, q = FALSE),
    "\n"
  )
  LandInG_setup$river_routing$gridheader <-
    lpjmlkit::read_header(LandInG_setup$river_routing$gridname, verbose = FALSE)
  LandInG_setup$river_routing$griddata <-
    lpjmlkit::read_grid(LandInG_setup$river_routing$gridname)$data
  # Determine name string for output file based on resolution.
  cols <- c("cellsize_lon", "cellsize_lat")
  tmp_res <- unique(
    ifelse(
      LandInG_setup$river_routing$gridheader$header[cols] < 1 / 60,
      3600,
      60
    ) * LandInG_setup$river_routing$gridheader$header[cols]
  )
  lpj_res_string <- paste(
    round(tmp_res),
    unique(
      ifelse(
        LandInG_setup$river_routing$gridheader$header[cols] < 1 / 60,
        "arcsec",
        "arcmin"
      )
    ),
    sep = "",
    collapse = "_by_"
  )
  rm(tmp_res, cols)
} else {
  stop(
    "Grid input ", sQuote(LandInG_setup$river_routing$gridname, q = FALSE),
    " does not exist."
  )
}
## River routing (such as created by create_river_routing_input.sh):          ##
if (file.exists(LandInG_setup$river_routing$drainname)) {
  cat(
    "Loading river routing input",
    sQuote(LandInG_setup$river_routing$drainname, q = FALSE),
    "\n"
  )
  drainheader <- lpjmlkit::read_header(
    LandInG_setup$river_routing$drainname,
    verbose = FALSE
  )
  # Check compatibility with grid.
  chk_vals <- c("cellsize_lon", "cellsize_lat")
  if (
    LandInG_setup$river_routing$gridheader$header["ncell"] !=
      drainheader$header["ncell"] ||
      !isTRUE(
        all.equal(
          LandInG_setup$river_routing$gridheader$header[chk_vals],
          drainheader$header[chk_vals],
          tolerance = LandInG_setup$single.eps
        )
      )
  ) {
    sink(stderr())
    message("It seems that your grid file and river routing file do not match.")
    message(LandInG_setup$river_routing$gridname)
    print(LandInG_setup$river_routing$gridheader)
    message(LandInG_setup$river_routing$drainname)
    print(drainheader)
    sink()
    stop("Incompatible files")
  }
  if (drainheader$header["version"] < 3) {
    # Default data type for version-1 or version-2 headers is incorrect for
    # drainage file.
    message("Resetting default data type for drainheader to 4-byte integer.")
    drainheader$header["datatype"] <- 2
  }
  draindata <- drop(
    lpjmlkit::read_io(
      LandInG_setup$river_routing$drainname,
      datatype = drainheader$header["datatype"],
      band_names = c("nextcell", "distance")
    )$data
  )
  # Consistency check
  if (any(draindata[, "nextcell"] >= drainheader$header["ncell"])) {
    # nextcell must be index (starting at 0) of downstream cell in griddata
    stop(
      "River routing file ",
      sQuote(LandInG_setup$river_routing$drainname, q = FALSE),
      " appears to contain cells outside of grid file ",
      sQuote(LandInG_setup$river_routing$gridname, q = FALSE)
    )
  }
  # Only nextcell required. Cell indices for nextcell start counting at 0,
  # change to 1 to use as R indices; negative values indicate end cell.
  LandInG_setup$river_routing$nextcell <- as.integer(
    ifelse(
      draindata[, "nextcell"] < 0,
      -1,
      draindata[, "nextcell"] + 1
    )
  )
  rm(draindata, drainheader)
} else {
  stop(
    "River routing input ",
    sQuote(LandInG_setup$river_routing$drainname, q = FALSE),
    " does not exist."
  )
}
## Land fraction (optional)                                                   ##
if (
  !is.null(LandInG_setup$river_routing$landfracname) &&
    file.exists(LandInG_setup$river_routing$landfracname)
) {
  cat(
    "Loading land fraction file",
    sQuote(LandInG_setup$river_routing$landfracname, q = FALSE),
    "\n"
  )
  landfracheader <- lpjmlkit::read_header(
    LandInG_setup$river_routing$landfracname,
    verbose = FALSE
  )
  # Check compatibility with grid.
  chk_vals <- c("cellsize_lon", "cellsize_lat")
  if (
    LandInG_setup$river_routing$gridheader$header["ncell"] !=
      landfracheader$header["ncell"] ||
      !isTRUE(
        all.equal(
          LandInG_setup$river_routing$gridheader$header[chk_vals],
          landfracheader$header[chk_vals],
          tolerance = LandInG_setup$single.eps
        )
      )
  ) {
    sink(stderr())
    message("It seems that your grid file and land fraction file do not match.")
    message(LandInG_setup$river_routing$gridname)
    print(LandInG_setup$river_routing$gridheader)
    message(LandInG_setup$river_routing$landfracname)
    print(landfracheader)
    sink()
    stop("Incompatible files")
  }
  if (landfracheader$header["nbands"] != 1) {
    stop(
      "Unexpected number of bands ", landfracheader$header["nbands"],
      " in land fraction file ",
      sQuote(LandInG_setup$river_routing$landfracname, q = FALSE)
    )
  }
  landfrac <- drop(
    lpjmlkit::read_io(LandInG_setup$river_routing$landfracname)$data
  )
  if (max(landfrac) > 1.00001 && landfracheader$header["version"] > 1) {
    # Fractions should be less than or equal 1
    stop(
      length(which(landfrac > 1.00001)),
      " cells in land fraction file ",
      sQuote(LandInG_setup$river_routing$landfracname, q = FALSE),
      " exceed land fraction of 1."
    )
  } else if (max(landfrac) > 1.00001) {
    # Assume missing scalar and scale down values.
    warning(
      "Missing scalar information. Assuming a scalar of ",
      1 / round(max(landfrac)),
      " to scale values in land fraction file ",
      sQuote(LandInG_setup$river_routing$landfracname, q = FALSE),
      call. = FALSE,
      immediate. = TRUE
    )
    landfrac <- landfrac / round(max(landfrac))
  }
} else if (!is.null(LandInG_setup$river_routing$landfracname)) {
  stop(
    "Land fraction file ",
    sQuote(LandInG_setup$river_routing$landfracname, q = FALSE),
    " does not exist."
  )
} else {
  cat("Running without land fraction file & setting all cells to 100% land.\n")
  # Default land fraction
  landfrac <- rep(1.0, LandInG_setup$river_routing$gridheader$header["ncell"])
}
## Derive grid cell area. Function calc_cellarea() from lpjmlkit package      ##
LandInG_setup$river_routing$gridarea <- lpjmlkit::calc_cellarea(
  LandInG_setup$river_routing$griddata[, "lat"],
  LandInG_setup$river_routing$gridheader$header["cellsize_lon"],
  LandInG_setup$river_routing$gridheader$header["cellsize_lat"],
  earth_radius = LandInG_setup$earthradius,
  return_unit = "m2"
) * landfrac
rm(list = intersect(c("landfracheader", "landfrac"), ls()))
################################################################################

################################################################################
## Filenames of files created by river_routing.R and used by                  ##
## neighbour_irrigation.R                                                     ##
if (!LandInG_setup$river_routing$search_area %in% c("region", "adjacent")) {
  stop(
    "Invalid 'search_area' setting ",
    sQuote(LandInG_setup$river_routing$search_area, q = FALSE)
  )
}
# Upstream and downstream cell lists are saved to one file:
LandInG_setup$river_routing$drainage_celllists_RData <- paste0(
  "drainage_celllists_",
  ifelse(
    nchar(LandInG_setup$river_routing$version_string) > 0,
    paste0(LandInG_setup$river_routing$version_string, "_"),
    ""
  ),
  lpj_res_string,
  ".RData"
)
# Upstream areas are saved to a separate file to reduce memory requirements if
# data is reused in a different script.
LandInG_setup$river_routing$upstreamarea_RData <- paste0(
  "drainage_upstreamarea_",
  ifelse(
    nchar(LandInG_setup$river_routing$version_string) > 0,
    paste0(LandInG_setup$river_routing$version_string, "_"),
    ""
  ),
  lpj_res_string,
  ".RData"
)
## Filename of created neighbour irrigation file.                             ##
LandInG_setup$river_routing$neighbour_filename <- paste0(
  "neighbour_irrig_",
  ifelse(
    nchar(LandInG_setup$river_routing$version_string) > 0,
    paste0(LandInG_setup$river_routing$version_string, "_"),
    ""
  ),
  lpj_res_string,
  switch(
    LandInG_setup$river_routing$search_area,
    adjacent = "_adjacent",
    region = paste0("_", LandInG_setup$river_routing$search_radius, "m_radius")
  ),
  ifelse(
    LandInG_setup$river_routing$exclude_downstream,
    "_exclude_downstream",
    ""
  ),
  ifelse(
    LandInG_setup$river_routing$exclude_upstream,
    "_exclude_upstream",
    ""
  ),
  ifelse(
    LandInG_setup$river_routing$idw_power_par != 0 &&
      LandInG_setup$river_routing$search_area == "region",
    "_idw",
    ""
  ),
  ".", tolower(LandInG_setup$river_routing$neighbour_format)
)

## Check if RData files exist already pointing to previous script run.        ##
if (file.exists(LandInG_setup$river_routing$drainage_celllists_RData)) {
  chk_env <- new.env()
  load(LandInG_setup$river_routing$drainage_celllists_RData, envir = chk_env)
  if (
    is.null(chk_env$LandInG_version) ||
      chk_env$LandInG_version != LandInG_setup$LandInG_version
  ) {
    stop(
      sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
      " was created with a different version of LandInG. Please delete file",
      " and rerun 'river_routing.R'."
    )
  }
  if (
    nrow(LandInG_setup$river_routing$griddata) != nrow(chk_env$griddata) ||
      any(LandInG_setup$river_routing$griddata != chk_env$griddata)
  ) {
    stop(
      "Output file ",
      sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
      " exists already but has been created for a different grid.",
      "\nPlease rerun 'river_routing.R'."
    )
  }
  if (!identical(chk_env$nextcell, LandInG_setup$river_routing$nextcell)) {
    stop(
      "nextcell in ",
      sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
      " does not match currently loaded nextcell.",
      "\nPlease rerun 'river_routing.R'."
    )
  }
  rm(chk_env)
}
if (file.exists(LandInG_setup$river_routing$upstreamarea_RData)) {
  chk_env <- new.env()
  load(LandInG_setup$river_routing$upstreamarea_RData, envir = chk_env)
  if (
    is.null(chk_env$LandInG_version) ||
      chk_env$LandInG_version != LandInG_setup$LandInG_version
  ) {
    stop(
      sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE),
      " was created with a different version of LandInG. Please delete file",
      " and rerun 'river_routing.R'."
    )
  }
  if (
    nrow(LandInG_setup$river_routing$griddata) != nrow(chk_env$griddata) ||
      any(LandInG_setup$river_routing$griddata != chk_env$griddata)
  ) {
    stop(
      "Output file ",
      sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE),
      " exists already but has been created for a different grid.",
      "\nPlease rerun 'river_routing.R'."
    )
  }
  # Compare gridarea to check whether landfrac was used
  if (any(chk_env$gridarea != LandInG_setup$river_routing$gridarea)) {
    stop(
      "Output file ",
      sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE),
      " exists already but gridarea does not match. ",
      "Maybe a different landfrac was used.",
      "\nPlease rerun 'river_routing.R'."
    )
  }
  if (!identical(chk_env$nextcell, LandInG_setup$river_routing$nextcell)) {
    stop(
      "nextcell in ",
      sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE),
      " does not match currently loaded nextcell.",
      "\nPlease rerun 'river_routing.R'."
    )
  }
  rm(chk_env)
}
