################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Script to derive upstream cells, downstream cells, and upstream area from  ##
## river routing file.                                                        ##
## Upstream cells of a cell are all cells draining into that cell. For outlow ##
## cells these are all cells belonging to the same basin.                     ##
## Upstream area of a cell is the area of the cell itself plus the area of    ##
## all upstream cells draining into that cell.                                ##
## Downstream cells of a cell are all cells that water drains into either     ##
## directly or indirectly through intermediate cells.                         ##
## Requires an LPJmL grid input file and an LPJmL river routing input file.   ##
## The latter may be created using create_river_routing_input.sh in this      ##
## directory.                                                                 ##
## You may also provide an optional input giving the land fraction in each    ##
## cell. Otherwise, all cells are assumed to be 100% covered by land. This    ##
## may have a small impact on upstream areas along coast lines.               ##
## This script creates two RData files: "drainage_upstreamarea_*.RData" and   ##
## "drainage_celllists_*.RData" where * is replaced with an optional version  ##
## string (if provided) and a resolution string.                              ##
################################################################################
# Clean up memory
rm(list = ls(all = TRUE))

################################################################################
## Basic setup:                                                               ##
## Working directory:                                                         ##
## This is where outputs from this script will be saved.                      ##
drainagedir <- ""
if (nchar(drainagedir) > 0) {
  setwd(drainagedir)
}
## Grid:                                                                      ##
gridname <- "ADD_PATH_TO_GRID_FILE_HERE"
## River routing:                                                             ##
drainname <- "ADD_PATH_TO_RIVER_ROUTING_FILE_HERE"
## Land fraction (optional, comment line to run without):                     ##
# landfracname <- "ADD_PATH_TO_LANDFRACTION_FILE_HERE_OR_COMMENT_THIS_LINE"
## Version string (optional string added to filenames of files created by     ##
## this script, e.g. if you have more than one routing network (spatial       ##
## resolution is added automatically).                                        ##
version_string <- ""
################################################################################

################################################################################
## Helper functions for LPJmL input format                                    ##
## The script lpjml_format_helper_functions.R is saved in the parent          ##
## directory by default.                                                      ##
################################################################################
if (file.exists("../lpjml_format_helper_functions.R")) {
  source("../lpjml_format_helper_functions.R")
} else {
  stop("Please update path to script with LPJmL input format helper function")
}

################################################################################
## Load source files                                                          ##
## Functions read_header(), get_headersize(), and get_datatype() defined in   ##
## ../lpjml_format_helper_functions.R                                         ##
if (file.exists(gridname)) {
  cat("Loading grid file", sQuote(gridname), "\n")
  gridheader <- read_header(gridname)
  zz <- file(gridname, "rb")
  seek(zz, get_headersize(gridheader))
  griddata <- matrix(
    readBin(
      zz,
      what = get_datatype(gridheader)$type,
      size = get_datatype(gridheader)$size,
      n = gridheader$header["ncell"] * gridheader$header["nbands"],
      endian = gridheader$endian
    ) * gridheader$header["scalar"],
    ncol = gridheader$header["nbands"],
    byrow = TRUE,
    dimnames = list(NULL, c("lon", "lat"))
  )
  close(zz)
  # Determine name string for output file based on resolution.
  tmp_res <- unique(
    ifelse(
      gridheader$header[c("cellsize_lon", "cellsize_lat")] < 1 / 60,
      3600,
      60
    ) * gridheader$header[c("cellsize_lon", "cellsize_lat")]
  )
  lpj_res_string <- paste(
    round(tmp_res),
    unique(
      ifelse(
        gridheader$header[c("cellsize_lon", "cellsize_lat")] < 1 / 60,
        "arcsec",
        "arcmin"
      )
    ),
    sep = "",
    collapse = "_by_"
  )
  rm(tmp_res)
} else {
  stop("Grid input ", gridname, " does not exist.")
}
if (file.exists(drainname)) {
  cat("Loading river routing input", sQuote(drainname), "\n")
  drainheader <- read_header(drainname)
  # Check compatibility with grid.
  if (any(
    gridheader$header[c("ncell", "cellsize_lon", "cellsize_lat")] !=
    drainheader$header[c("ncell", "cellsize_lon", "cellsize_lat")]
  )) {
    sink(stderr())
    message("It seems that your grid file and river routing file do not match.")
    message(gridname)
    print(gridheader)
    message(drainname)
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
  zz <- file(drainname, "rb")
  seek(zz, get_headersize(drainheader))
  draindata <- matrix(
    readBin(
      zz,
      what = get_datatype(drainheader)$type,
      size = get_datatype(drainheader)$size,
      n = drainheader$header["ncell"] * drainheader$header["nbands"],
      endian = drainheader$endian
    ) * drainheader$header["scalar"],
    ncol = drainheader$header["nbands"],
    byrow = TRUE,
    dimnames = list(NULL, c("nextcell", "distance"))
  )
  close(zz)
  # Consistency check
  if (any(draindata[, "nextcell"] >= drainheader$header["ncell"])) {
    # nextcell must be index (starting at 0) of downstream cell in griddata
    stop(
      "River routing file ", drainname,
      " appears to contain cells outside of grid file ", gridname
    )
  }
} else {
  stop("River routing input ", drainname, " does not exist.")
}

if (exists("landfracname") && file.exists(landfracname)) {
  cat("Loading land fraction file", sQuote(landfracname), "\n")
  landfracheader <- read_header(landfracname)
  # Check compatibility with grid.
  if (any(
    gridheader$header[c("ncell", "cellsize_lon", "cellsize_lat")] !=
    landfracheader$header[c("ncell", "cellsize_lon", "cellsize_lat")]
  )) {
    sind(stderr())
    message("It seems that your grid file and land fraction file do not match.")
    message(gridname)
    print(gridheader)
    message(landfracname)
    print(landfracheader)
    sink()
    stop("Incompatible files")
  }
  zz <- file(landfracname, "rb")
  seek(zz, get_headersize(landfracheader))
  landfrac <- matrix(
    readBin(
      zz,
      what = get_datatype(landfracheader)$type,
      size = get_datatype(landfracheader)$size,
      n = landfracheader$header["ncell"] * landfracheader$header["nbands"],
      endian = landfracheader$endian
    ) * landfracheader$header["scalar"],
    ncol = landfracheader$header["nbands"],
    byrow = TRUE
  )
  close(zz)
  if (ncol(landfrac) != 1) {
    stop(
      "Unexpected number of bands ", landfracheader$header["nbands"],
      " in land fraction file ", landfracname
    )
  }
  if (max(landfrac) > 1.00001 && landfracheader$header["version"] > 1) {
    # Fractions should be less than or equal 1
    stop(
      length(which(landfrac > 1.00001)),
      " cells in land fraction file ", landfracname,
      " exceed land fraction of 1."
    )
  } else if (max(landfrac) > 1.00001) {
    # Assume missing scalar and scale down values.
    warning(
      "Missing scalar information. Assuming a scalar of ",
      1 / round(max(landfrac)),
      " to scale values in land fraction file ", landfracname,
      call. = FALSE,
      immediate. = TRUE
    )
    landfrac <- landfrac / round(max(landfrac))
  }
} else if (exists("landfracname")) {
  stop("Land fraction file ", landfracname, " does not exist.")
} else {
  cat("Running without land fraction file & setting all cells to 100% land.\n")
  # Default land fraction
  landfrac <- rep(1.0, gridheader$header["ncell"])
}
# Derive grid cell area. Function cellarea() defined in
# ../lpjml_format_helper_functions.R
gridarea <- cellarea(
  griddata[, "lat"],
  gridheader$header["cellsize_lon"],
  gridheader$header["cellsize_lat"]
) * landfrac
################################################################################


################################################################################
## Filenames of files created by this script:                                 ##
# Upstream and downstream cell lists are saved to one file:
drainage_celllists_RData <- paste0(
  "drainage_celllists_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string,
  ".RData"
)
# Upstream areas are saved to a separate file to reduce memory requirements if
# data is reused in a different script.
upstreamarea_RData <- paste0(
  "drainage_upstreamarea_",
  ifelse(nchar(version_string) > 0, paste0(version_string, "_"), ""),
  lpj_res_string,
  ".RData"
)
# Check if files exist already pointing to previous script run.
if (file.exists(drainage_celllists_RData)) {
  # Remember variables existing before data is loaded from file
  before_load <- ls()
  load(drainage_celllists_RData)
  if (nrow(griddata) != nrow(drainage_griddata) ||
    any(griddata != drainage_griddata)
  ) {
    stop(
      "Output file ", sQuote(drainage_celllists_RData),
      " exists already but has been created for a different grid.",
      "\nPlease rename or delete file before running this script."
    )
  }
  # Clean up variables loaded from file
  rm(list = setdiff(ls(), before_load))
}
if (file.exists(upstreamarea_RData)) {
  # Remember variables existing before data is loaded from file
  before_load <- ls()
  load(upstreamarea_RData)
  if (nrow(griddata) != nrow(drainage_griddata) ||
    any(griddata != drainage_griddata)
  ) {
    stop(
      "Output file ", sQuote(upstreamarea_RData),
      " exists already but has been created for a different grid.",
      "\nPlease rename or delete file before running this script."
    )
  }
  # Compare gridarea to check whether landfrac was used
  if (any(drainage_gridarea != gridarea)) {
    stop(
      "Output file ", sQuote(upstreamarea_RData),
      "exists already but gridarea does not match. ",
      "Maybe a different landfrac was used.",
      "\nPlease rename or delete file before running this script."
    )
  }
  # Clean up variables loaded from file
  rm(list = setdiff(ls(), before_load))
}
# No need to run again if both output files exist already.
if (file.exists(drainage_celllists_RData) && file.exists(upstreamarea_RData)) {
  stop(
    "Both output files ",
    sQuote(drainage_celllists_RData), " and ", sQuote(upstreamarea_RData),
    "exist already.\nRename or delete them to force this script to run again."
  )
}
################################################################################
## Change C index into R index.                                               ##
## By default, outflow and inland sinks are marked with -1 while cells not    ##
## part of original drainage direction map are marked with -9. Set all to -1. ##
nextcell <- ifelse(draindata[, "nextcell"] < 0, -1, draindata[, "nextcell"] + 1)

################################################################################
## Derive outflow cell, number of downstream cells until outflow cell and     ##
## downstream cell list.                                                      ##
cellstoend <- endcell <- integer(length(nextcell))
# Downstream cell list
dsclist <- list()
cat(
  "Deriving downstream cells and outflow cells for grid with",
  gridheader$header["ncell"], "cells.\n"
)
progress_step <- ifelse(length(nextcell) < 100000, 5, 20)
procstart <- proc.time()["elapsed"]
for (c in seq_along(nextcell)) {
  endcell[c] <- c
  if (c %% round(length(nextcell) / progress_step) == 0) {
    cat(round(c / length(nextcell) * 100), "% ")
  }
  while (nextcell[endcell[c]] > 0) {
    # Walk one cell downstream
    endcell[c] <- nextcell[endcell[c]]
    # Add cell to downstream cell list
    if (length(dsclist) < c) {
      dsclist[[c]] <- endcell[c]
    } else {
      # Cells can have multiple downstream cells.
      dsclist[[c]] <- c(dsclist[[c]], endcell[c])
    }
    # Increment counter for number of downstream cells until outflow cell
    cellstoend[c] <- cellstoend[c] + 1
  }
}
cat("\n")
proctime <- proc.time()["elapsed"] - procstart
cat(
  "Processing of downstream cells and outflow cells took ",
  proctime %/% 3600, ":",
  formatC(proctime %% 3600 %/% 60, width = 2, flag = "0"), ":",
  formatC(round(proctime %% 3600 %% 60), width = 2, flag = "0"),
  "\n", sep = ""
)

################################################################################

################################################################################
## Derive upstream cells and upstream area.                                   ##
## This is done by "routing" cell areas through the river system aggregating  ##
## areas along the way.                                                       ##
# Initialize upstream area for each cell with its own area.
upstreamarea <- gridarea
# Route areas through river network starting with cells that are farthest away
# from their outflow cell. This uses the number of cells to outflow "cellstoend"
# created above.
routing_steps <- sort(unique(cellstoend), decreasing = TRUE)
# Upstream cell list
usclist <- list()
cat(
  "Deriving upstream cells and upstream areas for grid with",
  gridheader$header["ncell"], "cells.\n"
)
progress_step <- ifelse(length(nextcell) < 100000, 10, 50)
progress <- length(nextcell) / progress_step
procstart <- proc.time()["elapsed"]
processed <- 0
for (rstep in seq(2, length(routing_steps))) {
  if (rstep %% 5 == 0 || routing_steps[rstep] < 5) {
    processed <- length(which(cellstoend > routing_steps[rstep]))
  }
  if (processed > progress) {
    cat(
      round(processed / length(nextcell) * 100),
      "% finished after",
      round(proc.time()["elapsed"] - procstart), "seconds\n"
    )
    progress <- progress + length(nextcell) / progress_step
  }
  # Loop over all cells which are at the current position in routing_steps
  for (c in which(cellstoend == routing_steps[rstep])) {
    # Find all cells draining directly into cell c
    usc <- which(nextcell == c)
    if (length(usc) > 0) {
      # Add upstream areas of all usc to upstream area of c.
      upstreamarea[c] <- sum(upstreamarea[c(usc, c)])
      # Add usc to usclist of cell c
      usclist[[c]] <- usc
      # Add upstream cells of all cells in usc to usclist of c
      usclist[[c]] <- c(usclist[[c]], unlist(usclist[usc]))
    }
  }
}
proctime <- proc.time()["elapsed"] - procstart
cat(
  "Processing of upstream cells and areas took ",
  proctime %/% 3600, ":",
  formatC(proctime %% 3600 %/% 60, width = 2, flag = "0"), ":",
  formatC(round(proctime %% 3600 %% 60), width = 2, flag = "0"),
  "\n", sep = ""
)
################################################################################

################################################################################
## Write data to RData files so that it can be used by other scripts.         ##
# Rename variables for saving to file. This is to avoid name conflicts if data
# is reused in a different script.
for (var in c(
  "griddata",
  "dsclist",
  "usclist",
  "endcell",
  "cellstoend",
  "upstreamarea",
  "gridarea",
  "nextcell"
)) {
  assign(paste0("drainage_", var), get(var))
  rm(list = var)
}
cat(
  "Saving upstream and downstream cell lists to",
  drainage_celllists_RData, "\n"
)
save(
  drainage_griddata,
  drainage_dsclist,
  drainage_usclist,
  drainage_endcell,
  drainage_cellstoend,
  drainage_nextcell,
  file = drainage_celllists_RData
)
cat("Saving upstream areas to", upstreamarea_RData, "\n")
save(
  drainage_upstreamarea,
  drainage_griddata,
  drainage_gridarea,
  drainage_nextcell,
  file = upstreamarea_RData
)
