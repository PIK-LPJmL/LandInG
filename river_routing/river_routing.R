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
## Load setup from river_routing_setup.R:                                     ##
source("river_routing_setup.R")
################################################################################


################################################################################
## Filenames of files created by this script:                                 ##
# No need to run again if both output files exist already.
if (
  file.exists(LandInG_setup$river_routing$drainage_celllists_RData) &&
    file.exists(LandInG_setup$river_routing$upstreamarea_RData)
) {
  stop(
    "Both output files ",
    sQuote(LandInG_setup$river_routing$drainage_celllists_RData, q = FALSE),
    " and ",
    sQuote(LandInG_setup$river_routing$upstreamarea_RData, q = FALSE),
    "exist already.\nRename or delete them to force this script to run again."
  )
}

################################################################################
## Derive outflow cell, number of downstream cells until outflow cell and     ##
## downstream cell list.                                                      ##
cellstoend <- endcell <- integer(length(LandInG_setup$river_routing$nextcell))
# Downstream cell list
dsclist <- list()
cat(
  "Deriving downstream cells and outflow cells for grid with",
  LandInG_setup$river_routing$gridheader$header["ncell"], "cells.\n"
)
progress_step <- ifelse(
  length(LandInG_setup$river_routing$nextcell) < 100000,
  5,
  20
)
procstart <- proc.time()["elapsed"]
# Pre-allocate a buffer to reduce time for memory allocation in loop.
dsclist[which(LandInG_setup$river_routing$nextcell > 0)] <- list(
  rep(NA, max(10, length(LandInG_setup$river_routing$nextcell) / 1000))
)
for (c in seq_along(LandInG_setup$river_routing$nextcell)) {
  endcell[c] <- c
  if (
    c %%
      round(length(LandInG_setup$river_routing$nextcell) / progress_step) == 0
  ) {
    cat(round(c / length(LandInG_setup$river_routing$nextcell) * 100), "% ")
  }
  while (LandInG_setup$river_routing$nextcell[endcell[c]] > 0) {
    # Walk one cell downstream
    endcell[c] <- LandInG_setup$river_routing$nextcell[endcell[c]]
    # Add cell to downstream cell list
    if (length(dsclist) < c || length(dsclist[[c]]) == 0) {
      dsclist[[c]] <- endcell[c]
    } else {
      # Cells can have multiple downstream cells.
      dsclist[[c]][cellstoend[c] + 1] <- endcell[c]
    }
    # Increment counter for number of downstream cells until outflow cell
    cellstoend[c] <- cellstoend[c] + 1
  }
  if (length(dsclist) >= c && !is.null(dsclist[[c]])) {
    # Remove left-over buffer
    length(dsclist[[c]]) <- cellstoend[c]
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
upstreamarea <- LandInG_setup$river_routing$gridarea
# Route areas through river network starting with cells that are farthest away
# from their outflow cell. This uses the number of cells to outflow "cellstoend"
# created above.
routing_steps <- sort(unique(cellstoend), decreasing = TRUE)
# Upstream cell list
usclist <- list()
cat(
  "Deriving upstream cells and upstream areas for grid with",
  LandInG_setup$river_routing$gridheader$header["ncell"], "cells.\n"
)
progress_step <- ifelse(
  length(LandInG_setup$river_routing$nextcell) < 100000,
  10,
  50
)
progress <- length(LandInG_setup$river_routing$nextcell) / progress_step
procstart <- proc.time()["elapsed"]
processed <- 0
unique_nc <- unique(LandInG_setup$river_routing$nextcell)
for (rstep in seq(2, length(routing_steps))) {
  if (rstep %% 5 == 0 || routing_steps[rstep] < 5) {
    processed <- length(which(cellstoend > routing_steps[rstep]))
  }
  if (processed > progress) {
    cat(
      round(processed / length(LandInG_setup$river_routing$nextcell) * 100),
      "% finished after",
      round(proc.time()["elapsed"] - procstart), "seconds\n"
    )
    progress <- progress + length(LandInG_setup$river_routing$nextcell) /
      progress_step
  }
  # Loop over all cells which are at the current position in routing_steps
  step_cells <- which(cellstoend == routing_steps[rstep])
  # Filter for cells which have no upstream cells.
  step_cells <- step_cells[which(step_cells %in% unique_nc)]
  for (c in step_cells) {
    # Find all cells draining directly into cell c
    usc <- which(LandInG_setup$river_routing$nextcell == c)
    # Add upstream areas of all usc to upstream area of c.
    upstreamarea[c] <- sum(upstreamarea[c(usc, c)])
    # Add usc to usclist of cell c
    # Add upstream cells of all cells in usc to usclist of c
    usclist[[c]] <- c(
      usc,
      unlist(usclist[usc], recursive = FALSE, use.names = FALSE)
    )
    rm(usc)
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
LandInG_version <- LandInG_setup$LandInG_version
griddata <- LandInG_setup$river_routing$griddata
gridarea <- LandInG_setup$river_routing$gridarea
nextcell <- LandInG_setup$river_routing$nextcell
var_list <- c(
  "griddata",
  "dsclist",
  "usclist",
  "endcell",
  "cellstoend",
  "nextcell",
  "LandInG_version"
)
cat(
  "Saving upstream and downstream cell lists to",
  LandInG_setup$river_routing$drainage_celllists_RData, "\n"
)
save(
  list = var_list,
  file = LandInG_setup$river_routing$drainage_celllists_RData
)
cat(
  "Saving upstream areas to",
  LandInG_setup$river_routing$upstreamarea_RData, "\n"
)
var_list <- c(
  "griddata",
  "upstreamarea",
  "gridarea",
  "nextcell",
  "LandInG_version"
)
save(list = var_list, file = LandInG_setup$river_routing$upstreamarea_RData)
################################################################################
