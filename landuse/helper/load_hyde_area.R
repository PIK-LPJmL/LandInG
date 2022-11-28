################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to load and gap-fill HYDE area                                    ##
## Parameters:                                                                ##
## filename: Raster file providing cell area grid                             ##
## fileunits: Spatial area unit used in filename                              ##
## faounits: Spatial area unit used in FAOSTAT data (used for return value of ##
##           this function)                                                   ##
## unitraster: Raster object exemplifying finest resolution used; HYDE area   ##
##             is aggregated to that resolution if necessary. Set to NULL to  ##
##             get HYDE area in source resolution, but still converted to     ##
##             faounits                                                       ##
## gextent: Extent object used for comparison, normally use global_extent     ##
################################################################################
load_hyde_area <- function(filename,
                           fileunits = hyde_area_file_units,
                           faounits = fao_area_units,
                           unitraster = gadm_raster,
                           gextent = global_extent
                          ) {
  cat("Loading HYDE area from", sQuote(filename), "\n")
  # Load and convert to FAOSTAT units.
  hyde_area <- raster(filename) * ud.convert(1, fileunits, faounits)
  # Check spatial extent
  if (matching_extent(
    extent(hyde_area),
    gextent,
    xres(hyde_area),
    yres(hyde_area)
  )) {
    hyde_area <- setExtent(hyde_area, gextent)
  }
  if (!is.null(unitraster) && !matching_extent(
    extent(hyde_area),
    extent(unitraster),
    xres(hyde_area),
    yres(hyde_area))
  ) {
    stop("Spatial units and HYDE area have different spatial extent")
  }
  # HYDE area only has values in its landmask, fill globally.
  if (anyNA(values(hyde_area))) {
    # Fill band-wise. This assumes that all cells in one latitude band have the
    # same area.
    # Confirm that bands have one unique value
    if (any(
      apply(as.array(hyde_area), 1, function(indata) {
        ifelse(all(is.na(indata)), 1, length(unique(na.omit(indata))))
      }) != 1)
    ) {
      stop("HYDE area has multiple values per latitude band. Cannot gap-fill.")
    }
    values(hyde_area) <- rep(
      apply(as.array(hyde_area), 1, function(indata) {
        ifelse(all(is.na(indata)), NA, unique(na.omit(indata)))
      }),
      each = ncol(hyde_area)
    )
    # Check if there are still empty rows
    emptyrows <- apply(as.array(hyde_area), 1, function(indata) {
      all(is.na(indata))
    })
    for (r in which(emptyrows)) {
      # Check mirrored band
      opposite <- which(abs(-yFromRow(hyde_area, r) - yFromRow(hyde_area)) <
        yres(hyde_area) * 0.001        
      )
      if (length(opposite) == 1 && !opposite %in% which(emptyrows)) {
        hyde_area[cellFromRow(hyde_area, r)] <-
          hyde_area[cellFromRow(hyde_area, opposite)]
      }
    }
    # Check again if there are still empty rows
    emptyrows <- apply(as.array(hyde_area), 1, function(indata) {
      all(is.na(indata))
    })
    if (any(emptyrows)) {
      # Still missing values, fill using cellarea() function from
      # helper_functions.R
      hyde_area[cellFromRow(hyde_area, which(emptyrows))] <- rep(
        ud.convert(
          cellarea(
            yFromRow(hyde_area, which(emptyrows)),
            xres(hyde_area),
            yres(hyde_area)
          ), # cellarea returns area in m2
          "m2",
          faounits
        ),
        each = ncol(hyde_area)
      )
    }
  }
  # Aggregate to spatial resolution of unitraster if necessary.
  if (!is.null(unitraster) && any(res(hyde_area) < res(unitraster))) {
    hyde2gadm <- round(res(hyde_area) / res(unitraster), 4)
    if (max(hyde2gadm %% 1) != 0) {
      stop(
        "Target resolution ", toString(round(res(unitraster), 5)),
        " is not an integer multiple of HYDE resolution ",
        toString(round(res(hyde_area), 5)), "\n",
        "Cannot aggregate."
      )
    }
    # Calculate global sum to check after aggregation
    areasum <- cellStats(hyde_area, sum, na.rm = TRUE)
    # Aggregate
    hyde_area <- aggregate(hyde_area, hyde2gadm)
    if (areasum != cellStats(hyde_area, sum, na.rm = TRUE)) {
      stop("Error aggregating hyde_area")
    }
  } else if (!is.null(unitraster) &&
      any(res(hyde_area) / res(unitraster) > 1.00001)
  ) {
    stop(
      "Target resolution ", toString(round(res(unitraster), 5)),
      " is finer than HYDE resolution ",
      toString(round(res(hyde_area), 5)), "\n",
      "Output resolution cannot be finer than any gridded source data."
    )
  } else if (!is.null(unitraster)) {
    hyde2gadm <- c(1, 1)
  } else {
    message(
      "Info: HYDE area is returned in its native resolution because you have ",
      "not supplied a unitraster argument. hyde2gadm is set to NULL."
    )
    if (ud.convert(1, fileunits, faounits) != 1) {
      message(
        "Still converting HYDE area from ", sQuote(fileunits),
        " to ", sQuote(faounits)
      )
    }
    hyde2gadm <- NULL
  }
  list(area = hyde_area, unit = faounits, hyde2gadm = hyde2gadm)
}
