################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to load and gap-fill HYDE (or any alternative) area dataset       ##
## Parameters:                                                                ##
## filename: Raster file providing cell area grid                             ##
## fileunits: Spatial area unit used in filename                              ##
## return_units: Spatial area unit used for return value of this function     ##
## return_raster: SpatRaster exemplifying finest resolution used; HYDE area   ##
##                is aggregated to that resolution if necessary. Set to NULL  ##
##                to get HYDE area in source resolution, but still converted  ##
##                to return_units                                             ##
## earth_radius: Earth radius to use in calc_cellarea function                ##
## gextent: SpatExtent object used for comparison, normally use global_extent ##
################################################################################
load_hyde_area <- function(filename,
                           fileunits,
                           return_units,
                           return_raster,
                           earth_radius,
                           gextent
                          ) {
  cat("Loading cell area from", sQuote(filename), "\n")
  # Load and convert to return unit.
  hyde_area <- terra::rast(filename) *
    units::ud_convert(1, fileunits, return_units)
  # Check spatial extent
  if (matching_extent(
    terra::ext(hyde_area),
    gextent,
    terra::xres(hyde_area),
    terra::yres(hyde_area)
  )) {
    terra::ext(hyde_area) <- gextent
  }
  if (!is.null(return_raster) &&
    !matching_extent(
      terra::ext(hyde_area),
      terra::ext(return_raster),
      terra::xres(hyde_area),
      terra::yres(hyde_area)
    )
  ) {
    stop("Return units and area data have different spatial extent")
  }
  # HYDE area only has values in its landmask, fill globally.
  if (anyNA(terra::values(hyde_area))) {
    # Fill band-wise. This assumes that all cells in one latitude band have the
    # same area.
    # Confirm that bands have one unique value
    if (any(
      apply(
        terra::as.array(hyde_area),
        1,
        function(x) ifelse(all(is.na(x)), 1, length(unique(na.omit(x))))
      ) != 1
    )
    ) {
      stop("Area data has multiple values per latitude band. Cannot gap-fill.")
    }
    terra::set.values(
      hyde_area,
      cells = seq_len(terra::ncell(hyde_area)),
      values = rep(
        apply(
          terra::as.array(hyde_area),
          1,
          function(x) ifelse(all(is.na(x)), NA, unique(na.omit(x)))
        ),
        each = terra::ncol(hyde_area)
      )
    )
    # Check if there are still empty rows
    emptyrows <- apply(terra::as.array(hyde_area), 1, function(x) all(is.na(x)))
    for (r in which(emptyrows)) {
      # Check mirrored band
      opposite <- which(
        abs(-terra::yFromRow(hyde_area, r) - terra::yFromRow(hyde_area)) <
          terra::yres(hyde_area) * 0.001
      )
      if (length(opposite) == 1 && !opposite %in% which(emptyrows)) {
        hyde_area[r, ] <-
          hyde_area[opposite, ]
      }
    }
    # Check again if there are still empty rows
    emptyrows <- apply(terra::as.array(hyde_area), 1, function(x) all(is.na(x)))
    if (any(emptyrows)) {
      # Still missing values, fill using calc_cellarea() function from lpjmlkit
      hyde_area[which(emptyrows), ] <- rep(
        lpjmlkit::calc_cellarea(
          terra::yFromRow(hyde_area, which(emptyrows)),
          terra::xres(hyde_area),
          terra::yres(hyde_area),
          earth_radius = earth_radius,
          return_unit = "m2"
        ),
        each = terra::ncol(hyde_area)
      ) * units::ud_convert(1, "m2", return_units)
    }
  }
  # Aggregate to spatial resolution of return_raster if necessary.
  if (
    !is.null(return_raster) &&
      any(terra::res(hyde_area) < terra::res(return_raster))
  ) {
    hyde2gadm <- round(terra::res(return_raster) / terra::res(hyde_area), 4)
    if (max(hyde2gadm %% 1) != 0) {
      stop(
        "Target resolution ", toString(round(terra::res(return_raster), 5)),
        " is not an integer multiple of source resolution ",
        toString(round(terra::res(hyde_area), 5)), "\n",
        "Cannot aggregate."
      )
    }
    if (any(hyde2gadm > 1)) {
      # Calculate global sum to check after aggregation
      areasum <- ul(terra::global(hyde_area, "sum", na.rm = TRUE))
      # Aggregate
      hyde_area <- terra::aggregate(
        hyde_area,
        fact = rev(hyde2gadm), # res() returns lon/lat, fact is lat/lon
        fun = "sum",
        na.rm = TRUE
      )
      if (
        !isTRUE(
          all.equal(areasum, ul(terra::global(hyde_area, "sum", na.rm = TRUE)))
        )
      ) {
        stop("Error aggregating hyde_area")
      }
    }
  } else if (
    !is.null(return_raster) &&
      any(terra::res(hyde_area) / terra::res(return_raster) > 1.00001)
  ) {
    stop(
      "Target resolution ", toString(round(terra::res(return_raster), 5)),
      " is finer than source resolution ",
      toString(round(terra::res(hyde_area), 5)), "\n",
      "Output resolution cannot be finer than any gridded source data."
    )
  } else if (!is.null(return_raster)) {
    hyde2gadm <- c(1, 1)
  } else {
    message(
      "Info: area is returned in its native resolution because you have ",
      "not supplied a return_raster argument. hyde2gadm is set to NULL."
    )
    if (units::ud_convert(1, fileunits, return_units) != 1) {
      message(
        "Still converting area from ", sQuote(fileunits),
        " to ", sQuote(return_units)
      )
    }
    hyde2gadm <- NULL
  }
  terra::units(hyde_area) <- return_units
  list(area = hyde_area, unit = return_units, hyde2gadm = hyde2gadm)
}
