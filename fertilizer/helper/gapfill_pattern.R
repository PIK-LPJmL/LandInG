################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This function can be used to fill gaps in spatial patterns.                ##
## The base assumption for gap-filling is that values are constant within     ##
## spatial units, usually administrative units, specified as input.           ##
##                                                                            ##
## Parameters:                                                                ##
## filedata: RasterBrick or RasterLayer providing spatial pattern to fill,    ##
##           may include additional band specifying data source.              ##
## strip_zero: Whether to replace 0 with NA in filedata.                      ##
## unit_raster: RasterLayer or RasterBrick containing spatial units used for  ##
##              gap-filling. Must include at least national level, can also   ##
##              include subnational levels. Code expects 1 or 3 levels.       ##
## unit_raster_names: Optional data.frame providing names and ISO codes       ##
##                    matching IDs used in unit_raster. Used for GADM levels. ##
## unit_border_raster: Optional RasterLayer or RasterBrick providing the      ##
##                     number of unique admin units in each grid cell. Used   ##
##                     determine border cells. Number of bands must be 1 or   ##
##                     equal to number of bands in unit_raster.               ##
## assign_grid_threshold: Minimum number of source cells required to use      ##
##                        value for admin unit. Accounts for possible         ##
##                        inconsistencies between admin masks used. Set to 1  ##
##                        if you know unit_raster is valid mask for filedata. ##
## fill_regional_data: Whether to use multi-national regions to gap-fill      ##
##                     countries without any non-NA values.                   ##
## assign_country_threshold: Minimum number of countries required in group to ##
##                           use said group value. Can be used to avoid single##
##                           country values possibly used for many other      ##
##                           countries. Default: 1.                           ##
## country_grouping: Optional data.frame providing country groups, required   ##
##                   if fill_regional_data == TRUE. Also used to determine    ##
##                   country names in the absence of unit_raster_names.       ##
## country_grouping_col: Name of column in country_grouping that contains     ##
##                       codes matching unit_raster.                          ##
## is_national: Whether source data is national or subnational. Subnational   ##
##              gap-filling is only possible if unit_raster provides          ##
##              subnational units.                                            ##
## fert_band: Band number of fertilizer data in filedata if it is a           ##
##            RasterBrick. Defaults to NULL if filedata is RasterLayer.       ##
## source_band: Band number of source information in filedata if it is a      ##
##              RasterBrick. Defaults to NULL if filedata does not include    ##
##              source information.                                           ##
## source_country_vals: Values in optional source band that denote grid cells ##
##                      with national values (as opposed to subnational       ##
##                      values). Default values are for Mueller et al. data   ##
## verbose: Whether to provide diagnostics messages. Either FALSE (only error ##
##          messages), TRUE (informational messages), or integer value > 1    ##
##          (extended informational messages).                                ##
##                                                                            ##
## Returns a RasterBrick with three layers:                                   ##
## 1. gap-filled fertilizer pattern                                           ##
## 2. code specifying at which spatial level gap-filling was conducted        ##
## 3. Number of source cells/source countries used to calculate gap-fill value##
################################################################################
gapfill_pattern <- function(filedata,
                            # Whether zero values should be set to NA, set to
                            # TRUE if dataset uses zero in cells with missing
                            # values
                            strip_zero,
                            unit_raster,
                            unit_raster_names = NULL,
                            unit_border_raster = NULL,
                            assign_grid_threshold = 5,
                            fill_regional_data = TRUE,
                            assign_country_threshold = 1,
                            country_grouping = NULL,
                            country_grouping_col = NULL,
                            is_national = FALSE,
                            fert_band = NULL,
                            source_band = NULL,
                            source_country_vals = c(3, 3.25, 5, 5.25, 5.5, 5.75,
                                                    6, 6.25, 6.5, 6.75),
                            # Whether to print warning statements
                            verbose = TRUE
                           ) {
  if (!class(filedata) %in% c("RasterLayer", "RasterBrick")) {
    stop(
      "file data must be a RasterLayer or RasterBrick.",
      "\nProvided: ", class(filedata)
    )
  }
  if (class(filedata) == "RasterLayer" &&
    ((!is.null(fert_band) && (any(fert_band > 1) || length(fert_band) > 1)) ||
    !is.null(source_band))
  ) {
    # RasterLayer only has one band by definition, cannot access more.
    stop(
      "Provided values for fert_band ",
      sQuote(ifelse(is.null(fert_band), "NULL", toString(fert_band))),
      " and source_band ",
      sQuote(ifelse(is.null(source_band), "NULL", toString(source_band))),
      " do not match filedata"
    )
  }
  if (class(filedata) == "RasterBrick" &&
    ((is.null(fert_band) || any(fert_band > nlayers(filedata))) ||
    (!is.null(source_band) && any(source_band > nlayers(filedata))))
  ) {
    # Cannot access any bands that are not included in RasterBrick
    stop(
      "Provided values for fert_band ",
      sQuote(ifelse(is.null(fert_band), "NULL", toString(fert_band))),
      " and source_band ",
      sQuote(ifelse(is.null(source_band), "NULL", toString(source_band))),
      " do not match filedata"
    )
  }
  # Check that fert_band and source_band are not identical
  if (!is.null(fert_band) && !is.null(source_band) &&
    length(intersect(fert_band, source_band)) > 0) {
    stop("fert_band and source_band must not have same value")
  }
  # Check that fert_band and source_band have same length
  if (!is.null(fert_band) && !is.null(source_band) &&
    length(fert_band) != length(source_band)
  ) {
    stop(
      "fert_band and source_band must have same length or source_band must ",
      "be NULL."
    )
  }
  # Set empty fert_band to default value of 1.
  if (is.null(fert_band))
    fert_band <- 1
  # Check type of unit_raster
  if (!class(unit_raster) %in% c("RasterLayer", "RasterBrick")) {
    stop(
      "unit_raster must be a RasterLayer or RasterBrick.",
      "\nProvided: ", class(unit_raster)
    )
  }
  # Check that non-missing grid cells are the same in all bands of unit_raster
  if (class(unit_raster) == "RasterBrick" && nlayers(unit_raster) > 1) {
    for (l in seq(2, nlayers(unit_raster))) {
      if (!identical(
        is.na(values(subset(unit_raster, l))),
        is.na(values(subset(unit_raster, 1)))
      )) {
        stop(
          "Land mask in layer", l, "of unit_raster differs from layer 1.",
          "\nPlease make sure all admin layers use the same land mask."
        )
      }
    }
  }
  if (!is_national && nlayers(unit_raster) == 1 && verbose) {
    warning(
      "You have stated that your filedata has ",
      "subnational data (is_national == FALSE) but have only provided one ",
      "level of admin data in unit_raster.",
      immediate. = TRUE
    )
  }
  if (is_national && nlayers(unit_raster) > 1 && verbose) {
    warning(
      "You have stated that your filedata is ",
      "national but have supplied ", nlayers(unit_raster), " levels of admin ",
      "data in unit_raster. Ignoring subnational levels.",
      immediate. = TRUE
    )
  }
  if (nlayers(unit_raster) > 3 && verbose) {
    warning(
      "unit_raster has ", nlayers(unit_raster),
      " layers but this code normally assumes at most 3.\n",
      "Gap-fill status codes will not be comparable with datasets that use",
      "fewer admin levels.",
      immediate. = TRUE
    )
  }
  unit_levels <- max(3, nlayers(unit_raster))
  if (nlayers(unit_raster) == 2 && verbose) {
    warning(
      "unit_raster has ", nlayers(unit_raster),
      " layers but this code normally assumes 1 or 3 layers.\n",
      "Assuming that layers correspond to admin level 1 and 0.",
      immediate. = TRUE
    )
  }
  if (unit_levels > 3 && verbose) {
    warning(
      "unit_raster has ", nlayers(unit_raster),
      " layers but this code normally assumes 1 or 3 layers.\n",
      "Gap-fill status codes may not be comparable with datasets that use ",
      "only national or 3 admin layers.",
      immediate. = TRUE
    )
  }
  # Border layer
  if (!is.null(unit_border_raster) &&
    !class(unit_border_raster) %in% c("RasterBrick", "RasterLayer")
  ) {
    stop(
      "unit_border_raster must be a RasterBrick or RasterLayer.",
      "\nProvided:", class(unit_border_raster)
    )
  }
  if (!is.null(unit_border_raster) && nlayers(unit_border_raster) != 1 &&
    nlayers(unit_border_raster) != nlayers(unit_raster)
  ) {
    # Either use one border layer for all admin layers or one border layer per
    # admin layer.
    stop("Number of layers in unit_border_raster does not match unit_raster")
  }
  # Check that unit_raster and unit_border_raster have same spatial
  # characteristics.
  if (!is.null(unit_border_raster) &&
    (any(extent(unit_border_raster) != extent(unit_raster)) ||
    any(res(unit_border_raster) != res(unit_raster)))
  ) {
    stop(
      "Spatial characteristics do not match between unit_border_raster and ",
      "unit_raster"
    )
  }
  if (class(unit_border_raster) == "RasterBrick" &&
    nlayers(unit_border_raster) == 1
  ) {
    # Convert RasterBrick to RasterLayer because single-layer brick causes
    # problems in raster package code.
    unit_border_raster <- subset(unit_border_raster, 1)
  }

  # Check optional data frame with unit names
  if (!is.null(unit_raster_names)) {
    # Note this is currently only supported for the GADM level 0-2 raster and
    # the corresponding unit name table.
    # Expect unit codes in "level*_ID" column
    value_cols <- grep(
      "level\\d+.id",
      colnames(unit_raster_names),
      ignore.case = TRUE,
      value = TRUE
    )
    if (length(value_cols) != nlayers(unit_raster)) {
      stop("unit_raster_names does not match unit_raster")
    }
    code_cols <- grep(
      "level\\d+.code",
      colnames(unit_raster_names),
      ignore.case = TRUE,
      value = TRUE
    )
    if (length(code_cols) != nlayers(unit_raster)) {
      stop("Error detecting code columns in unit_raster_names")
    }
    for (l in seq_along(value_cols)) {
      mismatch <- setdiff(
        values(subset(unit_raster, l)),
        unit_raster_names[, value_cols[l]]
      )
      if (length(mismatch) > 1 && !all(is.na(mismatch))) {
        stop(
          "Layer ", l,
          "of unit_raster has codes missing in unit_raster_names: ",
          toString(mismatch)
        )
      }
    }
    # Layer order in search: Start with smallest admin units
    unit_raster_order <- order(
      apply(
        unit_raster_names[, value_cols, drop = FALSE],
        2,
        function(indata) length(unique(indata))
      ),
      decreasing = TRUE
    )
  } else {
    # Derive layer order based on the number of unique codes in each layer
    unit_raster_order <- order(
      cellStats(unit_raster, function(indata, na.rm) length(unique(indata))),
      decreasing = TRUE
    )
  }
  # Set whether filedata has source information
  has_source <- !is.null(source_band)

  # Check if filedata is global
  if (matching_extent(
    extent(filedata),
    global_extent,
    xres(filedata),
    yres(filedata)
  )) {
    filedata <- setExtent(filedata, global_extent)
  }
  # Adjust unit_raster if it has higher resolution than filedata
  unit_raster <- match_admin_to_data(
    filedata,
    unit_raster,
    fun = modal_ties_first,
    verbose = verbose
  )
  # Adjust unit_border_raster if it has higher resolution than filedata
  if (!is.null(unit_border_raster)) {
    unit_border_raster <- match_admin_to_data(
      filedata,
      unit_border_raster,
      fun = max,
      verbose = verbose
    )
  }
  # Check if unit_raster covers full filedata
  if (xmin(unit_raster) - xmin(filedata) > xres(filedata) * 0.01 ||
    xmax(filedata) - xmax(unit_raster) > xres(filedata) * 0.01 ||
    ymin(unit_raster) - ymin(filedata) > yres(filedata) * 0.01 ||
    ymax(filedata) - ymax(unit_raster) > yres(filedata) * 0.01
  ) {
    # Crop filedata to spatial extent of unit_raster
    cat("Cropping filedata to smaller spatial extent of unit_raster\n")
    filedata <- crop(filedata, unit_raster)
  }
  # Check if filedata covers full unit_raster
  if (xmin(filedata) - xmin(unit_raster) > xres(filedata) * 0.01 ||
    xmax(unit_raster) - xmax(filedata) > xres(filedata) * 0.01 ||
    ymin(filedata) - ymin(unit_raster) > yres(filedata) * 0.01 ||
    ymax(unit_raster) - ymax(filedata) > yres(filedata) * 0.01
  ) {
    # Extend filedata to cover full spatial extent of unit_raster
    if (verbose) {
      warning(
        "filedata does not cover full spatial extent of unit_raster. ",
        "Extending filedata, first filled with NA, then subjected to ",
        "gap-filling.\n",
        "Please consider providing source data that matches unit_raster.",
        immediate. = TRUE
      )
    }
    filedata <- extend(filedata, unit_raster, value = NA)
  }
  # Check if spatial resolutions match
  file2unit <- res(filedata) / res(unit_raster)
  # Split filedata into fertilizer band and source band
  sourcedata <- NULL
  if (class(filedata) == "RasterBrick") {
    # Extract source_band
    if (!is.null(source_band)) {
      sourcedata <- subset(filedata, source_band)
      if (strip_zero) {
        # Replace 0 by NA
        sourcedata <- mask(sourcedata, sourcedata, maskvalue = 0)
      }
      # Mueller et al. NetCDF files use NaN as defined missing value which
      # causes errors when trying to access individual cell values. Reset.
      if (is.na(NAvalue(sourcedata))) {
        NAvalue(sourcedata) <- 1e20
      }
    }
    # Reduce filedata to fert_band
    filedata <- subset(filedata, fert_band)
    if (strip_zero) {
      # Replace 0 by NA
      filedata <- mask(filedata, filedata, maskvalue = 0)
    }
  } else if (strip_zero) {
    # Replace 0 by NA
    filedata <- mask(filedata, filedata, maskvalue = 0)
  }
  # Mueller et al. NetCDF files use NaN as defined missing value which
  # causes errors when trying to access individual cell values. Reset.
  if (is.na(NAvalue(filedata))) {
    NAvalue(filedata) <- 1e20
  }
  if (any(file2unit < 0.999)) {
    if (verbose) {
      warning(
        "filedata has finer resolution than unit_raster. ",
        "You should normally provide unit_raster and filedata at the same ",
        "resolution.\nTrying to aggregate filedata.",
        immediate. = TRUE
      )
    }
    scale_unit <- 1 / file2unit
    scale_unit <- ifelse(scale_unit > 1, scale_unit, 1)
    scale_unit <- round(scale_unit)
    # Use mean over source grid cells
    filedata <- aggregate(filedata, scale_unit, mean)
    # In Mueller et al. data, low source values denote data from
    # higher-resolution source, higher values from lower-resolution source.
    # Aggregate source data using highest value.
    if (!is.null(sourcedata)) {
      sourcedata <- aggregate(sourcedata, scale_unit, max)
    }
    # Update spatial scaling factor
    file2unit <- res(filedata) / res(unit_raster)
    if (any(file2unit > 1.001 & file2unit %% 1 > 0.001)) {
      stop("Resolution of filedata is not an integer multiple of unit_raster")
    }
    if (any(file2unit < 0.999 & (1 / file2unit) %% 1 > 0.001)) {
      stop("Resolution of unit_raster is not an integer multiple of filedata")
    }
  }
  if (ncell(unit_raster) != ncell(filedata)) {
    stop(
      "Error matching spatial extent and resolution of filedata and unit_raster"
    )
  }
  
  if (assign_grid_threshold < 1) {
    if (verbose) {
      warning("assign_grid_threshold cannot be < 1.", immediate. = TRUE)
    }
    assign_grid_threshold <- 1
  }
  if (assign_grid_threshold > 1 && verbose) {
    cat("assign_grid_threshold of", assign_grid_threshold, "used.\n")
  }
  if (assign_country_threshold < 1) {
    if (verbose) {
      warning("assign_country_threshold cannot be < 1.", immediate. = TRUE)
    }
    assign_country_threshold <- 1
  }
  if (assign_country_threshold > 1 && verbose) {
    cat("assign_country_threshold of", assign_country_threshold, "used.\n")
  }

  # Data preparation finished.

  # Return variables
  # Gap-filled fertilizer rates
  output_filedata <- filedata
  # Gap-filling information
  # Admin level used for gap-filling
  output_gapfillstats <- brick(filedata, nl = length(fert_band))
  output_gapfillstats[] <- NA
  # Number of cells/countries used for gap-filling
  output_gapfillsources <- brick(filedata, nl = length(fert_band))
  output_gapfillsources[] <- NA
  # Load raster objects into memory to speed up processing. Comment this part if
  # your system does not have enough memory.
  if (!inMemory(unit_raster) && ncell(unit_raster) < 1e8) {
    unit_raster <- readAll(unit_raster)
  }
  if (!is.null(unit_border_raster) && !inMemory(unit_border_raster) &&
    ncell(unit_border_raster) < 1e8
  ) {
    unit_border_raster <- readAll(unit_border_raster)
  }
  
  # Process each fert_band individually.
  for (band in seq_along(fert_band)) {
    if (length(fert_band) > 1 && verbose)
      cat("+++ Processing band", band, "of", length(fert_band), "+++\n")

    # Extract current band from input and output variables
    band_stats <- values(subset(output_gapfillstats, band))
    band_data <- values(subset(filedata, band))
    if (!is.null(sourcedata)) {
      band_source <- values(subset(sourcedata, band))
    } else {
      band_source <- NULL
    }
    band_gapfillsources <- values(subset(output_gapfillsources, band))
    output_band_data <- values(subset(output_filedata, band))

    # Mask out any cells in output_filedata that may be located outside of land
    # according to unit_raster
    outside_valid <- intersect(
      which(!is.na(band_data)),
      which(is.na(values(subset(unit_raster, 1))))
    )
    if (length(outside_valid) > 0) {
      output_band_data[outside_valid] <- NA
      if (verbose) {
        cat(
          "Removing", length(outside_valid), "non-missing values in cells",
          "not covered by unit_raster\n"
        )
      }
    }

    # Find missing and non-missing cells in filedata
    missing_cells <- intersect(
      which(is.na(band_data)),
      which(!is.na(values(subset(unit_raster, 1))))
    )
    valid_cells <- intersect(
      which(!is.na(band_data)),
      which(!is.na(values(subset(unit_raster, 1))))
    )
    
    # Table with country values, used to calculate country group averages
    if (!is.null(unit_raster_names)) {
      # Find columns in unit_raster_names containing country IDs and ISO codes.
      country_col <- grep(
        "level0.id",
        colnames(unit_raster_names),
        ignore.case = TRUE
      )
      if (length(country_col) != 1) {
        stop("Error finding country ID column in unit_raster_names")
      }
      iso_col <- grep(
        "level0.code",
        colnames(unit_raster_names),
        ignore.case = TRUE
      )
      if (length(iso_col) != 1) {
        stop("Error finding country ISO code column in unit_raster_names")
      }
      # All country codes from name table
      isocode <- unique(na.omit(unit_raster_names[, iso_col]))
      country_avg_table <- data.frame(
        code = isocode,
        apprate = rep(NA, length(isocode)),
        nsource = rep(NA, length(isocode)),
        stringsAsFactors = FALSE
      )
    } else {
      # All country codes from raster
      isocode <- na.omit(
        cellStats(
          subset(unit_raster, unit_raster_order[length(unit_raster_order)]),
          unique
        )
      )
      country_avg_table <- data.frame(
        code = formatC(isocode, format = "d"),
        apprate = rep(NA, length(isocode)),
        nsource = rep(NA, length(isocode)),
        stringsAsFactors = FALSE
      )
    }    
  
    # All cells with source data get 1
    band_stats[valid_cells] <- 1

    m_data <- which(is.na(output_band_data))
    m_gap <- which(is.na(band_stats))
    if (!identical(m_data, m_gap)) {
      stop(
        "Inconsistency in band ", band, ": ",
        length(m_data), " cells with values ",
        ifelse(length(m_data) != length(m_gap), "!= ", "== "),
        length(m_gap), " cells with gapfillstats.",
        "\nvalid_cells: ", length(valid_cells), ", missing_cells: ",
        length(missing_cells)
      )
    }
    # First loop over countries and determine whether source only has national
    # or also subnational data.
    l <- unit_raster_order[length(unit_raster_order)] # Assumes that "shortest"
    # in unit_raster is countries.
    country_layer_vals <- values(subset(unit_raster, l))
    # Determine border cells. These are all cells which contain more than one
    # country. Source data from border cells is not used because mismatches
    # between country assignment in source data and country assignment used here
    # are more likely in border cells.
    if (!is.null(unit_border_raster)) {
      if (nlayers(unit_border_raster) > 1) {
        country_border_vals <- values(subset(unit_border_raster, l))
      } else {
        country_border_vals <- values(subset(unit_border_raster, 1))
      }
    } else {
      # If unit_border_raster was not provided use dummy where all cells contain
      # only one admin unit
      country_border_vals <- rep(1, ncell(unit_raster))
    }
    multi_country_cells <- which(country_border_vals > 1)
    for (country in na.omit(unique(country_layer_vals))) {
      country_cells <- which(country_layer_vals == country)
      country_missing <- intersect(missing_cells, country_cells)
      country_valid <- intersect(valid_cells, country_cells)
      # Find country in names list
      if (!is.null(unit_raster_names)) {
        country_r <- match(country, unit_raster_names[, country_col])
        # Determine corresponding ISO code
        isocode <- regmatches(
          unit_raster_names[country_r, iso_col],
          regexpr("^[A-Z]+", unit_raster_names[country_r, iso_col])
        )
        country_name <- unit_raster_names[country_r, country_col + 2]
      } else if (!is.null(country_grouping) && !is.null(country_grouping_col)) {
        name_col <- grep(
          "country",
          colnames(country_grouping), ignore.case = TRUE
        )
        country_r <- match(country, country_grouping[, country_grouping_col])
        if (is.na(country_r) || length(name_col) == 0) {
          country_name <- country
        } else {
          country_name <- country_grouping[country_r, name_col[1]]
        }
      } else {
        country_name <- country
      }
      cat("** Country", sQuote(country_name), "**\n")
      # Mask out potential border mismatch
      country_border <- intersect(country_cells, multi_country_cells)
      if (length(country_border) > 0 &&
        length(country_border) < length(country_cells)
      ) {
        # Mask out border cells unless they make up all cells in country.
        m <- length(intersect(country_valid, country_border))
        if (verbose && m > 0) {
          cat(
            "Masking", m, "border cells in country",
            sQuote(country_name), "\n"
          )
        }
        country_valid <- setdiff(country_valid, country_border)
      }
      if (length(country_valid) >= assign_grid_threshold ||
        length(country_valid) > length(country_cells) / 4) {
        # If there are any non-missing cells in country check whether sourcedata
        # lists only national-level values.
        if ((!is.null(sourcedata) && !is.null(source_country_vals) &&
          all(band_source[country_valid] %in% source_country_vals)) ||
          length(country_missing) == 0 || is_national
        ) {
          # Only search at country level because all valid cells are in
          # source_country_vals. Also limit search to country level if
          # is_national == TRUE or if there are no missing values in country.
          # Country average needed for country_avg_table.
          search_layers <- unit_raster_order[length(unit_raster_order)]
        } else {
          # Search all available admin layers because at least some non-missing
          # cells are listed as subnational and cells need gap-filling.
          search_layers <- unit_raster_order
          if (verbose && nlayers(unit_raster) > 1)
            cat("Country has subnational data. Gap-filling subnationally.\n")
        }
        # Loop over admin levels determined in previous step
        for (l in search_layers) {
          unit_layer_vals <- values(subset(unit_raster, l))[country_cells]
          # Determine unique admin units in this layer
          unit_layer_units <- unique(na.omit(unit_layer_vals))
          if (!is.null(unit_raster_names)) {
            # Determine column in names table matching current admin level
            code_col <- grep(
              paste0("level", l - 1, ".id"),
              colnames(unit_raster_names),
              ignore.case = TRUE
            )
          } 
          has_missing <- 0
          was_filled <- 0
          if (length(country_missing) > 0) {
            if (verbose) {
              cat(
                "Trying to fill ", length(country_missing), " missing cells ",
                "in country ",
                sQuote(country_name),
                " with data from same admin unit (layer ", l, ", ",
                length(unit_layer_units), " units)\n",
                sep = ""
              )
            }
          } else {
            has_missing <- 0
          }
          # Check each admin unit in current unit_raster layer for missing cells
          # and valid cells
          for (u in unit_layer_units) {
            unit_cells <- country_cells[which(unit_layer_vals == u)]
            # Find cells in admin unit with missing values
            unit_missing <- intersect(
              unit_cells,
              country_missing
            )
            # Determine unit name if unit_raster_names was provided
            if (!is.null(unit_raster_names)) {
              # Find row for current unit in unit_raster_names
              r <- match(u, unit_raster_names[, code_col])
            }
            if (length(unit_missing) > 0)
              has_missing <- has_missing + 1
            if (length(unit_missing) > 0 ||
              l == unit_raster_order[length(unit_raster_order)]
            ) {
              # Find cells in admin unit with non-missing values
              unit_valid <- intersect(
                unit_cells,
                country_valid
              )
              if (l == unit_raster_order[length(unit_raster_order)] &&
                !is.null(sourcedata) && !is.null(source_country_vals) &&
                any(band_source[unit_valid] %in% source_country_vals)
              ) {
                # Some cells in country are marked as having national value.
                # Use only those. May still be more than one unique value in case
                # of inconsistencies between country masks.
                unit_national <- which(
                  band_source[unit_valid] %in% source_country_vals
                )
                if (length(unit_national) > length(unit_valid) / 100 &&
                  (length(unit_national) >= assign_grid_threshold ||
                  length(unit_national) >= ceiling(length(unit_cells) / 4))
                ) {
                  # At least 1% of non-missing cells in country must be marked as
                  # national to derive national average from those. Otherwise use
                  # all non-missing cells in country. Done to avoid known
                  # mis-assignment issue in GADM where admin unit borders do not
                  # match units used in Mueller et al. dataset.
                  if (verbose && length(unit_national) < length(unit_valid)) {
                    cat("Limiting search to cells marked as national values\n")
                  }
                  unit_valid <- unit_valid[unit_national]
                } else if (verbose) {
                  cat(
                    "Discard filtering data down to",
                    length(unit_national), "cells marked as national values",
                    "in source. Use all non-missing cells in country.\n"
                  )
                }
                rm(unit_national)
              }
              if (length(unit_valid) < assign_grid_threshold &&
                length(unit_valid) < ceiling(length(unit_cells) / 4)) {
                # Not enough non-missing cells in unit. Skip to next unit.
                next
              }
              if (length(unique(band_data[unit_valid])) == 1) {
                # Use value for missing cells if only one unique value in unit
                if (verbose > 1) {
                  # This prompt is only printed in case of extended verbose.
                  cat(
                    "Fill", length(unit_missing), "cells in admin unit",
                    ifelse(
                      is.null(unit_raster_names),
                      u,
                      paste(
                        sQuote(unit_raster_names[r, code_col + 2]),
                        ifelse(
                          l != unit_raster_order[length(unit_raster_order)],
                          paste0(
                            "(", unit_raster_names[country_r, "country"], ")"
                          ),
                          ""
                        )
                      )
                    ),
                    "\n"
                  )
                }
                # Fill data
                output_band_data[unit_missing] <- unique(band_data[unit_valid])
                # Set gap-filling code to 2 for Admin level 2, 3 for Admin
                # level 1, 4 for country
                band_stats[unit_missing] <- unit_levels - l + 2
                # Save number of source cells used for Admin unit value
                band_gapfillsources[unit_missing] <- length(unit_valid)
                # Success counter
                was_filled <- was_filled + 1
                if (l == unit_raster_order[length(unit_raster_order)]) {
                  # Set country value
                  r_avg <- match(
                    ifelse(is.null(unit_raster_names), u, isocode),
                    country_avg_table$code
                  )
                  country_avg_table[r_avg, "apprate"] <-
                    unique(output_band_data[unit_valid])
                  country_avg_table[r_avg, "nsource"] <- length(unit_valid)
                }
              } else if (length(unit_valid) > 0 &&
                modal(band_data[unit_valid], ties = "lowest") ==
                median(band_data[unit_valid])
              ) {
                # If there are several values in source cells, but the most
                # frequent one is equal to the median use that value for all
                # missing cells.
                # This probably indicates that admin units do not match exactly
                # with units used in source data but are likely to refer to the
                # same admin unit.
                if (verbose > 1) {
                  # This prompt is only printed in case of extended verbose.
                  cat(
                    "Fill", length(unit_missing), "cells in admin unit",
                    ifelse(
                      is.null(unit_raster_names),
                      u,
                      paste(
                        sQuote(unit_raster_names[r, code_col + 2]),
                        ifelse(
                          l != unit_raster_order[length(unit_raster_order)],
                          paste0(
                            "(", unit_raster_names[country_r, "country"], ")"
                          ),
                          ""
                        )
                      )
                    ),
                    "with median and most frequent of",
                    length(unique(band_data[unit_valid])),
                    "source values:\n"
                  )
                  print(
                    data.frame(
                      sort(table(band_data[unit_valid]), decreasing = TRUE)
                    )
                  )
                }
                # Fill data
                output_band_data[unit_missing] <- median(band_data[unit_valid])
                # Set gap-filling code to 2 for Admin level 2, 3 for Admin
                # level 1, 4 for country
                band_stats[unit_missing] <- unit_levels - l + 2
                # Save number of source cells used for Admin unit value
                band_gapfillsources[unit_missing] <- length(unit_valid)
                # Success counter
                was_filled <- was_filled + 1
                if (l == unit_raster_order[length(unit_raster_order)]) {
                  # Set country value
                  r_avg <- match(
                    ifelse(is.null(unit_raster_names), u, isocode),
                    country_avg_table$code
                  )
                  country_avg_table[r_avg, "apprate"] <-
                    median(output_band_data[unit_valid])
                  country_avg_table[r_avg, "nsource"] <- length(unit_valid)
                }
              } else if (length(unit_valid) > 0) {
                # Multiple source values without one clear dominant value
                # indicate that source data comes from multiple admin units.
                # Use median value.
                if (verbose) {
                  warning(
                    "Median value ",
                    median(band_data[unit_valid]),
                    " does not correspond to most frequent value ",
                    modal(band_data[unit_valid], ties = "lowest"),
                    " in admin unit ",
                    ifelse(
                      is.null(unit_raster_names),
                      u,
                      paste(
                        sQuote(unit_raster_names[r, code_col + 2]),
                        ifelse(
                          l != unit_raster_order[length(unit_raster_order)],
                          paste0(
                            "(", unit_raster_names[country_r, "country"], ")"
                          ),
                          ""
                        )
                      )
                    ),
                    ". Assigning median.",
                    call. = FALSE,
                    immediate. = TRUE
                  )
                  print(
                    data.frame(
                      sort(table(band_data[unit_valid]), decreasing = TRUE)
                    )
                  )
                }
                # Fill data
                output_band_data[unit_missing] <- median(band_data[unit_valid])
                # Set gap-filling code to 2 for Admin level 2, 3 for Admin
                # level 1, 4 for country
                band_stats[unit_missing] <- unit_levels - l + 2
                # Save number of source cells used for Admin unit value
                band_gapfillsources[unit_missing] <- length(unit_valid)
                # Success counter
                was_filled <- was_filled + 1
                if (l == unit_raster_order[length(unit_raster_order)]) {
                  # Set country value
                  r_avg <- match(
                    ifelse(is.null(unit_raster_names), u, isocode),
                    country_avg_table$code
                  )
                  country_avg_table[r_avg, "apprate"] <-
                    median(band_data[unit_valid])
                  country_avg_table[r_avg, "nsource"] <- length(unit_valid)
                }
              }
              # Check consistency between output_filedata and output_gapfillstats
              m_data <- which(is.na(output_band_data))
              m_gap <- which(is.na(band_stats))
              if (!identical(m_data, m_gap)) {
                stop(
                  "Inconsistency in unit ",
                  ifelse(
                    is.null(unit_raster_names),
                    u,
                    paste(
                      sQuote(unit_raster_names[r, code_col + 2]),
                      ifelse(
                        l != unit_raster_order[length(unit_raster_order)],
                        paste0(
                          "(", unit_raster_names[country_r, "country"], "): "
                        ),
                        ": "
                      )
                    )
                  ),
                  length(m_data), " cells with values ",
                  ifelse(length(m_data) != length(m_gap), "!= ", "== "),
                  length(m_gap), " cells with gapfillstats.",
                  "\nunit_valid: ", length(unit_valid), ", unit_missing: ",
                  length(unit_missing)
                )
              }
              rm(unit_valid)
            }
            rm(unit_cells, unit_missing)
          } # End loop over admin units in current layer

          # Update missing_cells. Only use cells from original data for further
          # gap-filling, therefore do not update valid_cells.
          missing_cells <- intersect(
            which(is.na(output_band_data)),
            which(!is.na(values(subset(unit_raster, 1))))
          )
          country_missing <- intersect(missing_cells, country_cells)
          if (has_missing > 0 && verbose) {
            cat(
              "Gap-filled",
              length(which(band_stats[country_cells] == unit_levels - l + 2)),
              "cells in", was_filled, "of", has_missing, "admin units.",
              ifelse(
                length(country_missing) > 0,
                paste(
                  length(country_missing), "missing values still remaining.\n"
                ),
                "\n"
              )
            )
          }
        } # End loop over admin layers
        if (length(country_missing) > 0 && verbose) {
          cat(
            length(country_missing),
            "cell(s) still contain missing values in country",
            sQuote(country_name), "\n"
          )
        }
      } else if (length(country_missing) > 0 && verbose) {
        # All cells missing in country. Need to fill with region average,
        warning(
          ifelse(length(country_valid) > 0, "Not enough", "No"),
          " non-missing values found in country ",
          sQuote(country_name),
          call. = FALSE,
          immediate. = TRUE
        )
      }
      rm(country_cells, country_missing, country_valid)
    }
    # End of country loop
    # Update output variables
    output_filedata <- setValues(output_filedata, output_band_data, layer = band)
    output_gapfillstats <- setValues(
      output_gapfillstats, band_stats, layer = band
    )
    output_gapfillsources <- setValues(
      output_gapfillsources, band_gapfillsources, layer = band
    )
  
    # Check multi-country regions to fill any remaining missing cells.
    # Find columns with region codes in country_grouping. Region averages are
    # used to fill countries with no non-missing values.
    if (!is.null(country_grouping) && !is.null(country_grouping_col) &&
      fill_regional_data && length(missing_cells) > 0
    ) {
      if (verbose) {
        cat(
          "Trying to fill any remaining missing values using average from", 
          "country groups.\n"
        )
      }
      reg_col <- grep(
        "region.code",
        colnames(country_grouping),
        ignore.case = TRUE,
        value = TRUE
      )
      if (length(reg_col) == 0 && verbose) {
        warning(
          "Could not determine columns ",
          "containing region codes in country_grouping. ",
          "Expected pattern: '*region.code'. ",
          "Column names: ", toString(sQuote(colnames(country_grouping))),
          immediate. = TRUE
        )
      }
      # Find columns with development status. Further programming assumes that
      # country_grouping distinguishes LDCs, LLDCs and SIDS, but not developing
      # and developed countries. If this changes check further code.
      dev_col <- grep(
        "develop",
        colnames(country_grouping),
        ignore.case = TRUE,
        value = TRUE
      )
      if (length(dev_col) == 0 && verbose) {
        warning(
          "Could not determine columns ",
          "containing development status in country_grouping. ",
          "Expected pattern: '*develop*'. ",
          "Column names: ", toString(sQuote(colnames(country_grouping))),
          immediate. = TRUE
        )
      }
      # Countries in each development group.
      dev_with_avg <- list()
      for (dev in dev_col) {
        dev_r <- grep("x", country_grouping[, dev], ignore.case = TRUE)
        dev_with_avg[[dev]] <- country_grouping[dev_r, country_grouping_col]
      }
      # All countries with country average value in country_avg_table
      iso_with_avg <-
        country_avg_table$code[which(!is.na(country_avg_table$apprate))]

      # Now check all countries again if they still have missing cells.
      for (country in na.omit(unique(country_layer_vals))) {
        country_cells <- which(country_layer_vals == country)
        country_missing <- intersect(missing_cells, country_cells)
        country_valid <- intersect(valid_cells, country_cells)
        # Find country in names list
        if (!is.null(unit_raster_names)) {
          country_col <- grep(
            "level0.id",
            colnames(unit_raster_names),
            ignore.case = TRUE
          )
          iso_col <- grep(
            "level0.code",
            colnames(unit_raster_names),
            ignore.case = TRUE
          )
          country_r <- match(country, unit_raster_names[, country_col])
          isocode <- regmatches(
            unit_raster_names[country_r, iso_col],
            regexpr("^[A-Z]+", unit_raster_names[country_r, iso_col])
          )
          country_name <- unit_raster_names[country_r, country_col + 2]
        } else {
          # Use country code from unit_raster to match with entry in
          # country_grouping
          isocode <- country
          # row in country_grouping
          country_r <- match(isocode, country_grouping[, country_grouping_col])
          name_col <- grep(
            "country",
            colnames(country_grouping), ignore.case = TRUE
          )
          if (is.na(country_r) || length(name_col) == 0) {
            country_name <- country
          } else {
            country_name <- country_grouping[country_r, name_col[1]]
          }
        }
        if (length(country_missing) > 0) {
          # Mask out potential border mismatch
          country_border <- intersect(country_cells, multi_country_cells)
          if (length(country_border) > 0 &&
            length(country_border) < length(country_cells)
          ) {
            # Mask out border cells unless they make up all cells in country.
            m <- length(intersect(country_valid, country_border))
            if (verbose && m > 0) {
              cat(
                "Masking", m, "border cells in country",
                sQuote(country_name), "\n"
              )
            }
            country_valid <- setdiff(country_valid, country_border)
          }
        }
        if (length(country_missing) > 0 && length(country_valid) > 0) {
          # This should not happen. Previous gap-filling algorithm should fill
          # all countries with some non-missing values completely.
          # Only exception: if country_valid < assign_grid_threshold.
          cat(
            "Missing values in country",
            sQuote(country_name),
            "after grid-based gap-filling despite the presence of",
            "non-missing values.",
            ifelse(
              length(country_valid) < assign_grid_threshold &&
                assign_grid_threshold > 1,
              "Consider lowering assign_grid_threshold.\n",
              "\n"
            )
          )
        }
        if (length(country_missing) > 0) {
          # Check if missing country is in country group data
          if (!isocode %in% country_grouping[, country_grouping_col]) {
            if (verbose) {
              cat(
                "Country", sQuote(country_name),
                "missing in country_grouping.\n"
              )
            }
            # If country is not in country group data, try to find replacement
            # in fao_gadm_country_mapping from helper/fao_gadm_country_mapping.R
            if (exists("fao_gadm_country_mapping") &&
              isocode %in% unlist(fao_gadm_country_mapping)
            ) {
              # Find all countries in fao_gadm_country_mapping that contain
              # isocode and also contain country codes actually found in
              # country_grouping. May be more than one.
              country_match <- which(
                sapply(
                  fao_gadm_country_mapping,
                  function(codes, search, all_valid) {
                    search %in% codes && any(all_valid %in% codes)
                  },
                  search = isocode,
                  all_valid = country_grouping[, country_grouping_col]
                )
              )
              # Sort found countries by number of included ISO codes, use shortest.
              if (length(country_match) > 0) {
                replacement_name <- names(
                  sort(sapply(fao_gadm_country_mapping[country_match], length))
                )[1]
                # Country in fao_gadm_country_mapping consists of several ISO
                # codes. Replace with first ISO code that is included in
                # country_grouping, assuming that main ISO code is listed first.
                isocode <- intersect(
                  fao_gadm_country_mapping[[replacement_name]],
                  country_grouping[, country_grouping_col]
                )[1]
                if (verbose) {
                  if (!is.null(unit_raster_names)) {
                    replacement_r <- match(
                      isocode,
                      unit_raster_names[, iso_col]
                    )
                    replacement_name <-
                      unit_raster_names[replacement_r, country_col + 2]
                  } else {
                    replacement_r <- match(
                      isocode,
                      country_grouping[, country_grouping_col]
                    )
                    if (is.na(replacement_r) || length(name_col) == 0) {
                      replacement_name <- isocode
                    } else {
                      replacement_name <-
                        country_grouping[replacement_r, name_col[1]]
                    }
                  }
                  cat(
                    "Replace",
                    sQuote(country_name),
                    "with",
                    sQuote(replacement_name),
                    "based on fao_gadm_country_mapping.\n"
                  )
                }
              }
            }
          }
          r_avg <- match(isocode, country_avg_table$code)
          if (is.finite(country_avg_table$apprate[r_avg])) {
            # Replacement country already has country average, use to gap-fill
            # unit.
            output_band_data[country_missing] <-
              country_avg_table$apprate[r_avg]
            band_stats[country_missing] <- unit_levels + 1
            # Save number of source cells used for Admin unit value
            band_gapfillsources[country_missing] <-
              country_avg_table$nsource[r_avg]
            if (verbose) {
              cat(
                "Country",
                sQuote(country_name),
                "filled using country average from",
                sQuote(replacement_name), "\n"
              )
            }
            # Do not add value to country_avg_table because this may create a
            # bias if repeated country average values are used to calculate
            # group averages.
            # Continue to next country.
            next
          }
          # Try to find regions that country belongs to.
          if (isocode %in% country_grouping[, country_grouping_col]) {
            group_r <- match(isocode, country_grouping[, country_grouping_col])
            region_has_values <- list()
            # Check development status of country
            dev_country <- grep(
              "x", country_grouping[group_r, dev_col], ignore.case = TRUE
            )
            # Source attribute for output_gapfillstats depending on region level.
            # By default: 5 for Intermediate Region, 6 for Sub-region, 7 for
            # Region. This assumes column order in country_grouping is Region ->
            # Sub-region -> Intermediate Region. Change if column order changes.
            gapfill_source <- unit_levels + length(reg_col) -
              seq_along(reg_col) + 2
            for (reg in seq_along(reg_col)) {
              region_has_values[[reg_col[reg]]] <- character(0)
              regcode <- country_grouping[group_r, reg_col[reg]]
              if (!is.na(regcode)) {
                # Find all other countries in region
                group_members_r <- which(
                  country_grouping[, reg_col[reg]] == regcode
                )
                group_iso <- intersect(
                  country_grouping[group_members_r, country_grouping_col],
                  iso_with_avg
                )
                # If country has development status, reduce group list to
                # countries with same development status
                if (length(dev_country) > 0) {
                  tmp_iso <- intersect(
                    group_iso,
                    unlist(dev_with_avg[dev_country])
                  )
                  if (length(tmp_iso) < assign_country_threshold) {
                    # Be less strict and allow all countries in group with any
                    # development status. This assumes that development states are
                    # "LDC", "LLDC" and "SIDS". Not be applicable anymore if
                    # country_grouping also distinguishes developing and developed
                    # countries.
                    tmp_iso <- intersect(group_iso, unlist(dev_with_avg))
                    # Add 0.25 to source attribute because we are using less strict
                    # group assignment.
                    if (length(dev_country) < length(dev_col))
                      gapfill_source[reg] <- gapfill_source[reg] + 0.25
                  }
                  group_iso <- tmp_iso
                  rm(tmp_iso)
                } else {
                  # If country has no development status, reduce group list to
                  # include only countries which also have no development status.
                  group_iso <- setdiff(group_iso, unlist(dev_with_avg))
                }
                region_has_values[[reg_col[reg]]] <- group_iso
              }
            }
            if (
              any(sapply(region_has_values, length) >= assign_country_threshold)
            ) {
              # Region with smallest number of countries with existing country
              # average
              reg <- which.min(
                sapply(
                  region_has_values,
                  function(indata) {
                    if(length(indata) >= assign_country_threshold) {
                      length(indata)
                    } else NA
                  }
                )
              )
              group_iso <- region_has_values[[reg]]
              reg_name_col <- sub(".Code", ".Name", reg_col[reg])
              if (!reg_name_col %in% colnames(country_grouping))
                reg_name_col <- reg_col[reg]
              if (verbose) {
                if (!is.null(unit_raster_names)) {
                  group_members_r <- match(
                    group_iso, unit_raster_names[, iso_col]
                  )
                  group_members_name <-
                    unit_raster_names[group_members_r, country_col + 2]
                } else {
                  group_members_r <- match(
                    group_iso,
                    country_grouping[, country_grouping_col]
                  )
                  if (length(name_col) == 0) {
                    group_members_name <- group_iso
                  } else {
                    group_members_name <-
                      country_grouping[group_members_r, name_col[1]]
                  }
                }
                cat(
                  "Using",
                  ifelse(length(group_iso) > 1, "median", "value"), "of",
                  ifelse(
                    length(group_iso) > 5,
                    paste(length(group_iso), "countries"),
                    toString(sQuote(group_members_name))
                  ),
                  "in",
                  sub(".code", "", names(reg), ignore.case = TRUE),
                  sQuote(country_grouping[group_r, reg_name_col]),
                  ifelse(
                    gapfill_source[reg] %% 1 == 0,
                    "(accounting for development status)",
                    ifelse(
                      gapfill_source[reg] %% 1 == 0.25,
                      "(partially accounting for development status)",
                      ""
                    )
                  ),
                  "as fill value for",
                  length(country_missing), "missing cells in",
                  sQuote(country_name), "\n"
                )
              }
              # Use median across countries in region group, regardless of country
              # size.
              r <- match(group_iso, country_avg_table$code)
              output_band_data[country_missing] <- median(
                country_avg_table$apprate[r]
              )
              rm(r)
              band_stats[country_missing] <- gapfill_source[reg]
              # Save number of source countries used for Admin unit value
              band_gapfillsources[country_missing] <- length(group_iso)
              # Continue to next country
              next
            }
            # Try global average if no suitable value found in regions.
            # All countries part of group.
            group_iso <- iso_with_avg
            # Source attribute for output_gapfillstats depending on region level.
            # By default: 8 for Global. Global follows after original data (1),
            # different admin units used (usually 2-4), region used (usually 5-7).
            gapfill_source <- unit_levels + length(reg_col) + 2
            # Check if country has development status
            if (isocode %in% country_grouping[, country_grouping_col]) {
              group_r <- match(isocode, country_grouping[, country_grouping_col])
              # Check development status of country
              dev_country <- grep(
                "x", country_grouping[group_r, dev_col], ignore.case = TRUE
              )
              # If country has development status, reduce group list to countries
              # with same development status
              if (length(dev_country) > 0) {
                tmp_iso <- intersect(group_iso, unlist(dev_with_avg[dev_country]))
                if (length(tmp_iso) < assign_country_threshold) {
                  # Be less strict and allow all countries in group with any
                  # development status. This assumes that development states are
                  # "LDC", "LLDC" and "SIDS". Not be applicable anymore if
                  # country_grouping also distinguishes developing and developed
                  # countries.
                  tmp_iso <- intersect(group_iso, unlist(dev_with_avg))
                  # Add 0.25 to source attribute because we are using less strict
                  # group assignment.
                  if (length(dev_country) < length(dev_col))
                    gapfill_source <- gapfill_source + 0.25
                }
                if (length(tmp_iso) >= assign_country_threshold) {
                  # Only reduce global country list to smaller list with same or
                  # similar development status if any countries are left. Otherwise,
                  # use all countries because there is no other search option left.
                  group_iso <- tmp_iso
                } else if (length(dev_country) < length(dev_col)) {
                  # Add 0.5 to source attribute because we are not using development
                  # status.
                  gapfill_source <- gapfill_source + 0.5
                }
                rm(tmp_iso)
              } else {
                # If country has no development status, reduce group list to
                # include only countries which also have no development status.
                tmp_iso <- setdiff(group_iso, unlist(dev_with_avg))
                if (length(tmp_iso) >= assign_country_threshold) {
                  # Only reduce global country list to smaller list with no
                  # development status if any countries are left. Otherwise, use all
                  # because there is no other search option left.
                  group_iso <- tmp_iso
                } else {
                  # Add 0.75 to source attribute because we are not using
                  # development status.
                  gapfill_source <- gapfill_source + 0.75
                }
                rm(tmp_iso)
              }
            } else {
              # Add 0.75 to source attribute because we are not using
              # development status.
              gapfill_source <- gapfill_source + 0.75
            }
            if (length(group_iso) >= assign_country_threshold) {
              if (verbose) {
                if (!is.null(unit_raster_names)) {
                  group_members_r <- match(
                    group_iso, unit_raster_names[, iso_col]
                  )
                  group_members_name <-
                    unit_raster_names[group_members_r, country_col + 2]
                } else {
                  group_members_r <- match(
                    group_iso,
                    country_grouping[, country_grouping_col]
                  )
                  if (length(name_col) == 0) {
                    group_members_name <- group_iso
                  } else {
                    group_members_name <-
                      country_grouping[group_members_r, name_col[1]]
                  }
                }
                cat(
                  "Using",
                  ifelse(length(group_iso) > 1, "median", "value"), "of",
                  ifelse(
                    length(group_iso) > 5,
                    paste(length(group_iso), "countries worldwide"),
                    toString(sQuote(group_members_name))
                  ),
                  ifelse(
                    gapfill_source %% 1 == 0,
                    "(accounting for development status)",
                    ifelse(
                      gapfill_source %% 1 == 0.25,
                      "(partially accounting for development status)",
                      ""
                    )
                  ),
                  "as fill value for",
                  length(country_missing), "missing cells in",
                  sQuote(country_name), "\n"
                )
              }
              # Use median across countries, regardless of country size, but maybe
              # accounting for development status.
              output_band_data[country_missing] <- median(
                country_avg_table$apprate[match(group_iso, country_avg_table$code)]
              )
              band_stats[country_missing] <- gapfill_source
              # Save number of source countries used for Admin unit value
              band_gapfillsources[country_missing] <- length(group_iso)
              # Continue to next country
              next
            }
          } else {
            # Use global data because country is not in country_grouping.
            group_iso <- iso_with_avg
            # Add 0.75 to global source attribute because we are not using
            # development status because country is not in country_grouping.
            gapfill_source <- unit_levels + length(reg_col) + 2 + 0.75
            if (length(group_iso) >= assign_country_threshold) {
              if (verbose) {
                if (!is.null(unit_raster_names)) {
                  group_members_r <- match(
                    group_iso, unit_raster_names[, iso_col]
                  )
                  group_members_name <-
                    unit_raster_names[group_members_r, country_col + 2]
                } else {
                  group_members_r <- match(
                    group_iso, country_grouping[, country_grouping_col]
                  )
                  if (length(name_col) == 0) {
                    group_members_name <- group_iso
                  } else {
                    group_members_name <-
                      country_grouping[group_members_r, name_col[1]]
                  }
                }
                cat(
                  "Using",
                  ifelse(length(group_iso) > 1, "median", "value"), "of",
                  ifelse(
                    length(group_iso) > 5,
                    paste(length(group_iso), "countries worldwide"),
                    toString(sQuote(group_members_name))
                  ),
                  ifelse(
                    gapfill_source %% 1 == 0,
                    "(accounting for development status)",
                    ifelse(
                      gapfill_source %% 1 == 0.25,
                      "(partially accounting for development status)",
                      ""
                    )
                  ),
                  "as fill value for",
                  length(country_missing), "missing cells in",
                  sQuote(country_name), "\n"
                )
              }
              # Use median across countries, regardless of country size.
              output_band_data[country_missing] <- median(
                country_avg_table$apprate[match(group_iso, country_avg_table$code)]
              )
              band_stats[country_missing] <- gapfill_source
              # Save number of source countries used for Admin unit value
              band_gapfillsources[country_missing] <- length(group_iso)
            }
          }
        }
        # End second loop over countries
        rm(country_cells, country_missing, country_valid)
      }
      # Update missing_cells
      missing_cells <- intersect(
        which(is.na(output_band_data)),
        which(!is.na(values(subset(unit_raster, 1))))
      )
    }
    
    if (length(missing_cells) > 0) {
      # Gap-filling was not successful.
      warning("Incomplete gap-filling.")
      if (verbose)
        cat(length(missing_cells), "missing cells remaining\n")
    }

    # Update output variables
    output_filedata <- setValues(output_filedata, output_band_data, layer = band)
    output_gapfillstats <- setValues(
      output_gapfillstats, band_stats, layer = band
    )
    output_gapfillsources <- setValues(
      output_gapfillsources, band_gapfillsources, layer = band
    )
  } # End of band loop
  # Find country band un unit_raster
  l <- unit_raster_order[length(unit_raster_order)]
  output_data <- brick(
    output_filedata,
    output_gapfillstats,
    output_gapfillsources,
    subset(unit_raster, l)
  )
  rm(output_filedata, output_gapfillstats, output_gapfillsources, missing_cells,
     valid_cells, band_data, band_gapfillsources, band_stats, output_band_data)
  return(output_data)
}
