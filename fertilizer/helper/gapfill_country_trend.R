################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This function can be used to fill gaps in data.frames containing country   ##
## trend time series.                                                         ##
## Parameters:                                                                ##
## data_ts: Data time series.                                                 ##
## data_level: Gap-filling level used to derive values in data_ts.            ##
## data_sources: Number of sources used to derive values in data_ts.          ##
## max_national_fill_level: Maximum code in data_level denoting countries     ##
##                          filled at national or subnational level.          ##
## add_list: ISO codes of countries to add to trend time series.              ##
## country_grouping: data.frame providing country groups.                     ##
## country_grouping_col: Column in country_grouping containing codes used in  ##
##                       data.                                                ##
## global_fill_code: Optional code used in data_level to mark years with no   ##
##                   non-zero values.                                         ##
## assign_country_threshold: Minimum number of countries required in group to ##
##                           use said group value. Can be used to avoid single##
##                           country values possibly used for many other      ##
##                           countries. Default: 1.                           ##
## verbose: Whether to provide diagnostics messages.                          ##
##                                                                            ##
## Returns a list with gap-filled versions of data_ts, data_level and         ##
## data_sources.                                                              ##
################################################################################
gapfill_country_trend <- function(data_ts,
                                  data_level,
                                  data_sources,
                                  max_national_fill_level,
                                  add_list,
                                  country_grouping,
                                  country_grouping_col,
                                  global_fill_code = NULL,
                                  assign_country_threshold = 1,
                                  verbose = TRUE
                         ) {
  if (!country_grouping_col %in% colnames(country_grouping)) {
    stop("Invalid country_grouping_col ", sQuote(country_grouping_col))
  }
  if (!identical(dimnames(data_ts), dimnames(data_level))) {
    stop("Dimension inconsistency between data_ts and data_level")
  }
  if (!identical(dimnames(data_ts), dimnames(data_sources))) {
    stop("Dimension inconsistency between data_ts and data_sources")
  }
  # Find columns in country_grouping that denote regions
  reg_col <- grep(
    "global|region.code",
    colnames(country_grouping),
    ignore.case = TRUE,
    value = TRUE
  )
  if (length(reg_col) == 0 && (length(add_list) > 0 || anyNA(data_ts))) {
    stop(
      "Could not determine columns containing region codes in ",
      "country_grouping. ",
      "Expected pattern: '*region.code'. ",
      "Column names: ", toString(sQuote(colnames(country_grouping)))
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
  # Find country name column in country_grouping.
  name_col <- grep("country", colnames(country_grouping), ignore.case = TRUE)
  # Check for optional global_fill_code
  if (!is.null(global_fill_code)) {
    # Check that all or no countries have global_fill_code in each year
    any_global <- apply(
      data_level,
      1,
      function(indata, needle) any(indata %in% needle, na.rm = TRUE),
      needle = global_fill_code
    )
    all_global <- apply(
      data_level,
      1,
      function(indata, needle) all(indata %in% needle, na.rm = TRUE),
      needle = global_fill_code
    )
    if (!identical(any_global, all_global)) {
      stop("Unexpected occurrence of global_fill_code in data_level")
    }
  }
  # Check if countries need to be added
  add_list <- setdiff(add_list, dimnames(data_ts)[[2]])
  if (length(add_list) > 0) {
    data_ts <- cbind(
      data_ts,
      array(
        dim = c(nrow(data_ts), length(add_list)),
        dimnames = list(dimnames(data_ts)[[1]], add_list)
      )
    )
    data_level <- cbind(
      data_level,
      array(
        dim = c(nrow(data_level), length(add_list)),
        dimnames = list(dimnames(data_level)[[1]], add_list)
      )
    )
    data_sources <- cbind(
      data_sources,
      array(
        dim = c(nrow(data_sources), length(add_list)),
        dimnames = list(dimnames(data_sources)[[1]], add_list)
      )
    )
  }
  if (!is.null(global_fill_code)) {
    # Flush fill years with global_fill_code
    data_ts[which(all_global), add_list] <- 0
    data_level[which(all_global), add_list] <- global_fill_code
  }
  # Find countries that need gap-filling.
  gapfill_countries <- apply(data_ts, 2, anyNA)
  # Only use countries for gap-filling that have not been filled with region
  # values themselves.
  source_countries <- apply(
    data_level,
    2,
    function(indata, limit, gf) {
      !anyNA(indata) && all(indata <= limit | indata %in% gf, na.rm = TRUE)
    },
    limit = max_national_fill_level,
    gf = global_fill_code
  )
  source_countries <- names(which(source_countries))
  # Countries in each development group.
  dev_countries <- list()
  for (dev in dev_col) {
    dev_r <- grep("x", country_grouping[, dev], ignore.case = TRUE)
    dev_countries[[dev]] <- country_grouping[dev_r, country_grouping_col]
  }
  for (isocode in names(which(gapfill_countries))) {
    missing_years <- names(which(is.na(data_ts[, isocode])))
    # Try to find regions that country belongs to.
    if (isocode %in% country_grouping[, country_grouping_col]) {
      group_r <- match(isocode, country_grouping[, country_grouping_col])
      if (length(name_col) > 0) {
        country_name <- country_grouping[group_r, name_col[1]]
      } else {
        country_name <- isocode
      }
      region_has_values <- list()
      # Check development status of country
      dev_country <- grep(
        "x", country_grouping[group_r, dev_col], ignore.case = TRUE
      )
      # Source attribute for data_level depending on region level.
      # By default: 5 for Intermediate Region, 6 for Sub-region, 7 for
      # Region. This assumes column order in country_grouping is Region ->
      # Sub-region -> Intermediate Region. Change if column order changes.
      gapfill_source <- max_national_fill_level + length(reg_col) -
        seq_along(reg_col) + 1
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
            source_countries
          )
          # If country has development status, reduce group list to countries
          # with same development status.
          if (length(dev_country) > 0) {
            tmp_iso <- intersect(
              group_iso,
              unlist(dev_countries[dev_country])
            )
            if (length(tmp_iso) < assign_country_threshold) {
              # Be less strict and allow all countries in group with any
              # development status. This assumes that development states are
              # "LDC", "LLDC" and "SIDS". Not be applicable anymore if
              # country_grouping also distinguishes developing and developed
              # countries.
              tmp_iso <- intersect(group_iso, unlist(dev_countries))
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
            group_iso <- setdiff(group_iso, unlist(dev_countries))
          }
          region_has_values[[reg_col[reg]]] <- group_iso
        }
      }
      if (
        any(sapply(region_has_values, length) >= assign_country_threshold)
      ) {
        # Region with smallest number of countries with existing country average
        reg <- which.min(
          sapply(
            region_has_values,
            function(indata) {
              if(length(indata) >= assign_country_threshold) length(indata)
              else NA
            }
          )
        )
        group_iso <- region_has_values[[reg]]
        reg_name_col <- sub(".Code", ".Name", reg_col[reg])
        if (!reg_name_col %in% colnames(country_grouping))
          reg_name_col <- reg_col[reg]
        if (verbose) {
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
          cat(
            "Using",
            ifelse(length(group_iso) > 1, "median", "value"),
            "of",
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
            sQuote(country_name),
            "\n"
          )
        }
        # Use median across countries in region group, regardless of country
        # size.
        data_ts[missing_years, isocode] <- apply(
          data_ts[missing_years, group_iso, drop = FALSE], 1, median
        )
        # Set region level used for gap-filling
        data_level[missing_years, group_iso] <- gapfill_source[reg]
        # Save number of source countries used for Admin unit value
        data_sources[missing_years, group_iso] <- length(group_iso)
      }
    }
  }
  list(data_ts = data_ts, data_level = data_level, data_sources = data_sources)
}
