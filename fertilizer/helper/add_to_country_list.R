################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This function can be used to add country entries to UNSD country listing   ##
## using other countries as template.                                         ##
## Sets name and country codes but takes region assignments and development   ##
## status from template country.                                              ##
## Parameters:                                                                ##
## country_data: data.frame with UNSD country data                            ##
## add_data: data.frame containing one row for each country to add to         ##
##           country_data                                                     ##
##                                                                            ##
## Returns data.frame conbining country_data and added countries.             ##
################################################################################
add_to_country_list <- function(country_data, add_data) {
  # First try to find columns in country_data
  name_col <- grep("country", colnames(country_data), ignore.case = TRUE)
  m49_col <- grep("m49", colnames(country_data), ignore.case = TRUE)
  iso2_col <- grep("iso.alpha2", colnames(country_data), ignore.case = TRUE)
  iso3_col <- grep("iso.alpha3", colnames(country_data), ignore.case = TRUE)
  for (col in c("name_col", "m49_col", "iso2_col", "iso3_col")) {
    if (length(get(col)) != 1) {
      stop("Cannot determine ", sQuote(col), "in country_data")
    }
  }
  # Expected columns in add_data
  add_columns <- c("name", "m49", "iso2", "iso3", "template")
  if (!all(add_columns %in% colnames(add_data))) {
    stop(
      "Expected column(s) ",
      toString(sQuote(setdiff(add_columns, colnames(add_data)))),
      " missing in add_data"
    )
  }
  if (nrow(add_data) == 0)
    return(country_data)
  for (r in seq_len(nrow(add_data))) {
    if (add_data[r, "name"] %in% country_data[, name_col] ||
      add_data[r, "m49"] %in% country_data[, m49_col] || 
      add_data[r, "iso2"] %in% country_data[, iso2_col] ||
      add_data[r, "iso3"] %in% country_data[, iso3_col]
    ) {
      name_match <- match(add_data[r, "name"], country_data[, name_col])
      code_match <- match(add_data[r, "m49"], country_data[, m49_col])
      iso2_match <- match(add_data[r, "iso2"], country_data[, iso2_col])
      iso3_match <- match(add_data[r, "iso3"], country_data[, iso3_col])
      warning(
        "Line ", r, " in add_data for country ", sQuote(add_data[r, "name"]),
        " overlaps with line ",
        toString(unique(c(name_match, code_match, iso2_match, iso3_match))),
        " in country_data. Skipping.",
        immediate. = TRUE
      )
    } else if (!add_data[r, "template"] %in% country_data[, iso3_col]) {
      warning(
        "Template code for line ", r, " in add_data ",
        sQuote(add_data[r, "template"]),
        " missing in country_data. Cannot add country.",
        immediate. = TRUE
      )
    } else {
      # Find template in country_data
      template_row <- match(add_data[r, "template"], country_data[, iso3_col])
      template_data <- country_data[template_row, ]
      # Replace with final values
      template_data[, c(name_col, m49_col, iso2_col, iso3_col)] <- 
        add_data[r, c("name", "m49", "iso2", "iso3")]
      # Add row to country_data
      country_data <- rbind(
        country_data,
        template_data,
        stringsAsFactors = FALSE
      )
    }
    cat(
      "Add to country_data:",
      toString(
        country_data[nrow(country_data), c(name_col, m49_col, iso3_col)]
      ),
      "\n"
    )
  }
  return(country_data)
}
