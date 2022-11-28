################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This file creates compound_countries, a list of countries that seize to    ##
## exist and split into smaller countries at some point in time. Make sure to ##
## keep this in sync with FAOSTAT country definitions list.                   ##
################################################################################
if (!exists("fix_cyprus")) {
  # Should normally be set in landuse_setup.R
  fix_cyprus <- FALSE
}
compound_countries <- list(
  "Belgium-Luxembourg" = c("Belgium", "Luxembourg"),
  "Czechoslovakia" = c("Czechia", "Slovakia"),
  "Ethiopia PDR" = c("Ethiopia", "Eritrea"),
  "Netherlands Antilles (former)" = c(
    "Bonaire, Sint Eustatius and Saba",
    "Curacao",
    "Sint Maarten (Dutch part)"
  ),
  "Pacific Islands Trust Territory" = c(
    "Northern Mariana Islands",
    "Micronesia (Federated States of)",
    "Marshall Islands",
    "Palau"
  ),
  "Serbia and Montenegro" = c("Serbia", "Montenegro"),
  "Sudan (former)" = c("South Sudan", "Sudan"),
  "USSR" = c(
    "Russian Federation",
    "Ukraine",
    "Belarus",
    "Armenia",
    "Azerbaijan",
    "Estonia",
    "Georgia",
    "Kazakhstan",
    "Kyrgyzstan",
    "Latvia",
    "Lithuania",
    "Republic of Moldova",
    "Tajikistan",
    "Turkmenistan",
    "Uzbekistan"
  ),
  "Yugoslav SFR" = c(
    "Bosnia and Herzegovina",
    "Croatia",
    "The former Yugoslav Republic of Macedonia",
    "Slovenia",
    "Serbia and Montenegro",
    "North Macedonia"
  )
  # Note that "Serbia and Montenegro" is a part of "Yugoslav SFR", but is also
  # a compound country itself.
  # Note also: "The former Yugoslav Republic of Macedonia" and "North Macedonia"
  # refer to the same country; recently renamed in FAOSTAT.
)

# Fix for Cyprus
# If future versions of FAOSTAT distinguish Northern and Southern Cyprus, update
# names in mapping file if necessary.
if (fix_cyprus) {
  compound_countries[["Cyprus"]] <- c("Cyprus_Northern", "Cyprus_Southern")
}
