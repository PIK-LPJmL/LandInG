################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This file contains several fixes related to admin unit datasets and UNSD   ##
## country group data. These fixes have been put together for the dataset     ##
## versions available at the time of development. Check if they are still     ##
## valid.                                                                     ##
################################################################################

################################################################################
## List with additional codes for GADM units                                  ##
# Check for Taiwan and a few other ISO codes from GADM data in UNSD
# country_group_data. If not present, add to listing.
add_list_gadm <- rbind(
  # Use China as template for Taiwan (development status, world regions)
  data.frame(
    name = "Taiwan", m49 = 158, iso2 = "TW", iso3 = "TWN", template = "CHN"
  ),
  # Caspian Sea; bordered by Azerbaijan, Kazakhstan, Iran, Russia and
  # Turkmenistan. HYDE only has land cells in Caspian Sea along Russian
  # shoreline so use Russia as template.
  data.frame(
    name = "Caspian Sea", m49 = -1, iso2 = "XC", iso3 = "XCA", template = "RUS"
  ),
  # Paracel Islands, claimed by China, Taiwan and Vietnam, use Antarctica as
  # template to assign to none. Not relevant for agriculture.
  data.frame(
    name = "Paracel Islands", m49 = -2, iso2 = "XP", iso3 = "XPI",
    template = "ATA"
  ),
  # Spratly Islands, claimed by China, Malaysia, Philippines, Taiwan, Vietnam,
  # use Antarctica to assign to none. Not relevant for agriculture.
  data.frame(
    name = "Spratly Islands", m49 = -3, iso2 = "XS", iso3 = "XSP",
    template = "ATA"
  )
)
## Optional data.frame with ISO code replacements. Use this if you cannot add ##
## and ISO code to the list above and want to replace it with another ISO code##
## that is in the admin dataset.                                              ##
ccode_replacement_gadm <- rbind(
  # Dummy row illustrating format
  data.frame(source = character(0), replacement = character(0)),
  # Add any replacement rules here.
  stringsAsFactors = FALSE
)
################################################################################

################################################################################
## List with additional codes for LUH2 units                                  ##
## Check for Taiwan in UNSD country_group_data. If not present, add to        ##
## listing using regions and development status for China. Also add           ##
## definitions of former countries used in LUH2.                              ##
## Column "m49" refers to country code used in LUH2.                          ##
add_list_luh2 <- rbind(
  # Use China as template for Taiwan (development status, regions)
  data.frame(
    name = "Taiwan", m49 = 158, iso2 = "TW", iso3 = "TWN", template = "CHN"
  ),
  # Use Sudan after secession of South Sudan as template for Sudan before
  # secession; dummy ISO codes (also make sure to avoid overlapping iso2 with
  # Spratly Islands above)
  data.frame(
    name = "Sudan (former)", m49 = 736, iso2 = "XD", iso3 = "XSD",
    template = "SDN"
  ),
  # LUH2 uses former Soviet Union as one unit; use Russian Federation as
  # template
  data.frame(
    name = "USSR", m49 = 901, iso2 = "SU", iso3 = "SUN", template = "RUS"
  ),
  # Netherland Antilles, use Curacao as template
  data.frame(
    name = "Netherland Antilles", m49 = 530, iso2 = "AN", iso3 = "ANT",
    template = "CUW"
  ),
  # LUH2 groups Czechia and Slovakia together, use Czechia as template
  data.frame(
    name = "Czechoslovakia", m49 = 903, iso2 = "CS", iso3 = "CSK",
    template = "CZE"
  ),
  # LUH2 has former Yugoslavia, use Serbia as template (all successor states
  # except Macedonia currently have same development status in UNSD listing)
  data.frame(
    name = "Yugoslavia", m49 = 902, iso2 = "YU", iso3 = "YUG",
    template = "SRB"
  )
)
## Try to fix country codes used in LUH2 country mask that are not in UNSD    ##
## country group listing.                                                     ##
## Set a new valid code to allow associating countries with country groups.   ##
## Use only if you cannot add country to list in step above.                  ##
ccode_replacement_luh2 <- rbind(
  # Single cell on Canadian/US border, assign to Canada
  data.frame(source = 125, replacement = 124),
  # LUH2 has separate code for Alaska, assign to USA.
  data.frame(source = 900, replacement = 840)
)
################################################################################
