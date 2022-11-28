################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This file creates fao_gadm_country_mapping, a list linking country names   ##
## from FAOSTAT to ISO codes used in GADM. This list has been created for     ##
## GADM version 3.6.                                                          ##
## Countries are sometimes renamed in FAOSTAT so this list may need to be     ##
## updated manually to match names used there.                                ##
## This list includes countries that start to or seize to exist at some point ##
## in time such as "USSR".                                                    ##
################################################################################
if (!exists("fix_cyprus")) {
  # Should normally be set in landuse_setup.R
  fix_cyprus <- FALSE
}
fao_gadm_country_mapping <- list(
  "Afghanistan" = "AFG",
  "Albania" = "ALB",
  "Algeria" = "DZA",
  "American Samoa" = "ASM",
  "Andorra" = "AND",
  "Angola" = "AGO",
  "Anguilla" = "AIA",
  "Antigua and Barbuda" = "ATG",
  "Argentina" = "ARG",
  "Armenia" = "ARM",
  "Aruba" = "ABW",
  "Australia" = c("AUS"),
  "Austria" = "AUT",
  "Azerbaijan" = "AZE",
  "Bahamas" = "BHS",
  "Bahrain" = "BHR",
  "Bangladesh" = "BGD",
  "Barbados" = "BRB",
  "Belarus" = "BLR",
  "Belgium" = "BEL",
  "Belgium-Luxembourg" = c("BEL", "LUX"),
  "Belize" = "BLZ",
  "Benin" = "BEN",
  "Bermuda" = "BMU",
  "Bhutan" = "BTN",
  "Bolivia (Plurinational State of)" = "BOL",
  "Bonaire, Sint Eustatius and Saba" = "BES",
  "Bosnia and Herzegovina" = "BIH",
  "Botswana" = "BWA",
  "Brazil" = c("BRA"),
  "British Virgin Islands" = "VGB",
  "Brunei Darussalam" = "BRN",
  "Bulgaria" = "BGR",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cabo Verde" = "CPV",
  "Cambodia" = "KHM",
  "Cameroon" = "CMR",
  "Canada" = "CAN",
  "Cayman Islands" = "CYM",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "Channel Islands" = c("GGY", "JEY"),
  # Guernsey & Jersey, missing in MIRCA, part of 992000 "Antarctica, Rest of
  # Islands", but part of Guernsey also allocated to France;
  "Chile" = "CHL",
  "China" = c("CHN", "HKG", "MAC", "TWN"),
  # Seems to be all of China, Hong Kong, Macao and Taiwan
  "China, Hong Kong SAR" = "HKG",
  "China, Macao SAR" = "MAC",
  # not in MIRCA, has no cropland according to FAOSTAT
  "China, mainland" = "CHN",
  # MIRCA provinces of China except "China_Hong_Kong"
  "China, Taiwan Province of" = "TWN",
  "Colombia" = "COL",
  "Comoros" = "COM",
  "Congo" = "COG",
  "Cook Islands" = "COK",
  "Costa Rica" = "CRI",
  "Cote d'Ivoire" = "CIV",
  "Croatia" = "HRV",
  "Cuba" = "CUB",
  "Curacao" = "CUW",
  "Cyprus" = c("CYP", "XAD", "XNC"),
  # XAD = Akrotiri and Dhekelia, two British military bases on Cyprus
  # XNC = Northern Cyprus (only recognized by Turkey as being Turkish)
  "Czechia" = "CZE",
  "Czechoslovakia" = c("CZE", "SVK"),
  "Democratic People's Republic of Korea" = "PRK",
  "Democratic Republic of the Congo" = "COD",
  "Denmark" = "DNK",
  "Djibouti" = "DJI",
  "Dominica" = "DMA",
  "Dominican Republic" = "DOM",
  "Ecuador" = "ECU",
  "Egypt" = "EGY",
  "El Salvador" = "SLV",
  "Equatorial Guinea" = "GNQ",
  "Eritrea" = "ERI",
  "Estonia" = "EST",
  "Eswatini" = "SWZ", # Swaziland
  "Ethiopia" = "ETH",
  "Ethiopia PDR" = c("ETH", "ERI"),
  # Eritrea and Ethiopia
  "Falkland Islands (Malvinas)" = "FLK",
  "Faroe Islands" = "FRO",
  "Fiji" = "FJI",
  "Finland" = c("FIN", "ALA"),
  # Aland is separate country in GADM, but is (autonomous) region of Finland
  "France" = c("FRA", "XCL", "BLM"),
  # XCL = Clipperton Island, BLM = Saint-Barthelemy
  # Note: Additional territories mentioned here have no cropland according to
  # HYDE; Saint-Martin individual entry below
  "French Guiana" = "GUF",
  "French Guyana" = "GUF",
  # French Guiana/Guyana renamed at some point, keeping both names
  "French Polynesia" = "PYF",
  "Gabon" = "GAB",
  "Gambia" = "GMB",
  "Georgia" = "GEO",
  "Germany" = "DEU",
  "Ghana" = "GHA",
  "Gibraltar" = "GIB",
  # Missing in MIRCA, is part of Spain there
  "Greece" = "GRC",
  "Greenland" = "GRL",
  "Grenada" = "GRD",
  "Guadeloupe" = "GLP",
  "Guam" = "GUM",
  "Guatemala" = "GTM",
  "Guinea" = "GIN",
  "Guinea-Bissau" = "GNB",
  "Guyana" = "GUY",
  "Haiti" = "HTI",
  "Holy See" = "VAT",
  "Honduras" = "HND",
  "Hungary" = "HUN",
  "Iceland" = "ISL",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Iran (Islamic Republic of)" = "IRN",
  "Iraq" = "IRQ",
  "Ireland" = "IRL",
  "Isle of Man" = "IMN",
  # Missing in MIRCA, part of 992000 "Antarctica, Rest of Islands";
  # has some cropland
  "Israel" = "ISR",
  "Italy" = "ITA",
  "Jamaica" = "JAM",
  "Japan" = "JPN",
  "Jordan" = "JOR",
  "Kazakhstan" = "KAZ",
  "Kenya" = "KEN",
  "Kiribati" = "KIR",
  "Kuwait" = "KWT",
  "Kyrgyzstan" = "KGZ",
  "Lao People's Democratic Republic" = "LAO",
  "Latvia" = "LVA",
  "Lebanon" = "LBN",
  "Lesotho" = "LSO",
  "Liberia" = "LBR",
  "Libya" = "LBY",
  "Liechtenstein" = "LIE",
  "Lithuania" = "LTU",
  "Luxembourg" = "LUX",
  "Madagascar" = "MDG",
  "Malawi" = "MWI",
  "Malaysia" = "MYS",
  "Maldives" = "MDV",
  "Mali" = "MLI",
  "Malta" = "MLT",
  "Marshall Islands" = "MHL",
  "Martinique" = "MTQ",
  "Mauritania" = "MRT",
  "Mauritius" = "MUS",
  "Mayotte" = "MYT",
  "Mexico" = "MEX",
  "Micronesia (Federated States of)" = "FSM",
  "Mongolia" = "MNG",
  "Montenegro" = "MNE",
  "Montserrat" = "MSR",
  "Morocco" = "MAR",
  "Mozambique" = "MOZ",
  "Myanmar" = "MMR",
  "Namibia" = "NAM",
  "Nauru" = "NRU",
  "Nepal" = "NPL",
  "Netherlands" = "NLD",
  "Netherlands Antilles (former)" = c("BES", "CUW", "SXM"),
  # Bonaire, Sint Eustatius, Saba; Curacao; Sint Maarten
  "New Caledonia" = "NCL",
  "New Zealand" = "NZL",
  "Nicaragua" = "NIC",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "Niue" = "NIU",
  "Norfolk Island" = "NFK",
  "Northern Mariana Islands" = "MNP",
  "Norway" = c("NOR"),
  "Occupied Palestinian Territory" = "PSE",
  "Palestine" = "PSE",
  # Renamed between January 2019 and September 2019, keeping both names
  "Oman" = "OMN",
  "Pacific Islands Trust Territory" = c("MNP", "FSM", "MHL", "PLW"),
  # Northern Mariana Islands, Micronesia (Federated States of), Marshall Islands
  # & Palau
  "Pakistan" = "PAK",
  "Palau" = "PLW",
  "Panama" = "PAN",
  "Papua New Guinea" = "PNG",
  "Paraguay" = "PRY",
  "Peru" = "PER",
  "Philippines" = "PHL",
  "Pitcairn Islands" = "PCN",
  "Pitcairn" = "PCN",
  # Renamed at some point, keeping both names
  "Poland" = "POL",
  "Portugal" = "PRT",
  "Puerto Rico" = "PRI",
  "Qatar" = "QAT",
  "Republic of Korea" = "KOR",
  "Republic of Moldova" = "MDA",
  "Reunion" = "REU",
  "Romania" = "ROU",
  "Russian Federation" = "RUS",
  "Rwanda" = "RWA",
  "Saint Barthelemy" = "BLM",
  "Saint Helena, Ascension and Tristan da Cunha" = "SHN",
  "Saint Kitts and Nevis" = "KNA",
  "Saint Lucia" = "LCA",
  "Saint Pierre and Miquelon" = "SPM",
  "Saint Vincent and the Grenadines" = "VCT",
  "Samoa" = "WSM",
  "San Marino" = "SMR",
  "Sao Tome and Principe" = "STP",
  "Saudi Arabia" = "SAU",
  "Senegal" = "SEN",
  "Serbia" = c("SRB", "XKO"),
  # Separate Serbia and Kosovo in GADM
  "Serbia and Montenegro" = c("MNE", "SRB", "XKO"),
  "Seychelles" = "SYC",
  "Sierra Leone" = "SLE",
  "Singapore" = "SGP",
  "Sint Maarten (Dutch part)" = "SXM",
  "Slovakia" = "SVK",
  "Slovenia" = "SVN",
  "Solomon Islands" = "SLB",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "South Sudan" = "SSD",
  "Spain" = "ESP",
  "Sri Lanka" = "LKA",
  "Sudan" = "SDN",
  "Sudan (former)" = c("SDN", "SSD"),
  # Both parts, Sudan and South Sudan
  "Suriname" = "SUR",
  "Sweden" = "SWE",
  "Switzerland" = "CHE",
  "Syrian Arab Republic" = "SYR",
  "Tajikistan" = "TJK",
  "Thailand" = "THA",
  "The former Yugoslav Republic of Macedonia" = "MKD",
  "North Macedonia" = "MKD",
  # Renamed between January 2019 and September 2019, keeping both names
  "Timor-Leste" = "TLS",
  "Togo" = "TGO",
  "Tokelau" = "TKL",
  "Tonga" = "TON",
  "Trinidad and Tobago" = "TTO",
  "Tunisia" = "TUN",
  "Turkey" = "TUR",
  "Turkmenistan" = "TKM",
  "Turks and Caicos Islands" = "TCA",
  "Tuvalu" = "TUV",
  "Uganda" = "UGA",
  "Ukraine" = "UKR",
  "United Arab Emirates" = "ARE",
  "United Kingdom" = c("GBR"),
  "United Kingdom of Great Britain and Northern Ireland" = c("GBR"),
  # Renamed at some point, keeping both names
  "United Republic of Tanzania" = "TZA",
  "United States of America" = c("USA"),
  "United States Virgin Islands" = "VIR",
  "Uruguay" = "URY",
  "USSR" = c(
    "RUS", "UKR", "BLR", "ARM", "AZE", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU",
    "MDA", "TJK", "TKM", "UZB"
  ),
  # Russian Federation, Ukraine, Belarus, Armenia, Azerbaijan, Estonia, Georgia,
  # Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Republic of Moldova, Tajikistan,
  # Turkmenistan, Uzbekistan
  "Uzbekistan" = "UZB",
  "Vanuatu" = "VUT",
  "Venezuela (Bolivarian Republic of)" = "VEN",
  "Viet Nam" = "VNM",
  "Wallis and Futuna Islands" = "WLF",
  "Western Sahara" = "ESH",
  "Yemen" = "YEM",
  "Yugoslav SFR" = c("BIH", "HRV", "MKD", "SVN", "SRB", "XKO", "MNE"),
  # Bosnia and Herzegovina, Croatia, The former Yugoslav Republic of Macedonia,
  # Slovenia, Serbia [separate Serbia and Kosovo], Montenegro
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE",
  # The following are not in FAO production or land use data but are found in
  # FAO groups and in GADM
  "Bouvet Island" = "BVT",
  # Dependency of Norway, no cropland in HYDE
  "British Indian Ocean Territory" = "IOT",
  # Overseas territory of UK, no cropland in HYDE
  "Christmas Island" = "CXR",
  # Australian external territory, no cropland in HYDE
  "Cocos (Keeling) Islands" = "CCK",
  # Australian external territory, no cropland in HYDE
  "French Southern and Antarctic Territories" = "ATF",
  # No cropland in HYDE
  "Heard and McDonald Islands" = "HMD",
  # Australian external territory, no cropland in HYDE
  "Monaco" = "MCO",
  # No cropland in HYDE
  "Saint-Martin (French Part)" = "MAF",
  "Saint-Martin (French part)" = "MAF",
  # Only in FAO country groups, but has cropland in HYDE
  # Renamed at some point, keeping both names
  "South Georgia and the South Sandwich Islands" = "SGS",
  # Overseas territory of UK, no cropland in HYDE
  "Svalbard and Jan Mayen Islands" = "SJM",
  # Jurisdictions of Norway, no cropland in HYDE
  "US Minor Is." = "UMI"
  # No cropland in HYDE
)
################################################################################


################################################################################
## Cyprus in FAOSTAT seems to cover all of Cyprus until ca. 1974 and then     ##
## Southern Cyprus. If a newer version of FAOSTAT changes that make sure to   ##
## update mapping.                                                            ##
## Since HYDE currently uses FAOSTAT cropland for "Cyprus" for all of Cyprus  ##
## even after 1974, option to not fix FAOSTAT.                                ##
if (fix_cyprus) {
  fao_gadm_country_mapping[["Cyprus_Northern"]] <- "XNC"
  # not in FAOSTAT
  fao_gadm_country_mapping[["Cyprus_Southern"]] <- c("CYP", "XAD")
  # Cyprus data in FAOSTAT seems to refer only to Southern Cyprus starting in
  # ca. 1975
  # XAD = Akrotiri and Dhekelia, two British military bases on Cyprus
}
################################################################################
