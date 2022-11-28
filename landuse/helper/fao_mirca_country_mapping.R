################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This file creates fao_mirca_country_mapping and fao_mirca_region_mapping,  ##
## two lists linking country names in FAOSTAT to unit codes used by MIRCA2000 ##
## Countries are sometimes renamed in FAOSTAT so these lists may need to be   ##
## updated manually to match names used there.                                ##
## This list includes countries that start to or seize to exist at some point ##
## in time such as "USSR".                                                    ##
################################################################################

################################################################################
## FAO country mapping                                                        ##
fao_mirca_country_mapping <- list(
  "Afghanistan" = 4000,
  "Albania" = 8000,
  "Algeria" = 12000,
  "American Samoa" = 16000,
  "Andorra" = 20000,
  "Angola" = 24000,
  "Anguilla" = 660000,
  "Antigua and Barbuda" = 28000,
  "Argentina" = 32001:32024,
  "Armenia" = 51000,
  "Aruba" = 533000,
  "Australia" = 36001:36008,
  "Austria" = 40000,
  "Azerbaijan" = 31000,
  "Bahamas" = 44000,
  "Bahrain" = 48000,
  "Bangladesh" = 50000,
  "Barbados" = 52000,
  "Belarus" = 112000,
  "Belgium" = 56000,
  "Belgium-Luxembourg" = c(56000, 442000),
  "Belize" = 84000,
  "Benin" = 204000,
  "Bermuda" = 60000,
  "Bhutan" = 64000,
  "Bolivia (Plurinational State of)" = 68000,
  "Bonaire, Sint Eustatius and Saba" = integer(0),
  # Only part of Netherlands Antilles (former) in MIRCA
  "Bosnia and Herzegovina" = 70000,
  "Botswana" = 72000,
  "Brazil" = seq(76001, 76027),
  "British Virgin Islands" = 92000,
  "Brunei Darussalam" = 96000,
  "Bulgaria" = 100000,
  "Burkina Faso" = 854000,
  "Burundi" = 108000,
  "Cabo Verde" = 132000,
  "Cambodia" = 116000,
  "Cameroon" = 120000,
  "Canada" = 124000,
  "Cayman Islands" = 136000,
  "Central African Republic" = 140000,
  "Chad" = 148000,
  "Channel Islands" = integer(0),
  # Guernsey & Jersey, missing in MIRCA, part of 992000 "Antarctica, Rest of
  # Islands", but part of Guernsey also allocated to France; some cropland on
  # Jersey
  "Chile" = 152000,
  "China" = c(seq(156001, 156031), 158000),
  # Seems to be all of China, Hong Kong, Macao and Taiwan
  "China, Hong Kong SAR" = 156031,
  "China, Macao SAR" = integer(0),
  # Not in MIRCA, has no cropland according to FAOSTAT
  "China, mainland" = seq(156001, 156030),
  # MIRCA provinces of China except "China_Hong_Kong"
  "China, Taiwan Province of" = 158000,
  "Colombia" = 170000,
  "Comoros" = 174000,
  "Congo" = 178000,
  "Cook Islands" = 184000,
  "Costa Rica" = 188000,
  "Cote d'Ivoire" = 384000,
  "Croatia" = 191000,
  "Cuba" = 192000,
  "Curacao" = integer(0),
  # Only part of Netherlands Antilles (former) in MIRCA
  "Cyprus" = 196000,
  # Note: Neither MIRCA nor FAOSTAT distinguish between Northern and Southern
  # Cyprus, but GADM does.
  # If you add Northern and Southern Cyprus make sure that names match between
  # MIRCA and GADM mapping
  "Czechia" = 203000,
  "Czechoslovakia" = c(203000, 703000),
  "Democratic People's Republic of Korea" = 408000,
  "Democratic Republic of the Congo" = 180000,
  "Denmark" = 208000,
  "Djibouti" = 262000,
  "Dominica" = 212000,
  "Dominican Republic" = 214000,
  "Ecuador" = 218000,
  "Egypt" = 818000,
  "El Salvador" = 222000,
  "Equatorial Guinea" = 226000,
  "Eritrea" = 232000,
  "Estonia" = 233000,
  "Eswatini" = 748000,
  # Swaziland
  "Ethiopia" = 231000,
  "Ethiopia PDR" = c(231000, 232000),
  # Eritrea and Ethiopia
  "Falkland Islands (Malvinas)" = 238000,
  "Faroe Islands" = 234000,
  "Fiji" = 242000,
  "Finland" = 246000,
  "France" = 250000,
  "French Guiana" = 254000,
  "French Guyana" = 254000,
  # French Guiana/Guyana renamed at some point, keeping both names
  "French Polynesia" = 258000,
  "Gabon" = 266000,
  "Gambia" = 270000,
  "Georgia" = 268000,
  "Germany" = 276000,
  "Ghana" = 288000,
  "Gibraltar" = integer(0),
  # Missing in MIRCA, is part of Spain
  "Greece" = 300000,
  "Greenland" = 304000,
  "Grenada" = 308000,
  "Guadeloupe" = 312000,
  "Guam" = 316000,
  "Guatemala" = 320000,
  "Guinea" = 324000,
  "Guinea-Bissau" = 624000,
  "Guyana" = 328000,
  "Haiti" = 332000,
  "Holy See" = integer(0),
  # Missing in MIRCA
  "Honduras" = 340000,
  "Hungary" = 348000,
  "Iceland" = 352000,
  "India" = 356001:356035,
  "Indonesia" = c(360001, 360002),
  "Iran (Islamic Republic of)" = 364000,
  "Iraq" = 368000,
  "Ireland" = 372000,
  "Isle of Man" = integer(0),
  # Missing in MIRCA, part of 992000 "Antarctica, Rest of Islands";
  # has some cropland
  "Israel" = 376000,
  "Italy" = 380000,
  "Jamaica" = 388000,
  "Japan" = 392000,
  "Jordan" = 400000,
  "Kazakhstan" = 398000,
  "Kenya" = 404000,
  "Kiribati" = 296000,
  "Kuwait" = 414000,
  "Kyrgyzstan" = 417000,
  "Lao People's Democratic Republic" = 418000,
  "Latvia" = 428000,
  "Lebanon" = 422000,
  "Lesotho" = 426000,
  "Liberia" = 430000,
  "Libya" = 434000,
  "Liechtenstein" = 438000,
  "Lithuania" = 440000,
  "Luxembourg" = 442000,
  "Madagascar" = 450000,
  "Malawi" = 454000,
  "Malaysia" = 458000,
  "Maldives" = 462000,
  "Mali" = 466000,
  "Malta" = 470000,
  "Marshall Islands" = 584000,
  "Martinique" = 474000,
  "Mauritania" = 478000,
  "Mauritius" = 480000,
  "Mayotte" = 175000,
  "Mexico" = 484000,
  "Micronesia (Federated States of)" = 583000,
  "Mongolia" = 496000,
  "Montenegro" = 499000,
  "Montserrat" = 500000,
  "Morocco" = 504000,
  "Mozambique" = 508000,
  "Myanmar" = 104000,
  "Namibia" = 516000,
  "Nauru" = 520000,
  "Nepal" = 524000,
  "Netherlands" = 528000,
  "Netherlands Antilles (former)" = 530000,
  "New Caledonia" = 540000,
  "New Zealand" = 554000,
  "Nicaragua" = 558000,
  "Niger" = 562000,
  "Nigeria" = 566000,
  "Niue" = 570000,
  "Norfolk Island" = 574000,
  "Northern Mariana Islands" = 580000,
  "Norway" = 578000,
  "Occupied Palestinian Territory" = 275000,
  "Palestine" = 275000,
  # Renamed between January 2019 and September 2019, keeping both names
  "Oman" = 512000,
  "Pacific Islands Trust Territory" = c(580000, 583000, 584000, 585000),
  # Northern Mariana Islands, Micronesia (Federated States of), Marshall Islands
  # & Palau
  "Pakistan" = 586000,
  "Palau" = 585000,
  "Panama" = 591000,
  "Papua New Guinea" = 598000,
  "Paraguay" = 600000,
  "Peru" = 604000,
  "Philippines" = 608000,
  "Pitcairn Islands" = 612000,
  "Pitcairn" = 612000,
  # Renamed at some point, keeping both names
  "Poland" = 616000,
  "Portugal" = 620000,
  "Puerto Rico" = 630000,
  "Qatar" = 634000,
  "Republic of Korea" = 410000,
  "Republic of Moldova" = 498000,
  "Reunion" = 638000,
  "Romania" = 642000,
  "Russian Federation" = 643000,
  "Rwanda" = 646000,
  "Saint Barthelemy" = integer(0),
  # Missing in MIRCA
  "Saint Helena, Ascension and Tristan da Cunha" = 654000,
  "Saint Kitts and Nevis" = 659000,
  "Saint Lucia" = 662000,
  "Saint Pierre and Miquelon" = 666000,
  "Saint Vincent and the Grenadines" = 670000,
  "Samoa" = 882000,
  "San Marino" = 674000,
  "Sao Tome and Principe" = 678000,
  "Saudi Arabia" = 682000,
  "Senegal" = 686000,
  "Serbia" = 688000, # incl. Kosovo
  "Serbia and Montenegro" = c(688000, 499000),
  "Seychelles" = 690000,
  "Sierra Leone" = 694000,
  "Singapore" = 702000,
  "Sint Maarten (Dutch part)" = integer(0),
  # Only part of Netherlands Antilles (former) in MIRCA
  "Slovakia" = 703000,
  "Slovenia" = 705000,
  "Solomon Islands" = 90000,
  "Somalia" = 706000,
  "South Africa" = 710000,
  "South Sudan" = integer(0),
  # Only Sudan (both parts) in MIRCA
  "Spain" = 724000,
  "Sri Lanka" = 144000,
  "Sudan" = integer(0),
  # Only Sudan (former) in MIRCA
  "Sudan (former)" = 736000,
  # Both parts before split into Sudan and South Sudan
  "Suriname" = 740000,
  "Sweden" = 752000,
  "Switzerland" = 756000,
  "Syrian Arab Republic" = 760000,
  "Tajikistan" = 762000,
  "Thailand" = 764000,
  "The former Yugoslav Republic of Macedonia" = 807000,
  "North Macedonia" = 807000,
  # Renamed between January 2019 and September 2019, keeping both names
  "Timor-Leste" = 626000,
  "Togo" = 768000,
  "Tokelau" = 772000,
  "Tonga" = 776000,
  "Trinidad and Tobago" = 780000,
  "Tunisia" = 788000,
  "Turkey" = 792000,
  "Turkmenistan" = 795000,
  "Turks and Caicos Islands" = 796000,
  "Tuvalu" = 798000,
  "Uganda" = 800000,
  "Ukraine" = 804000,
  "United Arab Emirates" = 784000,
  "United Kingdom" = 826000,
  "United Kingdom of Great Britain and Northern Ireland" = 826000,
  # Renamed at some point, keeping both names
  "United Republic of Tanzania" = 834000,
  "United States of America" = 840001:840051,
  "United States Virgin Islands" = 850000,
  "Uruguay" = 858000,
  "USSR" = c(
    643000, 804000, 112000, 51000, 31000, 233000, 268000, 398000, 417000,
    428000, 440000, 498000, 762000, 795000, 860000
  ),
  # Russian Federation, Ukraine, Belarus, Armenia, Azerbaijan, Estonia, Georgia,
  # Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Republic of Moldova, Tajikistan,
  # Turkmenistan, Uzbekistan
  "Uzbekistan" = 860000,
  "Vanuatu" = 548000,
  "Venezuela (Bolivarian Republic of)" = 862000,
  "Viet Nam" = 704000,
  "Wallis and Futuna Islands" = 876000,
  "Western Sahara" = 732000,
  "Yemen" = 887000,
  "Yugoslav SFR" = c(70000, 191000, 807000, 705000, 688000, 499000),
  # Bosnia and Herzegovina, Croatia, The former Yugoslav Republic of Macedonia,
  # Slovenia, Serbia, Montenegro
  "Zambia" = 894000,
  "Zimbabwe" = 716000,
  # The following are not in FAO production or land use data but are found in
  # MIRCA and FAO groups
  "British Indian Ocean Territory" = 86000,
  "Christmas Island" = 162000,
  "Cocos (Keeling) Islands" = 166000,
  "Svalbard and Jan Mayen Islands" = 744000
)
################################################################################


################################################################################
## Cyprus in FAOSTAT seems to cover all of Cyprus until ca. 1974 and then     ##
## Southern Cyprus. If a newer version of FAOSTAT changes that make sure to   ##
## update mapping.                                                            ##
## Since HYDE currently uses FAOSTAT cropland for "Cyprus" for all of Cyprus  ##
## even after 1974, option to not fix FAOSTAT.                                ##
if (!exists("fix_cyprus")) {
   # Should normally be set in landuse_setup.R
  fix_cyprus <- FALSE
}
if (fix_cyprus) {
  fao_gadm_country_mapping[["Cyprus_Northern"]] <- integer(0)
  # Not individual unit in MIRCA
  fao_gadm_country_mapping[["Cyprus_Southern"]] <- integer(0)
  # Not individual unit in MIRCA
}
################################################################################


################################################################################
## MIRCA region mapping                                                       ##
## Region names as used in GADM version 3.6.                                  ##
fao_mirca_region_mapping <- list(
  "Australia" = list(
    "Ashmore and Cartier Islands" = integer(0),
    # Not individual unit in MIRCA, outlying islands; not included in HYDE
    # land mask
    "Australian Capital Territory" = 36001,
    "Coral Sea Islands Territory" = integer(0),
    # Not individual unit in MIRCA, outlying islands; not included in HYDE land
    # mask
    "Jervis Bay Territory" = integer(0),
    # Not individual unit in MIRCA, very small region bordering New South Wales;
    # has cropland according to HYDE but no irrigated cropland
    "New South Wales" = 36002,
    "Northern Territory" = 36003,
    "Queensland" = 36004,
    "South Australia" = 36005,
    "Tasmania" = 36006,
    "Victoria" = 36007,
    "Western Australia" = 36008
  ),
  "Brazil" = list(
    "Acre" = 76001,
    "Alagoas" = 76002,
    "Amapa" = 76003,
    "Amazonas" = 76004,
    "Bahia" = 76005,
    "Ceara" = 76006,
    "Distrito Federal" = 76007,
    "Espirito Santo" = 76008,
    "Goias" = 76009,
    "Maranhao" = 76010,
    "Mato Grosso" = 76011,
    "Mato Grosso do Sul" = 76012,
    "Minas Gerais" = 76013,
    "Para" = 76014,
    "Paraiba" = 76015,
    "Parana" = 76016,
    "Pernambuco" = 76017,
    "Piaui" = 76018,
    "Rio Grande do Norte" = 76020,
    "Rio Grande do Sul" = 76021,
    "Rio de Janeiro" = 76019,
    "Rondonia" = 76022,
    "Roraima" = 76023,
    "Sao Paulo" = 76025,
    "Santa Catarina" = 76024,
    "Sergipe" = 76026,
    "Tocantins" = 76027
  ),
  "China" = list(
    "Anhui" = 156001,
    "Beijing" = 156002,
    "Chongqing" = 156003,
    "Fujian" = 156004,
    "Gansu" = 156005,
    "Guangdong" = 156006,
    "Guangxi" = 156007,
    "Guizhou" = integer(0),
    "Hainan" = 156009,
    "Hebei" = 156010,
    "Heilongjiang" = 156011,
    "Henan" = 156012,
    "Hubei" = 156013,
    "Hunan" = 156014,
    "Jiangsu" = 156016,
    "Jiangxi" = 156017,
    "Jilin" = 156018,
    "Liaoning" = 156019,
    "Nei Mongol" = 156015,
    "Ningxia Hui" = 156020,
    "Qinghai" = 156021,
    "Shaanxi" = 156022,
    "Shandong" = 156023,
    "Shanghai" = 156024,
    "Shanxi" = 156025,
    "Sichuan" = 156026,
    "Tianjin" = 156002,
    # Together with Beijing in MIRCA
    "Xinjiang Uygur" = 156028,
    "Xizang" = 156027,
    "Yunnan" = 156029,
    "Zhejiang" = 156031
  ),
  "India" = list(
    "Andaman and Nicobar" = 356034,
    "Andhra Pradesh" = 356001,
    "Arunachal Pradesh" = 356002,
    "Assam" = 356003,
    "Bihar" = 356004,
    "Chandigarh" = 356005,
    "Chhattisgarh" = 356006,
    "Dadra and Nagar Haveli" = 356007,
    "Daman and Diu" = 356008,
    "Goa" = 356010,
    "Gujarat" = 356011,
    "Haryana" = 356012,
    "Himachal Pradesh" = 356013,
    "Jammu and Kashmir" = 356014,
    "Jharkhand" = 356015,
    "Karnataka" = 356016,
    "Kerala" = 356017,
    "Lakshadweep" = 356035,
    "Madhya Pradesh" = 356018,
    "Maharashtra" = 356019,
    "Manipur" = 356020,
    "Meghalaya" = 356021,
    "Mizoram" = 356022,
    "NCT of Delhi" = 356009,
    # Just "Dehli" in MIRCA
    "Nagaland" = 356023,
    "Odisha" = 356024,
    # "Orissa" in MIRCA
    "Puducherry" = 356025,
    # "Pondicherry" in MIRCA
    "Punjab" = 356026,
    "Rajasthan" = 356027,
    "Sikkim" = 356028,
    "Tamil Nadu" = 356029,
    "Telangana" = 356001,
    # MIRCA only has "Andhra Pradesh" which split into "Andhra Pradesh" and
    # "Telangana" in 2014
    "Tripura" = 356030,
    "Uttar Pradesh" = 356032,
    "Uttarakhand" = 356031,
    # "Uttaranchal" in MIRCA
    "West Bengal" = 356033
  ),
  "United States of America" = list(
    "Alabama" = 840001,
    "Alaska" = 840002,
    "Arizona" = 840003,
    "Arkansas" = 840004,
    "California" = 840005,
    "Colorado" = 840006,
    "Connecticut" = 840007,
    "Delaware" = 840008,
    "District of Columbia" = 840051,
    "Florida" = 840009,
    "Georgia" = 840010,
    "Hawaii" = 840011,
    "Idaho" = 840012,
    "Illinois" = 840013,
    "Indiana" = 840014,
    "Iowa" = 840015,
    "Kansas" = 840016,
    "Kentucky" = 840017,
    "Louisiana" = 840018,
    "Maine" = 840019,
    "Maryland" = 840020,
    "Massachusetts" = 840021,
    "Michigan" = 840022,
    "Minnesota" = 840023,
    "Mississippi" = 840024,
    "Missouri" = 840025,
    "Montana" = 840026,
    "Nebraska" = 840027,
    "Nevada" = 840028,
    "New Hampshire" = 840029,
    "New Jersey" = 840030,
    "New Mexico" = 840031,
    "New York" = 840032,
    "North Carolina" = 840033,
    "North Dakota" = 840034,
    "Ohio" = 840035,
    "Oklahoma" = 840036,
    "Oregon" = 840037,
    "Pennsylvania" = 840038,
    "Rhode Island" = 840039,
    "South Carolina" = 840040,
    "South Dakota" = 840041,
    "Tennessee" = 840042,
    "Texas" = 840043,
    "Utah" = 840044,
    "Vermont" = 840045,
    "Virginia" = 840046,
    "Washington" = 840047,
    "West Virginia" = 840048,
    "Wisconsin" = 840049,
    "Wyoming" = 840050
  )
)
