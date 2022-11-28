# FAOSTAT data

This file describes the FAOSTAT data used in this toolbox.

FAOSTAT is provided by the Food and Agriculture Organization of the United
Nations (FAO).

## Input
The data can be downloaded from:
http://www.fao.org/faostat/en/#data/

## How to use
Download crop production data:
1. Select the Domain `Production` -> `Crops`.
2. Use `Bulk Download` -> `All Data` in right column (`All Data Normalized`
  has a different structure than what is expected by `../read_FAOSTAT.R`).
3. Extract the `\*All_Data.csv` from the downloaded archive.
4. Click on `Definitions and standards` in right column.
5. Select and save as individual files `Country/Region`, `Country Group`,
  `Item`, `Item Group`. These filenames need to be entered into
  `../landuse_setup.R` as `fao_production_country_file`,
  `fao_production_country_group_file`, `fao_production_item_file` and
  `fao_production_item_group_file`.

Download land use data:
1. Select Domain `Land, Inputs and Sustainability` -> `Land` -> `Land Use`.
2. Use `Bulk Download` -> `All Data` in right column (`All Data Normalized`
  has a different structure than what is expected by `../read_FAOSTAT.R`).
3. Extract the `\*All_Data.csv` from the downloaded archive.
4. Click on `Definitions and standards` in right column.
5. Select and save as individual files `Country/Region`, `Country Group`. These
  filenames need to be entered into `../landuse_setup.R` as
  `fao_landuse_country_file` and `fao_landuse_country_group_file`.

