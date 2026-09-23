# Changelog

## [1.1.0] - 2026-09-23

### Contributors

- author: Sebastian Ostberg (ostberg@pik-potsdam.de)

### General (all LandInG modules)

#### Added

- Changelog
- `LandInG_setup` environment, defined in new file `landing_setup.R`
- `earthradius` and `single.eps` (single precision floating point epsilon) set
  in `LandInG_setup`
- `LandInG_version` set in `VERSION`
- R_env_PIK.sh, a bash script that sets up the software environment on the PIK
  2024 high-performance cluster

#### Changed

- Use utility functions from `lpjmlkit` package to work with LPJmL file format.
- Deprecated/orphaned R packages `raster`, `rgdal` and `udunits2` replaced by
  `terra` and `units`.
- Functions from R packages called explicitly `package::function()` for better
  transparency and easier future updates.

#### Removed

- Support bash scripts specific to the PIK 2015 high-performance cluster
- `lpjml_format_helper_functions.R` (replaced by `landing_setup.R` & `lpjmlkit`)

### Elevation module

#### Fixed

- Fixed `download_and_resample_etopo.sh` to work with newer versions of GMT and
  with new PIK high-performance cluster

#### Removed

- PIK-specific bash script `r_modules.sh`

### Fertilizer module

#### Changed

- Variables set in `fertilizer_setup.R` are saved in `LandInG_setup` environment
- All scripts that depend on `fertilizer_setup.R` use `LandInG_setup`
- Replaced `raster::modal` with `collapse::fmode` because `terra::modal` does
  not provide equivalent functionality
- Script `helper/raster_helpers.R` renamed to `helper/terra_helpers.R`
- Messages regarding unsuccessful file downloads in
  `download_fertilizer_pattern.R` redirected from stdout to stderr
- Aggregation function for admin patterns changed from `modal_ties_lowest` to
  `modal_ties_first` in `gapfill_fertilizer_pattern.R`
- Name of variable in GADM administrative units mask set in `fertilizer_setup.R`
  instead of hardcoding

#### Fixed

- SLURM submission scripts `cft_input_timeseries_SLURM.sh`,
  `combine_fertilizer_pattern_trend_national_SLURM.sh`,
  `gapfill_fertilizer_pattern_SLURM.s` and `gapfill_fertilizer_trend_SLURM.sh`
  updated to PIK 2024 high-performance cluster
- Short sleep periods added in `download_fertilizer_pattern.R` to avoid
  triggering rate limit on Zenodo server
- `doParallel` cluster setup fixed in `combine_fertilizer_pattern_trend_national.R`,
  `gapfill_fertilizer_pattern.R` and `gapfill_fertilizer_trend.R`
- Aggregation factor and aggregation function corrected in `gapfill_pattern.R`,
  `load_hyde_area.R`, `match_admin_to_data.R` and `process_manure.R`
- Correctly determine whether to flip data in `process_manure.R`
- Use correct variable `cft_nut` instead of `nut` in `cft_input_timeseries.R`

#### Removed

- PIK-specific bash script `r_modules.sh`

### GADM module

#### Added

- Option to generate country code input without region band, as used in newer
  LPJmL versions
- Helper function `download_gadm()` to automatically download GADM data from the
  official server. GADM version specified as `gadm_data_version` in
  `gadm_setup.R`.
- SLURM submission scripts `1_map_admin_to_grid_proparation_SLURM.sh` and
  `3_map_admin_to_grid_collection_SLURM.sh`
- Consistency checks between step 1, 2 and 3 regarding intermediate shapefiles:
  files in `split_directory` and `intersect_directory` and shapes loaded into
  `cell_list_RData` should match.

#### Changed

- Variables set in `gadm_setup.R` are saved in `LandInG_setup` environment
- All scripts that depend on `gadm_setup.R` use `LandInG_setup`
- Merged functionality of step 1-3 with step 4-6, making use of faster shape
  intersection in previous step 5; file names updated
- Split shape intersection into smaller chunks to reduce memory requirement for
  high-resolution grids in step 2
- Updated SLURM settings in `2_map_admin_to_grid_intersection_SLURM.sh`
- Instead of simply aborting script run if output files exist in
  `3_map_admin_to_grid_collection.R` existing files are checked whether they
  match current script run
- Some performance updates in helper functions `gadm_helper.R`.
- Function `load_gadm()` expanded to support additional GADM versions, calls
  `download_gadm()` if data are not found in `gadm_dir`.
- `gadm_data_version` included in filenames of generated files.
- `README.md` expanded with warning about use of "s2" in "sf" package.
- Load `lpjgrid_shape` on every task only in case of MPI parallelization in
  `2_map_admin_to_grid_intersection.R`, otherwise load only once.

#### Fixed

- Multple bugs related to using spherical geometry with package `sf`
- Handle different GADM versions using different naming for country name column
- Coverage check in `2_map_admin_to_grid_intersection.R`
- Default setting for "s2" usage in "sf" set to `FALSE` and correctly exported
  to worker tasks `2_map_admin_to_grid_intersection.R`

#### Removed

- PIK-specific bash script `r_with_spatial_libs.sh`
- Previous steps 4-6 scripts because functionality is merged with steps 1-3
- Settings in `gadm_setup.R` no longer required due to merging of steps

### Lakes & rivers module

#### Changed

- Require `lwgeom` package only if functionality not included in `sf` package in
  `lakes_rivers_polygonbased.R`
- Update SLURM submission scripts `lakes_rivers_fraction_SLURM.sh` and
  `lakes_rivers_polygonbased_SLURM.sh` to PIK 2024 high-performance cluster
- Expand `README.md` with note that only version 1 of GLWD is supported
- Set default to `sf_use_s2(FALSE)` for `sf` package in
  `lakes_rivers_polygonbased.R` and export to worker tasks

#### Fixed

- Use correct lat/lon in aggregation factor in `lakes_rivers_fraction.R`


#### Removed

- PIK-specific bash script `r_modules.sh`

### Landuse module

#### Added

- new helper functions in `helper/add_version_string.R`,
  `helper/load_ha_fraction.R`, `helper/terra_helpers.R` and
  `helper/update_country_data.R`
- script to process GAEZ version 4 `multi_cropping_suitability_GAEZ_v4.R`, new
  corresponding variables in `landuse_setup.R`
- `GAEZ/README.md` expanded with information on GAEZ version 4
- SLURM submission script
  `split_global_harvested_areas_into_rainfed_irrigated_SLURM.sh` to account for
  additional software requirements of the corresponding R script


#### Changed

- Variables set in `landuse_setup.R` are saved in `LandInG_setup` environment.
- All scripts that depend on `landuse_setup.R` use `LandInG_setup`.
- Some variable names shortened to improve code formatting (e.g. `mon_*` instead
  of `monfreda_*`).
- Replaced `raster::modal` with `collapse::fmode` because `terra::modal` does
  not provide equivalent functionality.
- Script `read_AQUASTAT.R` renamed to `read_AQUASTAT_legacy.R` because it does
  not work for current AQUASTAT downloads.
- Script `helper/raster_helpers.R` renamed to `helper/terra_helpers.R`.
- When reloading data prepared by a different script LandInG version is checked
  for compatibility and attempts are made to make data from version 1.0.0
  compatible.
- Updated MIRCA2000 download link `MIRCA2000/README.md`
- Updated HYDE website link in `HYDE/README.md`
- SLURM submission scripts `aggregate_cft_timeseries_SLURM.sh`,
  `cft_input_timeseries_SLURM.sh`, `harvested_area_timeseries_SLURM.sh` and
  `harvested_fraction_SLURM.sh` updated to PIK 2024 high-performance cluster
- SLURM submission scripts `HYDE/step2_hyde_convert_SLURM.sh` and
  `HYDE/step3_hyde_interpol_merge_SLURM.sh` updated to PIK 2024 high-performance
  cluster

#### Fixed

- Explicit treatment of time dimension in data using `mon_refyear` (code
  previously accounted for time dimension in some places while simply assuming a
  single year in others). Note: Interpolation between multiple patterns still
  not implemented.
- Use correct source pattern (for crops aggregated across multiple sources) in
  gap-filling of Monfreda patterns (`harvested_fraction.R`)
- Aggregation factor and aggregation function corrected in
  `create_hyde_timeseries_max.R`, `get_crop_monfreda.R`,
  `harvested_area_timeseries.R`, `harvested_fraction.R`, `load_hyde_area.R` and
  `split_global_harvested_areas_into_rainfed_irrigated.R`
- `HYDE/step2_hyde_convert.sh` fixed for newer CDO versions which fail for ASCII
  grids with header lines and to correctly detect `VAR` for both "lu" and "pop"
  files from HYDE

#### Removed

- PIK-specific bash script `r_modules.sh`

### Reservoirs module

#### Changed

- Resolution of reservoir polygons and country maps reduced in diagnostic plots
  to reduce file size of PDF.

#### Fixed

- Datatype in header of generated reservoir input file corrected
- Sequence of `st_make_valid` and `st_simplify` swapped to avoid problems in
  `st_simplify` caused by invalid geometry

#### Removed

- PIK-specific bash script `r_modules.sh`

### River routing module

#### Added

- `river_routing_setup.R`, which contains all settings previously located in
  `river_routing.R` and `neighbour_irrigation.R`, uses `LandInG_setup`
  environment

#### Changed

- Some refactoring moving duplicated data processing code into
  `river_routing_setup.R`.
- SLURM job ID added to log file names of parallel tasks to prevent several jobs
  running in parallel from overwriting the files of other jobs

#### Fixed

- SLURM submission script `neighbour_irrigation_SLURM.sh` and
  `create_river_routing_input.sh` updated to PIK 2024 high-performance cluster
- `doParallel` cluster setup in `neighbour_irrigation.R`

#### Removed

- PIK-specific bash script `r_modules.sh`

### Soil module 

#### Fixed

- Error in gapfilling of missing soil data if search window crossed the dateline
- Error in detection whether target resolution is an integer multiple of source
  resolution
- Incorrect data type used for soil pH file if soil type and soil pH use
  different header version
- SLURM submission script `aggregate_soils_SLURM.sh` updated to PIK 2024
  high-performance cluster
- `doParallel` cluster setup in `aggregate_soils.R`
- Download link for HWSD data in `README.md`

#### Removed

- PIK-specific bash script `r_modules.sh`

## [1.0.0]

Initial release
