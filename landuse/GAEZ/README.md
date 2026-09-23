# GAEZ data

This file describes the GAEZ data used in the toolbox.

The toolbox uses agro-climatic resources from GAEZ version 3 or 4 to calculate
suitability for multiple cropping under rainfed and irrigated conditions.

## Input

GAEZ version 3 data can be downloaded from: https://www.gaez.iiasa.ac.at/

GAEZ version 4 data can be downloaded from: https://gaez.fao.org/

## How to use

### GAEZ version 3

Sign up for a free user account to download GAEZ version 3 data.

Download data for the following variables:

- `frost-free period`
- `temperature growing period`
- `thermal climates`
- `Tsum during temperature growing period`
- `Tsum during frost-free period`
- `reference length of growing period`

The first 5 variables are found under `Agro-climatic Resources` -> `Thermal
regimes`. The last variable is found under `Agro-climatic Resources` -> 
`Growing period`

We suggest to select `Baseline (1961-1990)` as the time period for the download.
Unzip each variable in a subdirectory corresponding to its name in 
`../landuse_setup.R`.

### GAEZ version 4

No sign up is necessary to download GAEZ version 4 data. The data can be
accessed via the Data Viewer.

If data are processed at the native GAEZ resolution, download multiple cropping
classes directly. The variables are found under `Agro climatic Resources` ->
`Climate Classification` and are called:

- `mci` - Multi-cropping class (with irrigation)
- `mcr` - Multi-cropping class (rain-fed)

If data need to be aggregated download data for the following variables:

- `lt3` - Temperature growing period (LGPt=10); number of days with Ta > 10°C
- `lt2` - Temperature growing period (LGPt=5); number of days with Ta > 5 °C
- `ts3` - Annual temperature sum for days with Ta>10 °C (degree-days)
- `ts2` - Annual temperature sum for days with Ta>5 °C (degree-days)
- `lgd` - Total number of growing period days
- `mcl` - Thermal Climates (temperatures reduced to sea level)

The first 4 variables are found under `Agro climatic Resources` ->
`Thermal Regime`. `lgd` is found under `Agro climatic Resources` ->
`Growing Period`, `mcl` is found under `Agro climatic Resources` ->
`Climate Classification`

The default settings in `multi_cropping_suitability_GAEZ_v4.R` assume data for
the time period `1981-2010` sourced from `CRUTS32`.
