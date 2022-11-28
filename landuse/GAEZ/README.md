# GAEZ data

This file describes the GAEZ data used in the toolbox.

The toolbox uses agro-climatic resources from GAEZ version 3 to calculate
suitability for multiple cropping under rainfed and irrigated conditions.

## Input
GAEZ version 3 data can be downloaded from: https://www.gaez.iiasa.ac.at/

## How to use
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
