#!/bin/bash

################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Script to resample ETOPO1 Global Relief Model data from 1 arc minute       ##
## source resolution to desired target resolution using the Generic Mapping   ##
## Tools (GMT). GMT is available for Windows, MacOS and Linux from            ##
## https://www.generic-mapping-tools.org/                                     ##
################################################################################

################################################################################
## ETOPO1 data is available from NOAA National Centers for Environmental      ##
## Information (NCEI): https://www.ngdc.noaa.gov/mgg/global/global.html       ##
## Use ETOPO1 Ice Surface, grid-registered:
## Download URL:                                                              ##
etopo1_src_url="https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/"\
"ice_surface/grid_registered/netcdf/ETOPO1_Ice_g_gmt4.grd.gz"
## Local filename:                                                            ##
etopo1_src_local="ETOPO1_Ice_g_gmt4.grd.gz"
## Whether to download ETOPO1 data (TRUE or FALSE). Skip if downloaded already##
download_etopo1=TRUE
## curl is used to download file and gzip is used to unpack.                  ##
################################################################################

################################################################################
## Desired target resolution:                                                 ##
## Resolution consists of a value and a unit. Should usually be coarser than  ##
## source resolution.                                                         ##
## Possible units: (d)egree, (m)inute, (s)econd                               ##
xres="5m"
yres="5m"
## Operation to perform to get from source to target resolution. Either       ##
## "AGGREGATE" or "DISAGGREGATE". Default: AGGREGATE.                         ##
resample_operation="AGGREGATE"
## Aggregation function to use in case resample_operation is "AGGREGATE".     ##
## Either "blockmedian" or "blockmean". Default: blockmedian.                 ##
aggregation_function="blockmedian"
## Mask out ocean cells before processing ETOPO1 data to target resolution:   ##
mask_ocean=TRUE
## Name for resampled ETOPO1 grid:                                            ##
etopo1_tgt_name="ETOPO1_resampled_"${xres}"_by_"${yres}".nc"
################################################################################

################################################################################
## On PIK cluster load GMT module. This is probably not necessary in other    ##
## software environments.                                                     ##
module load gmt/6.1.1
################################################################################


################################################################################
## Download data                                                              ##
if [ "$download_etopo1" == "TRUE" ]; then
  echo Downloading ETOPO1 data from $etopo1_src_url
  curl -o $etopo1_src_local $etopo1_src_url
  echo Unpacking downloaded file $etopo1_src_local
  gzip -d -k $etopo1_src_local
else
  if [ ! -e $etopo1_src_local ]; then
    echo Warning: Local file $etopo1_src_local does not exist. \
      Have you downloaded it already?
  fi
fi
# remove ".gz" from filename
etopo1_src_local_unpacked=${etopo1_src_local/.gz/}
################################################################################

################################################################################
## Create a land/ocean mask at 1 arc minute resolution to mask out ocean      ##
## pixels before aggregation. This uses the Global Self-consistent,           ##
## Hierarchical, High-resolution Geography Database (GSHHG) included with GMT ##
## Parameters in gmt grdlandmask:                                             ##
## -R: region, "d" for global                                                 ##
## -I: resolution of gridded output, "01m" for 1 arc-minute; should match the ##
##     resolution of your elevation source data                               ##
## -r: registration, "g" for grid or "p" for pixel; should match the          ##
##     registration of your elevation source data                             ##
## -D: resolution of the GSHHG source data to be used, "f" for full           ##
## -N: mask values for ocean/land/lake/island/pond; set all except oceans to 1##
## -A: skip features with an area smaller than threshold in km2, set to 3 to  ##
##     skip features smaller than 3km2 which roughly corresponds to cell area ##
##     of a cell at the equator at 1 arc-minute resolution; value set to skip ##
##     cells without full land cover                                          ##
## -G: name of grid file created, "GSHHG_earth_mask_01m_g.nc" by default,     ##
##     "=nb": format specification                                            ##
## --IO_NC4_DEFLATION_LEVEL=9 --IO_NC4_CHUNK_SIZE=4096: NetCDF compression    ##
##     settings                                                               ##
## --PROJ_ELLIPSOID: projection used, "Sphere"                                ##
if [ $mask_ocean == TRUE ]; then
  echo Applying land/ocean mask to elevation data
  # Create mask if it doesn't exist yet
  if [ ! -e GSHHG_earth_mask_01m_g.nc ]; then
    echo Creating land/ocean mask GSHHG_earth_mask_01m_g.nc
    gmt grdlandmask -Rd -I01m -rg -Df -NNaN/1/1/1/1 -A3 \
      -GGSHHG_earth_mask_01m_g.nc=nb --PROJ_ELLIPSOID=Sphere
  else
    echo Mask file GSHHG_earth_mask_01m_g.nc exists already. Not created again.
  fi
  # Apply the mask to ETOPO1 data
  etopo1_src_local_masked=${etopo1_src_local_unpacked/.grd/_masked.nc}
  if [ ! -e $etopo1_src_local_masked ]; then
    echo Applying mask to elevation data, new file: $etopo1_src_local_masked
    gmt grdmath -Rd $etopo1_src_local_unpacked GSHHG_earth_mask_01m_g.nc \
      MUL = $etopo1_src_local_masked
  else
    echo $etopo1_src_local_masked exists already. Not created again.
  fi
else
  # Simply use unpacked source data if mask_ocean == FALSE
  echo Using elevation data without land/ocean mask
  etopo1_src_local_masked=$etopo1_src_local_unpacked
fi
################################################################################

################################################################################
## Resample data                                                              ##
## In case of aggregation:                                                    ##
## gmt grd2xyz first converts raster data from source file into XYZ data      ##
## which is passed to gmt **aggregation_function** to aggregate to target     ##
## resolution.                                                                ##
## In case of disaggregation:                                                 ##
## gmt grdsample performs a bicubic interpolation although interpolation may  ##
## be changed by adding "-n" parameter.                                       ##
## -G: output grid file                                                       ##
## -I: outout resolution                                                      ##
## -rp: set pixel registration                                                ##
## -C: use the center of the block as the output location                     ##
## -R: derive regional extent from source file                                ##
if [ $resample_operation == "AGGREGATE" ]; then
  if [ "$aggregation_function" != "blockmean" ] &&  
     [ "$aggregation_function" != "blockmedian" ]; then
    echo Error: aggregation_function must be either blockmean or blockmedian
    exit -1
  fi
  # Add aggregation_function to filename
  etopo1_tgt_name=${etopo1_tgt_name/.nc/_${aggregation_function}.nc}
  echo Resampling $etopo1_src_local_masked to $etopo1_tgt_name using \
    $aggregation_function aggregation
  gmt grd2xyz $etopo1_src_local_masked | gmt $aggregation_function \
    -R$etopo1_src_local_unpacked -I${xres}/${yres} -C -rp -G$etopo1_tgt_name
else
  echo Resampling $etopo1_src_local_masked to $etopo1_tgt_name
  gmt grdsample $etopo1_src_local_masked -G$etopo1_tgt_name -I${xres}/${yres} -rp
fi
################################################################################
