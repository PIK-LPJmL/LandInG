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
## This scripts interpolates centennial/decadal HYDE data to annual timesteps ##
## and merges them into one continuous time series file.                      ##
## Requires Climate Data Operators (CDO) and NetCDF Operators (NCO).          ##
## Settings:                                                                  ##
## TYPE: "lu" for land use data; HYDE also contains population density data   ##
##       which can be interpolated and merged by setting TYPE to "pop"        ##
## STARTYEAR: first year of data to merge (HYDE data currently cover 10000    ##
##            BC to 2017. Data are centennial until 1700, decadal from 1700-  ##
##            2000 and annual after 2000). Must be equal to value in step 2   ##
##            script.                                                         ##
## LASTYEAR: last year of data to merge.                                      ##
## VARS: vector of HYDE variables that should be merged.                      ##
## NCDIR: directory where NetCDF files were created in step 2 script.         ##
## ODIR: directory where merged, interpolated timeseries is saved.            ##
## HYDE_VERSION_STRING: optional string added to final NetCDF timeseries      ##
##   filenames to denote different HYDE versions (compare                     ##
##   'hyde_version_string' setting in ../landuse_setup.R)                     ##
## SPLIT_YEAR_ANNUAL: last year where HYDE source data are not annual (used   ##
##                    for interpolation target, 2000 for HYDE 3.2.1)          ##
################################################################################

TYPE="lu" # pop"
STARTYEAR=1500
LASTYEAR=2017
VARS="cropland grazing tot_irri tot_rainfed "
NCDIR="ncdf_tmp"
HYDE_VERSION_STRING=""
SPLIT_YEAR_ANNUAL=2000

ODIR=NetCDF_full${STARTYEAR}-${LASTYEAR}

mkdir -p $ODIR
for VAR in $VARS;do
  echo $VAR
  if [[ ${#HYDE_VERSION_STRING} -gt 0 ]]; then
    outfilename=$ODIR/hyde_${VAR}_annual_${STARTYEAR}_${LASTYEAR}_${HYDE_VERSION_STRING}.nc4
  else
    outfilename=$ODIR/hyde_${VAR}_annual_${STARTYEAR}_${LASTYEAR}.nc4
  fi
  while [ ! -f $outfilename ];do
    ## Concatenate timesteps from source data
    while [ ! -f $ODIR/${VAR}_${STARTYEAR}_${LASTYEAR}.nc4 ];do
      echo " cat..."
      cdo -s -O -z zip -cat \
          $NCDIR/*_$TYPE/$VAR*.nc4 \
          $ODIR/${VAR}_${STARTYEAR}_${LASTYEAR}.nc4
    done
    ## interpolate to annual timesteps until split year
    while [ ! -f $ODIR/${VAR}_annual_${STARTYEAR}_${SPLIT_YEAR_ANNUAL}.nc4 ];do
      echo " inttime ${STARTYEAR}-${SPLIT_YEAR_ANNUAL} ..."
      cdo -s -z zip -inttime,${STARTYEAR}-01-01,00:00:00,1year \
          -selyear,${STARTYEAR}/${SPLIT_YEAR_ANNUAL} \
          $ODIR/${VAR}_${STARTYEAR}_${LASTYEAR}.nc4 \
          $ODIR/${VAR}_annual_${STARTYEAR}_${SPLIT_YEAR_ANNUAL}.nc4
      ncatted -O -h -a units,$VAR,o,c,"km2" \
          $ODIR/${VAR}_annual_${STARTYEAR}_${SPLIT_YEAR_ANNUAL}.nc4
    done
    ## select years after split year 
    while [ ! -f $ODIR/${VAR}_annual_$(($SPLIT_YEAR_ANNUAL+1))_${LASTYEAR}.nc4 ];do
      echo " selyear $(($SPLIT_YEAR_ANNUAL+1))-${LASTYEAR}..."
      cdo -s -z zip -selyear,$(($SPLIT_YEAR_ANNUAL+1))/${LASTYEAR} \
          $ODIR/${VAR}_${STARTYEAR}_${LASTYEAR}.nc4 \
          $ODIR/${VAR}_annual_$(($SPLIT_YEAR_ANNUAL+1))_${LASTYEAR}.nc4
      ncatted -O -h -a units,$VAR,o,c,"km2" \
          $ODIR/${VAR}_annual_$(($SPLIT_YEAR_ANNUAL+1))_${LASTYEAR}.nc4
    done
    ## concatenate both annual partial timeseries
    echo " cat annual ${STARTYEAR}-${SPLIT_YEAR_ANNUAL} and $(($SPLIT_YEAR_ANNUAL+1))-${LASTYEAR}..."
    cdo -s -O -z zip -cat \
        $ODIR/${VAR}_annual_${STARTYEAR}_${SPLIT_YEAR_ANNUAL}.nc4 \
        $ODIR/${VAR}_annual_$(($SPLIT_YEAR_ANNUAL+1))_${LASTYEAR}.nc4 \
        $outfilename
  done && \
    rm $ODIR/${VAR}_*
done

echo "...done"
