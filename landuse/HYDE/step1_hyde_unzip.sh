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
## Script to unzip single-year ASCII grids downloaded from HYDE data portal.  ##
## Settings:                                                                  ##
## TYPE: "lu" for land use data; HYDE also contains population density data   ##
##       which can be unzipped by setting TYPE to "pop"                       ##
## STARTYEAR: first year of data to unzip (HYDE data currently cover 10000 BC ##
##            to 2017. Data are centennial until 1700, decadal from 1700-2000 ##
##            and annual after 2000)                                          ##
## LASTYEAR: last year of data to unzip (default: 2017, last year of HYDE)    ##
## ASCIIDIR: directory where to unzip ASCII files. Default: unzipped          ##
################################################################################
TYPE="lu"
STARTYEAR=1500
LASTYEAR=2017
ASCIIDIR="unzipped"

## This assumes that HYDE baseline data are in a sub directory "baseline"
for FILE in baseline/zip/*_$TYPE.zip; do
  TIME=$(basename $FILE |awk -F"/" '{print $NF}'|sed s/_$TYPE//|sed s/.zip//)
  CAL=$(echo -n $TIME | tail -c2)
  case $CAL in
    AD)
      YEAR=$(echo $TIME|sed s/$CAL//)
      ;;
    BC)
      YEAR=$(echo -$TIME|sed s/$CAL//)
      ;;
  esac
  [[ $YEAR -lt $STARTYEAR || $YEAR -gt $LASTYEAR ]] && continue
  echo TIME=$TIME FILE=$FILE
  ODIR=$ASCIIDIR/${TIME}_$TYPE
  mkdir -p $ODIR
  unzip $FILE -d $ODIR
done
