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
## This script converts ASCII grids unzipped by step 1 into NetCDF files.     ##
## Requires Climate Data Operators (CDO).                                     ##
## Settings:                                                                  ##
## TYPE: "lu" for land use data; HYDE also contains population density data   ##
##       which can be converted by setting TYPE to "pop"                      ##
## STARTYEAR: first year of data to convert (HYDE data currently cover 10000  ##
##            BC to 2017. Data are centennial until 1700, decadal from 1700-  ##
##            2000 and annual after 2000)                                     ##
## LASTYEAR: last year of data to convert (default: 2017, last year of HYDE)  ##
## ASCIIDIR: directory where ASCII files are located (default: unzipped)      ##
## NCDIR: directory where NetCDF files are created (default: ncdf_tmp)        ##
## SKIP_LINES: some CDO versions require ASCII grids without header           ##
##             information whereas others require the header. Set SKIP_LINES  ##
##             to 6 to remove header lines. Otherwise, set it to 0.           ##
################################################################################

TYPE="lu"
STARTYEAR=1500
LASTYEAR=2017
ASCIIDIR="unzipped"
NCDIR="ncdf_tmp"
SKIP_LINES=6

for IDIR in $ASCIIDIR/*_$TYPE;do
  ODIR=$NCDIR/$(echo $IDIR|awk -F"/" '{print $NF}')

  for FILE in $IDIR/*.asc;do
    TIME=$(dirname $FILE |awk -F"/" '{print $NF}'|sed s/_$TYPE//)
    if [[ "$TYPE" == "lu" ]]; then
      VAR=$(basename $FILE .asc | sed s/$TIME//)
    else
      VAR=$(basename $FILE .asc | sed s/_$TIME//)
    fi
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

    mkdir -p $ODIR
    echo " $FILE $VAR $CAL $YEAR ..."
    skip=$(($SKIP_LINES + 1))
    ## This keeps trying until files are converted successfully.
    while [ ! -f $ODIR/${VAR}_$YEAR.nc4 ];do
      # Optionally remove header lines from input file
      tail -n +$skip $FILE | \
      cdo --history -m -9999 -f nc4c -z zip -s \
          -setreftime,$STARTYEAR-01-01,00:00:00,1years \
          -settaxis,$YEAR-01-01,00:00:00,1years \
          -setname,$VAR -setmissval,1E+20 -invertlatdata -setunit,km^2 \
          -input,grid.txt $ODIR/${VAR}_$YEAR.nc4
    done
  done
done
echo "...done"
