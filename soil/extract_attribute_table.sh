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
## The Soil Attribute Database of HWSD is provided in MS Access format.       ##
## mdbtools provide command-line read access to MS Access files. If not       ##
## in your distribution, mdbtools can be downloaded from                      ##
## https://github.com/mdbtools/mdbtools and compiled from source.             ##
## This script can then be used to extract the relevant attribute table from  ##
## the MS Access file and convert it to CSV format for further processing.    ##
################################################################################

## Path to "mdb-export" executable from mdbtools
mdb_export_bin="mdb-export"

## Path to HWSD Soil Attribute Database
HWSD_mdb="HWSD.mdb"

## Name of main attribute table to extract from MS Access database
HWSD_table="HWSD_DATA"

## Path to CSV file where main attribute table will be saved
HWSD_csv="HWSD_DATA.csv"

## Name of meta table giving index values used for USDA texture classes
HWSD_tex_classes="D_USDA_TEX_CLASS"

## Path to CSV file for USDA texture class indices
HWSD_tex_csv="D_USDA_TEX_CLASS.csv"

## Call to extract data
$mdb_export_bin $HWSD_mdb $HWSD_table>$HWSD_csv

$mdb_export_bin $HWSD_mdb $HWSD_tex_classes>$HWSD_tex_csv

################################################################################
## You may use mdb-tables from mdbtools to query the names of all tables      ##
## included in the HWSD Soil Attribute Database file.                         ##
################################################################################
