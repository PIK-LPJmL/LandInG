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
## This script creates a river routing input file for LPJmL from a drainage   ##
## direction map.                                                             ##
## Parameters:                                                                ##
##   - DRAINAGEBIN: Path to drainage executable from LPJmL utilities.         ##
##     Drainage utility is included in LPJmL source code repository and       ##
##     compiled with "make all".                                              ##
##   - LPJGRID: Path to LPJmL grid input file for which a river routing input ##
##     will be created.                                                       ##
##   - DDMASCII: Path to a drainage direction map as an ASCII grid. This must ##
##     have the same spatial resolution as LPJGRID and cover at least the     ##
##     full spatial extent of LPJGRID. For encoding of drainage directions    ##
##     see /src/utils/drainage.c in LPJmL source code reposity.               ##
##   - RIVERROUTINGFILE: River routing input file to be created.              ##
################################################################################

DRAINAGEBIN="./drainage"
LPJGRID=""
DDMASCII=""
RIVERROUTINGFILE=""

## Convert drainage direction map into LPJmL river routing input
$DRAINAGEBIN "$LPJGRID" "$DDMASCII" "$RIVERROUTINGFILE"
