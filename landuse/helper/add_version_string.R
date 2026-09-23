################################################################################
## Copyright (C) 2026 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to add optional version string to file name                       ##
## This function checks whether the supplied version string is not empty and, ##
## if so, returns it preceeded by an underscore. Otherwise, returns an empty  ##
## string.                                                                    ##
## Parameters:                                                                ##
## version_string: version string to check and potentially add                ##
################################################################################
add_version_string <- function(version_string) {
  if (
    !is.null(version_string) && length(version_string) == 1 &&
      nchar(version_string) > 0
  ) {
    paste0("_", version_string)
  } else {
    ""
  }
}
