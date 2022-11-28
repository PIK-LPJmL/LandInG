################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## This file contains a number of utility functions to work with the LPJmL    ##
## input format. Functions defined here are used by scripts in several        ##
## subdirectories.                                                            ##
################################################################################

################################################################################
## Reads a header from an LPJmL input file                                    ##
## Tries to determine header version unless force_version is provided.        ##
## Return value: list with 3 components:                                      ##
## - header name, e.g. LPJGRID                                                ##
## - header values (11 in total), if header version is <3, partially filled   ##
##   with default values                                                      ##
## - endian of file (little or big)                                           ##
################################################################################
read_header <- function(filename, force_version = NULL) {
  if (!file.exists(filename)) {
    stop(sQuote(filename), " does not exist")
  }
  zz <- file(filename, "rb")
  # Header name length was variable in header version 1, read first 30 bytes
  headername <- rawToChar(readBin(zz, raw(), n = 30), multiple = TRUE)
  # Determine actual header name by looking for alphanumeric characters and "_"
  index <- seq_len(min(which(!grepl("[[:alpha:]_]", headername))) - 1)
  headername <- headername[index]
  headername <- paste(headername, collapse = "")
  if (substr(headername, 1, 3) != "LPJ") {
    close(zz)
    stop("invalid header name ", sQuote(headername))
  }
  # Skip over header
  seek(zz, nchar(headername))
  # Determine file endian, try platform-specific endian as default
  endian <- .Platform$endian
  # Read first integer value
  version <- readBin(zz, integer(), size = 4, n = 1, endian = endian)
  # Determine endian used in file
  if (bitwAnd(version, 0xff) == 0) {
    endian <- ifelse(endian == "little", "big", "little")
    seek(zz, nchar(headername))
    version <- readBin(zz, integer(), size = 4, n = 1, endian = endian)
  }
  # Version usually determined from file header, but can also be forced
  if (!is.null(force_version)) {
    message("Forcing header version to ", force_version)
    version <- force_version
  }
  # Read main header data that is included in all header versions
  headerdata <- readBin(zz, integer(), size = 4, n = 6, endian = endian)
  names(headerdata) <- c(
    "order",
    "firstyear",
    "nyear",
    "firstcell",
    "ncell",
    "nbands"
  )
  # Header version 2 added two more parameters
  if (version == 2) {
    headerdata <- c(
      headerdata,
      readBin(zz, double(), size = 4, n = 2, endian = endian)
    )
    names(headerdata) <- c(names(headerdata[1:6]), "cellsize_lon", "scalar")
  }
  # Header version 3 added two more parameters on top of version 2
  if (version == 3) {
    headerdata <- c(
      headerdata,
      readBin(zz, double(), size = 4, n = 3, endian = endian)
    )
    headerdata <- c(
      headerdata,
      readBin(zz, integer(), size = 4, n = 1, endian = endian)
    )
    names(headerdata) <- c(
      names(headerdata[seq_len(length(headerdata) - 4)]),
      "cellsize_lon",
      "scalar",
      "cellsize_lat",
      "datatype"
    )
  } else {
    # Add default values for parameters not included in header version 1
    if (length(headerdata) == 6) {
      headerdata <- c(
        headerdata,
        cellsize_lon = 0.5,
        scalar = 1,
        cellsize_lat = 0.5,
        datatype = 1
      )
      warning(
        "Type 1 header. Adding default values for cellsize, scalar and ",
        "datatype which may not be correct in all cases",
        call. = TRUE, immediate. = TRUE
      )
    }
    # Add default values for parameters not included in header version 2
    if (length(headerdata) == 8) {
      headerdata <- c(
        headerdata,
        cellsize_lat = as.double(headerdata["cellsize_lon"]),
        datatype = 1
      )
      warning(
        "Type 2 header. Adding default value for datatype which may not be ",
        "correct in all cases",
        call. = TRUE, immediate. = TRUE
      )
    }
  }
  close(zz)
  list(
    name = headername,
    header = c(version = version, headerdata),
    endian = endian
  )
}

################################################################################
## Creates a header from scratch which can be saved to a file using           ##
## write_header().                                                            ##
## Parameters without defaults that you always need to provide:               ##
## - nyear                                                                    ##
## - ncell                                                                    ##
## - nbands                                                                   ##
################################################################################
create_header <- function(name = "LPJGRID",
                          version = 3,
                          order = 1,
                          firstyear = 1901,
                          nyear,
                          firstcell = 0,
                          ncell,
                          nbands,
                          cellsize_lon = 0.5,
                          scalar = 1,
                          cellsize_lat = cellsize_lon,
                          datatype = 1,
                          endian = .Platform$endian
                         ) {
  header <- list()
  if (is.character(name) && length(name) == 1) {
    header[["name"]] <- name
  } else {
    stop("'name' must be a character vector of length 1")
  }
  header[["header"]] <- numeric(0)
  # Check that valid values have been provided for all parameters included
  # header version 1
  for (check in c(
    "version",
    "order",
    "firstyear",
    "nyear",
    "firstcell",
    "ncell",
    "nbands"
  )) {
    if (is.numeric(get(check)) && length(get(check)) == 1 &&
      get(check) == as.integer(get(check))
    ) {
      header[["header"]] <- c(header[["header"]], get(check))
      names(header[["header"]])[length(header[["header"]])] <- check
    } else {
      stop(sQuote(check), " must be an integer of length 1")
    }
  }
  if (version >= 2) {
    # Check that valid values have been provided for additional parameters in
    # header version 2
    for (check in c("cellsize_lon", "scalar")) {
      if (is.numeric(get(check)) && length(get(check)) == 1) {
        header[["header"]] <- c(header[["header"]], get(check))
        names(header[["header"]])[length(header[["header"]])] <- check
      } else {
        stop(sQuote(check), " must be a float of length 1")
      }
    }
    # Check that valid values have been provided for additional parameters in
    # header version 3
    if (version >= 3) {
      if (is.numeric(cellsize_lat) && length(cellsize_lat) == 1) {
        header[["header"]] <- c(
          header[["header"]],
          cellsize_lat = as.double(cellsize_lat)
        )
      } else {
        stop("'cellsize_lat' must be a float of length 1")
      }
      if (length(datatype) == 1) {
        if (!is.null(get_datatype(c(header[["header"]], datatype = datatype)))) {
          header[["header"]] <- c(
            header[["header"]],
            datatype = as.integer(datatype)
          )
          cat(
            "Setting datatype to", typeof(get_datatype(header)$type),
            "with size", get_datatype(header)$size, "\n"
          )
        } else {
          stop("Unknown 'datatype' ", datatype)
        }
      } else {
        stop("'datatype' must be integer of length 1")
      }
    } else {
      # Add defaults for values missing in type 2 headers
      header[["header"]] <- c(
        header[["header"]],
        cellsize_lat = as.double(header[["header"]]["cellsize_lon"]),
        datatype = 1
      )
      warning(
        "Type 2 header. Adding default value for datatype which may not be ",
        "correct in all cases",
        call. = TRUE, immediate. = TRUE
      )
    }
  } else {
    # Add defaults for values missing in type 1 headers
    header[["header"]] <- c(
      header[["header"]],
      cellsize_lon = 0.5,
      scalar = 1,
      cellsize_lat = 0.5,
      datatype = 1
    )
    warning(
      "Type 1 header. Adding default values for resolution, scalar and ",
      "datatype which may not be correct in all cases",
      call. = TRUE, immediate. = TRUE
    )
  }
  header[["endian"]] <- endian
  header
}

################################################################################
## Write a header to a file.                                                  ##
## Expects a list following the structure returned by read_header() or        ##
## create_header()                                                            ##
## Will fail if output file exists, unless overwrite set to TRUE              ##
################################################################################
write_header <- function(filename, header, overwrite = FALSE) {
  # Check that provided header is valid
  if (!is.list(header)) {
    stop("header must be a list() object")
  }
  if (is.null(header[["name"]]) || is.null(header[["header"]]) ||
    is.null(header[["endian"]])
  ) {
    stop("header must have elements 'name', 'header' and 'endian'")
  }
  # Check if file exists already
  if (file.exists(filename)) {
    if (!overwrite) {
      stop(
        sQuote(filename), " exists already.\n",
        "Set overwrite to TRUE if you want to force it to be overwritten."
      )
    }
    warning(
      sQuote(filename), " exists already and will be overwritten.",
      call. = TRUE, immediate. = TRUE
    )
  }
  zz <- file(filename, "wb")
  # Write header name
  writeBin(charToRaw(header$name), zz)
  # Write values included in all header versions
  index <- c(
    "version",
    "order",
    "firstyear",
    "nyear",
    "firstcell",
    "ncell",
    "nbands"
  )
  writeBin(
    as.integer(header$header[index]),
    zz,
    size = 4,
    endian = header$endian
  )
  if (header$header["version"] > 1) {
    # Write values included in header version 2
    writeBin(
      as.double(header$header[c("cellsize_lon", "scalar")]),
      zz,
      size = 4,
      endian = header$endian
    )
  }
  if (header$header["version"] > 2) {
    # Write values included in header version 3
    writeBin(
      as.double(header$header["cellsize_lat"]),
      zz,
      size = 4,
      endian = header$endian
    )
    writeBin(
      as.integer(header$header["datatype"]),
      zz,
      size = 4,
      endian = header$endian
    )
  }
  close(zz)
}

################################################################################
## This function can be used within readBin().                                ##
## Given an LPJ file header (as returned by read_header()) it returns a list  ##
## with 2 members:                                                            ##
## - type (to be used as the "what" parameter in readBin()                    ##
## - size (to be used as the "size" parameter in readBin()                    ##
## Datatypes as currently implemented:                                        ##
## 0: character (size 1) (corresponds to LPJ_BYTE in LPJmL model code)        ##
## 1: short (integer size 2) (corresponds to LPJ_SHORT in LPJmL model code)   ##
## 2: integer (integer size 4) (corresponds to LPJ_INT in LPJmL model code)   ##
## 3: float (numeric size 4) (corresponds to LPJ_FLOAT in LPJmL model code)   ##
## 4: double (numeric size 8) (corresponds to LPJ_DOUBLE in LPJmL model code) ##
################################################################################
get_datatype <- function(header) {
  if (is.list(header)) {
    header <- header$header
  }
  if (!is.finite(as.integer(header["datatype"]))) {
    stop("Header does not contain 'datatype' field")
  }
  switch(
    as.integer(header["datatype"]) + 1,
    list(type = raw(), size = 1),
    list(type = integer(), size = 2),
    list(type = integer(), size = 4),
    list(type = double(), size = 4),
    list(type = double(), size = 8)
  )
}

################################################################################
## This function can be used within seek() to skip over a header in a LPJmL   ##
## input file.                                                                ##
## Expects a header as returned by read_header() or create_header()           ##
################################################################################
get_headersize <- function(header) {
  if (!is.list(header)) {
    stop("Header must be a list")
  }
  if (!("name" %in% names(header)) || !("version" %in% names(header$header))) {
    stop("Invalid header. Header must contain 'name' and 'version' fields.")
  }
  nchar(header$name) + switch(as.integer(header$header["version"]), 7, 9, 11) * 4
}

################################################################################
## This function computes gridcell areas in m2 based on latitude coordinate & ##
## angular cell size (resolution), assuming the earth to be a sphere.         ##
## lat can be a vector, res_lon and res_lat should normally be single values. ##
################################################################################
earthradius <- 6371000.785
cellwidth <- earthradius * pi / 180
cellarea <- function(lat, res_lon = 0.5, res_lat = res_lon) {
  as.double(cellwidth * res_lon) * as.double(cellwidth * res_lat) *
    as.double(cos(lat / 180 * pi))
}
