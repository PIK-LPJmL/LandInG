################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to aggregate arrays representing spatial data                     ##
## This function should be faster than aggregate() included in raster         ##
## package. Also, aggregate() sometimes does not treat NAs correctly.         ##
## Requires C library rescale.so (or name corresponding to your OS).          ##
## Parameters:                                                                ##
## indata: array which is to be aggregated (either two-dimensional or three-  ##
##         dimensional with first two dimensions being longitude and latitude)##
## fact: spatial aggregation factor, either one integer (used for both        ##
##       directions or two integer values used for longitude and latitude)    ##
## fun: aggregation function (as character string), either 'sum' or 'mean'    ##
## verbose: whether to C function should print non-error messages             ##
################################################################################
aggregate_array <- function(indata, fact, fun = "sum", verbose = TRUE) {
  if (length(dim(indata)) < 2 || length(dim(indata)) > 3) {
    stop("indata must be 2- or 3-dimensional array")
  }
  # Make sure there is always a third dimension for call to C function
  add_dim <- FALSE
  if (length(dim(indata)) == 2) {
    dim(indata) <- c(dim(indata), 1)
    add_dim <- TRUE
  }
  # Make sure there is always  and aggregation factor in x and y direction
  if (length(fact) == 1)
    fact <- rep(fact, 2)
  # Check that specified aggregation function is valid
  if (fun != "sum" && fun != "mean") {
    stop("fun must be either 'sum' or 'mean'")
  }
  # Make sure that data will be aggregated in at least one dimension
  if (max(fact) < 2) {
    stop("fact must be larger than 1 for at least 1 dimension")
  }
  # Make sure there are no "incomplete" cells, i.e. trying to aggregate a
  # 3 by 3 array by a factor of 2.
  if (any((dim(indata)[1:2] / fact) %% 1 != 0)) {
    stop(
      "Spatial dimension of indata [", toString(dim(indata)[1:2]), "] ",
      "is not an integer multiple of fact [", toString(fact), "]"
    )
  }
  # Mask NA values for hand-off to C function
  # Note: This assumes that 1e20 is not a valid value in indata.
  indata[which(is.na(indata))] <- 1e20

  # Aggregation by sum
  if (fun == "sum") {
    if (!is.loaded("aggregate_array")) {
      stop("External function aggregate_array not loaded")
    }
    result <- .C(
      "aggregate_array",
      indata = as.double(indata),
      ncols = as.integer(dim(indata)[1]),
      nrows = as.integer(dim(indata)[2]),
      nyears = as.integer(dim(indata)[3]),
      fact_x = as.integer(fact[1]),
      fact_y = as.integer(fact[2]),
      NAval = as.double(1e20),
      aggregated = double(
        dim(indata)[1] / fact[1] * dim(indata)[2] / fact[2] * dim(indata)[3]
      ), # this allocates space for the return value
      error = as.integer(0), # will be set to error code by external function
      verbose = as.integer(verbose)
    )
    # result returned by .C call is a list with named values for all parameters
    # supplied to .C().
    if (result$error != 0) {
      # An error occured in the aggregation function. Set result to original
      # data
      result$aggregated <- indata
    }
    # Remove all values from result list except the data part
    result <- result$aggregated
  }

  # Aggregation by mean
  if (fun == "mean") {
    if (!is.loaded("average_array")) {
      stop("external function average_raster not loaded")
    }
    result <- .C(
      "average_array",
      indata = indata,
      ncols = as.integer(dim(indata)[1]),
      nrows = as.integer(dim(indata)[2]),
      nyears = as.integer(dim(indata)[3]),
      fact_x = as.integer(fact[1]),
      fact_y = as.integer(fact[2]),
      NAval = 1e20,
      aggregated = double(
        dim(indata)[1] / fact[1] * dim(indata)[2] / fact[2] * dim(indata)[3]
      ), # this allocates space for the return value
      error = as.integer(0), # will be set to error code by external function
      verbose = as.integer(verbose)
    )
    # result returned by .C call is a list with named values for all parameters
    # supplied to .C().
    if (result$error != 0) {
      # An error occured in the aggregation function. Set result to original
      # data
      result$aggregated <- indata
    }
    # Remove all values from result list except the data part
    result <- result$aggregated
  }

  # Re-insert NAs
  result[which(result == 1e20)] <- NA
  # result has no dimensions. Set based on indata.
  dim(result) <- c(
    nrow(indata) / fact[1],
    ncol(indata) / fact[2],
    dim(indata)[3]
  )

  # Remove dummy third dimension if it was added at beginning of function
  if (length(dim(result)) == 3) {
    if (dim(result)[3] == 1 && add_dim) {
      dim(result) <- dim(result)[1:2]
    }
  }
  result
}
