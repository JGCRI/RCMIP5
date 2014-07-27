#' Exploration, manipulation, and summarizing of CMIP5 data. 
#'
#' Working with CMIP5 data can be tricky, forcing scientists to 
#' write custom scripts and programs. The `RCMIP5` package aims 
#' to ease this process, providing a standard, robust, and 
#' high-performance set of scripts to (i) explore what data 
#' have been downloaded, (ii) identify missing data, (iii) 
#' average (or apply other mathematical operations) across 
#' experimental ensembles, (iv) produce both temporal and spatial 
#' statistical summaries, (v) regrid data, and (vi) produce 
#' easy-to-work-with graphical and data summaries. 
#'
#' ...
#' 
#' @references Todd-Brown, K. and Bond-Lamberty, B, 2014: XXXX.
#' @references Taylor, K. E., Stouffer, R. J., and Meehl, G. A., 2012:
#'   An overview of CMIP5 and the experiment design, Bulletin of the American 
#'   Meteorological Society, 93, 485-498. 
#'   \url{http://dx.doi.org/10.1175/BAMS-D-11-00094.1}
#' @import plyr abind ncdf4
#' @docType package
#' @name RCMIP5
NULL

#' Constructor for 'cmip5data' class
#'
#' @param x list
#' @return A class \code{cmip5data} object, which is a list with 
#' (not necessarily all of) the following fields:
#'  \item{files}{A character vector containing the filenames data came from}
#'  \item{val}{A multidimensional array [lon, lat, time] holding the data}
#'  \item{valUnit}{A string containing the value units}
#'  \item{timeUnit}{A string containing the time units}
#'  \item{calendarStr}{A string defining the calendar type}
#'  \item{lat}{A numeric vector containing latitude values}
#'  \item{lon}{A numeric vector containing longitude values}
#'  \item{time}{A numeric vector containing time values}
#' @docType class
cmip5data <- function(x=list()) {
    if (!is.list(x)) stop("x must be a list")
    structure(x, class="cmip5data")
}

print.cmip5data <- function(x) {
    return("CMIP5 data")
    # TODO: print one-liner summarizing dataset
}

summary.cmip5data <- function(x) {
    # TODO: print more detailed summary of dataset
}
