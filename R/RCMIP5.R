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
#' the following fields:
#'  \item{files}{A character vector containing the filenames data came from}
#'  \item{val}{A multidimensional array [lon, lat, time] holding the data}
#'  \item{valUnit}{A string containing the value units}
#'  \item{timeUnit}{A string containing the time units}
#'  \item{calendarStr}{A string defining the calendar type}
#'  \item{lat}{A numeric vector containing latitude values}
#'  \item{lon}{A numeric vector containing longitude values}
#'  \item{time}{A numeric vector containing time values}
#'  \item{variable}{Variable described by this dataset}
#'  \item{model}{Model of this dataset}
#'  \item{experiment}{Experiment of this dataset}
#'  \item{ensembles}{Ensemble(s) included in this dataset}
#' @docType class
cmip5data <- function(x=list()) {
    if (!is.list(x)) stop("x must be a list")
    structure(x, class="cmip5data")
}

#' Print a 'cmip5data' class object
#'
#' @param x A \code{\link{cmip5data}} object.
#' @details Prints a one-line summary of the object. 
print.cmip5data <- function(x, ...) {
    nfiles <- length(x$files)
    nensembles <- length(x$ensembles)
    yearRange <- round(range(compute_yearIndex(x)),2)
    yearString <- paste(yearRange[1],yearRange[2],sep="-")
    
    cat(paste("CMIP5:", x$variable, x$model, x$experiment, yearString,
          paste0("[", paste(dim(x$val), collapse=" "), "]"),
          "from", nensembles, ifelse(nensembles==1, "ensemble", "ensembles"),
          nfiles, ifelse(nfiles==1,"file","files"), ...))
}

#' Summarize a 'cmip5data' class object
#'
#' @param x A \code{\link{cmip5data}} object.
#' @details Prints a short summary of the object. 
summary.cmip5data <- function(x) {
    cat("CMIP5 data")
    if(!is.null(x$numMonths)) {
        cat(" - annual summary\n")
        cat("(Mean months summarized:", mean(x$numMonths), "\n")
    } else if(!is.null(x$numYears)) {
        cat(" - monthly summary\n")
        cat("Mean years summarized:", mean(x$numYears), "\n")
    } else cat("\n")
    
    yearRange <- round(range(compute_yearIndex(x)),2)
    yearString <- paste(yearRange[1],yearRange[2],sep="-")

    cat(yearString, "\n\n")
    cat("Variable:", x$variable)
    cat("Model:", x$model, "\n")
    cat("Experiment:", x$experiment, "\n")
    cat("Ensembles:", x$ensembles, "\n")
    cat("Data: ", x$valUnit, ", dimensions ", paste(dim(x$val), collapse=" "), "\n", sep="")
    cat("Time: ", x$timeUnit, ", length ", length(x$time), ", calendar ", x$calendarStr, "\n", sep="")
    cat("Size: ")
    print(object.size(x), units="MB")
}

#' Compute year index from parsed cmip5data information
#' 
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @return yearIndex A numeric vector of years
#' @details This function uses information in a \code{\link{cmip5data}} object 
#' to compute the yearIndex, i.e. the (perhaps fractional) years associated with
#' each timepoint. It does this by parsing \code{calendarStr} to determine the
#' number of days per year; parsing \code{timeUnit} to get the starting date;
#' and then using the values in the \code{time} vector.
#' @note This is an internal RCMIP5 function and not exported.
compute_yearIndex <- function(x) {
    stopifnot(class(x)=="cmip5data")
    
    # TODO: is calendarStr guaranteed to have # days in positions 1-3? 
    # Would it better to split the string based on underscore?
    numDays <- as.numeric(substr(x$calendarStr, 1, 3))
    stopifnot(is.numeric(numDays) & numDays>0)
    
    # timeUnit is a string like "days since 1859-12-01". Extract startDate from this
    # TODO: this will not (?) handle annual data correctly
    startYrArr <- as.numeric(unlist(strsplit(
        regmatches(x$timeUnit, regexpr('\\d+.\\d+.\\d+', x$timeUnit)), '-')))
    startYr <- startYrArr[1] + (startYrArr[2]-1)/12 + (startYrArr[3]-1)/numDays
    
    # Sanity checks
    stopifnot(startYrArr[2] %in% 1:12)
    stopifnot(startYrArr[3] %in% 1:31)
    stopifnot(startYr >= 1850 & startYr < 2300)
    
    return(x$time/numDays + startYr)
} # compute_yearIndex
