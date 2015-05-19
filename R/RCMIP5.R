#' Tools for Manipulating and Summarizing CMIP5 Data
#'
#' Working with CMIP5 data can be tricky, forcing scientists to
#' write custom scripts and programs. The `RCMIP5` package aims
#' to ease this process, providing a standard, robust, and
#' high-performance set of functions to (i) explore what data
#' have been downloaded, (ii) identify missing data, (iii)
#' average (or apply other mathematical operations) across
#' experimental ensembles, (iv) produce both temporal and spatial
#' statistical summaries, and (v) produce
#' easy-to-work-with graphical and data summaries.
#'
#' ...
#'
#' @references Todd-Brown and Bond-Lamberty, 2014: (in prep).
#' @references Taylor et al., 2012:
#'   An overview of CMIP5 and the experiment design, Bulletin of the American
#'   Meteorological Society, 93, 485-498.
#'   \url{http://dx.doi.org/10.1175/BAMS-D-11-00094.1}
#' @import dplyr digest abind assertthat
#' @docType package
#' @name RCMIP5
NULL

#' The 'cmip5data' class
#'
#' This constructor has two functions. First, given a list, it makes the list
#' a cmip5data-class object (no check is made that the list has appropriate
#' fields though). Second, if given a numeric value(s), it returns sample/
#' example data in the newly constructed object. This is used extensively by
#' the testing code.
#'
#' @param x A list or numeric. If x is a list then the fields are expected to match those of the returned cmip5data object. If x is a numeric sample data is created where the numeric indicates the years of sample data to return.
#' @param lonlat Boolean indicating whether to create lon and lat dimensions
#' @param lonsize Integer size of longitude dimension
#' @param latsize Integer size of latitude dimension
#' @param Z logical. Create Z dimension?
#' @param Zsize integer. Size of Z dimension
#' @param time logical. Create time dimension?
#' @param monthly logical. Monthly (if not, annual) data?
#' @param randomize logical. Random sample data?
#' @param verbose logical. Print info as we go?
#' @param loadAs a string identifying possible structures for values. Currently: 'data.frame' and 'array' the only valid options.
#' @return A cmip5data object, which is a list with the following fields:
#'  \item{files}{Array of strings containg the file(s) included in this dataset}
#'  \item{variable}{String containg the variable name described by this dataset}
#'  \item{model}{String containing the model name of this dataset}
#'  \item{experiment}{String containing the experiment name of this dataset}
#'  \item{ensembles}{Array of strings containg the ensemble(s) included in this dataset}
#'  \item{domain}{String containing the domain name of this dataset}
#'  \item{val}{Data frame holding data, with fields lon, lat, Z, time}
#'  \item{valUnit}{String containing the value units}
#'  \item{lon}{Numeric vector containing longitude values; may be \code{NULL}}
#'  \item{lat}{Numeric vector containing latitude values; may be \code{NULL}}
#'  \item{Z}{Numeric vector Z values; may be \code{NULL}}
#'  \item{time}{Numeric vector containing time values; may be \code{NULL}}
#'  \item{dimNames}{Array of strings containing the original (NetCDF) dimension names}
#'  \item{calendarStr}{String defining the calendar type; may be \code{NULL}}
#'  \item{debug}{List with additional data (subject to change)}
#'  \item{provenance}{Data frame with the object's provenance. See \code{\link{addProvenance}}}
#'  \item{numPerYear}{Numeric vector; only present after \code{\link{makeAnnualStat}}}
#'  \item{numYears}{Numeric vector; only present after \code{\link{makeMonthlyStat}}}
#'  \item{numCells}{Numeric vector; only present after \code{\link{makeGlobalStat}}}
#'  \item{filtered}{Logical; only present after \code{\link{filterDimensions}}}
#' @docType class
#' @examples
#' cmip5data(1970)  # produces monthly sample data for year 1970
#' cmip5data(1970:2014)
#' cmip5data(1970:2014, monthly=FALSE)  # annual data
#' cmip5data(1970:2014, randomize=TRUE) # randomized data
#' cmip5data(1970:2014, Z=TRUE)  # four-dimensional data
#' cmip5data(0, time=FALSE)  # sample 'fx' data, two-dimensional
#' cmip5data(list())  # makes this (here empty) list class into 'cmip5data'
#' @export
cmip5data <- function(x=list(),
                      # parameters for making sample data
                      lonlat=TRUE, lonsize=10, latsize=10,
                      Z=FALSE, Zsize=5,
                      time=TRUE, monthly=TRUE,
                      randomize=FALSE, verbose=FALSE, loadAs='data.frame') {
    
    # Sanity checks
    assert_that(is.flag(lonlat))
    assert_that(is.numeric(lonsize))
    assert_that(is.numeric(latsize))
    assert_that(is.flag(Z))
    assert_that(is.numeric(Zsize))
    assert_that(is.flag(time))
    assert_that(is.flag(monthly))
    assert_that(is.flag(randomize))
    assert_that(is.flag(verbose))
    assert_that(loadAs %in% c("data.frame", "array"))
    
    if (is.list(x)) {          # If x is a list then we are done.
        # Just cast it directly to a cmip5data object
        if(verbose) cat("Casting list to cmip5data\n")
        structure(x, class="cmip5data")
        
    } else if(is.numeric(x)) {  # Create sample data
        if(verbose) cat("Creating new cmip5data\n")
        
        # Construct two lists which will be used to create the sample data:
        # ... result and debug.
        
        # result holds the primary data of interest
        result <- list(
            files=NULL,
            variable="var",
            model="model",
            experiment="experiment",
            ensembles="ensemble",
            domain="domain",
            val=NULL,
            valUnit=NULL,
            lon=NULL,
            lat=NULL,
            Z=NULL,
            time=NULL,
            dimNames=NULL
        )
        
        debug <- list()
        
        # If this data will have spatial dimensions, construct
        if(lonlat) {
            if(verbose) cat("Adding spatial dimensions\n")
            
            # realistic lon (0 to 360) and lat (-90 to 90) numbers
            result$lon <- 360/lonsize * c(0:(lonsize-1))  + 360/lonsize/2
            result$lat <- 180/latsize * c(0:(latsize-1)) - 90 + 180/latsize/2
            # Convert to two dimensions
            result$lon <- array(result$lon, dim=c(lonsize, latsize))
            result$lat <- array(rep(result$lat, 1, each=lonsize), dim=c(lonsize, latsize))
            result$dimNames=c("lon", "lat")
            debug$lonUnit <- "degrees_east"
            debug$latUnit <- "degrees_north"
        } else {
            result$dimNames <- c(NA, NA)
        }
        
        # If this data will have Z dimension, construct
        if(Z) {
            if(verbose) cat("Adding Z dimensions\n")
            
            result$Z <- c(0:(Zsize-1))
            result$dimNames <- c(result$dimNames, "Z")
            debug$ZUnit <- "m"
        } else {
            result$dimNames <- c(result$dimNames, NA)
        }
        
        # If this data will have time dimension, construct
        if(time) {
            if(verbose) cat("Adding time dimensions\n")
            
            years <- x
            ppy <- ifelse(monthly, 12, 1)  # periods per year
            result$calendarStr <- "360_day"
            debug$timeFreqStr <- ifelse(monthly, "mon", "yr")
            debug$startYr <- years[1]
            debug$calendarStr <- "360_day"
            debug$timeUnit <- paste0("days since ",years[1],"-01-01")
            
            if(monthly) {
                # '+15' initalizes all time stamps to be middle of the month
                debug$timeRaw <- (360/ppy*c(0:(length(years)*ppy-1) )+15)    
                result$time <- debug$timeRaw/360+min(years)    
            } else {
                debug$timeRaw <- result$time <- years
            }
            
            # convert day based calandar to year based
            result$dimNames <- c(result$dimNames, "time")
        } else { # no time
            result$dimNames <- c(result$dimNames, NA)
            result$domain <- "fx"
        }
        
        # Create the data array
        finalDim <- c(length(result$lon[,1]), length(result$lat[1,]),
                      length(result$Z), length(result$time))
        finalDim <- finalDim[finalDim > 0]
        if(randomize) {
            result$val <- runif(n=prod(finalDim))
        } else {
            result$val <- rep(1, prod(finalDim))
        } 
        dim(result$val) <- finalDim

        # TODO: KTB - probably want to call restoreMissingDimensions here?
        # The array isn't guaranteed to be four-dimensional
        
        # Miscellany
        result$valUnit <- "unit"
        result$debug <- debug
        
        # Add debug info and set class
        result <- structure(result, class="cmip5data")
        
        # Convert to data frame representation, if requested
        if(loadAs == 'data.frame') {
            result$val <- convert_array_to_df(result, verbose)
        }

        # Initialize provenance and return
        addProvenance(result, "Dummy data created")
    } else {
        stop("Don't know what to do with this class of parameter")
    }
}

#' Print a 'cmip5data' class object.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param ... Other parameters passed to cat
#' @details Prints a one-line summary of the object
#' @method print cmip5data
#' @export
#' @keywords internal
print.cmip5data <- function(x, ...) {
    
    if(is.null(x$variable)) {
        cat("(Empty cmip5data object)")
        return()
    }
    
    ansStr <- paste0('CMIP5: ', x$variable, ", ", x$model, " ", x$experiment)
    
    spaceStr <- paste(length(x$lon), "x", length(x$lat), "x", length(x$Z), "x", length(x$time))
    ansStr <- paste0(ansStr, ", ", spaceStr)
    
    timeStr <- "no time"
    if(!is.null(x$time) & length(x$time) > 0) {
        timeStr <- paste(floor(min(x$time, na.rm=TRUE)), "to",
                         floor(max(x$time, na.rm=TRUE)))
    }
    ansStr <- paste0(ansStr, ", ", timeStr)
    
    if(!is.null(x$ensembles)) {
        ansStr <- paste0(ansStr, ", from ", length(x$ensembles), " ",
                         ifelse(length(x$ensembles)==1, "ensemble", "ensembles"))
    }
    
    cat(ansStr, "\n")
    cat(...)
} # print.cmip5data

#' Summarize a 'cmip5data' class object.
#'
#' @param object A \code{\link{cmip5data}} object
#' @param ... ignored
#' @details Prints a short summary of the object.
#' @return A summary structure of the object.
#' @method summary cmip5data
#' @export
#' @keywords internal
summary.cmip5data <- function(object, ...) {
    
    ans <- list()
    class(ans) <- "summary.cmip5data"
    
    # cmip5 objects should always have the following defined:
    ans$variable <- object$variable
    ans$valUnit <- object$valUnit
    ans$domain <- object$domain
    ans$model <- object$model
    ans$experiment <- object$experiment
    ans$ensembles <- object$ensembles
    
    ans$type <- "CMIP5 data"
    
    #    if(grepl('makeAnnualStat', rev(object$provenance$caller)[1])) {
    #        ans$type <-  paste(ans$type, "annual summary [",
    #                           paste(unique(object$numPerYear), collapse=', '),
    #                           "]") 
    #    }
    
    if(!is.null(object$numPerYear)) {
        ans$type <-  paste0(ans$type, " (annual summary of ", mean(object$numPerYear), " times)")  
    }
    if(!is.null(object$numYears)) {
        ans$type <- paste0(ans$type, " (monthly summary of ", mean(object$numYears), " years)")
    } 
    if(!is.null(object$numCells)) {
        ans$type <- paste0(ans$type, " (spatial summary of ", object$numCells, " cells)")
    } 
    if(!is.null(object$numZs)) {
        ans$type <- paste0(ans$type, " (Z summary of ", object$numZs, " levels)")
    } 
    
    if(!is.null(object$filtered)) {
        ans$type <- paste(ans$type, "(filtered)")
    }
    
    #    if(!is.null(object$area)) {
    #        ans$type <- paste(ans$type, "(regridded)")
    #    }
    
    ans$spatial <- paste0("lon [", length(object$lon),
                          "] lat [", length(object$lat),
                          "] Z [", length(object$Z), "]")
    
    ans$time <- paste0(object$debug$timeFreqStr, " [", length(object$time), "] ", object$debug$timeUnit)
    ans$size <- as.numeric(object.size(object))
    if(is.array(object$val)) {
        ans$valsummary <- c(min(object$val, na.rm=TRUE),
                            mean(object$val, na.rm=TRUE),
                            max(object$val, na.rm=TRUE))
    } else {
        ans$valsummary <- c(min(object$val$value, na.rm=TRUE),
                            mean(object$val$value, na.rm=TRUE),
                            max(object$val$value, na.rm=TRUE))
        #} else {
        #    stop('Class of value is not recognized.')
    }
    ans$provenance <- object$provenance
    
    return(ans)
} # summary.cmip5data

#' Print the summary for a 'cmip5data' class object.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param ... Other parameters passed to cat
#' @details Prints a one-line summary of the object
#' @method print summary.cmip5data
#' @export
#' @keywords internal
print.summary.cmip5data <- function(x, ...) {
    cat(x$type, "\n")
    cat("Variable: ", x$variable, " (", x$valUnit, ") from model ", x$model, "\n", sep="")
    cat(sprintf("Data range: %.2g to %.2g Mean: %.2g\n", x$valsummary[1], x$valsummary[3],  x$valsummary[2]))
    cat("Experiment:", x$experiment, "-", length(x$ensembles), "ensemble(s)\n")
    cat("Spatial dimensions:", x$spatial, "\n")
    cat("Time dimension:", x$time, "\n")
    cat("Size:", format(round(x$size/1024/1024, 1), nsmall=1), "MB\n")
    cat("Provenance has", nrow(x$provenance), "entries\n")
} # print.summary.cmip5data

#' Convert a cmip5data object to a data frame
#'
#' @param x A \code{\link{cmip5data}} object
#' @param ... Other parameters
#' @param originalNames logical. Use original dimension names from file?
#' @return The object converted to a data frame
#' @export
#' @keywords internal
as.data.frame.cmip5data <- function(x, ..., originalNames=FALSE) {
    
    # Sanity checks
    assert_that(is.flag(originalNames))
    
    # Suppress stupid NOTEs from R CMD CHECK
    lon <- lat <- Z <- time <- NULL
    dplyr::arrange(x$val, lon, lat, Z, time)
} # as.data.frame.cmip5data

#' Convert a cmip5data object to an array
#'
#' @param x A \code{\link{cmip5data}} object
#' @param ... Other parameters
#' @param drop logical. Drop degenerate dimensions?
#' @return The object converted to an array
#' @export
#' @keywords internal
as.array.cmip5data <- function(x, ..., drop=TRUE) {
    
    # Sanity checks
    assert_that(is.flag(drop))
    
    dimList <- c(length(unique(x$val$lon)),
                 length(unique(x$val$lat)),
                 length(unique(x$val$Z)),
                 length(unique(x$val$time)))
    
    # Remove degenerate dimensions
    if(drop) {
        dimList <- dimList[!dimList %in% 1]        
    }
    
    # Suppress stupid NOTEs from R CMD CHECK
    lon <- lat <- Z <- time <- NULL
    
    # Note we sort data frame before converting to array!
    array(dplyr::arrange(x$val, time, Z, lat, lon)$value, dim=dimList)
} # as.array.cmip5data

#' Make package datasets and write them to disk.
#'
#' @param path root of directory tree
#' @param maxSize max size (in MB) of dataset to write
#' @param outpath directory to write to
#' @details Writes all available ensembles to disk as Rdata files, subject to
#' a maximum size parameter (CRAN says keep sample data < 5MB).
#' @note This is an internal RCMIP5 function and not exported.
#' @keywords internal
makePackageData <- function(path="./sampledata", maxSize=Inf, outpath="./data") {
    
    # Sanity checks
    assert_that(is.dir(path))
    assert_that(is.readable(path))
    assert_that(is.numeric(maxSize))
    if(!file.exists(outpath)) dir.create(outpath)
    assert_that(is.dir(outpath))
    assert_that(is.writeable(outpath))
    
    datasets <- getFileInfo(path)
    if(is.null(datasets)) return()
    
    for(i in 1:nrow(datasets)) {
        cat("-----------------------\n", datasets[i, "filename"], "\n")
        d <- with(datasets[i,],
                  loadEnsemble(variable, model, experiment, ensemble, path=path, verbose=T)
        )
        print(object.size(d), units="MB")
        if(object.size(d)/1024/1024 <= maxSize) {
            objname <- gsub("_[0-9]{4,}-[0-9]{4,}.nc$", "", basename(d$files[1])) # strip dates
            assign(objname, d)
            cat("Writing", objname, "\n")
            save(list=objname, file=paste0(outpath, "/", objname, ".rda"))
        } else {
            cat("Too big; skipping\n")
        }
    }
} # makePackageData

#' Alternative weighted mean
#'
#' @param x vector of data
#' @param w vector of weights
#' @param na.rm Remove NAs in both data and weights?
#' @return Weighted mean of \code{x}, using weights \code{d}.
#' @details The \code{stats} version of weighted.mean doesn't handle weights in a very 
#' useful way. Specifically, it will remove missing \code{x} values, but not missing weights.
#' A fair number of CMIP5 models have \code{NA} values in their grid areas, so this will
#' quickly cause problems. This function removes (if \code{na.rm=TRUE}) missing observations
#' and weights before computing the weighted mean.
#' @export
#' @seealso \code{\link{weighted.mean}}
cmip5.weighted.mean <- function(x, w=rep(1, length(x)), na.rm=TRUE) {
    assert_that(length(x) == length(w))
    na <- FALSE
    if(na.rm) na <- is.na(x) | is.na(w)
    sum((x*w)[!na]) / sum(w[!na])
} # cmip5.weighted.mean

#' Return data values
#'
#' @param x A \code{\link{cmip5data}} object
#' @return Data values in \code{x}.
#' @details Abstracts away getting values, the method for which
#' varies depending on backend implementation.
#' @keywords internal
#' @note This is an internal RCMIP5 function and not exported.
vals <- function(x) {
    assert_that(class(x)=="cmip5data")
    if(is.data.frame(x$val)) {
        x$val$value
    } else if(is.array(x$val)) {
        x$val
    } else
        stop("Unknown data implementation")
} # vals

#' Return number of data values
#'
#' @param x A \code{\link{cmip5data}} object
#' @return Number of data values in \code{x}.
#' @details Abstracts away getting number of values, the method for which
#' varies depending on backend implementation.
#' @keywords internal
#' @note This is an internal RCMIP5 function and not exported.
nvals <- function(x) {
    assert_that(class(x)=="cmip5data")
    if(is.data.frame(x$val)) {
        nrow(x$val)
    } else if(is.array(x$val)) {
        length(x$val)
    } else
        stop("Unknown data implementation")
} # nvals
