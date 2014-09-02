#' Exploration, manipulation, and summarizing of CMIP5 data.
#'
#' Working with CMIP5 data can be tricky, forcing scientists to
#' write custom scripts and programs. The `RCMIP5` package aims
#' to ease this process, providing a standard, robust, and
#' high-performance set of functions to (i) explore what data
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
#' @import plyr abind ncdf4 reshape2
#' @docType package
#' @name RCMIP5
NULL

#' Constructor for 'cmip5data' class.
#'
#' This constructor has two functions. First, given a list, it makes the list
#' a cmip5data-class object (no check is made that the list has appropriate
#' fields though). Second, if given a numeric value(s), it returns sample/
#' example data in the newly constructed object. This is used extensively by
#' the testing code.
#'
#' @param x list, or numeric (in which case is years of sample data to return)
#' @param monthly Monthly or annual data?
#' @param depth Create depth dimension?
#' @param lev Create lev dimension?
#' @param randomize Randomize initial data?
#' @param lonsize Size of longitude dimension
#' @param latsize Size of latitude dimension
#' @param depthsize Size of depth dimension
#' @param levsize Size of lev dimension
#' @return A cmip5data object, which is a list with the following fields:
#'  \item{val}{A multidimensional array [lon, lat, time] holding the data}
#'  \item{valUnit}{A string containing the value units}
#'  \item{timeUnit}{A string containing the time units}
#'  \item{calendarStr}{A string defining the calendar type}
#'  \item{lat}{A numeric vector containing latitude values}
#'  \item{lon}{A numeric vector containing longitude values}
#'  \item{depth}{A numeric vector depth values; optional}
#'  \item{lev}{A numeric vector level values; optional}
#'  \item{time}{A numeric vector containing time values}
#'  \item{variable}{Variable described by this dataset}
#'  \item{model}{Model of this dataset}
#'  \item{experiment}{Experiment of this dataset}
#'  \item{ensembles}{Ensemble(s) included in this dataset}
#' @docType class
#' @export
cmip5data <- function(x=list(),
                      # parameters for making sample data
                      monthly=TRUE, depth=FALSE, lev=FALSE, randomize=FALSE,
                      lonsize=10, latsize=10, depthsize=5, levsize=5) {
    stopifnot(is.logical(c(monthly, depth, lev, randomize)))

    if (is.list(x)) {
        structure(x, class="cmip5data")
    } else if(is.numeric(x)) {

        # Create sample data. 'x' is years
        years <- x
        ppy <- ifelse(monthly, 12, 1)  # periods per year
        valdims <- c(lonsize, latsize, ppy*length(years))
        depthdim <- NULL
        if(depth) {
            valdims <- c(valdims[1:(length(valdims)-1)], depthsize, valdims[length(valdims)])
            depthdim <- c(0:(depthsize-1))
        }
        levdim <- NULL
        if(lev) {
            valdims <- c(valdims[1:(length(valdims)-1)], levsize, valdims[length(valdims)])
            levdim <- c(0:(levsize-1))
        }

        valData <- 1:2
        if(randomize) valData  <- runif(prod(valdims))

        debuglist <- list(lonUnit="degrees_east",
                          latUnit="degrees_north",
                          startYr=years[1],
                          calendarStr="360_day",
                          timeUnit=paste0("days since ",years[1],"-01-01"),
                          timeRaw=(360/ppy*c(0:(length(years)*ppy-1) )+15)
        )

        if(depth) debuglist$depthUnit <- "m"
        if(lev) debuglist$levUnit <- "m"

        x <- cmip5data(list(variable="dummyvar",
                            domain="dummydomain",
                            model="dummymodel",
                            experiment="dummyexperiment",
                            ensemble="dummyensemble",
                            val=array(valData, dim=valdims),
                            valUnit="dummy unit",
                            calendarStr="360_day",
                            timeFreqStr=ifelse(monthly, "mon", "yr"),

                            # realistic lon (0 to 360) and lat (-90 to 90) numbers
                            lat=180/latsize * c(0:(latsize-1)) - 90 + 180/latsize/2,
                            lon=360/lonsize * c(0:(lonsize-1))  + 360/lonsize/2,

                            depth=depthdim,
                            lev=levdim,
                            time=debuglist$timeRaw/360+min(years),
                            debug=debuglist
        ))
        x <- addProvenance(x, "Dummy data created")
        x
    } else
        stop("Don't know what to do with this class of parameter")
}

#' Print a 'cmip5data' class object.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param ... Other parameters passed to cat
#' @details Prints a one-line summary of the object
#' @method print cmip5data
#' @export
print.cmip5data <- function(x, ...) {

    if(is.null(x$variable)) {
        cat("(Empty cmip5data object)")
        return()
    }

    ansStr <- sprintf('CMIP5: %s %s %s', x$variable, x$model, x$experiment)

    if(!is.null(x$time)){
        ansStr <- sprintf('%s over years %d to %d', ansStr,
                          floor(min(x$time, na.rm=TRUE)),
                          ceiling(max(x$time, na.rm=TRUE)))
    }

    if(!is.null(x$ensembles)){
        ansStr <- sprintf('%s from %d %s', ansStr,
                          length(x$ensembles),
                          ifelse(length(x$ensembles)==1,
                                 "ensemble", "ensembles"))
        }



    print(ansStr)
} # print.cmip5data

#' Summarize a 'cmip5data' class object.
#'
#' @param object A \code{\link{cmip5data}} object
#' @param ... ignored
#' @details Prints a short summary of the object.
#' @return A summary structure of the object.
#' @method summary cmip5data
#' @export
summary.cmip5data <- function(object, ...) {

    ans <- list()
    class(ans) <- "summary.cmip5data"

    #Always assume that cmip5 objects have the following defined:
    ans$variable <- object$variable
    ans$valUnit <- object$valUnit

    ans$domain <- object$domain
    ans$model <- object$model
    ans$experiment <- object$experiment
    ans$ensembles <- object$ensembles

    if(!is.null(object$numMonths)) {
        ans$type <- paste("annual summary (of", mean(object$numMonths), "months)")
    } else if(!is.null(object$numYears)) {
        ans$type <- paste("monthly summary (of", mean(object$numYears), "years)")
    } else if(!is.null(object$numCells)) {
        ans$type <- paste("spatial summary (of", object$numCells, "cells)")
    } else {
        ans$type <- "primary data"
    }

    if(!is.null(object$filtered)) {
        ans$type <- paste(ans$type, "(filtered)")
    }

    if(!is.null(object$area)) {
        ans$type <- paste(ans$type, "(regridded)")
    }


    ans$spatial <- paste0("lon [", length(object$lon),
                          "] lat [", length(object$lat),
                          "] depth [", length(object$depth),
                          "] lev [", length(object$lev), "]")

    if(!is.null(object$time)){
        ans$time <- paste0(object$timeFreqStr, " [", length(object$time), "] ", object$debug$timeUnit)
    }
    ans$size <- as.numeric(object.size(object))
    ans$valsummary <- c(min(as.vector(object$val), na.rm=TRUE),
                        mean(as.vector(object$val), na.rm=TRUE),
                        max(as.vector(object$val), na.rm=TRUE))
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
print.summary.cmip5data <- function(x, ...) {
    cat("CMIP5 data -", x$type, "\n")
    cat("Variable: ", x$variable, " (", x$valUnit, ") from model ", x$model, "\n", sep="")
    cat("Data range: ", round(x$valsummary[1], 2), "-", round(x$valsummary[3], 2),
        "  Mean: ", round(x$valsummary[2], 2), "\n", sep="")
    cat("Experiment:", x$experiment, "-", length(x$ensembles), "ensemble(s)\n")
    cat("Spatial dimensions:", x$spatial, "\n")
    cat("Time dimension:", x$time, "\n")
    cat("Size:", format(round(x$size/1024/1024, 1), nsmall=1), "MB\n")
    cat("Provenance has", length(x$provenance), "entries\n")
} # print.summary.cmip5data

#' Convert a cmip5data object to a data frame
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @return The object converted, as well as possible, to a data frame
#' @export
as.data.frame.cmip5data <- function(x, verbose=FALSE) {
    years <- x$time

    if(verbose) cat("Melting...\n")
    df <- reshape2::melt(x$val)

    if(verbose) cat("Filling in dimensional data...\n")
    df[,1] <- x$lon[df[,1]]
    df[,2] <- x$lat[df[,2]]
    names(df)[1:2] <- c("lon","lat")
    timeindex <- 4   # Assume there's depth or lev
    if(!is.null(x$lev)) {
        if(verbose) cat("Found lev")
        df[,3] <- x$lev[df[,3]]
        names(df)[3] <- "lev"
    } else if(!is.null(x$depth)) {
        if(verbose) cat("Found depth")
        df[,3] <- x$depth[df[,3]]
        names(df)[3] <- "depth"
    } else
        timeindex <- 3
    df[,timeindex] <- years[df[,timeindex]]
    names(df)[timeindex] <- "time"

    if(!is.null(x$variable))
        df$variable <- factor(x$variable)
    if(!is.null(x$model))
        df$model <- factor(x$model)
    if(!is.null(x$experiment))
        df$experiment <- factor(x$experiment)
    if(!is.null(x$valUnit))
        df$valUnit <- factor(x$valUnit)
    return(df)
} # as.data.frame.cmip5data

#' Make package datasets and write them to disk.
#'
#' @param path root of directory tree
#' @param maxSize max size (in MB) of dataset to write
#' @param outpath directory to write to
#' @details Writes all available ensembles to disk as Rdata files, subject to
#' a maximum size parameter (CRAN says keep sample data < 5MB).
#' @note This is an internal RCMIP5 function and not exported.
makePackageData <- function(path="./sampledata", maxSize=Inf, outpath="./data") {
    if(!file.exists(outpath)) dir.create(outpath)
    stopifnot(file.exists(outpath))
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
