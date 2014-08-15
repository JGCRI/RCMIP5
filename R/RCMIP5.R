library(reshape2)

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

#' Constructor for 'cmip5data' class.
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

#' Print a 'cmip5data' class object.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param ... Other parameters passed to print
#' @details Prints a one-line summary of the object
#' @export
print.cmip5data <- function(x, ...) {
    nfiles <- length(x$files)
    nensembles <- length(x$ensembles)
    
    yearString <- "[date parse error]"
    try({
        yearRange <- round(range(x$time), 2)
        yearString <- paste(yearRange[1], yearRange[2], sep="-")
    }, silent=T)
    
    cat("CMIP5 ")
    if(!is.null(x$numMonths)) {
        cat("- annual summary: ")
    } else if(!is.null(x$numYears)) {
        cat(" - monthly summary ")
    } else if(!is.null(x$numCells)) {
        cat(" - spatial summary ")
    }
    
    cat(paste(x$variable, x$model, x$experiment, yearString,
              paste0("[", paste(dim(x$val), collapse=" "), "]"),
              "from", nensembles, ifelse(nensembles==1, "ensemble", "ensembles"),
              nfiles, ifelse(nfiles==1,"file","files"), "\n", ...))
}

#' Summarize a 'cmip5data' class object.
#'
#' @param object A \code{\link{cmip5data}} object
#' @param ... Other parameters
#' @details Prints a short summary of the object
#' @export
summary.cmip5data <- function(object, ...) {
    cat("CMIP5 data")
    ##TODO This may be unnessacary with the addition of provenance
    if(!is.null(object$numMonths)) {
        cat(" - annual summary\n")
        cat("(Mean months summarized:", mean(object$numMonths), "\n")
    } else if(!is.null(object$numYears)) {
        cat(" - monthly summary\n")
        cat("Mean years summarized:", mean(object$numYears), "\n")
    } else if(!is.null(object$numCells)) {
        cat(" - spatial summary\n")
        cat("Cells summarized:", object$numCells, "\n")
    } else cat("\n")
    
    cat("Variable:", object$variable, '\n')
    cat("Domain:", object$domain, '\n')
    cat("Model:", object$model, "\n")
    cat("Eobjectperiment:", object$eobjectperiment, "\n")
    cat("Ensembles:", object$ensembles, "\n")
    cat("Spatial: lon [", length(object$lon), "] lat [", length(object$lat),
        "] lev [", length(object$lev), "] or depth [", length(object$depth), "]\n")
    if(!object$timeFreqStr %in% 'fobject'){ ##Only show the time if not fobject
        cat("Time: step size [",object$time[2] - object$time[1],"] yrs, length ",
            length(object$time), ", range [",object$time[1], ', ', rev(object$time)[1] ,"]\n",
            sep="")
    }
    cat("Data: [", object$valUnit, "], dimensions [", paste(dim(object$val), collapse=" "),
        "]\n", sep="")
    print(summary(as.vector(object$val)))
    cat("Size: ")
    print(object.size(object), units="MB")
    
    cat('\nProvenance:\n')
    cat(paste(object$provenance, '\n', collapse=' '))
}

#' Convert a cmip5data object to a data frame
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @return The object converted, as well as possible, to a data frame
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
    
    return(df)
} # as.data.frame.cmip5data

#' Make package datasets and write them to disk
#'
#' @param path root of directory tree
#' @param maxSize max size (in MB) of dataset to write
#' @param outpath directory to write to
#' @details Writes all available ensembles to disk as Rdata files, subject to
#' a maximum size parameter (CRAN says keep sample data < 5MB!).
#' @note This is an internal RCMIP5 function and not exported.
makePackageData <- function(path="./sampledata", maxSize=Inf, outpath="./inst/extdata") {
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
