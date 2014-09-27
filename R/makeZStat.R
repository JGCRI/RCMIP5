#' Compute Z statistic of a variable
#'
#' Some CMIP5 data are four-dimensional: in addition to longitude, latitude,
#' and time, they include a Z dimensions (typically 'depth' or 'lev'). This function
#' computes a summary statistic for all Z values.
#' The default statistic is \link{mean}, but any summary
#' function that returns a numeric result (including weighted.mean, if you 
#' want to apply weights) can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across Zs
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the mean of the
#' variable across Zs A \code{numZs} field is also added
#' recording the number of Z values averaged for each year, and x's original
#' Z field is removed.
#' @details No status bar is printed when processing in parallel,
#' but progress is logged to a file (call with verbose=T) that can be monitored.
#' @note The \code{val} component of the returned object will always be the same structure
#' as \code{x}, i.e. of dimensions {x, y, 1, t}.
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @examples
#' d <- cmip5data(1970:2014, Z=TRUE)   # sample data
#' makeZStat(d)
#' summary(makeZStat(d, verbose=FALSE))
#' summary(makeZStat(d, verbose=FALSE, parallel=TRUE))
#' summary(makeZStat(d, verbose=FALSE, FUN=sd))
#' @export
makeZStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {

    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # The ordering of x$val dimensions is lon-lat-Z?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    if(timeIndex < 4 | is.null(x$Z)) {
        warning("makeZStat called for data with no Z")
        return(x)
    }
    
    if(verbose) cat("Computing on", x$dimNames[3], "\n")
    
    # Check that data array dimensions match those of Z
    stopifnot(identical(dim(x$val)[3], length(x$Z)))
    
    # Prepare for main computation
    if(parallel) parallel <- require(doParallel)
    if(parallel) {  # go parallel, woo hoo!
        registerDoParallel()
        if(verbose) {
            cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            
            # Set up tempfile to log progress
            tf <- tempfile()
            cat(date(), "Started\n", file=tf)
            if(verbose) cat("Progress logged to", tf, "\n")
        }
    } else if(verbose) {
        cat("Running in serial\n")
        pb <- txtProgressBar(min=0, max=length(x$time), style=3)
    }        
    
    # Main computation code
    timer <- system.time({  # time the main computation, below
        # The computation below splits time across available cores (1), falling back
        # to serial operation if no parallel backend is available. For each time slice,
        # we use asub (2) to extract the correct array slice and use aaply to apply FUN.
        # When finished, combine results using the abind function (3). For this the 'plyr'
        # and 'abind' packages are made available to the child processes (4).
        ans <- foreach(i=1:length(x$time),                                     # (1)
                       .combine = function(...)  abind(..., along=timeIndex),  # (3)
                       .packages=c('plyr', 'abind')) %dopar% {                 # (4)
                           if(verbose & parallel) cat(date(), i, "\n", file=tf, append=T)
                           if(verbose & !parallel) setTxtProgressBar(pb, i)
                           # Get a timeslice (ts) of data and send to aaply (2)
                           ts <- asub(x$val, idx=x$time[i] == x$time, dims=timeIndex, drop=FALSE)
                           aaply(ts, 1:2, .drop=FALSE, FUN, ...)                                                                                     
                       }
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # We now have new computed data. Overwrite original data and update provenance
    x$val <- unname(ans)
    x$numZs <- length(x$Z)
    x$Z <- NULL
    addProvenance(x, paste("Calculated", as.character(substitute(FUN)),
                           "for Z (", x$dimNames[3], ")") )
} # makeDepthLevStat
