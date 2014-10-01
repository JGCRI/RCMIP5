#' Compute global statistic of a variable
#' 
#' We frequently want a global summary for CMIP5 data, usually weighted by the 
#' grid cell areas used by each particular model. This function does that. If no
#' area weighting is supplied, a warning is given. The default statistic is \link{weighted.mean},
#' but any summary function that returns a numeric result can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param area An area \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across grid
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, in which the \code{val} dimensions are the
#' same as the caller for Z (if present) and time, but lon and lat are reduced to 
#' 1 (i.e. no dimensionality). A \code{numCells} field is also added, recording the number
#' of cells in the spatial grid.
#' @details If a Z dimension is present, the stat function is calculated
#' for all combinations of these. No status bar is printed when processing in parallel,
#' but progress is logged to a file (call with verbose=T) that can be monitored.
#' 
#' This function is more complicated than the other makeXxxStat functions, because
#' it provides explicit support for area-weighted functions. We expect that 
#' weighted.mean and a weighted sum will be the most frequent
#' calculations needed. The former is built into R, and the latter can generally
#' be calculated as weighted.mean * sum(area). A user-supplied stat function must 
#' follow the weighted.mean syntax, in particular 
#' accepting parameters 'x' (data) and 'w' (weights) of equal size.
#' 
#' If the user requests parallel processing (via parallel=T) makeGlobalStat
#' (i) attempts to load the \code{doParallel} package, and (ii) registers it as a 
#' parallel backend \emph{unless} the user has already done this (e.g. set up a 
#' virtual cluster with particular, desired characteristics). In that case, 
#' makeGlobalStat respects the existing cluster.
#' @note The \code{val} component of the returned object will always be the same structure
#' as \code{x}, i.e. of dimensions {1, 1, z, t}.
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeZStat}} \code{\link{makeMonthlyStat}} 
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' makeGlobalStat(d)
#' summary(makeGlobalStat(d, verbose=FALSE))
#' summary(makeGlobalStat(d, verbose=FALSE, parallel=TRUE))
#' @export
makeGlobalStat <- function(x, area=NULL, verbose=FALSE, parallel=FALSE, FUN=weighted.mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(area) | class(area)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # The ordering of x$val dimensions is lon-lat-Z?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    # Get and check area data, using 1's if nothing supplied
    areavals <- NA
    if(is.null(area)) {
        if(verbose) cat("No grid areas supplied; using calculating values\n")
        x <- addProvenance(x, "About to compute global stat. Grid areas calculated.")
        areavals <- calcGridArea(x$lon, x$lat, verbose=verbose)
    } else {
        stopifnot(identical(x$lat, area$lat) & identical(x$lon, area$lon))  # must match
        x <- addProvenance(x, "About to compute global stat. Grid areas from following data:")
        x <- addProvenance(x, area)
        areavals <- area$val
        dav <- dim(areavals)
        
        # Because we're now saying all cmip5data are four dimensional, all the time,
        # we probably have to strip off extra dimensions
        if(length(dav) > 2) {
            areavals <- asub(areavals, as.list(rep(1, length(dav)-2)), dims=3:length(dav))       
        }
    }
    if(verbose) cat("Area grid dimensions", dim(areavals), "\n")
    
    # Prepare for main computation
    doParallelAlreadyLoaded <- "package:doParallel" %in% search()
    if(parallel) parallel <- require(doParallel, quietly=!verbose)
    margins <- 3:timeIndex
    if(verbose) cat("Margins are", margins, "\n")
    
    if(parallel) {  # go parallel, woo hoo!
        if(!doParallelAlreadyLoaded) # if the user has already set up a parallel
            registerDoParallel()     # environment, don't mess with it
        
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
    timer <- system.time({ # time the main computation
        # The computation below splits time across available cores (1), falling back
        # to serial operation if no parallel backend is available. For each time slice,
        # we use asub (2) to extract the correct array slice and use aaply to apply FUN.
        # When finished, combine results using the abind function (3). For this the 'plyr'
        # and 'abind' packages are made available to the child processes (4).
        i <- 1  # this is here only to avoid a CRAN warning (no visible binding inside foreach)
        ans <- foreach(i=1:length(x$time),                                     # (1)
                       .combine = abind,                                       # (3)
                       .packages=c('plyr', 'abind')) %dopar% {                 # (4)
                           if(verbose & parallel) cat(date(), i, "\n", file=tf, append=T)
                           if(verbose & !parallel) setTxtProgressBar(pb, i)
                           # Get a timeslice (ts) of data and send to aaply (2)
                           ts <- asub(x$val, idx=x$time[i] == x$time, dims=timeIndex, drop=FALSE)
                           aaply(ts, margins, .drop=FALSE, FUN, w=areavals, ...)
                       }
        ans <- array(ans, dim=c(1, 1, dim(x$val)[margins]))
        
        if(parallel) stopImplicitCluster()
        
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # Finish up
    x$val <- unname(ans)
    x[c('lat', 'lon')] <- NULL
    x$variable <- paste(as.character(substitute(FUN)), "of", x$variable)
    x$numCells <- length(areavals)
    addProvenance(x, paste("Computed global", x$variable))
} # makeGlobalStat

#' Weighted sum--i.e., sum of weighted means. Convenience function
#'
#' @param x vector of data
#' @param w vector of weights
#' @param ... passed on to weighted.mean
#' @return Weighted mean multipled by sum of weights.
#' @export
#' @seealso \code{\link{weighted.mean}}
weighted.sum <- function(x, w=rep(1, length(x)), ...) { weighted.mean(x, w, ...) * sum(w) }
