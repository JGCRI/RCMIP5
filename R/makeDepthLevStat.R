#' Compute depth or level statistic of a variable
#'
#' Some CMIP5 data are four-dimensional: in addition to longitude, latitude,
#' and time, they include a 'depth' or 'lev' (level) dimension. This function
#' computes a summary statistic for all depths or levels.
#' The default statistic is \link{mean}, but any summary
#' function that returns a numeric result (including weighted.mean, if you 
#' want to apply weights) can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across depths or levels
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the mean of the
#' variable across depths. A \code{numDepths} or \code{numLevels} field is also added
#' recording the number of depths or levels averaged for each year, and x's original
#' 'depths' or 'levs' field is removed.
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @export
makeDepthLevStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    if(timeIndex == 3) {
        warning("makeDepthLevStat called for data with no depth or lev")
        return(x)
    }
    
    if(!is.null(x$depth)) {
        depthvals <- x$depth
        x$numDepths <- length(depthvals)
    } else if(!is.null(x$lev)) {
        depthvals <- x$lev
        x$numLevs <- length(depthvals)
    } else
        stop("Data structure is missing depth/lev data")
    
    # Check that data array dimensions match those of lon, lat, and time
    stopifnot(identical(dim(x$val)[3], length(depthvals)))
    
    # Parallel processing uses the foreach and doParallel packages, if available
    if(parallel) parallel <- require(foreach) & require(doParallel)
    
    timer <- system.time(  # time the main computation, below
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) {
                cat("Running in parallel [", getDoParWorkers(),
                            "cores ]\n")
                
                # Set up tempfile to log progress
                tf <- tempfile()
                cat(date(), "Started\n", file=tf)
                if(verbose) cat("Progress logged to", tf, "\n")
            }
            # To parallelize this computation, split time across available cores (1).
            # When finished, combine results using the abind function (2). Make the 'plyr'
            # package available to the child processes (3). The computation in each process
            # is equivalent to the inside of the serial loop below.
            ans <- foreach(i=1:length(x$time),                                     # (1)
                           .combine = function(...)  abind(..., along=timeIndex),  # (2)
                           .packages='plyr')  %dopar% {                            # (3)
                               if(verbose) cat(date(), i, "\n", file=tf, append=T)
                               aaply(x$val[,,,i], 1:2, .drop=FALSE, FUN, ...)                                                                                               
                           }
        } else { # not parallel
            if(verbose) cat("Running in serial\n")
            ans <- list()
            pb <- txtProgressBar(min=1, max=length(x$time), style=3)
            for(i in 1:length(x$time)) {  # for each time slice...
                if(verbose) setTxtProgressBar(pb, i)
                # ...apply the annual stat function to the data subset for which
                # the time matches the current loop. Uses aaply from 'plyr',
                # asub and abind from 'abind' packages.
                ans[[i]] <- aaply(x$val[,,,i], 1:2, .drop=FALSE, FUN, ...)
            } # for
            ans <- abind(ans, along=timeIndex) # convert list to array
        } # if(parallel)
        
    ) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # We now have new computed data. Overwrite original data and update provenance
    x$val <- unname(ans)
    x <- addProvenance(x, paste("Calculated", as.character(substitute(FUN)),
                                "for depth/lev"))
    
    return(x)
} # makeDepthLevStat
