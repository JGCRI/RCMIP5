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
#' same as the caller for lev/depth (if present) and time, but lon and lat are reduced to 
#' 1 (i.e. no dimensionality). A \code{numCells} field is also added, recording the number
#' of cells in the spatial grid.
#' @details If 'lev' and/or 'depth' dimensions are present, the stat function is calculated
#' for all combinations of these. 
#' @note We expect that weighted.mean and a weighted sum will be the most frequent
#' calculations needed. The former is built into R, and the latter can generally
#' be calculated as weighted.mean * sum(area). A user-supplied stat function must 
#' follow the weighted.mean syntax, in particular 
#' accepting parameters 'x' (data) and 'w' (weights) of equal size.
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeDepthLevStat}} \code{\link{makeMonthlyStat}} 
#' @export
makeGlobalStat <- function(x, area=NULL, verbose=TRUE, parallel=FALSE, FUN=weighted.mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(area) | class(area)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
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
        if(length(dav) > 2) {
            warning("Ignoring extra area dimensions")
            areavals <- asub(areavals, as.list(rep(1, length(dav)-2)), dims=3:length(dav))       
        }
    }
    if(verbose) cat("Area grid dimensions", dim(areavals), "\n")
    
    # Main computation code
    if(parallel) parallel <- require(foreach) & require(doParallel)
    timer <- system.time({ # time the main computation
        margins <- NULL
        if(timeIndex > 3) margins <- c(3:(timeIndex-1))
        ans <- list()
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) {
                cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
                
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
                           .combine = function(...) abind(..., along=timeIndex-2), # (2)
                           .packages=c('plyr', 'abind')) %dopar% {                 # (3)
                               if(verbose) cat(date(), i, "\n", file=tf, append=T)
                               aaply(asub(x$val, idx=x$time[i] == x$time, dims=timeIndex), 
                                     margins, FUN, w=areavals, ...)                
                           }
        } else {
            if(verbose) cat("Running in serial\n")
            pb <- txtProgressBar(min=1, max=length(x$time), style=3)
            for(i in 1:length(x$time)) {
                if(verbose) setTxtProgressBar(pb, i)
                ans[[i]] <- aaply(asub(x$val, idx=x$time[i] == x$time, dims=timeIndex), 
                                  margins, FUN, w=areavals, ...)
            }
            # All done, now combine answer list with correct 'along' ordering
            # (When both lev and depth are present, timeIndex=5, along=3. When only
            # one is, TimeIndex=4, along=2; and when no lev/depth info, 3/1.)
            # The abind'ing is done by foreach in the parallel logic above.
            ans <- abind(ans, along=timeIndex-2)
        }
        dim(ans) <- c(1, 1, dim(ans))      # add back in spatial dimensions of 1 (as placeholders)      
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # Finish up
    x$val <- ans
    x[c('lat', 'lon')] <- NULL
    x$variable <- paste(as.character(substitute(FUN)), "of", x$variable)
    x$numCells <- length(areavals)
    x <-  addProvenance(x, paste("Computed global", x$variable))
    return(x)
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
