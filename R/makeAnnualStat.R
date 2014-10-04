#' Compute annual statistic of a variable
#'
#' Most CMIP5 data are monthly, and we frequently want to summarize these to annual
#' numbers. This function does that (although annual files also occur, and will be
#' handled as well). The default statistic is \link{mean}, but any summary
#' function that returns a numeric result can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the annual
#' mean of the variable. A \code{numMonths} field is also added
#' recording the number of months averaged for each year.
#' @details If Z dimension is present, the stat function is calculated
#' for all values of these. No status bar is printed when processing in parallel,
#' but progress is logged to a file (call with verbose=T) that can be monitored.
#' 
#' If the user requests parallel processing (via parallel=T) makeAnnualStat
#' (i) attempts to load the \code{doParallel} package, and (ii) registers it as a 
#' parallel backend \emph{unless} the user has already done this (e.g. set up a 
#' virtual cluster with particular, desired characteristics). In that case, 
#' makeAnnualStat respects the existing cluster.
#' @note The \code{val} component of the returned object will always be the same structure
#' as \code{x}, i.e. of dimensions {x, y, z, t}.
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' makeAnnualStat(d)
#' summary(makeAnnualStat(d, verbose=FALSE))
#' \dontrun{
#' summary(makeAnnualStat(d, verbose=FALSE, parallel=TRUE))
#' }
#' summary(makeAnnualStat(d, verbose=FALSE, FUN=sd))
#' @seealso \code{\link{makeZStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @export
makeAnnualStat <- function(x, verbose=FALSE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    #stopifnot(x$debug$timeFreqStr %in% 'mon')    # val array in months?
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # The ordering of x$val dimensions is lon-lat-Z?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    stopifnot(identical(dim(x$val)[timeIndex], length(x$time)))
    
    # uniqueYears holds the different years in x's time vector
    uniqueYears <- unique(floor(x$time))
    
    # Prepare for main computation
    doParallelAlreadyLoaded <- "package:doParallel" %in% search()
    if(parallel) parallel <- require(doParallel, quietly=!verbose)
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
    timer <- system.time({  # time the main computation, below
        # The computation below splits time across available cores (1), falling back
        # to serial operation if no parallel backend is available. For each time slice,
        # we use asub (2) to extract the correct array slice and use aaply to apply FUN.
        # When finished, combine results using the abind function (3). For this the 'plyr'
        # and 'abind' packages are made available to the child processes (4).
        i <- 1  # this is here only to avoid a CRAN warning (no visible binding inside foreach)
        ans <- foreach(i=1:length(uniqueYears),                                     # (1)
                       .combine = function(...)  abind(..., along=timeIndex),       # (3)
                       .packages=c('plyr', 'abind')) %dopar% {                      # (4)
                           if(verbose & parallel) cat(date(), i, "\n", file=tf, append=T)
                           if(verbose & !parallel) setTxtProgressBar(pb, i)
                           # Get a timeslice (ts) of data and send to aaply (2)
                           ts <- asub(x$val, idx=uniqueYears[i] == floor(x$time), dims=timeIndex, drop=FALSE)
                           aaply(ts, c(1:(timeIndex-1)), .drop=FALSE, FUN, ...)
                       } # %dopar%
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # We now have new computed data. Overwrite original data, record # of months per year,
    # and update the time vector, time frequency string, and provenance
    x$val <- unname(ans)
    x$numMonths <- table(floor(x$time))
    x$time <- uniqueYears
    x$debug$timeFreqStr <- "years (summarized)"
    addProvenance(x, paste("Calculated", as.character(substitute(FUN)),
                           "for years", min(x$time), "-", max(x$time)))
} # makeAnnualStat
