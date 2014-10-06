#' Compute monthly statistic of a variable
#' 
#' We frequently want to summarize CMIP5 data by month, e.g. to understand how
#' air temperature varies over the year for a particular data range. This function 
#' does that for monthly data. The default statistic is \link{mean}, but any 
#' summary function that returns a numeric result can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the monthly
#' mean of the variable. A \code{numYears} field is also added
#' recording the number of years averaged for each month.
#' @details If a Z dimension is present, the stat function is calculated
#' for all combinations of these. No status bar is printed when processing in parallel,
#' but progress is logged to a file (call with verbose=T) that can be monitored.
#' 
#' If the user requests parallel processing (via parallel=T) makeMonthlyStat
#' (i) attempts to load the \code{doParallel} package, and (ii) registers it as a 
#' parallel backend \emph{unless} the user has already done this (e.g. set up a 
#' virtual cluster with particular, desired characteristics). In that case, 
#' makeMonthlyStat respects the existing cluster.
#' @note The \code{val} component of the returned object will always be the same structure
#' as \code{x}, i.e. of dimensions {x, y, z, 12}.
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeZStat}} \code{\link{makeGlobalStat}}
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' makeMonthlyStat(d)
#' summary(makeMonthlyStat(d, verbose=FALSE))
#' \dontrun{
#' summary(makeMonthlyStat(d, verbose=FALSE, parallel=TRUE))
#' }
#' summary(makeMonthlyStat(d, verbose=FALSE, FUN=sd))
#' @export
makeMonthlyStat <- function(x, verbose=FALSE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numYears))
    stopifnot(x$debug$timeFreqStr=="mon")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(length(dim(x$val)) %in% c(3, 4, 5)) # that's all we know
    
    # The ordering of x$val dimensions is lon-lat-Z?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    stopifnot(identical(dim(x$val)[timeIndex], length(x$time)))
    
    # uniqueYears holds the different years in x's time vector
    uniqueYears <- unique(floor(x$time))
    monthIndex <- floor((x$time %% 1) * 12 + 1)
    
    # Prepare for main computation
    if(parallel) {  # go parallel, woo hoo!
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
    timer <- system.time({ # time the main computation, below
        # The computation below splits time across available cores (1), falling back
        # to serial operation if no parallel backend is available. For each time slice,
        # we use asub (2) to extract the correct array slice and use aaply to apply FUN.
        # When finished, combine results using the abind function (3). For this the 'plyr'
        # and 'abind' packages are made available to the child processes (4).
        i <- 1  # this is here only to avoid a CRAN warning (no visible binding inside foreach)
        ans <- suppressWarnings(foreach(i=1:12,                              # (1)
                       .combine=function(...) abind(..., along=timeIndex),   # (3)
                       .packages=c('plyr', 'abind')) %dopar% {               # (4)
                           if(verbose & parallel) cat(date(), i, "\n", file=tf, append=T)
                           if(verbose & !parallel) setTxtProgressBar(pb, i)
                           # Get a timeslice (ts) of data and send to aaply (2)
                           ts <- asub(x$val, idx=(i == monthIndex), dims=timeIndex, drop=FALSE)
                           aaply(ts, c(1:(timeIndex-1)), .drop=FALSE, FUN, ...)
                       })
    }) # system.time
    
    if(verbose) cat('\nTook',timer[3], 's\n')
    
    x$val <- unname(ans)
    x$numYears <- unname(table(floor(monthIndex)))
    x$timeUnit <- "months (summarized)"
    x$time <- 1:12
    addProvenance(x, paste("Calculated", as.character(substitute(FUN)), 
                           "for months", min(x$time), "-", max(x$time)))
} # makeMonthlyStat
