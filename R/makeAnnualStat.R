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
#' @details If 'lev' and/or 'depth' dimensions are present, the stat function is calculated
#' for all combinations of these. 
#' @seealso \code{\link{makeDepthLevStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @export
makeAnnualStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    #stopifnot(x$timeFreqStr %in% 'mon')    # val array in months?
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    # Check that data array dimensions match those of lon, lat, and time
    stopifnot(identical(dim(x$val)[c(1, 2, timeIndex)],
                        c(length(x$lon), length(x$lat), length(x$time))))
    
    # uniqueYears holds the different years in x's time vector
    uniqueYears <- unique(floor(x$time))
    
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
            # To parallelize this computation, split years across available cores (1).
            # When finished, combine results using the abind function (2). Make the 'plyr'
            # package available to the child processes (3). The computation in each process
            # is equivalent to the inside of the serial loop below.
            ans <- foreach(i=1:length(uniqueYears),                                     # (1)
                           .combine = function(...)  abind(..., along=timeIndex),       # (2)
                           .packages=c('plyr', 'abind')) %dopar% {                      # (3)
                               if(verbose) cat(date(), i, "\n", file=tf, append=T)
                               aaply(asub(x$val, idx=uniqueYears[i] == floor(x$time), dims=timeIndex),
                                     c(1:(timeIndex-1)), FUN, ...)
                           } # %dopar%
        } else { # not parallel
            if(verbose) cat("Running in serial\n")
            ans <- list()
            pb <- txtProgressBar(min=1, max=length(uniqueYears), style=3)
            for(i in 1:length(uniqueYears)) {  # For each year...
                if(verbose) setTxtProgressBar(pb, i)
                # ...apply the annual stat function to the data subset for which
                # the years match the current year of the loop. Uses aaply from 'plyr',
                # asub and abind from 'abind' packages.
                ans[[i]] <- aaply(asub(x$val,
                                       idx=uniqueYears[i] == floor(x$time),
                                       dims=timeIndex),
                                  c(1:(timeIndex-1)), FUN, ...)
            } # for
            ans <- abind(ans, along=timeIndex) # convert list to array
        } # if(parallel)
        
    ) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # We now have new computed data. Overwrite original data, record # of months per year,
    # and update the time vector, time frequency string, and provenance
    x$val <- unname(ans)
    x$numMonths <- table(floor(x$time)) ##KTB numMonths should be changed to numPerYr
    x$time <- uniqueYears
    x$timeFreqStr <- "years (summarized)"
    x <- addProvenance(x, paste("Calculated",as.character(substitute(FUN)),
                                "for years", min(x$time), "-", max(x$time)))
    
    return(x)
} # makeAnnualStat
