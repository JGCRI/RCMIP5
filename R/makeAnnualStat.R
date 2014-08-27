#' Compute annual statistic of a variable.
#'
#' Most CMIP5 data are monthly, and we frequently want to summarize these to annual
#' numbers. This function does that (although annual files also occur, and will be
#' handled as well). The default statistic is \link{mean}, but any summary
#' function that returns a numeric result can be used.
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the annual
#' mean of the variable. A \code{numMonths} field is also added
#' recording the number of months averaged for each year.
#' @export
#' @seealso \code{\link{makeMonthlyStat}}
makeAnnualStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    #stopifnot(x$timeFreqStr %in% 'mon')    # val array in months?
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    if(verbose) cat('starting makeAnnualStat\n')
    
    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
    # Anything else is not valid.
    stopifnot(length(dim(x$val)) %in% c(3, 4)) # that's all we know
    
    # Figure out where the time index is (always last dimension)
    timeIndex <- length(dim(x$val))
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
            if(verbose) cat("Running in parallel [", getDoParWorkers(),
                            "cores ]\n")
            
            # To parallelize this computation, split years across available cores (1).
            # When finished, combine results using the abind function (2). Make the 'plyr'
            # package available to the child processes (3). The computation in each process
            # (4) is equivalent to the inside of the serial loop below.
            ans <- foreach(i=1:length(uniqueYears),                                     # (1)
                           .combine = function(...)  abind(..., along=timeIndex),       # (2)
                           .packages='plyr')  %dopar% {                                 # (3)
                aaply(asub(x$val, idx=uniqueYears[i] == floor(x$time), dims=timeIndex), # (4)
                      c(1:(timeIndex-1)), FUN, ...)                                                                                                                   
            } # %dopar%
        } else { # not parallel
            if(verbose) cat("Running in serial\n")
            ans <- list()
            for(i in 1:length(uniqueYears)) {  # For each year...
                if(verbose) cat(i, " ")
                # ...apply the annual stat function to the data subset for which
                # the years match the current year of the loop. Uses aaply from 'plyr',
                # asub and abind from 'abind' packages.
                ans[[i]] <- aaply(asub(x$val,
                                       idx=uniqueYears[i] == floor(x$time),
                                       dims=timeIndex),
                                  c(1:(timeIndex-1)), FUN, ...)
            } # for
            ans <- abind(ans, along=timeIndex) # convert list to array
        }# if(parallel)
        
    ) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # We now have new computed data. Overwrite original data, record # of months per year,
    # and update the time vector, time frequency string, and provenance
    x$val <- unname(ans)
    x$numMonths <- table(floor(x$time)) ##KTB numMonths should be changed to numPerYr
    x$time <- uniqueYears
    x$timeFreqStr <- "years (summarized)"
    x$provenance <- addProvenance(x$provenance,
                                  paste("Calculated",
                                        as.character(substitute(FUN)),
                                        "for years",
                                        min(x$time), "-", max(x$time)))
    
    if(verbose) cat('done with makeAnnualStat\n')
    return(x)
} # makeAnnualStat
