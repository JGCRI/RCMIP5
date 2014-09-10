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
#' @details If 'lev' and/or 'depth' dimensions are present, the stat function is calculated
#' for all combinations of these. 
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeDepthLevStat}} \code{\link{makeGlobalStat}}
#' @export
makeMonthlyStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numYears))
    stopifnot(x$timeFreqStr=="mon")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(length(dim(x$val)) %in% c(3, 4, 5)) # that's all we know
    
    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
    # Anything else is not valid.
    timeIndex <- length(dim(x$val))
    stopifnot(timeIndex %in% c(3, 4)) # that's all we know
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    stopifnot(dim(x$val)[c(1,2,timeIndex)]==c(length(x$lon),length(x$lat),length(x$time)))
    
    uniqueYears <- unique(floor(x$time))
    monthIndex <- floor((x$time %% 1) * 12 + 1)
    
    if(parallel) parallel <- require(foreach) & require(doParallel)
    timer <- system.time( # time the main computation, below
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) {
                cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
                
                # Set up tempfile to log progress
                tf <- tempfile()
                cat(date(), "Started\n", file=tf)
                if(verbose) cat("Progress logged to", tf, "\n")
            }
            # To parallelize this computation, split years across available cores (1).
            # When finished, combine results using the abind function (2). Make the 'plyr'
            # and 'abind' packages available to the child processes (3). The computation 
            # in each process is equivalent to the inside of the serial loop below.
            ans <- foreach(i=1:12,                                               # (1)
                           .combine=function(...) abind(..., along=timeIndex),   # (2)
                           .packages=c('plyr', 'abind')) %dopar% {               # (3)
                               if(verbose) cat(date(), i, "\n", file=tf, append=T)
                               aaply(asub(x$val, idx=(i == monthIndex), dims=timeIndex), 
                                     c(1:(timeIndex-1)), FUN, ...)
                           }
        } else {
            if(verbose) {
                cat("Running in serial\n")
                pb <- txtProgressBar(min=0, max=12, style=3)
            }
            ans <- list()
            for(i in 1:12) {
                if(verbose) setTxtProgressBar(pb, i)
                ans[[i]] <- aaply(
                    asub(x$val, idx=(i == monthIndex), dims=timeIndex), c(1:(timeIndex-1)), FUN, ...)
            }
            ans <- abind(ans, along=timeIndex)
        }
    ) # system.time
    
    if(verbose) cat('\nTook',timer[3], 's\n')
    
    x$val <- unname(ans)
    x$numYears <- unname(table(floor(monthIndex)))
    x$timeUnit <- "months (summarized)"
    x$time <- 1:12
    x <- addProvenance(x, paste("Calculated", as.character(substitute(FUN)), 
                                "for months", min(x$time), "-", max(x$time)))
    return(x)
} # makeMonthlyStat
