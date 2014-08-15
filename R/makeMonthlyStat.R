library(plyr)
library(abind)

#' Compute monthly means (or other function) of a variable
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the monthly
#' mean of the variable. A \code{numYears} field is also added
#' recording the number of years averaged for each month.
#' @export
#' @seealso \code{\link{makeAnnualStat}}
makeMonthlyStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numYears))
    stopifnot(x$timeFreqStr=="mon")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(length(dim(x$val)) %in% c(3, 4, 5)) # that's all we know
    
    timeIndex <- length(dim(x$val))  # time is always the last index
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    stopifnot(dim(x$val)[c(1,2,timeIndex)]==c(length(x$lon),length(x$lat),length(x$time)))
    
    uniqueYears <- unique(floor(x$time))
    monthIndex <- floor((x$time %% 1) * 12 + 1)
    
    if(parallel) parallel <- require(foreach) & require(doParallel)
    timer <- system.time( # time the main computation, below
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            ans <- foreach(i=1:12, .combine=function(...) abind(..., along=timeIndex), .packages='plyr') %dopar% {
                aaply(asub(x$val, idx=(i == monthIndex), dims=timeIndex), c(1:(timeIndex-1)), FUN, ...)
            }
        } else {
            if(verbose) cat("Running in serial\n")
            ans <- list()
            for(i in 1:12) {
                if(verbose) cat(i, " ")
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
    x$provenance <- addProvenance(x$provenance, paste("Calculated", as.character(substitute(FUN)), 
                                                      "for months", min(x$time), "-", max(x$time)))
    return(x)
} # makeMonthlyStat
