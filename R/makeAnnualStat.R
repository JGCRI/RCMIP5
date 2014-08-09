library(plyr)
library(abind)

#' Compute annual mean (or other function) of a variable
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @return A \code{\link{cmip5data}} object.
#' @export
#' @examples
#' makeAnnualStat(loadModel('nbp','HadGEM2-ES','rcp85',verbose=TRUE,demo=TRUE))
#' @seealso \code{\link{makeMonthlyMean}}
makeAnnualStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numMonths))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(length(dim(x$val)) %in% c(3, 4, 5)) # that's all we know
    
    timeIndex <- length(dim(x$val))  # time is always the last index
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    stopifnot(dim(x$val)[c(1,2,timeIndex)]==c(length(x$lon),length(x$lat),length(x$time)))
    
    uniqueYears <- unique(floor(x$time))
    
    if(parallel) parallel <- require(foreach) & require(doParallel) & require(abind)
    timer <- system.time( # time the main computation, below
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            ans <- foreach(i=1:length(uniqueYears), .combine = function(...) abind(..., along=timeIndex), .packages='plyr') %dopar% {
                aaply(asub(x$val, idx=uniqueYears[i] == floor(x$time), dims=timeIndex), c(1:(timeIndex-1)), FUN)
            }
        } else {
            if(verbose) cat("Running in serial\n")
            ans <- list()
            for(i in 1:length(uniqueYears)) {
                if(verbose & floor(i/1)==i/1) cat(i, " ")
                ans[[i]] <- aaply(asub(x$val, idx=uniqueYears[i] == floor(x$time), 
                                       dims=timeIndex), c(1:(timeIndex-1)), FUN)
            }
            ans <- abind(ans, along=timeIndex)
        }
    ) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    x$val <- unname(ans)
    x$time <- uniqueYears
    x$timeUnit <- "years (summarized)"
    x$numMonths <- table(floor(x$time))
    x$provenance <- addProvenance(x$provenance, paste("Calculated", as.character(substitute(FUN)), 
                                                      "for years", min(x$time), "-", max(x$time)))
    return(x)
} # makeAnnualStat