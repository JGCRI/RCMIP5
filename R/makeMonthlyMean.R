library(plyr)

#' Compute monthly means of a variable
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param yearRange numeric vector. Limit computation to this year range (e.g. for testing)
#' @param FUN function. Function to apply across months of year
#' @return A \code{\link{cmip5data}} object.
#' @export
#' @examples
#' makeMonthlyMean(loadModel('nbp','HadGEM2-ES','rcp85',verbose=TRUE,demo=TRUE))
#' @seealso \code{\link{makeAnnualMean}}
makeMonthlyMean <- function(x, yearRange=c(1, Inf), verbose=TRUE, parallel=FALSE, FUN=mean) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numYears))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(yearRange)==2 & is.numeric(yearRange))
    stopifnot(all(yearRange > 0))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(dim(x$val)==c(length(x$lon),length(x$lat),length(x$time)))
    
    yearIndex <- compute_yearIndex(x)
    yearFilter <- floor(yearIndex) >= min(yearRange) & floor(yearIndex) <= max(yearRange)
    uniqueYears <- unique(floor(yearIndex))
    monthIndex <- floor((yearIndex %% 1) * 12 + 1)
    ans <- array(NA_real_, dim=c(dim(x$val)[c(1,2)], 12))
    
    if(parallel) parallel <- require(foreach) & require(doParallel) & require(abind)
    timer <- system.time( # time the main computation, below
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            ans <- foreach(i=1:12, .combine = function(...) abind(..., along=3), .packages='plyr') %dopar% {
                aaply(x$val[,,(i == monthIndex) & yearFilter], c(1,2), FUN)
            }
        } else {
            if(verbose) cat("Running in serial\n")
            for(i in 1:12) {
                if(verbose) cat(i, " ")
                ans[,,i] <- aaply(x$val[,,(i == monthIndex) & yearFilter], c(1,2), FUN)
            }
            if(verbose) cat("\n")
        }
    ) # system.time
    
    if(verbose) cat('Took',timer[3], 's\n')
    
    x$val <- unname(ans)
    x$numYears <- unname(table(floor(monthIndex)))
    return(x)
} # makeMonthlyMean
