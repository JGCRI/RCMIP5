library(plyr)

#' Compute monthly means of a variable
#'
#' @param x list. A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param maxyears numeric. Limit computation to this many years (e.g. for testing)
#' @param FUN function. Function to apply across months of year
#' @return list with elements 'files', 'val', 'valUnit', timeUnit', 'calendarStr',
#'      'lat', 'lon', and 'time'.
#' @export
#' @examples
#' makeSeasonalMean(loadModel('nbp','HadGEM2-ES','rcp85',verbose=T))
makeMonthlyMean <- function(x, yearRange=c(-Inf, Inf), verbose=TRUE, parallel=FALSE, FUN=mean) {
    
    # Sanity checks
    stopifnot(length(x)==8 & is.list(x))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(yearRange)==2 & is.numeric(yearRange))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # TODO: is calendarStr guaranteed to have # days in positions 1-3? 
    # Would it better to split the string based on underscore?
    numDays <- as.numeric(substr(x$calendarStr, 1, 3))
    stopifnot(numDays>0)
    
    # timeUnit is a string like "days since 1859-12-01". Extract startDate from this
    # TODO: this code shared with makeAnnualMean, could abstract into function
    startYrArr <- as.numeric(unlist(strsplit(
        regmatches(x$timeUnit, regexpr('\\d+.\\d+.\\d+', x$timeUnit)), '-')))
    startYr <- startYrArr[1] + (startYrArr[2]-1)/12 + (startYrArr[3]-1)/numDays
    
    # More sanity checks
    stopifnot(startYrArr[2] %in% 1:12)
    stopifnot(startYrArr[3] %in% 1:31)
    stopifnot(startYr >= 1850 & startYr < 2300)
    
    yearIndex <- x$time/numDays + startYr
    yearFilter <- floor(yearIndex) >= min(yearRange) & floor(yearIndex) <= max(yearRange)
    uniqueYears <- unique(floor(yearIndex))
    monthIndex <- floor((yearIndex %% 1) * 12 + 1)
    ans <- array(NA_real_, dim=c(dim(x$val)[c(1,2)], 12))
    
    timer <- system.time( # time the main computation, below
        
        if(parallel & 
               require(foreach) & require(doParallel) & require(abind)) {  # go parallel, woo hoo!
            
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            ans <- foreach(i=1:12, .combine = function(...) abind(..., along=3), .packages='plyr') %dopar% {
                aaply(x$val[,,(i == monthIndex) & yearFilter], c(1,2), FUN)            }
        } else {
            if(verbose) cat("Running in serial\n")
            for(i in 1:12) {
                if(verbose) cat(i, " ")
                ans[,,i] <- aaply(x$val[,,(i == monthIndex) & yearFilter], c(1,2), FUN)
            }
        }
    ) # system.time
    
    if(verbose) cat('Took',timer[3], 's\n')
    
    # TODO: do we want to add information about years summarized?
    invisible(list(val=ans, files=x$files,
                   lat=x$lat, lon=x$lon,
                   valUnit=x$valUnit))
} # makeMonthlyMean
