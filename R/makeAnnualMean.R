library(plyr)

#' Compute annual mean of a variable
#'
#' @param x list. A structure returned from loadEnsemble() or loadModel()
#' @param yearRange numeric vector. Limit computation to this year range (e.g. for testing)
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @return list with elements 'files', 'val', 'valUnit', timeUnit', 'calendarStr',
#'      'lat', 'lon', and 'time'.
#' @export
#' @examples
#' makeAnnualMean(loadModel('nbp','HadGEM2-ES','rcp85',verbose=TRUE,demo=TRUE))
makeAnnualMean <- function(x, yearRange=c(1, Inf), verbose=TRUE, parallel=FALSE, FUN=mean) {
    
    # Sanity checks
    stopifnot(length(x)==8 & is.list(x))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(yearRange)==2 & is.numeric(yearRange))
    stopifnot(all(yearRange > 0))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(dim(x$val)==c(length(x$lon),length(x$lat),length(x$time)))
    
    # TODO: is calendarStr guaranteed to have # days in positions 1-3? 
    # Would it better to split the string based on underscore?
    numDays <- as.numeric(substr(x$calendarStr, 1, 3))
    stopifnot(is.numeric(numDays) & numDays>0)
    
    # timeUnit is a string like "days since 1859-12-01". Extract startDate from this
    startYrArr <- as.numeric(unlist(strsplit(
        regmatches(x$timeUnit, regexpr('\\d+.\\d+.\\d+', x$timeUnit)), '-')))
    startYr <- startYrArr[1] + (startYrArr[2]-1)/12 + (startYrArr[3]-1)/numDays
    
    # More sanity checks
    stopifnot(startYrArr[2] %in% 1:12)
    stopifnot(startYrArr[3] %in% 1:31)
    stopifnot(startYr >= 1850 & startYr < 2300)
    
    yearIndex <- x$time/numDays + startYr
    uniqueYears <- unique(floor(yearIndex))
    uniqueYears <- uniqueYears[uniqueYears >= min(yearRange) & uniqueYears <= max(yearRange)]
    ans <- array(NA_real_, dim=c(dim(x$val)[c(1,2)], length(uniqueYears)))
    
    if(parallel) parallel <- require(foreach) & require(doParallel) & require(abind)
    timer <- system.time( # time the main computation, below; 4-5s/yr on my laptop
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            ans <- foreach(i=1:length(uniqueYears), .combine = function(...) abind(..., along=3), .packages='plyr') %dopar% {
                aaply(x$val[,,uniqueYears[i] == floor(yearIndex)], c(1,2), FUN)
            }
        } else {
            if(verbose) cat("Running in serial\n")
            for(i in 1:length(uniqueYears)) {
                if(verbose & floor(i/1)==i/1) cat(i, " ")
                ans[,,i] <- aaply(x$val[,,uniqueYears[i] == floor(yearIndex)], c(1,2), FUN)
            }
            if(verbose) cat("\n")
        }   
    ) # system.time
    
    if(verbose) cat('Took',timer[3], 's\n')
    
    invisible(list(val=unname(ans), files=x$files, year=uniqueYears,
                   numMonths=table(floor(yearIndex)),
                   lat=x$lat, lon=x$lon, valUnit=x$valUnit))
} # makeAnnualMean
