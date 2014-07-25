library(plyr)
require(abind)

#' Compute annual mean of a variable
#'
#' @param temp.ls a list structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @return #' @return list with elements 'files', 'val', 'valUnit', timeUnit', 'calendarStr',
#'      'lat', 'lon', and 'time'.
#' @export
#' @examples
#' makeAnnualMean(loadModel('nbp','HadGEM2-ES','rcp85',verbose=T))
makeAnnualMean <- function(temp.ls, verbose=TRUE, parallel=FALSE) {
    
    # Sanity checks
    stopifnot(length(temp.ls)==8 & is.list(temp.ls))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    
    # TODO: is calendarStr guaranteed to have # days in positions 1-3? 
    # Would it better to split the string based on underscore?
    numDays <- as.numeric(substr(temp.ls$calendarStr, 1, 3))
    stopifnot(numDays>0)
    
    # timeUnit is a string like "days since 1859-12-01". Extract startDate from this
    startYr <- as.numeric(unlist(strsplit(
        regmatches(temp.ls$timeUnit, regexpr('\\d+.\\d+.\\d+', temp.ls$timeUnit)), '-')))
    startYr <- startYr[1]+(startYr[2]-1)/12+(startYr[3]-1)/numDays
    
    # More sanity checks
    stopifnot(startYr[2] %in% 1:12)
    stopifnot(startYr[3] %in% 1:31)
    stopifnot(startYr >= 1850 & startYr < 2300)
    
    yrIndex <- temp.ls$time/numDays + startYr
    uniqueYears <- unique(floor(yrIndex))
    ans <- array(NA, dim=c(dim(temp.ls$val)[c(1,2)], length(uniqueYears)))
    
    timer <- system.time( # time the main computation, below; 4-5s/yr on my laptop
        
        if(parallel & require(foreach) & require(doParallel)) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "]\n")
            ans <- foreach(i=1:length(uniqueYears), .combine = function(...) abind(..., along=3), .packages='plyr') %dopar% {
                aaply(temp.ls$val[,,uniqueYears[i] == floor(yrIndex)], c(1,2), mean)
            }
        } else {
            if(verbose) cat("Running in serial\n")
            for(i in 1:length(uniqueYears)) {
                if(verbose & floor(i/1)==i/1) cat(i, " ")
                ans[,,i] <- aaply(temp.ls$val[,,uniqueYears[i] == floor(yrIndex)], c(1,2), mean)
            }
        }   
    ) # system.time
    
    if(verbose) cat('Took',timer[3], 's\n')
    
    invisible(list(val=ans, files=temp.ls$files,
                   year=uniqueYears,
                   numMonths=table(floor(yrIndex)),
                   lat=temp.ls$lat, lon=temp.ls$lon, valUnit=temp.ls$valUnit))
} # makeAnnualMean
