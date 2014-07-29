# TODO: add capability to filter levels

library(plyr)
library(abind)

#' Compute annual mean of a variable
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param yearRange numeric vector. Limit computation to this year range (e.g. for testing)
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @return A \code{\link{cmip5data}} object.
#' @export
#' @examples
#' makeAnnualMean(loadModel('nbp','HadGEM2-ES','rcp85',verbose=TRUE,demo=TRUE))
#' @seealso \code{\link{makeMonthlyMean}}
makeAnnualMean <- function(x, yearRange=c(1, Inf), verbose=TRUE, parallel=FALSE, FUN=mean) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numMonths))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(yearRange)==2 & is.numeric(yearRange))
    stopifnot(all(yearRange > 0))
    stopifnot(length(FUN)==1 & is.function(FUN))
    stopifnot(length(dim(x$val)) %in% c(3, 4)) # that's all we know
    
    timeIndex <- length(dim(x$val))  # time is always the last index
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    stopifnot(dim(x$val)[c(1,2,timeIndex)]==c(length(x$lon),length(x$lat),length(x$time)))
    
    yearIndex <- compute_yearIndex(x)
    uniqueYears <- unique(floor(yearIndex))
    uniqueYears <- uniqueYears[uniqueYears >= min(yearRange) & uniqueYears <= max(yearRange)]
    
    if(parallel) parallel <- require(foreach) & require(doParallel) & require(abind)
    timer <- system.time( # time the main computation, below; 4-5s/yr on my laptop
        
        if(parallel) {  # go parallel, woo hoo!
            registerDoParallel()
            if(verbose) cat("Running in parallel [", getDoParWorkers(), "cores ]\n")
            ans <- foreach(i=1:length(uniqueYears), .combine = function(...) abind(..., along=timeIndex), .packages='plyr') %dopar% {
                aaply(asub(x$val, idx=uniqueYears[i] == floor(yearIndex), dims=timeIndex), c(1:(timeIndex-1)), FUN)
            }           
        } else {
            if(verbose) cat("Running in serial\n")
            ans <- list()
            for(i in 1:length(uniqueYears)) {
                if(verbose & floor(i/1)==i/1) cat(i, " ")
                ans[[i]] <- aaply(
                    asub(x$val, idx=uniqueYears[i] == floor(yearIndex), dims=timeIndex), c(1:(timeIndex-1)), FUN)
            }
            ans <- abind(ans, along=timeIndex)
        }   
    ) # system.time
    
    if(verbose) cat('\nTook',timer[3], 's\n')
    
    x$val <- unname(ans)
    x$year <- uniqueYears
    x$numMonths <- table(floor(yearIndex))
    return(x)
} # makeAnnualMean

#' Compute year index from parsed cmip5data information
#' 
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @return yearIndex A numeric vector of years
#' @details This function uses information in a \code{\link{cmip5data}} object 
#' to compute the yearIndex, i.e. the (perhaps fractional) years associated with
#' each timepoint. It does this by parsing \code{calendarStr} to determine the
#' number of days per year; parsing \code{timeUnit} to get the starting date;
#' and then using the values in the \code{time} vector.
#' @note This is an internal RCMIP5 function and not exported.
compute_yearIndex <- function(x) {
    stopifnot(class(x)=="cmip5data")
    
    # TODO: is calendarStr guaranteed to have # days in positions 1-3? [NO]
    # Would it better to split the string based on underscore?
    # TODO: this code bombs with e.g. "proleptic gregorian"
    numDays <- as.numeric(substr(x$calendarStr, 1, 3))
    stopifnot(is.numeric(numDays) & numDays>0)                
    
    # timeUnit is a string like "days since 1859-12-01". Extract startDate from this
    # TODO: this will not (?) handle annual data correctly
    startYrArr <- as.numeric(unlist(strsplit(
        regmatches(x$timeUnit, regexpr('\\d+.\\d+.\\d+', x$timeUnit)), '-')))
    startYr <- startYrArr[1] + (startYrArr[2]-1)/12 + (startYrArr[3]-1)/numDays
    
    # Sanity checks
    stopifnot(startYrArr[2] %in% 1:12)
    stopifnot(startYrArr[3] %in% 1:31)
    stopifnot(startYr >= 1850 & startYr < 2300)
    
    return(x$time/numDays + startYr)
} # compute_yearIndex
