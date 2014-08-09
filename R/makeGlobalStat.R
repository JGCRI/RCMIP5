library(plyr)
library(abind)

#' Compute global mean (or other function) of a variable
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param area cmip5data An area cmip5data data structure
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across grid
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object.
#' @note We expect that weighted.mean and a weighted sum will be the most frequent
#' calculations needed. The former is built into R, and the latter can generally
#' be calculated as weighted.mean * sum(area).
#' A user-supplied function must follow the weighted.mean syntax, in particular 
#' accepting parameters 'x' (data) and 'w' (weights) of equal size.
#' @export
makeGlobalStat <- function(x, area=NULL, verbose=TRUE, parallel=FALSE, FUN=weighted.mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(area) | class(area)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    timeIndex <- length(dim(x$val))  # time is always the last index
    if(verbose) cat("Time index =", timeIndex, "\n")
    
    # Get and check area data, using 1's if nothing supplied
    areavals <- array(1, dim=dim(x$val)[1:2])
    if(is.null(area)) {
        if(verbose) cat("Using unweighted areas\n")
    } else {
        stopifnot(identical(x$lat, area$lat) & identical(x$lon, area$lon))  # must match
        areavals <- area$val
        dav <- dim(areavals)
        if(length(dav) > 2) {
            warning("Ignoring extra area dimensions")
            areavals <- asub(areavals, as.list(rep(1, length(dav)-2)), dims=3:length(dav))       
        }
    }
    if(verbose) cat("Area grid dimensions", dim(areavals), "\n")
    
    # Main computation code
    if(parallel) parallel <- require(foreach) & require(doParallel) & require(abind)
    if(verbose) cat("Running in", ifelse(parallel, "parallel", "serial"), "\n")
    timer <- system.time( # time the main computation
        x$val <- unname(aaply(x$val, c(3:timeIndex), .fun=FUN, w=areavals, .parallel=parallel, ...))
    ) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # Finish up
    x[c('lat', 'lon')] <- NULL
    x$variable <- paste(as.character(substitute(FUN)), "of", x$variable)
    x$numCells <- length(areavals)
    x$provenance <-  addProvenance(x$provenance, 
                                   paste("Computed global", as.character(substitute(FUN))))
    if(!is.null(area)) {
        x$provenance <-  addProvenance(x$provenance, "Used area data:")
        x$provenance <- addProvenance(x$provenance, area$provenance)        
    }
    return(x)
} # makeGlobalStat

#' Weighted sum--i.e., sum of weighted means. Convenience function
#'
#' @param x vector of data
#' @param w vector of weights
#' @param ... passed on to weighted.mean
#' @return weighted mean multipled by sum of weights
#' @export
#' @seealso \code{\link{weighted.mean}}
weighted.sum <- function(x, w, ...) { weighted.mean(x, w, ...) * sum(w) }
