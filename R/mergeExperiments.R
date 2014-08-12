library(abind)

#' Merge data for two separate experiments
#'
#' @param x cmip5data
#' @param y cmip5data
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @details Merge two separate experiments. The variable, units, spatial grid,
#' depths/levels, domain, and model (TODO: ???) must all match. The timesteps must be
#' identical, and time values non-overlapping. If the time gap between the two experiments
#' is different than their internal timesteps (e.g., if two monthly data objects are
#' separated by more than a month) a warning will be printed.
#' @export
mergeExperiments <- function(x, y, verbose=TRUE) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data" & class(y)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    if(verbose) cat("Checking that ancillary data are identical\n")
    stopifnot(x$domain == y$domain)
    stopifnot(x$variable == y$variable)
    stopifnot(x$model == y$model)
    stopifnot(x$valUnit == y$valUnit)
    stopifnot(x$lon == y$lon & x$lat == y$lat)
    stopifnot(x$depth == y$depth & x$lev == y$lev)
    
    # Ensemble check
    if(all(x$ensembles == y$ensembles)) {
        if(verbose) cat("OK: ensembles match\n")
    } else {
        warning("Ensembles differ between these objects.",
                "\nMerge proceeding but check carefully this is what you want!")   
    }
    
    # Time checks. This is important, and we try to identify obvious problems
    if(verbose) cat("Checking that time data match up\n")
    stopifnot(x$timeFreqStr == y$timeFreqStr)
    
    if(mean(x$time) > mean(y$time)) { # switch them
        temp <- x
        x <- y
        y <- temp
    }
    if(length(intersect(x$time, y$time)) > 0 | max(x$time) > min(y$time)) {
        warning("Overlap between times; can't merge")
        return(NULL)
    }
    timegap <- min(y$time) - max(x$time)
    tsx <- x$time[2] - x$time[1]
    tsy <- y$time[2] - y$time[1]
    if(isTRUE(all.equal(timegap, tsx)) & isTRUE(all.equal(timegap, tsy))) {
        if(verbose) cat("OK: time gap matches both timesteps\n")
    } else {
        warning("The time gap between objects is ", round(timegap, 3),
                " but their timesteps are ", round(tsx, 3), " and ", round(tsy, 3), 
                "\nMerge proceeding but check carefully this is what you want!")
    }
    
    # Go ahead and merge
    if(verbose) cat("Merging\n")
    x$time <- c(x$time, y$time)
    x$val <- unname(abind(x$val, y$val))
    x$files <- c(x$files, y$files)
    x$experiment <- paste(x$experiment, y$experiment, sep=".")
    x$provenance <- addProvenance(x$provenance, "Merged with another experiment:")
    x$provenance <- addProvenance(x$provenance, y$provenance)
    x
} # mergeExperiments
