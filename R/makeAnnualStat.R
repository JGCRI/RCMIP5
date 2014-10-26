#' Compute annual statistic of a variable
#'
#' Most CMIP5 data are monthly, and we frequently want to summarize these to annual
#' numbers. This function does that (although annual files also occur, and will be
#' handled as well). The default statistic is \link{mean}, but any summary
#' function that returns a numeric result can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the annual
#' mean of the variable. A \code{numMonths} field is also added
#' recording the number of months averaged for each year.
#' @details The stat function is calculated for all combinations of lon,
#' lat, and Z (if present).
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' makeAnnualStat(d)
#' summary(makeAnnualStat(d))
#' summary(makeAnnualStat(d, FUN=sd))
#' @seealso \code{\link{makeZStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @export
makeAnnualStat <- function(x, verbose=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(FUN)==1 & is.function(FUN))
    
    # Main computation code
    timer <- system.time({  # time the main computation, below
        x$val$time <- floor(x$val$time)        
        grp <- group_by(x$val, lon, lat, Z, time)
        x$val <- summarise(grp, value=FUN(value, ...))
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    x$time <- unique(floor(x$time))
    x$numPerYear <- table(floor(x$time))
    x$debug$timeFreqStr <- "years (summarized)"
    addProvenance(x, paste("Calculated", 
                           paste(deparse(substitute(FUN)), collapse="; "),
                           "for years", min(x$time), "-", max(x$time)))
} # makeAnnualStat
