#' Compute monthly statistic of a variable
#' 
#' We frequently want to summarize CMIP5 data by month, e.g. to understand how
#' air temperature varies over the year for a particular data range. This function 
#' does that for monthly data. The default statistic is \link{mean}, but any 
#' summary function that returns a numeric result can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the monthly
#' mean of the variable. A \code{numYears} field is also added
#' recording the number of years averaged for each month.
#' @details The stat function is calculated for all combinations of lon,
#' lat, and Z (if present).
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeZStat}} \code{\link{makeGlobalStat}}
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' makeMonthlyStat(d)
#' summary(makeMonthlyStat(d))
#' summary(makeMonthlyStat(d, FUN=sd))
#' @export
makeMonthlyStat <- function(x, verbose=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(x$numYears))
    stopifnot(x$debug$timeFreqStr=="mon")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(FUN)==1 & is.function(FUN))

    monthIndex <- floor((x$val$time %% 1) * 12 + 1)
    
    # Main computation code
    timer <- system.time({ # time the main computation, below
        x$val$time <- monthIndex  
        
        # Suppress stupid NOTEs from R CMD CHECK
        lon <- lat <- Z <- time <- value <- NULL
        
        grp <- group_by(x$val, lon, lat, Z, time)
        x$val <- summarise(grp, value=FUN(value, ...))
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # Finish up
    x$numYears <- unname(table(floor(monthIndex)))
    x$timeUnit <- "months (summarized)"
    x$time <- 1:12
    addProvenance(x, paste("Calculated", 
                           paste(deparse(substitute(FUN)), collapse="; "),
                           "for months", min(x$time), "-", max(x$time)))
} # makeMonthlyStat
