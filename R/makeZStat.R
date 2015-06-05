#' Compute Z-dimension statistic of a variable
#'
#' Some CMIP5 data are four-dimensional: in addition to longitude, latitude,
#' and time, they include a Z dimension (typically encoded in the NetCDF file as
#' 'depth' or 'lev'). This function computes a summary statistic for all Z values.
#' The default statistic is \link{mean}, but any summary
#' function that returns a numeric result (including weighted.mean, if you
#' want to apply weights) can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param sortData logical. Sort \code{x} and \code{area} before computing?
#' @param FUN function. Function to apply across Zs
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the mean of the
#' variable across Zs A \code{numZs} field is also added
#' recording the number of Z values averaged for each year, and x's original
#' Z field is removed.
#' @note If \code{x} is not in a needed order (for example, \code{FUN} uses
#' weights in a different order), be sure to specify \code{sortData=TRUE}.
#' @seealso \code{\link{makeAnnualStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @examples
#' d <- cmip5data(1970:1975, Z=TRUE)   # sample data
#' makeZStat(d)
#' summary(makeZStat(d, FUN=sd))
#' @export
makeZStat <- function(x, verbose=FALSE, sortData=FALSE, FUN=mean, ...) {
    
    # Sanity checks
    assert_that(class(x)=="cmip5data")
    assert_that(is.flag(verbose))
    assert_that(is.flag(sortData))
    assert_that(is.function(FUN))
    
    # Main computation code
    timer <- system.time({  # time the main computation, below
        if(is.array(x$val)){
            newDim <- dim(x$val)
            newDim[3] <- 1
            x$val <- apply(x$val, c(1,2,4), FUN, ...)
            dim(x$val) <- newDim
        }else{
            # Suppress stupid NOTEs from R CMD CHECK
            lon <- lat <- Z <- time <- value <- `.` <- NULL
            
            if(sortData) {
                if(verbose) cat("Sorting data...\n")
                x$val <- group_by(x$val, lon, lat, time, Z) %>%
                    arrange()
            }
            
            # Instead of "summarise(value=FUN(value, ...))", we use the do()
            # call below, because the former doesn't work (as of dplyr 0.3.0.9000):
            # the ellipses cause big problems. This solution thanks to Dennis
            # Murphy on the manipulatr listesrv.
            x$val <- group_by(x$val, lon, lat, time) %>%
                do(data.frame(value = FUN(.$value, ...))) %>%
                ungroup()
            x$val$Z <- NA
        }
        
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    # We now have new computed data. Overwrite original data and update provenance
    
    x$numZs <- length(x$Z)
    x$Z <- NULL
    addProvenance(x, paste("Computed", 
                           paste(deparse(substitute(FUN)), collapse="; "),
                           "for Z"))
} # makeZStat
