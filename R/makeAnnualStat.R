#' Compute annual statistic of a variable.
#'
#' Most CMIP5 data are monthly, and we frequently want to summarize these to annual
#' numbers. This function does that (although annual files also occur, and will be
#' handled as well). The default statistic is \link{mean}, but any summary
#' function that returns a numeric result can be used.
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the annual
#' mean of the variable. A \code{numMonths} field is also added
#' recording the number of months averaged for each year.
#' @export
#' @seealso \code{\link{makeMonthlyStat}}
makeAnnualStat <- function(x, verbose=TRUE, parallel=FALSE, FUN=mean, ...) {

    if(verbose) cat('starting makeAnnualStat\n')
    # Sanity checks ###################

    # Make sure we are dealing with the expected class
    stopifnot(class(x)=="cmip5data")
    # Is the val array in months?
    #stopifnot(x$timeFreqStr %in% 'mon')

    # Check the boolean flags
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))

    # Check the annual statistic function
    stopifnot(length(FUN)==1 & is.function(FUN))

    # Check that the dimentions of the value is either lon-lat-time
    # ...or lon-lat-depth-time
    stopifnot(length(dim(x$val)) %in% c(3, 4, 5)) # KTB I don't think we need to handle level and depth in the same val

    # pull the time index which is always the last dimension
    timeIndex <- length(dim(x$val))
    if(verbose) cat("Time index =", timeIndex, "\n")

    # check that the dimensions match up
    stopifnot(identical(dim(x$val)[c(1,2,timeIndex)],
                        c(length(x$lon),length(x$lat),length(x$time))))

    # get the years that are covered
    uniqueYears <- unique(floor(x$time))

    # set this up to parallelize nicely
    if(parallel) parallel <- require(foreach) & require(doParallel)

    # time the main computation, below
    timer <- system.time(

            if(parallel) {  # go parallel, woo hoo!
                registerDoParallel()
                if(verbose) cat("Running in parallel [", getDoParWorkers(),
                                "cores ]\n")
                #KTB Ben can you break this down and comment
                ans <- foreach(i=1:length(uniqueYears), .combine = function(...)  abind(..., along=timeIndex), .packages='plyr')  %dopar% { aaply(asub(x$val, idx=uniqueYears[i] == floor(x$time), dims=timeIndex), c(1:(timeIndex-1)), FUN, ...)

                             }
            } else { #not parallel
                if(verbose) cat("Running in serial\n")
                ans <- list() #initalize the list to dump the annual stats into
                # go through each year
                # KTB should this be wrapped in an apply or similar?
                for(i in 1:length(uniqueYears)) {
                    # this is slow so let the user know where you are
                    if(verbose) cat(i, " ")
                    # apply the annual stat function to the subset where
                    # ...the years match the current year
                    ans[[i]] <- aaply(asub(x$val,
                                           idx=uniqueYears[i] == floor(x$time),
                                           dims=timeIndex),
                                      c(1:(timeIndex-1)), FUN, ...)
                } #end for-loop
                # Put everything back together into an array
                ans <- abind(ans, along=timeIndex)
            }# end parallel if-else

                         ) # system.time

    if(verbose) cat('\nTook', timer[3], 's\n')

    # reassign the value to the annual stat
    x$val <- unname(ans)

    # record the number of months that were averaged for each year
    ##KTB numMonths should be changed to numPerYr
    x$numMonths <- table(floor(x$time))

    # reset the time
    x$time <- uniqueYears

    # reset the frequency string for the time
    x$timeFreqStr <- "years (summarized)"

    # note the provenance
    x$provenance <- addProvenance(x$provenance,
                                  paste("Calculated",
                                        as.character(substitute(FUN)),
                                        "for years",
                                        min(x$time), "-", max(x$time)))

    if(verbose) cat('done with makeAnnualStat\n')
    return(x)
} # makeAnnualStat
