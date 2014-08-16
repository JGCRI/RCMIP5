#' Check for continuous time periods in CMIP5 files.
#' 
#' Check that all time periods match for multi-file ensembles. Before starting to 
#' process what may be hundreds or thousands of CMIP5 files, it's a good idea to verify
#' that your file set is complete and not missing any years.
#' 
#' @param fileInfo_df data.frame from getFileInfo
#' @return A \code{\link{cmip5data}} object.
#' @details Decimal time is (year + (month-1)/12).
#' Note that this only works for files that are in domains 'fx', '*mon', or '*yr'.
#' @export
#' @examples
#' checkTimePeriod(getFileInfo())
#' @seealso \link{getFileInfo}
checkTimePeriod <- function(fileInfo_df) {

    # Sanity checks
    stopifnot(is.data.frame(fileInfo_df))
    ddplyFields <- c("domain", "experiment","model","variable","ensemble")
    stopifnot(all(ddplyFields %in% colnames(fileInfo_df)))
    stopifnot("time" %in% colnames(fileInfo_df))

    # Use ddply to break up data frame, process and check time field, and return result
    invisible(ddply(fileInfo_df, ddplyFields, function(x) {
        # pull the time step from the domain name
        if(all(x$domain %in% 'fx')) { # fixed
            return(NULL)
        } else if(all(grepl('mon$', x$domain))) { # monthly
            timeStep <- 1/12
        } else if(all(grepl('yr$', x$domain))) { # annual
            timeStep <- 1
        } else stop("unknown or mixed time step(s): [", unique(x$domain), ']')

        curCombo <- matrix(unlist(strsplit(as.character(x$time), '-')),
                           ncol=2, byrow=TRUE)

        #Find the starting and ending decimal year
        startYear <- as.numeric(substr(curCombo[,1], 1, 4))
        endYear <- as.numeric(substr(curCombo[,2], 1, 4))

        if(timeStep == 1) { # annual data ('YYYY-YYYY')
            #do nothing
        } else if(timeStep == 1/12) { # monthly data ('YYYYMM-YYYYMM')
            startYear <- startYear + (as.numeric(substr(curCombo[,1], 5, 6))-1)/12
            endYear <- endYear +  (as.numeric(substr(curCombo[,2], 5, 6))-1)/12
        } else stop("Bad time step set, this shouldn't happen")

        #Figure out the target date for the start of the next file
        nextYear <- endYear + timeStep

        startIndex <- 1
        endIndex <- 1
        allHere <- TRUE

        # If multiple files, shift indexes to compare the start/stop values
        if(length(startYear) > 1) {
            startIndex <- c(2:length(startYear))
            endIndex <- c((2:length(startYear))-1)
            allHere <- all(abs(nextYear[endIndex] - startYear[startIndex]) < 1e-6)
        }
        # return answering data frame which contains
        #   yrStr - All orginal year strings for reference (useful if something is wrong).
        #   allHere - boolean saying if the strings match up
        #   startDate - earliest time stamp
        #   endDate - latest time stamp
        data.frame(yrStr=paste(x$time, collapse='_'),
                   allHere=allHere,
                   startDate=min(startYear),
                   endDate=max(endYear),
                   files=length(startYear))
    })) # ddply
} # checkTimePeriod
