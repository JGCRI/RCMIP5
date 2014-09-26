#' Check for continuous time periods in CMIP5 files
#'
#' Check that all time periods match for multi-file ensembles. Before starting to
#' process what may be hundreds or thousands of CMIP5 files, it's a good idea to verify
#' that your file set is complete and not missing any years.
#' Unfortunately it's impossible to automatically check the time signature for
#' sub-monthly frequencies quickly without opening the netcdf file. These time
#' signatures will be concatonated and the 'allHere' flag will be returned as
#' NA for these runs. Note that fixed variables are removed.
#'
#' @param fileInfo_df data.frame from getFileInfo
#' @return A data frame showing which ensembles are continuous, and which are not.
#' In addition to standard identifying fields in the data frame (domain, model,
#' experiment, variable, and ensemble),
#' the yrStr field will concatonate the time strings for all ensembles,
#' allHere is a quick check for yr and mon frequency, start and end dates are
#' the decimal earliest and latest dates for the ensemble, and files indicates
#' the number of files in the run.
#' @note this only works for files that are in domains 'fx', 'mon', or 'yr'.
#' @details Decimal time is (year + (month-1)/12).
#' @examples
#' checkdf <- checkTimePeriod(getFileInfo())
#' @seealso \code{\link{getFileInfo}}
#' @export
checkTimePeriod <- function(fileInfo_df) {

    # Sanity checks
    stopifnot(is.data.frame(fileInfo_df))
    ddplyFields <- c("domain", "experiment","model","variable","ensemble")
    stopifnot(all(ddplyFields %in% colnames(fileInfo_df)))
    stopifnot("time" %in% colnames(fileInfo_df))

    # Use ddply to break up data frame, process and check time field, and return result
    ddply(fileInfo_df, ddplyFields, function(x) {

        #spilt the time signiture
        curCombo <- matrix(unlist(strsplit(as.character(x$time), '-')),
                           ncol=2, byrow=TRUE)

        #Find the starting and ending decimal year
        startYear <- as.numeric(substr(curCombo[,1], 1, 4))
        endYear <- as.numeric(substr(curCombo[,2], 1, 4))

        # pull the time step from the domain name
        if(all(x$domain %in% 'fx')) { # fixed
            #don't even process fixed files
            return(NULL)
        } else if(all(grepl('mon$', x$domain))) { # monthly
            timeStep <- 1/12
            #convert to decimal years
            startYear <- startYear+(as.numeric(substr(curCombo[,1], 5, 6))-1)/12
            endYear <- endYear + (as.numeric(substr(curCombo[,2], 5, 6))-1)/12
        } else if(all(grepl('yr$', x$domain))) { # annual
            timeStep <- 1
        } else {
            # we can not process sub-monthly time scales because of not standard
            #... year lengths. The user must check the strings by hand.
            timeStep <- NA
        }

        if(is.na(timeStep)){
            allHere <- NA
        }else{
            #Figure out the target date for the start of the next file
            nextYear <- endYear + timeStep
        }

        # One file is always complete
        if(length(startYear) == 1){
            allHere <- TRUE
        # If multiple files, shift indexes to compare the start/stop values
        }else if( !is.na(timeStep) & length(startYear) > 1) {
            startIndex <- c(2:length(startYear))
            endIndex <- c((2:length(startYear))-1)
            allHere <- all(abs(nextYear[endIndex]-startYear[startIndex]) < 1e-6)
        }

        # return answering data frame which contains
        #   yrStr - All orginal year strings for reference (useful if something is wrong).
        #   allHere - boolean saying if the strings match up
        #   startDate - earliest time stamp
        #   endDate - latest time stamp
        data.frame(yrStr=paste(x$time, collapse=';'),
                   allHere=allHere,
                   startDate=min(startYear),
                   endDate=max(endYear),
                   files=length(startYear))
    }) # ddply
} # checkTimePeriod
