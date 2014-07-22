library(plyr) # for BIG data.frames we want to do this fast

#' Check that all time periods match for multi-file ensembles
#'
#' @param fileInfo_df data.frame from getFileInfo
#' @return data.frame from fileInfo_df: domain, experiment, model, variable, ensemble, and
#'          yrStr (string - all year boundries in relevent files)
#'          allHere (boolean - do the year boundries match up?)
#'          startYr (numeric - decimal time of minimum year
#'          endYr (numeric - decimal time of maximum year)
#' @details Decimal time is (year + (month-1)/12).
#'          Non-monthly time intervals and temporally fixed variables are the
#'          only ones dealt with; anything else will throw an error
#' @examples
#' checkTimePeriod(getFileInfo())
#' @seealso getFileInfo
checkTimePeriod <- function(fileInfo_df) {
    
    # Sanity checks
    stopifnot(is.data.frame(fileInfo_df))
    ddplyFields <- c("domain","experiment","model","variable","ensemble")
    stopifnot(all(ddplyFields %in% colnames(fileInfo_df)))
    stopifnot("time" %in% colnames(fileInfo_df))
    
    # Use ddply to break up data frame, process and check time field, and return result
    ddply(fileInfo_df, ddplyFields, function(x) {
        curCombo <- as.character(x$time)
        
        # find the starting and ending decimal year, and year next file should start with
        if(nchar(curCombo[1])==9) { # annual data ('YYYY-YYYY')
            startYear <- as.numeric(substr(curCombo, 1, 4))
            endYear <- as.numeric(substr(curCombo, 6, 9))
            nextYear <- endYear + 1
        } else if(nchar(curCombo[1])==13) { # monthly data ('YYYYMM-YYYYMM')
            startMonth <- as.numeric(substr(curCombo, 5, 6))
            startYear <- as.numeric(substr(curCombo, 1, 4)) + (startMonth-1)/12
            endMonth <- as.numeric(substr(curCombo, 12, 13))
            endYear <- as.numeric(substr(curCombo, 8, 11)) + (endMonth-1)/12
            nextYear <- endYear + 1/12          
        } else if(nchar(curCombo[1]==0)) { # probably an area file; ignore and bail
            return(NULL)
        } else stop("Incorrectly formatted time field: ",curCombo[1])
        
        startIndex <- 1
        endIndex <- 1
        allHere <- TRUE
        if(length(startYear) > 1) {   # If multiple files, shift indexes to compare the start/stop values
            startIndex <- c(2:length(startYear))
            endIndex <- c((2:length(startYear))-1)
            allHere <- all((nextYear[endIndex] - startYear[startIndex]) < 1e-6)
        }
        # return answering data frame which contains
        #   yrStr - All orginal year strings for reference (useful if something is wrong).
        #   allHere - boolean saying if the strings match up
        #   startDate - earliest time stamp
        #   endDate - latest time stamp
        data.frame(yrStr=paste(curCombo, collapse='_'),
                   allHere=allHere,
                   startDate=min(startYear),
                   endDate=max(endYear),
                   files=length(startYear))        
    })
}
