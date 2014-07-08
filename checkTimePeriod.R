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
    
    #fileInfo_df <- fileInfo     #Debugging
    
    # Check that we only have monthly and fixed variables
    # TODO: code for annual as well; or simply ignore non-monthly?
    valid <- (grepl('mon', fileInfo_df$domain) | grepl('fx', fileInfo_df$domain))
    if(!all(valid)){
        stop('Time checks for non-monthly variables not currently coded. Can not handle:',
             unique(fileInfo_df$domain[!valid]) )
    }
    
    # Use ddply to break up data frame, process and check time field, and return result
    ddply(fileInfo_df, ddplyFields, function(x) {
        curCombo <- as.character(x$time) # has form 'YYYYMM-YYYYMM'
        
        # find the starting and ending decimal year
        endMonth <- as.numeric(substr(curCombo, 12, 13))
        endYear <- as.numeric(substr(curCombo, 8, 11)) + (endMonth-1)/12
        startMonth <- as.numeric(substr(curCombo, 5, 6))
        startYear <- as.numeric(substr(curCombo, 1, 4)) + (startMonth-1)/12
        nextYear <- endYear + 1/12  # calculate year that next file should start with
        
        startIndex <- 1
        endIndex <- 1
        allHere <- TRUE
        if(length(startYear) > 1) {   # If multiple files specified, shift indexes to compare the right start/stop
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
                   endDate=max(endYear))        
    })
}
