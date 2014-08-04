#' DECREPIT
#' Compute year index from parsed cmip5data information
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param verbose logical. Print info as we go?
#' @return yearIndex A numeric vector of years
#' @details This function uses information in a \code{\link{cmip5data}} object
#' to compute the yearIndex, i.e. the (perhaps fractional) years associated with
#' each timepoint. It does this by parsing \code{calendarStr} to determine the
#' number of days per year; parsing \code{timeUnit} to get the starting date;
#' and then using the values in the \code{time} vector.
#' @note This is an internal RCMIP5 function and not exported.
computeYearIndex <- function(x, verbose=FALSE) {
    stopifnot(class(x)=="cmip5data")
    
    # Pull a numeric from the calendarStr and assume it's the days
    rmString <- regmatches(x$calendarStr, regexpr('\\d+', x$calendarStr))
    if(verbose) cat("From calendarStr regmatches extracts", rmString, "\n")
    numDays <- as.numeric(rmString)
    # If nothing matches set it to a default of 365
    if(length(numDays) == 0) numDays <- 365
    stopifnot(is.numeric(numDays) & numDays>0)
    
    # timeUnit is a string like "days since 1859-12-01". Extract startDate from this
    # TODO: this will not (?) handle annual data correctly
    rmString <- unlist(strsplit(
        regmatches(x$timeUnit, regexpr('\\d+.\\d+.\\d+', x$timeUnit)), '-'))
    if(verbose) cat("From timeUnit regmatches extracts", rmString, "\n")
    startYrArr <- as.numeric(rmString)
    startYr <- startYrArr[1] + (startYrArr[2]-1)/12 + (startYrArr[3]-1)/numDays
    
    # Sanity checks
    stopifnot(startYrArr[2] %in% 1:12)
    stopifnot(startYrArr[3] %in% 1:31)
    
    #return(x$time/numDays + startYr)
    return(x$time)
} # computeYearIndex

#' Add provenance information to a cmip5data object
#'
#' @param prov An optional provenance (currently vector of strings)
#' @param msg An optional message string
#' @return An updated provenance
#' @details We want to track computational steps applied to a particular
#' \code{\link{cmip5data}} object, for reproducibility and user debugging.
#' This function logs information from the caller to a 'provenance' field.
#' @details We'd rather not pass potentially very large cmip5data structures
#' around too much, so caller has to assign updated provenance to the
#' cmip5data object. Not ideal.
#' @note This is an internal RCMIP5 function and not exported.
addProvenance <- function(prov=NULL, msg=NULL) {
    MSG_PREFIX <- "--"
    stopifnot(class(prov) %in% c("character", "NULL"))
    
    # Get calling function's call (its name and parameters)
    parentcall <- match.call(def=sys.function(-1), call=sys.call(-1))
    parentcall <- gsub(" ", "", paste(capture.output(parentcall), collapse=""))
    parentcall <- gsub("\\\"", "'", parentcall)
    
    # Look for most recent call printed in provenance
    lastparentcall <- ""
    if(length(prov)) {
        for(i in length(prov)-1:1) {
            if(substr(prov[i], 1, length(MSG_PREFIX)) != MSG_PREFIX) {
                lastparentcall <- prov[i]
                break
            }
        }  
    }
    
    # Append the caller info (except when there's not been a change) and message
    if(is.null(prov) | parentcall != lastparentcall)
        prov <- c(prov, parentcall)
    if(!is.null(msg)) prov <- c(prov, paste(MSG_PREFIX, msg))
    prov
} # addProvenance
