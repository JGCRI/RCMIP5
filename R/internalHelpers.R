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
    MSG_PREFIX <- "-"
    stopifnot(class(prov) %in% c("character", "NULL"))
    stopifnot(class(msg) %in% c("character", "NULL"))
    
    # Get calling function's call (its name and parameters)
    parentcall <- "<parent unavailable>"
    try({
        parentcall <- match.call(def=sys.function(-1), call=sys.call(-1))
        parentcall <- gsub(" ", "", paste(capture.output(parentcall), collapse=""))
        parentcall <- gsub("\\\"", "'", parentcall)
    }, silent=TRUE)
    
    # TODO: would be nice to do addProvenance(x,"msg") and fake call-by-reference!
    
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
    if(is.null(prov) | parentcall != lastparentcall){
        prov <- c(prov, parentcall)
    }
    if(!is.null(msg)){
        prov <- c(prov, paste(MSG_PREFIX, msg, sep=''))
    }
    return(prov)
} # addProvenance

#' Generate dummy data for testing
#'
#' @param years years to generate data
#' @param monthly monthly or annual data?
#' @param depth add depth dimension?
#' @param lev add lev dimension?
#' @return dummy cmip5data structure
#' @note This is an internal RCMIP5 function and not exported.
dummydata <- function(years, monthly=TRUE, depth=FALSE, lev=FALSE) {
    ppy <- ifelse(monthly, 12, 1)  # periods per year
    
    valdims <- c(10, 10, ppy*length(years))
    depthdim <- NULL
    if(depth) {
        valdims <- c(valdims[1:(length(valdims)-1)], 5, valdims[length(valdims)])
        depthdim <- c(0:4)
    }
    levdim <- NULL
    if(lev) {
        valdims <- c(valdims[1:(length(valdims)-1)], 5, valdims[length(valdims)])
        levdim <- c(0:4)
}
#    print(valdims)
    
    cmip5data(list(files="dummy file", 
                   variable="dummyvar",
                   val=array(1:3, dim=valdims),
                   valUnit="dummy unit",
                   timeUnit=paste0("days since ",years[1],"-01-01"),
                   calendarStr="360_day",
                   timeFreqStr=ifelse(monthly, "mon", "yr"),
                   lat=c(0:9),
                   lon=c(0:9),
                   depth=depthdim,
                   lev=levdim,
                   time=(360/ppy*c(0:(length(years)*ppy-1) )+15)/360+min(years),
                   provenance=addProvenance(NULL, "Dummy data")
    ))
} # dummydata
