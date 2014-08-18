#' Add provenance information to a cmip5data object
#' 
#' It's important to track data provenance, the steps taken to produce a particular
#' dataset. Each operation in the RCMIP5 package adds provenance information via this function.
#'
#' @param prov An optional provenance (currently a vector of strings)
#' @param msg An optional message string
#' @return An updated provenance string
#' @details We want to track computational steps applied to a particular
#' \code{\link{cmip5data}} object, for reproducibility and user debugging.
#' This function logs information from the caller to a 'provenance' field.
#' @details We'd rather not pass potentially very large cmip5data structures
#' around too much, so caller has to assign updated provenance to the
#' cmip5data object. Not ideal; TODO.
#' @note This is an internal RCMIP5 function and not exported.
addProvenance <- function(prov=NULL, msg=NULL) {
    MSG_PREFIX <- "--"
    stopifnot(class(prov) %in% c("character", "NULL"))
    stopifnot(class(msg) %in% c("character", "NULL"))
    
    # Get calling function's call (its name and parameters)
    parentcall <- "<parent unavailable>"
    try({
        parentcall <- match.call(definition=sys.function(-1), call=sys.call(-1))
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
