#' Add provenance information to a cmip5data object.
#' 
#' It's important to track data provenance, the steps taken to produce a particular
#' dataset. Each operation in the RCMIP5 package adds provenance information via this function.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param msg Either a string (message to be added to the provenance) or
#' a cmip5data object, in which case the provenance of this latter object is appended
#' to that of 'x' (i.e., their histories are merged).
#' @param verbose logical. Print info as we go?
#' @return The original object, with an updated provenance.
#' @details We want to track computational steps applied to a particular
#' \code{\link{cmip5data}} object, for reproducibility and user debugging.
#' This function logs information from the caller to a 'provenance' data structure.
#' @note This is an internal RCMIP5 function and not exported.
addProvenance <- function(x, msg, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(class(msg) %in% c("character", "NULL", "cmip5data"))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    # Get package version number, allowing that there might not be one
    pkgv <- "???"
    try({
       pkgv <- packageVersion("RCMIP5") 
    }, silent=T)
    
    if(is.null(x$provenance)) { # create a new provenance
        if(verbose) cat("Creating new provenance\n")
        x$provenance <- data.frame(
            timestamp=Sys.time(),
            caller="addProvenance",
            message=paste("RCMIP5", pkgv, "under", R.version.string),
            dim="",
            digest="",
            stringsAsFactors=F
            )
    }
    
    # Get calling function's call (its name and parameters)
    parentcall <- "<parent call unavailable>"
    try({
        parentcall <- match.call(definition=sys.function(-1), call=sys.call(-1))
        parentcall <- gsub(" ", "", paste(capture.output(parentcall), collapse=""))
        parentcall <- gsub("\\\"", "'", parentcall)
    }, silent=TRUE)
 
    # Add to the provenance data structure. Two cases: msg is a string containing
    # actual message; or it's another cmip5 data object, in which case we want to 
    # append its provenance to that of x.
    nr <- nrow(x$provenance) + 1
    x$provenance[nr, "timestamp"] <- Sys.time()
    if(class(msg) == "character") {
        if(verbose) cat("Adding message to provenance")
        x$provenance[nr, "caller"] <- parentcall
        x$provenance[nr, "message"] <- msg
        x$provenance[nr, "dim"] <- paste(dim(x$val), collapse=",")
        x$provenance[nr, "digest"] <- digest::digest(x$val)        
    } else {
        if(verbose) cat("Appending provenances")
        x$provenance[nr, "caller"] <- "addProvenance"
        x$provenance[nr, "message"] <- "Merged (*) provenance follows:"
        yp <- msg$provenance[-1,]
        yp$message <- paste("*", yp$message)
        x$provenance <- rbind(x$provenance, yp)
    }
     
    x
} # addProvenance
