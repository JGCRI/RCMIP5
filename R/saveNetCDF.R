#' Save a cmip5data object to netCDF format
#' 
#' There are at least three ways to save a \code{\link{cmip5data}} object.
#' First, \link{save} it. Second, use \link{as.data.frame}. Third, this function
#' will write out a new netCDF file readable by any netCDF-aware software.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param file Filename desired. If omitted one will be generated automatically.
#' @param path File path.
#' @param verbose logical. Print info as we go?
#' @param saveProvenance Save the provenance separately?
#' @param originalNames logical. Use original dimension names from file?
#' @param force.ncdf Force use of the older ncdf package for testing?
#' @return The fully-qualified filename that was written (invisible).
#' @details If no filename is provided, a meaningful one will be assigned based on the
#' CMIP5 naming convention (but appending 'RCMIP5'). \code{\link{loadCMIP5}} should be
#' able to read this file. If \code{saveProvenance} is specified, the provenance is saved
#' separately in a comma-separated file of the same name but appending "_prov.csv".
#' (Provenance messages are always saved as netcdf file attributes.)
saveNetCDF <- function(x, file=NULL, path="./", verbose=FALSE, 
                       saveProvenance=TRUE, originalNames=FALSE, force.ncdf=FALSE) {
    
    # Sanity checks - class and length of parameters
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(file) | (length(file)==1 & is.character(file)))
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    # The ordering of x$val dimensions is lon-lat-Z?-time?
    # Anything else is not valid.
    stopifnot(length(dim(x$val)) %in% c(1, 2, 3, 4)) # that's all we know
    if(originalNames) 
        dimNames <- x$dimNames
    else
        dimNames <- c("lon", "lat", "Z", "time")
    if(verbose) cat("Writing with names", dimNames, "\n")
    
    # We prefer to use the 'ncdf4' package, but Windows has problems with this, so
    # if it's not installed can also use 'ncdf'
    if(force.ncdf | !require(ncdf4, quietly=!verbose)) {
        if(require(ncdf, quietly=!verbose)) {
            # The ncdf and ncdf4 functions are mostly parameter-identical. This makes
            # things easy--we redefine the ncdf4 function names to their ncdf equivalents
            .nc_create <- ncdf::create.ncdf
            .ncdim_def <- ncdf::dim.def.ncdf
            .ncvar_def <- ncdf::var.def.ncdf
            .ncatt_put <- ncdf::att.put.ncdf
            .ncvar_put <- ncdf::put.var.ncdf
            .nc_close <- ncdf::close.ncdf
        } else {
            stop("No netCDF (either 'ncdf4' or 'ncdf') package is available")            
        }
    } else {
        .nc_create <- ncdf4::nc_create
        .ncdim_def <- ncdf4::ncdim_def
        .ncvar_def <- ncdf4::ncvar_def
        .ncatt_put <- ncdf4::ncatt_put
        .ncvar_put <- ncdf4::ncvar_put
        .nc_close <- ncdf4::nc_close
    }
    
    # Create meaningful filename, if necessary
    if(is.null(file)) {
        ensembles <- paste(x$ensembles, collapse="")
        pretty <- function(x) {formatC(round(x, 0), width=2, flag="0")}
        mintime <- paste0( floor(min(x$time)), pretty(min(x$time) %% 1 * 12 + 0.5))
        maxtime <- paste0( floor(max(x$time)), pretty(max(x$time) %% 1 * 12 + 0.5))
        file <- paste(x$variable, x$domain, x$model, x$experiment, ensembles, 
                      paste(mintime, maxtime, sep="-"), "RCMIP5.nc", sep="_")
    }
    fqfn <- paste(path, file, sep="/")
    
    # Define spatial dimensions, if present
    if(verbose) cat("Defining netCDF dimensions...")
    dimlist <- c()
    if(!is.null(x$lon) & !is.null(x$lat)) {
        londim <- .ncdim_def("lon", x$debug$lonUnit, x$lon)
        latdim <- .ncdim_def("lat", x$debug$latUnit, x$lat)
        dimlist <- list(londim, latdim) # for now assume no Z/time        
    }
    
    # Define Z and time dimensions, if present
    if(!is.null(x$Z)) {
        Zdim <- .ncdim_def(dimNames[3], x$debug$ZUnit, x$Z)
        dimlist <- list(londim, latdim, Zdim)
    }
    if(!is.null(x$time)) {
        timedim <- .ncdim_def(dimNames[length(dimNames)], x$debug$timeUnit, x$debug$timeRaw, calendar=x$debug$calendarStr)
        dimlist[[length(dimlist)+1]] <- timedim     
    }
    
    if(verbose) cat(length(dimlist), "dimensions for", x$variable, "\n")
    
    # Define mandatory variable
    if(verbose) cat("Defining main netCDF variable\n")
    valvar <- .ncvar_def(x$variable, x$valUnit, dimlist)
    
    # Create the file and write mandatory variable
    if(verbose) cat("Creating and writing", file, "\n")
    nc <- .nc_create(fqfn, valvar)
    .ncvar_put(nc, valvar, x$val)
    
    # Write spatial dimensions, if present
    if(!is.null(x$lon) & !is.null(x$lat)) {
        if(verbose) cat("Writing lon and lat\n")
        lonvar <- .ncvar_def("lon", x$debug$lonUnit, londim)
        latvar <- .ncvar_def("lat", x$debug$latUnit, londim)
        .ncvar_put(nc, lonvar, x$lon)
        .ncvar_put(nc, latvar, x$lat)        
    }
    
    # Write Z and time variables, if present
    if(!is.null(x$Z)) {
        if(verbose) cat("Writing Z\n")
        Zvar <- .ncvar_def(dimNames[3], x$debug$ZUnit, Zdim)
        .ncvar_put(nc, Zvar, x$Z) 
    }
    
    # Get package version number, allowing that there might not be one
    pkgv <- "???"
    try({
        pkgv <- packageVersion("RCMIP5") 
    }, silent=T)
    
    # Write attributes
    if(verbose) cat("Writing attributes\n")    
    .ncatt_put(nc, 0, "software", paste("Written by RCMIP5", pkgv, 
                                        "under", R.version.string, date()))
    if(!is.null(x$time)) {
        .ncatt_put(nc, 0, "frequency", x$debug$timeFreqStr)
    }
    for(i in 1:nrow(x$provenance)) {
        .ncatt_put(nc, 0, paste0("provenance", i), x$provenance[i, "message"])
    }
    
    # All done with netCDF. Close file, inform user
    .nc_close(nc)
    if(verbose) cat("Wrote", round(file.info(fqfn)$size/1024/1024, 2), "MB\n")
    
    if(saveProvenance) {
        fqfn_prov <- paste0(fqfn, "_prov.csv")
        write.csv(as.data.frame(x$provenance), fqfn_prov)
        if(verbose) cat("Wrote provenance to", fqfn_prov, "\n")   
    }
    
    invisible(fqfn)
} # saveNetCDF
