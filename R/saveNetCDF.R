#' Save a cmip5data object to NetCDF format
#' 
#' There are at least three ways to save a \code{\link{cmip5data}} object.
#' First, \link{save} it. Second, use \link{as.data.frame} or \link{as.array}.
#' Third, this function, which will write out a new NetCDF file readable by 
#' any NetCDF-aware software.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param file Filename; if omitted one will be generated automatically.
#' @param path File path.
#' @param verbose logical. Print info as we go?
#' @param saveProvenance Save the provenance separately?
#' @param originalNames logical. Use original dimension names from file?
#' @return The fully-qualified filename that was written (invisible).
#' @details If no filename is provided, a meaningful one will be assigned based on 
#' the CMIP5 naming convention (but appending 'RCMIP5'). The \code{\link{loadCMIP5}} 
#' function should be able to read this file. If \code{saveProvenance} is specified, 
#' the provenance is saved separately in a comma-separated file of the same name but
#' appending "_prov.csv". (Provenance messages are always saved as NetCDF file attributes.)
#' @note This function requires the \code{ncdf4} package; \code{ncdf} is not supported.
#' @export
saveNetCDF <- function(x, file=NULL, path="./", verbose=FALSE, saveProvenance=TRUE, originalNames=FALSE) {
    
    # Sanity checks - class and length of parameters
    assert_that(class(x)=="cmip5data")
    assert_that(is.null(file) | (length(file)==1 & is.character(file)))
    assert_that(is.dir(path))
    assert_that(is.writeable(path))
    assert_that(is.flag(verbose))
    
    if(originalNames) 
        dimNames <- x$dimNames
    else
        dimNames <- c("lon", "lat", "Z", "time")
    if(verbose) cat("Writing with names", dimNames, "\n")
    
    # loadEnsemble() handles both ncdf and ncdf4, but saveNetCDF only supports the latter
    if(!requireNamespace('ncdf4', quietly=!verbose)) {
        stop("This requires the 'ncdf4' package. Download it from CRAN or http://cirrus.ucsd.edu/~pierce/ncdf/")            
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
    fqfn <- file.path(path, file)
    
    # Define spatial dimensions, if present
    dimlist <- list()
    varlist <- list()
    if(!is.null(x$lon) & !is.null(x$lat)) {
        if(verbose) cat("Defining spatial dimensions...")
        idim <- ncdf4::ncdim_def("i", "1", 1:ncol(x$lon), longname="cell index along first dimension (lon)")
        jdim <- ncdf4::ncdim_def("j", "1", 1:nrow(x$lat), longname="cell index along second dimension (lat)")
        dimlist <- list(idim, jdim) # for now assume no Z/time        
        assert_that(!is.null(x$debug$lonUnit))
        assert_that(!is.null(x$debug$latUnit))
        lonvar <- ncdf4::ncvar_def("lon", x$debug$lonUnit, dimlist)
        latvar <- ncdf4::ncvar_def("lat", x$debug$latUnit, dimlist)
        varlist <- list(lonvar, latvar)
    }
    
    # Define Z and time dimensions, if present
    if(!is.null(x$Z)) {
        if(verbose) cat("Defining Z dimension...")
        assert_that(!is.null(x$debug$ZUnit))
        Zdim <- ncdf4::ncdim_def(dimNames[3], x$debug$ZUnit, x$Z)
        dimlist[[length(dimlist)+1]] <- Zdim
    }
    if(!is.null(x$time)) {
        if(verbose) cat("Defining time dimension...")
        assert_that(!is.null(x$debug$timeUnit))
        assert_that(!is.null(x$debug$timeRaw))
        assert_that(!is.null(x$calendarStr))
        timedim <- ncdf4::ncdim_def(dimNames[length(dimNames)], x$debug$timeUnit, x$debug$timeRaw, calendar=x$calendarStr)
        dimlist[[length(dimlist)+1]] <- timedim     
    }
    
    if(verbose) cat(length(dimlist), "dimensions for", x$variable, "\n")
    
    # Define mandatory variable
    if(verbose) cat("Defining main NetCDF variable\n")
    valvar <- ncdf4::ncvar_def(x$variable, x$valUnit, dimlist)
    varlist[[length(varlist)+1]] <- valvar
    
    # Create the file and write mandatory variable
    # Note we make sure data is sorted correctly first TODO
    if(verbose) cat("Creating and writing", file, "\n")
    nc <- ncdf4::nc_create(fqfn, varlist)    
    ncdf4::ncvar_put(nc, valvar, as.array(x))
    
    # Write spatial dimension data, if present
    if(!is.null(x$lon) & !is.null(x$lat)) {
        if(verbose) cat("Writing lon and lat data\n")
        ncdf4::ncvar_put(nc, lonvar, x$lon)
        ncdf4::ncvar_put(nc, latvar, x$lat)        
    }
    
    # Write Z and time dimensions, if present
    if(!is.null(x$Z)) {
        if(verbose) cat("Writing Z data\n")
        Zvar <- ncdf4::ncvar_def(dimNames[3], x$debug$ZUnit, Zdim)
        ncdf4::ncvar_put(nc, Zvar, x$Z) 
    }
    if(!is.null(x$time)) {
        if(verbose) cat("Writing time data\n")
        timevar <- ncdf4::ncvar_def(dimNames[4], x$debug$timeUnit, timedim)
        ncdf4::ncvar_put(nc, timevar, x$debug$timeRaw) 
    }
    
    # Get package version number, allowing that there might not be one
    pkgv <- "???"
    try({ pkgv <- packageVersion("RCMIP5") }, silent=T)
    
    # Write attributes
    if(verbose) cat("Writing attributes\n")    
    ncdf4::ncatt_put(nc, 0, "software", paste("Written by RCMIP5", pkgv, 
                                              "under", R.version.string, date()))
    if(!is.null(x$time)) {
        ncdf4::ncatt_put(nc, 0, "frequency", x$debug$timeFreqStr)
    }
    for(i in 1:nrow(x$provenance)) {
        ncdf4::ncatt_put(nc, 0, paste0("provenance", i), x$provenance[i, "message"])
    }
    
    # All done. Close file, update provenance, return filename
    ncdf4::nc_close(nc)
    if(verbose) cat("Wrote", round(file.info(fqfn)$size/1024/1024, 2), "MB\n")
    
    if(saveProvenance) {
        fqfn_prov <- paste0(fqfn, "_prov.csv")
        write.csv(as.data.frame(x$provenance), fqfn_prov)
        if(verbose) cat("Wrote provenance to", fqfn_prov, "\n")   
    }
    
    invisible(fqfn)
} # saveNetCDF
