#' Save a cmip5data object to netCDF format.
#' 
#' There are at least three ways to save a \code{\link{cmip5data}} object.
#' First, \link{save} it. Second, use \link{as.data.frame}. Third, this function
#' will write out a new netCDF file readable by any netCDF-aware software.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param file Filename desired. If omitted one will be generated automatically.
#' @param path File path.
#' @param verbose logical. Print info as we go?
#' @return The fully-qualified filename that was written (invisible).
#' @details If no filename is provided, a meaningful one will be assigned based on the
#' CMIP5 naming convention (but appending 'RCMIP5'). \code{\link{loadEnsemble}} should be
#' able to read this file.
saveNetCDF <- function(x, file=NULL, path="./", verbose=TRUE) {

    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(length(file)==1 & (is.null(file) | is.character(file)))
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(length(verbose)==1 & is.logical(verbose))

    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
    # Anything else is not valid.
    stopifnot(length(dim(x$val)) %in% c(3, 4)) # that's all we know
    
    # Create meaningful filename, if necessary
    if(is.null(file)) {
        ensembles <- paste(x$ensemble, collapse="")
        pretty <- function(x) {formatC(round(x, 0), width=2, flag="0")}
        mintime <- paste0( floor(min(x$time)), pretty(min(x$time) %% 1 * 12 + 0.5))
        maxtime <- paste0( floor(max(x$time)), pretty(max(x$time) %% 1 * 12 + 0.5))
        file <- paste(x$variable, x$domain, x$model, x$experiment, ensembles, 
                      paste(mintime, maxtime, sep="-"), "RCMIP5.nc", sep="_")
    }
    fqfn <- normalizePath(paste(path, file, sep="/"))
    
    # Define dimensions
    if(verbose) cat("Defining netCDF dimensions...")
    londim <- ncdim_def("lon", x$debug$lonUnit, x$lon)
    latdim <- ncdim_def("lat", x$debug$latUnit, x$lat)
    timedim <- ncdim_def("time", x$debug$timeUnit, x$debug$timeRaw, calendar=x$debug$calendarStr)
    dimlist <- list(londim, latdim, timedim)
    if(!is.null(x$depth)) {
        depthdim <- ncdim_def("depth", x$debug$depthUnit, x$depth)
        dimlist <- list(londim, latdim, depthdim, timedim)
    } else if(!is.null(x$lev)) {
        levdim <- ncdim_def("lev", x$debug$levUnit, x$lev)
        dimlist <- list(londim, latdim, levdim, timedim)
    }
    if(verbose) cat(length(dimlist), "dimensions\n")
    
    # Create variable and write data
    if(verbose) cat("Defining netCDF variables\n")
    valvar <- ncvar_def(x$variable, x$valUnit, dimlist)
    
    if(verbose) cat("Creating and writing", file, "\n")
    nc <- nc_create(fqfn, valvar)
    ncvar_put(nc, valvar, x$val)
    
    # Write attributes
    if(verbose) cat("Writing attributes\n")
    for(i in 1:length(x$files)) {
        ncatt_put(nc, 0, paste0("file", i), x$files[i])
    }
    for(i in 1:length(x$provenance)) {
        ncatt_put(nc, 0, paste0("provenance", i), x$provenance[i])
    }
    ncatt_put(nc, 0, "software", paste("File written by RCMIP5,", R.version.string))
    nc_close(nc)
    
    if(verbose) cat("Wrote", round(file.info(file)$size/1024/1024, 2), "MB\n")
    invisible(fqfn)
} # saveNetCDF
