#' Filter dimensions, limiting to arbitrary lon/lat/time/lev/depth.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param lons numeric vector. Longitudes to filter (in).
#' @param lats numeric vector. Latitudes to filter (in).
#' @param depths numeric vector. Depths to filter.
#' @param levs numeric vector
#' @param years numeric vector
#' @param months numeric vector
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @export
filterDimensions <- function(x, lons=NULL, lats=NULL, depths=NULL, levs=NULL,
                             years=NULL, months=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(lons) | class(lons) %in% c("numeric", "integer"))
    stopifnot(is.null(lats) | class(lats) %in% c("numeric", "integer"))
    stopifnot(is.null(depths) | class(depths) %in% c("numeric", "integer"))
    stopifnot(is.null(levs) | class(levs) %in% c("numeric", "integer"))
    stopifnot(is.null(years) | class(years) %in% c("numeric", "integer"))
    stopifnot(is.null(months) | class(months) %in% c("numeric", "integer"))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    # The ordering of x$val dimensions is lon, lat, [depth,] [lev,] time
    # Anything else is not valid.
    ndim <- length(dim(x$val))
    stopifnot(ndim %in% c(3, 4, 5)) # that's all we know
    
    # Filter depth dimension
    if(!is.null(depths)) {
        ok <- TRUE
        if(is.null(x[["depth"]])) {
            warning("No depth data found")
            ok <- FALSE
        } else if(ndim == 4) {
            x$val <- x$val[,,x$depth %in% depths,]
        } else if(ndim == 5) {
            x$val <- x$val[,,x$depth %in% depths,,]
        } else {  # if we're here, there's a problem
            stop("Unknown structure - depth filter can't be applied")
        }
        if(ok) {
            x$depth <- x$depth[x$depth %in% depths]
            x$provenance <- addProvenance(x$provenance,
                                          paste("Filtered for depths in range [",
                                                paste(range(depths),
                                                      collapse=', '), "]"))
        }
    }
    
    # Filter lev dimension
    if(!is.null(levs)) {
        ok <- TRUE
        if(is.null(x[["lev"]])) {
            warning("No lev data found")
            ok <- FALSE
        } else if(ndim == 4) {
            x$val <- x$val[,,x$lev %in% levs,]
        } else if(ndim == 5) {
            x$val <- x$val[,,,x$lev %in% levs,]
        } else {  # if we're here, there's a problem
            stop("Unknown structure - lev filter can't be applied")
        }
        if(ok) {
            x$lev <- x$lev[x$lev %in% levs]
            x$provenance <- addProvenance(x$provenance,
                                          paste("Filtered for levs in range [",
                                                paste(range(levs),
                                                      collapse=', '), "]"))
        }
    }
    
    # Filter time (years) dimension
    if(!is.null(years)) {
        years <- floor(years) # no fractional years allowed
        ok <- TRUE
        if(is.null(x[["time"]])) {
            warning("No time data found")
            ok <- FALSE
        } else if(ndim == 3) {
            x$val <- x$val[,,floor(x$time) %in% years]
        } else if(ndim == 4) {
            x$val <- x$val[,,,floor(x$time) %in% years]
        } else if(ndim == 5) {
            x$val <- x$val[,,,,floor(x$time) %in% years]
        } else {  # if we're here, there's a problem
            stop("Unknown structure - time filter can't be applied")
        }
        if(ok) {
            x$time <- x$time[floor(x$time) %in% years]
            x$provenance <- addProvenance(x$provenance,
                                          paste("Filtered for years in range [",
                                                paste(range(years),
                                                      collapse=', '), "]"))
        }
    }
    
    x
} # filterDimensions
