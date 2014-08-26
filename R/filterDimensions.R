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
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    x <- filterDimensionsLon(x, lons, verbose)
    x <- filterDimensionsLat(x, lats, verbose)
    x <- filterDimensionDepth(x, depths, verbose)
    x <- filterDimensionLev(x, levs, verbose)
    x <- filterDimensionTimeYears(x, years, verbose)
    x <- filterDimensionTimeMonths(x, months, verbose)
    
    x
} # filterDimensions

#' Filter longitude dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param lons numeric vector. Longitudes to filter (in).
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
filterDimensionLon <- function(x, lons=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(is.null(lons) | class(lons) %in% c("numeric", "integer"))
    
    # Filter longitude dimension
    ndim <- length(dim(x$val))
    if(!is.null(lons)) {
        if(verbose) cat("Filtering by lon\n")
        if(is.null(x[["lon"]])) {
            warning("No lon data found")
        } else {
            x$val <- asub(x$val, x$lon %in% lons, 1)
            x$lon <- x$lon[x$lon %in% lons]
            x$provenance <- addFilterProvenance(x$provenance, lons)
        }
    }
    x
} # filterDimensionLon

#' Filter latitude dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param lats numeric vector. Latitudes to filter (in).
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
filterDimensionLat <- function(x, lats=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(is.null(lats) | class(lats) %in% c("numeric", "integer"))
    
    # The ordering of x$val dimensions is lon-lat-(depth|lev)?-time?
    # Anything else is not valid.
    ndim <- length(dim(x$val))
    stopifnot(ndim %in% c(2, 3, 4, 5)) # that's all we know
    
    # Filter latitude dimension
    if(!is.null(lats)) {
        if(verbose) cat("Filtering by lat\n")
        if(is.null(x[["lat"]])) {
            warning("No lat data found")
        } else {
            x$val <- asub(x$val, x$lat %in% lats, 2)
            x$lat <- x$lat[x$lat %in% lats]
            x$provenance <- addFilterProvenance(x$provenance, lats)
        }
    }
    x
} # filterDimensionLat

#' Filter depth dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param depths numeric vector. Depths to filter.
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
filterDimensionDepth <- function(x, depths=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(is.null(depths) | class(depths) %in% c("numeric", "integer"))
        
    # Filter depth dimension
    ndim <- length(dim(x$val))
    if(!is.null(depths)) {
        if(verbose) cat("Filtering by depth\n")
        if(is.null(x[["depth"]])) {
            warning("No depth data found")
        } else {
            x$val <- asub(x$val, x$depth %in% depths, ndim-1)
            x$depth <- x$depth[x$depth %in% depths]
            x$provenance <- addFilterProvenance(x$provenance, depths)
        }
    }    
    x
} # filterDimensionDepth

#' Filter lev dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param depths numeric vector. Depths to filter.
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
filterDimensionLev <- function(x, levs=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(is.null(levs) | class(levs) %in% c("numeric", "integer"))
    
    # Filter lev dimension
    ndim <- length(dim(x$val))
    if(!is.null(levs)) {
        if(verbose) cat("Filtering by lev\n")
        if(is.null(x[["lev"]])) {
            warning("No lev data found")
        } else {
            x$val <- asub(x$val, x$lev %in% levs, ndim-1)
            x$lev <- x$lev[x$lev %in% levs]
            x$provenance <- addFilterProvenance(x$provenance, levs)
        }
    }
    x
} # filterDimensionLev

#' Filter time (years) dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param years numeric vector
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
filterDimensionTimeYears <- function(x, years=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(is.null(years) | class(years) %in% c("numeric", "integer"))
        
    # Filter time (years) dimension
    ndim <- length(dim(x$val))
    if(!is.null(years)) {
        if(verbose) cat("Filtering by year\n")
        years <- floor(years) # no fractional years allowed
        if(is.null(x[["time"]])) {
            warning("No time data found")
        } else {
            x$val <- asub(x$val, floor(x$time) %in% years, ndim)
            x$time <- x$time[floor(x$time) %in% years]
            x$provenance <- addFilterProvenance(x$provenance, years)
        }
    }
    x
} # filterDimensionTimeYears

#' Filter time (months) dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param months numeric vector
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
filterDimensionTimeMonths <- function(x, months=NULL, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(is.null(months) | class(months) %in% c("numeric", "integer"))
    stopifnot(months %in% 1:12)
        
    # Filter time (months) dimension
    ndim <- length(dim(x$val))
    if(!is.null(years)) {
        if(verbose) cat("Filtering by month\n")
        fracmonths <- round((months-0.5) / 12, 2) # From Jan=1, Feb=2 to Jan 15=0.042, Feb15=0.123, etc.
        monthfilter <- round(x$time %% 1, 2) %in% fracmonths
        if(is.null(x[["time"]])) {
            warning("No time data found")
        } else {
            x$val <- asub(x$val, monthfilter, ndim)
            x$time <- x$time[monthfilter]
            x$provenance <- addFilterProvenance(x$provenance, months)
        }
    }
    x
} # filterDimensionTimeMonths

#' Add provenance information after filtering.
#' 
#' @param prov Provenance, a vector of strings
#' @param filtervar Variable filtering by.
#' @return Updated provenance.
#' @note This is an internal RCMIP5 function and not exported.
addFilterProvenance <- function(prov, filtervar) {
    addProvenance(prov,
                  paste("Filtered for", deparse(substitute(filtervar)), "in range [",
                        paste(range(filtervar),
                              collapse=', '), "]"))
} # addFilterProvenance
