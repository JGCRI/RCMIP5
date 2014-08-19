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
    
    # TODO: Do we want a function that does this? It's going to be very common
    # for users to only care about surface CO2, for example, and they don't want
    # makeAnnualMean() to run computations on all levels.

    # TODO: ask 'mask' (spatial lon/lat mask) above?
    
    # TODO: Should variables above be bounds, e.g. to filter to western hemisphere
    #   lons=c(-180, 0)
    # or straight numbers
    #   lons=c(-180, -175, ..., 0)
    # ? The first is simpler, while the second requires more work from the user but
    # gives more flexibility. I'm inclined to go with simple.
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(lons) | class(lons)=="numeric")
    stopifnot(is.null(lats) | class(lats)=="numeric")
    stopifnot(is.null(depths) | class(depths)=="numeric")
    stopifnot(is.null(levs) | class(levs)=="numeric")
    stopifnot(is.null(years) | class(years)=="numeric")
    stopifnot(is.null(months) | class(months)=="numeric")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
} # filterDimensions
