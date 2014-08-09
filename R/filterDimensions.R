#' Filter dimensions, limiting to arbitrary lon/lat/time/lev/depth
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param years numeric vector
#' @param months numeric vector
#' @param levs numeric vector
#' @param depths numeric vector. Depths to filter.
#' @param mask numeric array. Spatial mask of same dimensions as object.
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @return A \code{\link{cmip5data}} object.
#' @export
#' @examples
filterDimensions <- function() {
    
    # TODO: Do we want a function that does this? It's going to be very common
    # for users to only care about surface CO2, for example, and they don't want
    # makeAnnualMean() to run computations on all levels.
    
} # filterDimensions
