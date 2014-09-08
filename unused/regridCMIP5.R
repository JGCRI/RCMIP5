#' Regrid CMIP5 data.
#' 
#' Regrid CMIP5 data. TODO
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param area cmip5data An area cmip5data data structure
#' @param regridSize size of new cell, in degrees
#' @param verbose logical. Print info as we go?
#' @return The data regridded onto the new spatial grid.
#' @export
regridCMIP5 <- function(x, area=NULL, regridSize, verbose=TRUE) {
    
    # Sanity checks - parameter classes and lengths
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(area) | class(area)=="cmip5data")
    stopifnot(length(regridSize)==1 & is.numeric(regridSize))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    val <- x$val
    lon <- x$lon
    lat <- x$lat
    units <- x$valUnit
    
    # Assign or calculate (if none supplied) the grid cell areas
    if(is.null(area) & is.null(x$area)) {
        x$area <- calcGridArea(x$lon, x$lat, verbose=verbose)
    } else {
        x$area <- area$val
    }
    
    isDensity <- grepl('m(\\^)?-2', units) | grepl('%', units)
    if(verbose) cat('Density flag is', isDensity, '\n')
    isTemp <- grepl('K', units)
    if(verbose) cat('Temp flag is', isTemp, '\n')
    
    if(verbose) cat("Creating new projected data object\n")
    projection <- x
    projection$lon <- seq(0, 360-regridSize, by=regridSize) + 0.5*regridSize
    projection$lat <- seq(-90, 90-regridSize, by=regridSize) + 0.5*regridSize
    projection$area <- calcGridArea(projection$lon, projection$lat, verbose)
    projection <- addProvenance(projection, paste("Shifted lon/lat to new grid size", regridSize))
    
    if(verbose) cat('Regridding [', x$lon[2] - x$lon[1], 'x',
                    mean(x$lat[2:length(lat)] - x$lat[1:(length(lat)-1)]), '] degrees to [',
                    regridSize, 'x', regridSize, '] degrees\n')
    
    if(isTemp | isDensity) {
        x$val <- x$val * x$area  # e.g. from X/m2 to X (per grid cell)
        projection$val <- regridVal(x$val, x$lon, x$lat, x$area, projection$lon, projection$lat, projection$area, verbose=verbose)
        projection$val <- projection$val/projection$area  # and back to X/m2
    } else {
        projection$val <- regridVal(x$val, x$lon, x$lat, x$area, projection$lon, projection$lat, projection$area, verbose=verbose)
    } # if
    
    projection
} # regrid_data
