#' Regrid data.
#' 
#' Regrid data. TODO
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param area cmip5data An area cmip5data data structure
#' @param forceRegrid logical. TODO
#' @param verbose logical. Print info as we go?
#' @return The data regridded onto the new spatial grid.
#' @export
regridData <- function(x, area=NULL, regridSize, forceRegrid=FALSE, verbose=TRUE) {
    
    # Sanity checks - parameter classes and lengths
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(area) | class(area)=="cmip5data")
    stopifnot(length(forceRegrid)==1 & is.logical(forceRegrid))
    stopifnot(length(regridSize)==1 & is.numeric(regridSize))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    val <- x$val
    lon <- x$lon
    lat <- x$lat
    units <- x$valUnit
    
    if(is.null(x$area)) {
        if(verbose) cat("No area supplied; calculating...")
        area <- calcGridArea(lon=x$lon, lat=x$lat)
    } else {
        area <- area$val
    }
    
    isDensity <- grepl('m(\\^)?-2', units) | grepl('%', units)
    if(verbose) cat('Density flag is: [', isDensity, ']\n')
    
    isTemp <- grepl('K', units)
    if(verbose) cat('Temp flag is : [', isTemp, ']\n')
    
    projection <- x
    projection$lat <- seq(-90, 90-regridSize, by=regridSize) + 0.5*regridSize
    projection$lon <- seq(0, 360-regridSize, by=regridSize) + 0.5*regridSize
    projection$area <- calcGridArea(projection$lat, projection$lon)
    
    if(verbose) cat('Regridding [', x$lon[2] - x$lon[1], 'x',
                    mean(x$lat[2:length(lat)] - x$lat[1:(length(lat)-1)]), '] to [',
                    regridSize, 'x', regridSize, ']\n')
    
    if(isTemp|isDensity) {
        projection$val <- x$val * projection$area
        
        data <- regrid_val(orginal, projection, verbose=TRUE)
        orginal$val <- orginal$val/orginal$area
        data$val <- data$val/data$area  # correct for regridding artifacts
        data$area <- projection$area    # reset the old area after correction
    } else {
        data <- regrid_val(orginal, projection)
        data$val <- data$val/data$area  # correct for regridding artifacts
        data$area <- projection$area    # reset the old area after correction
        data$val <- data$val*data$area  # correct for regridding artifacts
    }
    
    
    return(data)
} # regrid_data
