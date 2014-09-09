#' Calculate the grid cella area for a centered lat/lon grid
#'
#' Calculate the grid cella area for a centered lat/lon grid.
#'
#' @param lat latitude coordinates of the grid centers
#' @param lon longitude coordinates of the grid centers
#' @param verbose logical. Print info as we go?
#' @return The grid cell area in m^2 (meter*meter)
#' @details Currently the lon must be uniform but the lat does not need to be.
#' @keywords internal
#' @export
calcGridArea<- function(lon, lat, verbose=FALSE) {

    # Sanity checks - parameter classes and lengths
    stopifnot(is.numeric(lat))
    stopifnot(is.numeric(lon))
    stopifnot(length(verbose)==1 & is.logical(verbose))

    if(verbose) cat('Calculating grid cell areas...\n')
    numLat <- length(lat)
    numLon <- length(lon)

    # Dummy check the given lat/lon
    # TODO: why is this happening?
    if(abs(lat[1]) == 90) {
        stop('Error: lat centered at 90\n')
    }
    if(lon[1] == 0) {
        if(lon[numLon] == 360) {
            stop('Error: lon centered at 0 and 360')
        }
        lon <- c(lon[2:numLon], 360)        # shift the longitude over
    }

    # Hackish, assume that the edges are at 0 and 360. TODO: change?
    temp <- (lon[2:numLon]-lon[1:(numLon-1)])/2
    deltaLon <- c(lon[1] + temp[1],
                  temp[2:length(temp)] + temp[2:length(temp)-1],
                  360-rev(lon)[1]+rev(temp)[1])

    # Assume the radius of the earth. Don't get too fancy: 6371e3 assumption agrees
    # better with area calc from models then trying to back out the NS/WE plane radius
    radius <- 6371e3 # meters
    N <- matrix(radius, nrow=numLon, ncol=numLat, byrow=TRUE)

    # Find the edges of the latitude grid
    # BBL: these calculations need to be better documented TODO
    midLat <- (lat[2:numLat]+lat[1:(numLat-1)])/2
#    if(verbose) cat("midLat =", midLat, "\n")
    latMin <- c(-90, midLat)
    latMax <- c(midLat, 90)

    latMin <- matrix(latMin, nrow=numLon, ncol=numLat, byrow=TRUE)
    latMax <- matrix(latMax, nrow=numLon, ncol=numLat, byrow=TRUE)
    deltaLon <- matrix(deltaLon, nrow=numLon, ncol=numLat)

    invisible(N^2 * (sin(latMax/180*pi) - sin(latMin/180*pi)) * deltaLon/180 * pi)
} # calcGridArea
