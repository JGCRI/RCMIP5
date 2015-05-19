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
#' @note This is an internal RCMIP5 function and not exported.
calcGridArea<- function(lon, lat, verbose=FALSE) {

    # Deal with backwards compatibility for old 1D arrays of lon and lat
    if(length(dim(lon)) == 0){
        lon <- matrix(lon, nrow=length(lon), ncol=length(lat))
    }
    if(length(dim(lat)) == 0){
        lat <- matrix(lat, nrow=dim(lon)[1], ncol=dim(lon)[2], byrow=TRUE)
    }
    
    # Sanity checks - parameter classes and lengths
    assert_that(is.numeric(lon) & is.numeric(lat))
    assert_that(length(dim(lon)) == 2 | all(dim(lon)[-2:-1] == 1))
    assert_that(length(dim(lat)) == 2 | all(dim(lat)[-2:-1] == 1))
    assert_that(is.flag(verbose))

    if(verbose) cat('Calculating grid cell areas...\n')
    numLon <- dim(lon)[1]
    numLat <- dim(lat)[2]
    
    # If for some reason we have a -180:180 lon base, reset to span 0:360
    lon[lon < 0] <- 360 + lon[lon < 0]

    # Calculate the longitude degrees spanned by a grid cell
    # ... modulo 360 to deal with wrapping boundries
    deltaLon <- (lon[c(2:numLon,1),] - lon[1:numLon,]) %% 360

    # Calculate the min/max latitude for each grid cell
    edgeLat <- (lat[,2:numLat]+lat[,2:numLat-1])/2
    minLat <- cbind(-90, edgeLat)
    maxLat <- cbind(edgeLat, 90)
    # Check that the latitudes are centered in the grids
    if(any(abs(lat) == 90)) {
        warning('Grid cells centered at poles will have zero area.')
        minLat[minLat < -90] <- -90
        maxLat[maxLat > 90] <- 90
    }
    
    # Convert from degree to radius
    deltaLon <- deltaLon/180*pi
    minLat <- minLat/180*pi
    maxLat <- maxLat/180*pi
    lat <- lat/180*pi

    # Assume the radius of the earth: 6371e3 meter
    R <- 6371e3 # meters

    # Calculate the east/west edges by assuming the earth is spherical and
    # ...east/west edges are defined by latitude arc lengths
    # ... => R*(maxLat-minLat)
    # Calculate the north/south edges by assuming the arc length of longitude
    # ...is the lattitude corrected radius (R*cos(lat)) times the change in lon
    # ... => (R*cos(lat))*deltaLon
    return( abs(R*(maxLat-minLat) * (R*cos(lat))*deltaLon))

    # Old formulation for reference (updated 29 September 2014)
    # ...no significant difference but harder to explain
    #R^2*(sin(maxLat)-sin(minLat))*deltaLon

} # calcGridArea
