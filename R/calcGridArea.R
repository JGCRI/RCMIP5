##Programer: Kathe Todd-Brown ktoddbrown@gmail.com
##Date: commented 23 April 2013
##Purpose: Calculate the grid cella area for a centered lat/lon grid
##Input: Accepts the latitude and longitude defining a global grid
##       Currently the lon must be uniform but the lat does not need to be
##Output: Returns the grid cell area in m^2 (meter*meter)

calcGridArea<- function(lat, lon){
#lat, lon coordinates for the centers of our grids of interest
  cat('calculating area...')
  numLat <- length(lat)
  numLon <- length(lon)

  ##Dummy check the given lat/lon
  if(abs(lat[1]) == 90){
      cat('Error: lat centered at 90\n')
      return(NA)
  }
  if(lon[1] == 0){
    if(lon[numLon] == 360){
      cat('Error: lon centered at 0 and 360')
      return(NA)
    }
    #shift the longitude over
    lon <- c(lon[2:numLon], 360)
  }

  ##Hackish, assume that the edges are at 0 and 360
  temp <- (lon[2:numLon]-lon[1:(numLon-1)])/2
  deltaLon <- c(lon[1] + temp[1],
                temp[2:length(temp)] + temp[2:length(temp)-1],
                360-rev(lon)[1]+rev(temp)[1])

  ##Assume the radius of the earth
  N <- 6371*1e3 ##Don't get too fancy, agrees better with area calc from models then trying to back out the NS/WE plane radius
  N <- matrix(N, nrow=numLon, ncol=numLat, byrow=TRUE)

  ##Find the edges of the latitude grid
  midLat <- (lat[2:numLat]+lat[1:(numLat-1)])/2
  latMin <- c(-90, midLat)
  latMax <- c(midLat, 90)

  latMin <- matrix(latMin, nrow=numLon, ncol=numLat, byrow=TRUE)
  latMax <- matrix(latMax, nrow=numLon, ncol=numLat, byrow=TRUE)
  deltaLon <- matrix(deltaLon, nrow=numLon, ncol=numLat)

  cat('done\n')
  return(N^2*(sin(latMax/180*pi)-sin(latMin/180*pi))*deltaLon/180*pi)
}
