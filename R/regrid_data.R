
regrid_data <- function(x, regridSize, forceRegrid=FALSE, verbose=TRUE){

##regrid_data <- function(val, area, longitude, latitude, regridSize,
#                        units, forceRegrid=FALSE, verbose=TRUE){
  val <- x$val
  longitude <- x$lon
  latitude <- x$lat
  units <- x$valUnit

  if(is.null(x$area)){
      area <- calcGridArea(lat=x$lat, lon=x$lon)
  }else{
      area <- x$area
  }


  isDensity <- grepl('m(\\^)?-2', units) | grepl('%', units)
  cat('Density flag is: [', isDensity, ']\n')

  isTemp <- grepl('K', units)
  cat('Temp flag is : [', isTemp, ']\n')

  orginal<- list(val = val, lon=longitude, lat=latitude, area=area)
#  orginal$deltaLon <- orginal$lon[2] - orginal$lon[1]
#  orginal$deltaLat <- orginal$lat[2] - orginal$lat[1]
                                        #cat('deltaLon [', orginal$deltaLon, '], deltaLat [', orginal$deltaLat, ']\n')
  ##calculate the area of the grid cells
  #orginal$area <- matrix(calcGridArea(orginal$lat,
  #                                    orginal$lat[2] - orginal$lat[1],
  #                                    orginal$lon[2] - orginal$lon[1])
  #                       , ncol=length(orginal$lat), nrow=length(orginal$lon), byrow=TRUE )

  ##set up the regridded projection we want to see
  #projection <-  list(deltaLat = regridSize,
  #                    deltaLon = regridSize)
  projection <- list()
  projection$lat <- seq(-90, 90-regridSize,
                        by=regridSize) + 0.5*regridSize
  projection$lon <- seq(0, 360-regridSize,
                        by=regridSize) + 0.5*regridSize
                                        #cat(head(projection$lat))
                                        #cat(class(projection$numLon))
  #projection$area <- matrix(
  #                          calcGridArea(projection$lat,
  #                                       projection$deltaLat,
  #                                       projection$deltaLon),
  #                          ncol=length(projection$lat),
   #                         nrow=length(projection$lon),
  #                          byrow=TRUE)
  projection$area <- calcGridArea(projection$lat, projection$lon)

  cat('regriding [', orginal$lon[2] - orginal$lon[1], 'x',
      mean(orginal$lat[2:length(latitude)] - orginal$lat[1:(length(latitude)-1)]), '] to [',
      regridSize, 'x', regridSize, ']\n')

  if(isTemp|isDensity){
    #dev.new()
    #par(mfrow=c(2,3))
    #source('world.plot.R')
    #world.plot(orginal$lon, orginal$lat, orginal$val, title='orginal grid [K]')
    #world.plot(orginal$lon, orginal$lat, orginal$area, title='orginal grid area')
    cat('val dim [',dim(orginal$val),']. area dim[', dim(orginal$area), ']\n')
    orginal$val <- orginal$val*orginal$area

    #world.plot(orginal$lon, orginal$lat, orginal$val, title='orginal grid (normed)')
    data <- regrid_val(orginal, projection, verbose=TRUE)
    #world.plot(data$lon, data$lat, data$val, title='projected grid (normed)')
    orginal$val <- orginal$val/orginal$area
    data$val <- data$val/data$area #correct for regriding artifacts
    data$area <- projection$area #reset the old area after correction
    #world.plot(data$lon, data$lat, data$area, title='projected grid area')
    #world.plot(data$lon, data$lat, data$val, title='projected grid [K]')
    #cat('mean orgianl [', mean(orginal$val, na.rm=TRUE), '] == [', mean(data$val, na.rm=TRUE), ']\n')
  }else{
    data <- regrid_val(orginal, projection)
    data$val <- data$val/data$area #correct for regriding artifacts
    data$area <- projection$area #reset the old area after correction
    data$val <- data$val*data$area #correct for regriding artifacts
  }


  return(data)
}
