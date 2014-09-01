regrid_val <- function(orginal = 0, projection=0,
                       maxLon = 360, minLon = 0, maxLat = 90, minLat = -90,
                   saveFileName = 'regridDefault',
                   verbose = FALSE, extremeVerbose = FALSE){
  #regrid_val <- function(orginal = NA, projection=NA,
  #                 saveFileName = 'regridDefault',
  #                 verbose = FALSE)
  #Purpose: The purpose of this function is to regrid global data
  #...to a grid defined in projection. Both grids must be center defined
  #...(each lat x lon marks the center of the grid). This function does
  #...not deal with wrapping boundry conditions! One key assumption in this
  #...regridding is that a degree is propotional to distance for neighboring
  #...grid cells.
  #Inputs: Both orginal and projection are lists containing:
  #...1) $lat - a vector containing latitude values
  #...2) $lon - a vector containing longitude values
  #...3) $deltaLon - a numeric with the longitude grid size --decripid
  #...4) $deltaLat - a numeric with the latitude grid size --decripid
  #...5) $area - a [lon, lat] matrix with the grid areas stored
  #...           orginal also contains:
  #...6) $val - a [lon, lat] matrix with the density [mass/area] at each grid
  #...saveFileName - a string with the name of the file to save the
  #...               results to or load results from if already calculated
  #...verbose - turns on EXTENSIVE debugging outputs, use with caution
  #Outputs: This function returns 'projection'
  #...a list with the variables 1)-5) mentioned in Inputs as well as a field
  #...$val - a [lon, lat] matrix with the values at each grid cell.
  #...A R file saving this list is also created if it does not already exist
  #Algorithm: This function goes through each projection point, finds the
  #...percent contribution from the orginal grid using the intersection
  #...length of the lat/log grid segments, use this to find the weighted
  #...sum of the orginal data, and finally correct for the change in grid size.
  #Programer: K Todd-Brown ktoddbrown@gmail.com
  #Date: 13 April 2011
  #Updates: 14 April 2011 - removed/consolodated some of the verbose
  #                         comments so that the code is easier to follow
  #                         and simplified the test matricies so they can
  #                         be movidfied more easily
  #         20 June 2011 - modified to include wrapping for orginal longitude
  #         22 July 2011 - seperated test fuction to 'test_regrid.R'

cat('regriding value\n')
cat('projecting [', dim(orginal$val), ']==[',
    dim(orginal$lon), ', ', dim(orginal$lat), '] to [',
    dim(projection$val), ']==[',
    dim(projection$lon), ', ', dim(projection$lat),']\n')
if(length(projection$lat) == length(orginal$lat) &
   length(projection$lon) == length(orginal$lon)){
  cat("No change in gridsize, returning orginal\n")
  return(orginal)
}

##################################################
##Pull the size of the lat, lon and overall grid##
##################################################
cat('pulling size\n')
projection$numLat <- length(projection$lat)
projection$numLon <- length(projection$lon)
orginal$numLat <- length(orginal$lat)
orginal$numLon <- length(orginal$lon)

numOrg <- orginal$numLat * orginal$numLon
numProj <- projection$numLat * projection$numLon

#Allocate the space for the regrided projection
projection$val <- matrix(nrow=projection$numLon, ncol=projection$numLat)

####################################################
##Fix gridding abnomalities in orginal grid
####################################################
cat('pull grid min/max\n')

proMid <- (projection$lat[2:projection$numLat]+projection$lat[1:(projection$numLat-1)])/2
pro.lat.min <- c(-90, proMid)
pro.lat.max <- c(proMid, 90)

###Pull the boundries of the grid cells
orgMid <- (orginal$lat[2:orginal$numLat]+orginal$lat[1:(orginal$numLat-1)])/2
org.lat.min <- c(-90, orgMid)#orginal$lat-orginal$deltaLat/2
org.lat.max <- c(orgMid, 90) #orginal$lat+orginal$deltaLat/2
deltaLon <- unique(orginal$lon[2:orginal$numLon]-
                   orginal$lon[1:(orginal$numLon-1)])

if(max(deltaLon)-min(deltaLon) > 1e-8){
  print(max(deltaLon)-min(deltaLon))
  cat('ERROR: non uniform longitude... returning NA.\n')
}else{
  deltaLon <- signif(deltaLon[1],8) #make the array a scalar... hackish I know
}

org.lon.min <- orginal$lon-deltaLon/2
org.lon.max <- orginal$lon+deltaLon/2
cat('orginal lat min: [', head(org.lat.min, n=3),
    '...] max:[', tail(org.lat.max, n=3),
    '] lon min: [', head(org.lon.min, n=3),
    '] max:[', tail(org.lon.max, n=3),']\n')

##check to see if these boundries fall over the min/max
cat('check lat boundry\n')

if((org.lat.min[1] < minLat) || (org.lat.max[length(org.lat.max)] > maxLat)){

  cat('!!!!!!!! \n Error! Wrapping latitude grid not valid. Attempting to salvage by trimming top and bottom of NA or 0 values. \n')
      cat('old dim of val: [', dim(orginal$val), '] lat: [', length(orginal$lat), ']\n')


    cat('Trim max lat values\n')
    orginal$area <- orginal$area[,1:(orginal$numLat-1)]
    orginal$val <- orginal$val[,1:(orginal$numLat-1)]
    orginal$lat <- orginal$lat[1:(orginal$numLat-1)]
    orginal$numLat <- length(orginal$lat)

  #if(all(is.na(orginal$val[,1])) | sum(orginal$val[,1]) == 0){
    cat('Trim min lat values\n')
    orginal$area <- orginal$area[,2:orginal$numLat]
    orginal$val <- orginal$val[,2:orginal$numLat]
    orginal$lat <- orginal$lat[2:orginal$numLat]
    orginal$numLat <- length(orginal$lat)
  #}
    cat('new dim of val: [', dim(orginal$val), '] lat: [', length(orginal$lat), ']\n')

  orgMid <- (orginal$lat[2:orginal$numLat]+orginal$lat[1:(orginal$numLat-1)])/2
  org.lat.min <- c(-90, orgMid)#orginal$lat-orginal$deltaLat/2
  org.lat.max <- c(orgMid, 90) #orginal$lat+orginal$deltaLat/2

  #org.lat.min <- orginal$lat-orginal$deltaLat/2
  #org.lat.max <- orginal$lat+orginal$deltaLat/2
  org.lon.min <- orginal$lon-deltaLon/2
  org.lon.max <- orginal$lon+deltaLon/2
  numOrg <- orginal$numLat * orginal$numLon

  if((org.lat.min[1] < minLat) || (org.lat.max[length(org.lat.max)] > maxLat)){
    cat(org.lat.min[1], '> ', minLat)
    cat('\n minLat:[', minLat, ']: test [', (org.lat.min[1] < minLat)
        , '] maxLat:[', maxLat, ']: test [', (org.lat.max[length(org.lat.max)] > maxLat), '] Trimming unsucessful. Exiting\n')
    return(NA)
  }else{
    cat('Trimming sucessful. Moving on. \n !!!!!\n')
  }
}

###move any lon grids which are below/above the minimum to the other side
cat('wrap lon boundries\n')
if(org.lon.min[1] < minLon){
  org.lon.min <- c(0, org.lon.min[2:length(org.lon.min)], org.lon.min[1]+maxLon)
  org.lon.max <- c(org.lon.max, maxLon)
  if(verbose){cat('lon dim [', dim(orginal$val), ']\n')}
  orginal$area <- rbind(orginal$area, orginal$area[1,])*
    (org.lon.max-org.lon.min)/deltaLon
  orginal$val <- rbind(orginal$val, orginal$val[1,])*
    (org.lon.max-org.lon.min)/deltaLon

  if(verbose){cat('Expanding lon min points to include:  [', min(org.lon.min), '] dimentions: , [', length(org.lon.min), ']\n')
  cat('data values are [', dim(orginal$val), ']\n')}
}

if(org.lon.max[length(org.lon.max)] > maxLon){
  cat('longitude is too high', org.lon.max[length(org.lon.max)],'>', maxLon,'\n')
  org.lon.max <- c(org.lon.max[length(org.lon.max)]-maxLon,
                   org.lon.max[1:(length(org.lon.max)-1)], maxLon)
  org.lon.min <- c(0, org.lon.min)
  orginal$area <- rbind(orginal$area[length(org.lon.max),] , orginal$val)*
    (org.lon.max-org.lon.min)/deltaLon
  orginal$val <- rbind(orginal$val[length(org.lon.max),] , orginal$val)*
    (org.lon.max-org.lon.min)/deltaLon

  if(verbose){cat('Expanding lon max points\n')}
}


cat('orginal lat min: [', head(org.lat.min, n=3),
            '...] max:[...', tail(org.lat.max, n=3),
    '] lon min: [', head(org.lon.min, n=3),
    '...] max:[...', tail(org.lon.max, n=3),']\n')

###################################################
##Regrid the values using weighted averaging
###################################################

#let the user know this will take a while
cat('projection centered at lat: [', head(projection$lat, n=3),
            '...', tail(projection$lat, n=3),
    '] lon : [', head(projection$lon, n=3),
    '...', tail(projection$lon, n=3),']\n')

cat('projecting [', dim(orginal$val), ']==[',
    dim(orginal$lon), ', ', dim(orginal$lat), '] to [',
    dim(projection$val), ']==[',
    dim(projection$lon), ', ', dim(projection$lat),']\n')
cat("regridding of ", projection$numLon," longitudes [takes a while]: ")

#Consider each grid cell in a for-loop
for(jj in 1:projection$numLon){
  cat( jj, " ")
  for(ii in 1:projection$numLat){
    ##############################################
    ##Calculate latitude intersections
    ##############################################
    pro.min <- pro.lat.min[ii]#projection$lat[ii] - projection$deltaLat/2
    pro.max <- pro.lat.max[ii]#projection$lat[ii] + projection$deltaLat/2

    ##Calcuate the length of the intersection between the orginal grid
    ##...and projected grid cell. Take the minimum of the max point in the
    ##...segment and subtract it from the maximum of the min point in the
    ##...segments.
    latInter <- pmin(org.lat.max, pro.max)-pmax(org.lat.min, pro.min)

    ##Normalize this to the size of the orginal grid to get a
    ##...percentage contribution of the orginal grid to the projected grid cell
    #latInter <- latInter/orginal$deltaLat
    latInter <- latInter/(org.lat.max-org.lat.min)


    ##Remove all negitive values which indicate non-intersecting segments
    latInter[latInter<0] <- 0

    ##############################################
    ##Calculate for the longitude
    ##############################################
    if(jj==1){
      pro.min <- 0
    }else{
      pro.min <- mean(projection$lon[(jj-1):jj])#projection$lon[jj] - projection$deltaLon/2
    }
    if(jj==projection$numLon){
      pro.max <- 360
    }else{
      pro.max <- mean(projection$lon[jj:(jj+1)])#projection$lon[jj] + projection$deltaLon/2
    }
    ##Dealing with longitude wrapping which was not an issue for latitude
    if(pro.min < 0 || pro.max > maxLon){
      cat('Projection can not wrap around 0!! Ending.\n')
      return(NA)
    }

    lonInter <- pmin(org.lon.max, pro.max)-pmax(org.lon.min, pro.min)

    #lonInter <- lonInter/orginal$deltaLon
    lonInter <- lonInter/(org.lon.max-org.lon.min)

    lonInter[lonInter<0] <- 0

    if(extremeVerbose){
      cat('dim val:', dim(orginal$val),
        ' dim of lonInter ', length(lonInter),
        ' dim(latInter) ', length(latInter),'\n')
      cat('\n')
      cat('orginal lat min: [', head(org.lat.min, n=3),
            '...] max:[...', tail(org.lat.max, n=3),
    '] lon min: [', head(org.lon.min, n=3),
    '...] max:[...', tail(org.lon.max, n=3),']\n')
      cat('pro lon max [',pro.max, ']\n')
      cat('pro lon min [',pro.min, ']\n')
      cat('lon maxs: ',pmin(org.lon.max, pro.max), 'lon mins: ',pmax(org.lon.min, pro.min),'\n')
      cat('lonInter: [', lonInter, ']\n')
    }
    ##############################################
    ##Pull the orginal grid cells which contribute
    ##to the projection
    ##############################################

    contributingGrids <- orginal$val[lonInter>0, latInter>0]
    #cat('dim area:[', dim(orginal$area),'] == [', length(lonInter), length(latInter), ']\n')
    areaGrids <- orginal$area[lonInter>0, latInter>0]

    #resize the grid since the grid structure was lost in the logical indexing
    #...(R is evil like that).
    dim(contributingGrids) <- c(length(lonInter[lonInter>0]),
                                length(latInter[latInter>0]) )
    dim(areaGrids) <- c(length(lonInter[lonInter>0]),
                                length(latInter[latInter>0]) )

    if(all(!is.finite(contributingGrids))){
      projection$val[jj, ii] <- NaN
    } else{
      contributingGrids[!is.finite(contributingGrids)] <- 0

    ##Calculate the projected grid value
    ##More accurate to sum the lat first instead of the lon
    projection$val[jj, ii] <-
      (t(contributingGrids%*%as.matrix(latInter[latInter>0]))
               %*%as.matrix(lonInter[lonInter>0]))
    }
    projection$area[jj, ii] <-
      (t(areaGrids%*%as.matrix(latInter[latInter>0]))
               %*%as.matrix(lonInter[lonInter>0]))

    if(extremeVerbose){
      cat(" ii: [", ii, "] jj: [", jj,"]\n")
      temp <- orginal$lat
      cat("--lat-- class(temp) [", class(temp),
          "], dim(temp), [", length(temp),"] temp [", temp, "]\n")
      temp <- projection$lat[ii]
      cat("--p.lat considering-- class(temp) [",
          class(temp), "] temp [", temp, "]\n")
      temp <- latInter
      cat("--latInter-- class(temp) [", class(temp), "] temp [", temp, "]\n")
      temp <- latInter
      cat("latInter class(temp) [", class(temp), "] dim(temp) [", length(temp),"] temp [",temp, "]\n")
      temp <- orginal$val
      cat("org class(temp) [", class(temp), "] dim(temp) [", dim(temp),"]\n")
      temp <- contributingGrids
      cat("pulled org: class(temp) [", class(temp),
        "] dim(temp) [", dim(temp),"]\n")
      cat("projected$val[",jj,",", ii,"]: [", projection$val[jj, ii], "]\n")
    }


    }
}

if(extremeVerbose){
  cat('orginal lat[', orginal$lat, ']\n')
  cat('orginal lon[', orginal$lon, ']\n')
  cat('orginal val[', orginal$val, ']\n')
}
if(verbose){
  cat('\n compare sums: [', sum(projection$val, na.rm=TRUE), '] =?= [',
      sum(orginal$val, na.rm=TRUE), ']\n') #orginal val has been normalized

}

cat('\n')
#save the projection to a file so we don't have to recalculate it
#save(projection, file=paste(saveFileName, 'RData', sep="."))
return(projection)

}#end function
