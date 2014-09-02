#' Regrid data.
#' 
#' This function regrids global data (in x) to a new grid (defined in y).
#' Both grids must be center defined (each lat x lon marks the center of the grid).
#' This function does not deal with wrapping boundary conditions! One key assumption
#' in this regridding is that a degree is proportional to distance for neighboring
#' grid cells.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object containing original data.
#' @param y cmip5data A cmip5data object with new grid for projection.
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @return The data regridded onto the new spatial grid.
#' @details This function goes through each y point, finds the
# percent contribution from the x grid using the intersection
# length of the lat/log grid segments, uses this to find the weighted
# sum of the x data, and finally corrects for the change in grid size.
#' @note This is an internal RCMIP5 function and not exported.
regridVal <- function(x, y, verbose=FALSE, parallel=FALSE,
                      maxLon=360, minLon=0, maxLat=90, minLat=-90,
                      extremeVerbose=FALSE) {
    cat("**** Entering regridVal\n")
    
    # Sanity checks - parameter classes and lengths
    stopifnot(class(x)=="cmip5data" & class(y)=="cmip5data")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    
    # Allocate space for the regridded data
    y_nlat <- length(y$lat)
    y_nlon <- length(y$lon)
    x_nlat <- length(x$lat)
    x_nlon <- length(x$lon)
    y$val <- matrix(NA_real_, nrow=y_nlon, ncol=y_nlat)
    
    if(verbose) cat('Regridding [', dim(x$val), ']==[', x_nlon, 
                    ',', x_nlat, '] to [', dim(y$val), ']==[',
                    y_nlon, ',', y_nlat,']\n')        
    
    if(y_nlat == x_nlat & y_nlon == x_nlon) {
        cat("No change in grid size\n")
        return(x)
    }
    
    ####################################################
    # Fix gridding abnomalities in original grid
    ####################################################
    
    proMid <- (y$lat[2:y_nlat]+y$lat[1:(y_nlat-1)])/2
    pro.lat.min <- c(-90, proMid)
    pro.lat.max <- c(proMid, 90)
    
    # Pull the boundries of the grid cells
    orgMid <- (x$lat[2:x_nlat]+x$lat[1:(x_nlat-1)])/2
    org.lat.min <- c(-90, orgMid)#x$lat-x$deltaLat/2
    org.lat.max <- c(orgMid, 90) #x$lat+x$deltaLat/2
    deltaLon <- unique(x$lon[2:x_nlon]-
                           x$lon[1:(x_nlon-1)])
    
    if(max(deltaLon)-min(deltaLon) > 1e-8) {
        print(max(deltaLon)-min(deltaLon))
        cat('ERROR: non uniform longitude... returning NA.\n')
    } else {
        deltaLon <- signif(deltaLon[1],8) # make the array a scalar... hackish I know
    }
    
    org.lon.min <- x$lon-deltaLon/2
    org.lon.max <- x$lon+deltaLon/2
    cat('x lat min: [', head(org.lat.min, n=3),
        '...] max:[', tail(org.lat.max, n=3),
        '] lon min: [', head(org.lon.min, n=3),
        '] max:[', tail(org.lon.max, n=3),']\n')
    
    # Check to see if these boundries fall over the min/max
    cat('check lat boundry\n')
    
    if((org.lat.min[1] < minLat) | (org.lat.max[length(org.lat.max)] > maxLat)) {
        
        cat('!!!!!!!! \n Error! Wrapping latitude grid not valid. Attempting to salvage by trimming top and bottom of NA or 0 values. \n')
        cat('old dim of val: [', dim(x$val), '] lat: [', length(x$lat), ']\n')
        
        
        cat('Trim max lat values\n')
        x$area <- x$area[,1:(x_nlat-1)]
        x$val <- x$val[,1:(x_nlat-1)]
        x$lat <- x$lat[1:(x_nlat-1)]
        x_nlat <- length(x$lat)
        
        #if(all(is.na(x$val[,1])) | sum(x$val[,1]) == 0){
        cat('Trim min lat values\n')
        x$area <- x$area[,2:x_nlat]
        x$val <- x$val[,2:x_nlat]
        x$lat <- x$lat[2:x_nlat]
        x_nlat <- length(x$lat)
        #}
        cat('new dim of val: [', dim(x$val), '] lat: [', length(x$lat), ']\n')
        
        orgMid <- (x$lat[2:x_nlat]+x$lat[1:(x_nlat-1)])/2
        org.lat.min <- c(-90, orgMid)#x$lat-x$deltaLat/2
        org.lat.max <- c(orgMid, 90) #x$lat+x$deltaLat/2
        
        #org.lat.min <- x$lat-x$deltaLat/2
        #org.lat.max <- x$lat+x$deltaLat/2
        org.lon.min <- x$lon-deltaLon/2
        org.lon.max <- x$lon+deltaLon/2
        
        if((org.lat.min[1] < minLat) || (org.lat.max[length(org.lat.max)] > maxLat)){
            cat(org.lat.min[1], '> ', minLat)
            cat('\n minLat:[', minLat, ']: test [', (org.lat.min[1] < minLat)
                , '] maxLat:[', maxLat, ']: test [', (org.lat.max[length(org.lat.max)] > maxLat), '] Trimming unsucessful. Exiting\n')
            return(NA)
        } else {
            cat('Trimming sucessful. Moving on. \n !!!!!\n')
        }
    } # if
    
    # Move any lon grids which are below/above the minimum to the other side
    cat('wrap lon boundries\n')
    if(org.lon.min[1] < minLon) {
        org.lon.min <- c(0, org.lon.min[2:length(org.lon.min)], org.lon.min[1]+maxLon)
        org.lon.max <- c(org.lon.max, maxLon)
        if(verbose){cat('lon dim [', dim(x$val), ']\n')}
        x$area <- rbind(x$area, x$area[1,])*
            (org.lon.max-org.lon.min)/deltaLon
        x$val <- rbind(x$val, x$val[1,])*
            (org.lon.max-org.lon.min)/deltaLon
        
        if(verbose) cat('Expanding lon min points to include: [', min(org.lon.min), 
                        '] dimentions: , [', length(org.lon.min), ']\n')
        cat('data values are [', dim(x$val), ']\n')
    } # if
    
    if(org.lon.max[length(org.lon.max)] > maxLon) {
        cat('longitude is too high', org.lon.max[length(org.lon.max)],'>', maxLon,'\n')
        org.lon.max <- c(org.lon.max[length(org.lon.max)]-maxLon,
                         org.lon.max[1:(length(org.lon.max)-1)], maxLon)
        org.lon.min <- c(0, org.lon.min)
        x$area <- rbind(x$area[length(org.lon.max),] , x$val)*
            (org.lon.max-org.lon.min)/deltaLon
        x$val <- rbind(x$val[length(org.lon.max),] , x$val)*
            (org.lon.max-org.lon.min)/deltaLon
        
        if(verbose){cat('Expanding lon max points\n')}
    }
    
    
    cat('x lat min: [', head(org.lat.min, n=3),
        '...] max:[...', tail(org.lat.max, n=3),
        '] lon min: [', head(org.lon.min, n=3),
        '...] max:[...', tail(org.lon.max, n=3),']\n')
    
    ###################################################
    # Regrid the values using weighted averaging
    ###################################################
    
    #let the user know this will take a while
    cat('y centered at lat: [', head(y$lat, n=3),
        '...', tail(y$lat, n=3),
        '] lon : [', head(y$lon, n=3),
        '...', tail(y$lon, n=3),']\n')
    
    cat('projecting [', dim(x$val), ']==[',
        dim(x$lon), ', ', dim(x$lat), '] to [',
        dim(y$val), ']==[',
        dim(y$lon), ', ', dim(y$lat),']\n')
    cat("regridding of ", y_nlon," longitudes [takes a while]: ")

    # Parallel processing uses the foreach and doParallel packages, if available
    if(parallel) parallel <- require(foreach) & require(doParallel)
    # parallel TODO
    
    # Consider each grid cell in a for-loop
    for(jj in 1:y_nlon) {
        cat( jj, " ")
        for(ii in 1:y_nlat) {
            ##############################################
            # Calculate latitude intersections
            ##############################################
            pro.min <- pro.lat.min[ii]#y$lat[ii] - y$deltaLat/2
            pro.max <- pro.lat.max[ii]#y$lat[ii] + y$deltaLat/2
            
            # Calcuate the length of the intersection between the x grid and
            # projected grid cell. Take the minimum of the max point in the
            # segment and subtract it from the maximum of the min point in the
            # segments.
            latInter <- pmin(org.lat.max, pro.max)-pmax(org.lat.min, pro.min)
            
            # Normalize this to the size of the x grid to get a
            # percentage contribution of the x grid to the projected grid cell
            latInter <- latInter/(org.lat.max-org.lat.min)
            
            # Remove all negitive values which indicate non-intersecting segments
            latInter[latInter<0] <- 0
            
            ##############################################
            # Calculate for the longitude
            ##############################################
            if(jj==1) {
                pro.min <- 0
            } else {
                pro.min <- mean(y$lon[(jj-1):jj])#y$lon[jj] - y$deltaLon/2
            }
            if(jj==y_nlon) {
                pro.max <- 360
            } else {
                pro.max <- mean(y$lon[jj:(jj+1)])#y$lon[jj] + y$deltaLon/2
            }
            # Dealing with longitude wrapping which was not an issue for latitude
            if(pro.min < 0 || pro.max > maxLon) {
                cat('y can not wrap around 0!! Ending.\n')
                return(NA)
            }
            
            lonInter <- pmin(org.lon.max, pro.max)-pmax(org.lon.min, pro.min)            
            lonInter <- lonInter/(org.lon.max-org.lon.min)
            lonInter[lonInter<0] <- 0
            
            if(extremeVerbose) {
                cat('dim val:', dim(x$val),
                    ' dim of lonInter ', length(lonInter),
                    ' dim(latInter) ', length(latInter),'\n')
                cat('\n')
                cat('x lat min: [', head(org.lat.min, n=3),
                    '...] max:[...', tail(org.lat.max, n=3),
                    '] lon min: [', head(org.lon.min, n=3),
                    '...] max:[...', tail(org.lon.max, n=3),']\n')
                cat('pro lon max [',pro.max, ']\n')
                cat('pro lon min [',pro.min, ']\n')
                cat('lon maxs: ',pmin(org.lon.max, pro.max), 'lon mins: ',pmax(org.lon.min, pro.min),'\n')
                cat('lonInter: [', lonInter, ']\n')
            }
            
            ##############################################
            # Pull the x grid cells which contribute to y
            ##############################################
            
            contributingGrids <- x$val[lonInter>0, latInter>0]
            areaGrids <- x$area[lonInter>0, latInter>0]
            
            # Resize the grid since the grid structure was lost in the logical indexing
            #...(R is evil like that).
            dim(contributingGrids) <- c(length(lonInter[lonInter>0]),
                                        length(latInter[latInter>0]) )
            dim(areaGrids) <- c(length(lonInter[lonInter>0]),
                                length(latInter[latInter>0]) )
            
            if(all(!is.finite(contributingGrids))) {
                y$val[jj, ii] <- NaN
            } else {
                contributingGrids[!is.finite(contributingGrids)] <- 0
                
                # Calculate the projected grid value
                # More accurate to sum the lat first instead of the lon
                y$val[jj, ii] <-
                    (t(contributingGrids%*%as.matrix(latInter[latInter>0]))
                     %*%as.matrix(lonInter[lonInter>0]))
            }
            y$area[jj, ii] <-
                (t(areaGrids%*%as.matrix(latInter[latInter>0]))
                 %*%as.matrix(lonInter[lonInter>0]))
            
            if(extremeVerbose) {
                cat(" ii: [", ii, "] jj: [", jj,"]\n")
                temp <- x$lat
                cat("--lat-- class(temp) [", class(temp),
                    "], dim(temp), [", length(temp),"] temp [", temp, "]\n")
                temp <- y$lat[ii]
                cat("--p.lat considering-- class(temp) [",
                    class(temp), "] temp [", temp, "]\n")
                temp <- latInter
                cat("--latInter-- class(temp) [", class(temp), "] temp [", temp, "]\n")
                temp <- latInter
                cat("latInter class(temp) [", class(temp), "] dim(temp) [", length(temp),"] temp [",temp, "]\n")
                temp <- x$val
                cat("org class(temp) [", class(temp), "] dim(temp) [", dim(temp),"]\n")
                temp <- contributingGrids
                cat("pulled org: class(temp) [", class(temp),
                    "] dim(temp) [", dim(temp),"]\n")
                cat("projected$val[",jj,",", ii,"]: [", y$val[jj, ii], "]\n")
            } # if(extremeVerbose)
        }
    }
    
    if(extremeVerbose) {
        cat('x lat[', x$lat, ']\n')
        cat('x lon[', x$lon, ']\n')
        cat('x val[', x$val, ']\n')
    }
    if(verbose) {
        cat('\n compare sums: [', sum(y$val, na.rm=TRUE), '] =?= [',
            sum(x$val, na.rm=TRUE), ']\n') #x val has been normalized
    }
    
    y    
} # regridVal
