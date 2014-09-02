#' Regrid data.
#' 
#' This function regrids global data (in x) to a new grid (defined in y_area).
#' Both grids must be center defined (each lat x lon marks the center of the grid).
#' This function does not deal with wrapping boundary conditions! One key assumption
#' in this regridding is that a degree is proportional to distance for neighboring
#' grid cells.
#'
#' @param x an array containing original data; rows=lon and columns=lat
#' @param x_area a matrix, same dimensions as x, containing area data
#' @param y_area a matrix of the new (projected) dimensions, containing area data
#' @param verbose logical. Print info as we go?
#' @param parallel logical. Parallelize if possible?
#' @return The data regridded onto the new spatial grid.
#' @details This function goes through each y point, finds the
# percent contribution from the x grid using the intersection
# length of the lat/log grid segments, uses this to find the weighted
# sum of the x data, and finally corrects for the change in grid size.
#' @note This is an internal RCMIP5 function and not exported.
regridVal <- function(x, x_lon, x_lat, x_area, 
                      y_lon, y_lat, y_area,
                      verbose=FALSE, parallel=FALSE,
                      maxLon=360, minLon=0, maxLat=90, minLat=-90,
                      extremeVerbose=FALSE) {
    cat("**** Entering regridVal\n")

    # Sanity checks - parameter classes and lengths
    stopifnot(class(x)=="array")
    stopifnot(class(x_lon) == "numeric" & class(x_lat) == "numeric")
    stopifnot(class(x_area)=="matrix")
    stopifnot(class(y_lon) == "numeric" & class(y_lat) == "numeric")
    stopifnot(class(y_area)=="matrix")
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(parallel)==1 & is.logical(parallel))
    
    # Sanity checks - data is consistent w.r.t. lon, lat, and dimensions
    stopifnot(length(x_lon) == nrow(x_area) & length(x_lat) == ncol(x_area))
    stopifnot(length(y_lon) == nrow(y_area) & length(y_lat) == ncol(y_area))
    stopifnot(all(dim(x)[1:2] == dim(x_area)))
    
    # Allocate space for the regridded data
    y_nlat <- length(y_lat)
    y_nlon <- length(y_lon)
    x_nlat <- length(x_lat)
    x_nlon <- length(x_lon)
    y <- matrix(NA_real_, nrow=y_nlon, ncol=y_nlat)
    
    if(verbose) {
        cat('Regridding [', dim(x), '] to [', dim(y), ']\n')   
    }
    
    if(y_nlat == x_nlat & y_nlon == x_nlon) {
        cat("No change in grid size\n")
        return(x)
    }
    
    ####################################################
    # Fix gridding abnomalities in original grid
    ####################################################
    
    yMid <- (y_lat[2:y_nlat] + y_lat[1:(y_nlat-1)]) / 2
    y.lat.min <- c(-90, yMid)
    y.lat.max <- c(yMid, 90)
    
    # Pull the boundries of the grid cells
    xMid <- (x_lat[2:x_nlat]+x_lat[1:(x_nlat-1)])/2
    x.lat.min <- c(-90, xMid) #x_lat-x$deltaLat/2
    x.lat.max <- c(xMid, 90) #x_lat+x$deltaLat/2
    deltaLon <- unique(x_lon[2:x_nlon] - x_lon[1:(x_nlon-1)])
    
    if(max(deltaLon)-min(deltaLon) > 1e-8) {
        print(max(deltaLon) - min(deltaLon))
        warning('ERROR: non uniform longitude... returning NA.\n')
        return(NA)
    } else {
        deltaLon <- signif(deltaLon[1], 8) # make the array a scalar... hackish I know
    }
    
    x.lon.min <- x_lon-deltaLon/2
    x.lon.max <- x_lon+deltaLon/2
    cat('x lat min: [', head(x.lat.min, n=3),
        '...] max:[', tail(x.lat.max, n=3),
        '] lon min: [', head(x.lon.min, n=3),
        '] max:[', tail(x.lon.max, n=3),']\n')
    
    # Check to see if these boundries fall over the min/max
    cat('check lat boundry\n')
    
    if((x.lat.min[1] < minLat) | (x.lat.max[length(x.lat.max)] > maxLat)) {
        
        cat('!!!!!!!! \n Error! Wrapping latitude grid not valid. Attempting to salvage by trimming top and bottom of NA or 0 values. \n')
        cat('old dim of val: [', dim(x), '] lat: [', length(x_lat), ']\n')
        
        cat('Trim max lat values\n')
        x_area <- x_area[,1:(x_nlat-1)]
        x <- x[,1:(x_nlat-1)]
        x_lat <- x_lat[1:(x_nlat-1)]
        x_nlat <- length(x_lat)
        
        #if(all(is.na(x[,1])) | sum(x[,1]) == 0){
        cat('Trim min lat values\n')
        x_area <- x_area[,2:x_nlat]
        x <- x[,2:x_nlat]
        x_lat <- x_lat[2:x_nlat]
        x_nlat <- length(x_lat)
        #}
        cat('new dim of val: [', dim(x), '] lat: [', length(x_lat), ']\n')
        
        xMid <- (x_lat[2:x_nlat]+x_lat[1:(x_nlat-1)])/2
        x.lat.min <- c(-90, xMid)#x_lat-x$deltaLat/2
        x.lat.max <- c(xMid, 90) #x_lat+x$deltaLat/2
        
        #org.lat.min <- x_lat-x$deltaLat/2
        #org.lat.max <- x_lat+x$deltaLat/2
        x.lon.min <- x_lon-deltaLon/2
        x.lon.max <- x_lon+deltaLon/2
        
        if((x.lat.min[1] < minLat) || (x.lat.max[length(x.lat.max)] > maxLat)){
            cat(x.lat.min[1], '> ', minLat)
            cat('\n minLat:[', minLat, ']: test [', (x.lat.min[1] < minLat)
                , '] maxLat:[', maxLat, ']: test [', (x.lat.max[length(x.lat.max)] > maxLat), '] Trimming unsucessful. Exiting\n')
            return(NA)
        } else {
            cat('Trimming sucessful. Moving on. \n !!!!!\n')
        }
    } # if
    
    # Move any lon grids which are below/above the minimum to the other side
    cat('wrap lon boundries\n')
    if(x.lon.min[1] < minLon) {
        x.lon.min <- c(0, x.lon.min[2:length(x.lon.min)], x.lon.min[1]+maxLon)
        x.lon.max <- c(x.lon.max, maxLon)
        x_area <- rbind(x_area, x_area[1,])*
            (x.lon.max-x.lon.min)/deltaLon
        x <- rbind(x, x[1,])*
            (x.lon.max-x.lon.min)/deltaLon
        
        if(verbose) cat('Expanding lon min points to include: [', min(x.lon.min), 
                        '] dimentions: , [', length(x.lon.min), ']\n')
        cat('data values are [', dim(x), ']\n')
    } # if
    
    if(x.lon.max[length(x.lon.max)] > maxLon) {
        cat('longitude is too high', x.lon.max[length(x.lon.max)],'>', maxLon,'\n')
        x.lon.max <- c(x.lon.max[length(x.lon.max)]-maxLon,
                         x.lon.max[1:(length(x.lon.max)-1)], maxLon)
        x.lon.min <- c(0, x.lon.min)
        x_area <- rbind(x_area[length(x.lon.max),] , x)*
            (x.lon.max-x.lon.min)/deltaLon
        x <- rbind(x[length(x.lon.max),] , x)*
            (x.lon.max-x.lon.min)/deltaLon
        
        if(verbose) cat('Expanding lon max points\n')
    }
       
    cat('x lat min: [', head(x.lat.min, n=3),
        '...] max:[...', tail(x.lat.max, n=3),
        '] lon min: [', head(x.lon.min, n=3),
        '...] max:[...', tail(x.lon.max, n=3),']\n')
    
    ###################################################
    # Regrid the values using weighted averaging
    ###################################################
    
    # Let the user know this will take a while
    cat('y centered at lat: [', head(y_lat, n=3),
        '...', tail(y_lat, n=3),
        '] lon : [', head(y_lon, n=3),
        '...', tail(y_lon, n=3),']\n')
    
    cat('projecting [', dim(x), ']==[',
        dim(x_lon), ', ', dim(x_lat), '] to [',
        dim(x), ']==[',
        dim(y_lon), ', ', dim(y_lat),']\n')
    cat("regridding of ", y_nlon," longitudes [takes a while]: ")
    
    # Parallel processing uses the foreach and doParallel packages, if available
    if(parallel) parallel <- require(foreach) & require(doParallel)
    # parallel TODO
    
    stop('ok')
    
    # Consider each grid cell in a for-loop
    for(jj in 1:y_nlon) {
        cat( jj, " ")
        for(ii in 1:y_nlat) {
            ##############################################
            # Calculate latitude intersections
            ##############################################
            y.min <- y.lat.min[ii]#y_lat[ii] - y$deltaLat/2
            y.max <- y.lat.max[ii]#y_lat[ii] + y$deltaLat/2
            
            # Calcuate the length of the intersection between the x grid and
            # projected grid cell. Take the minimum of the max point in the
            # segment and subtract it from the maximum of the min point in the
            # segments.
            latInter <- pmin(x.lat.max, y.max)-pmax(x.lat.min, y.min)
            
            # Normalize this to the size of the x grid to get a
            # percentage contribution of the x grid to the projected grid cell
            latInter <- latInter/(x.lat.max-x.lat.min)
            
            # Remove all negitive values which indicate non-intersecting segments
            latInter[latInter<0] <- 0
            
            ##############################################
            # Calculate for the longitude
            ##############################################
            if(jj==1) {
                y.min <- 0
            } else {
                y.min <- mean(y_lon[(jj-1):jj])#y_lon[jj] - y$deltaLon/2
            }
            if(jj==y_nlon) {
                y.max <- 360
            } else {
                y.max <- mean(y_lon[jj:(jj+1)])#y_lon[jj] + y$deltaLon/2
            }
            # Dealing with longitude wrapping which was not an issue for latitude
            if(y.min < 0 || y.max > maxLon) {
                cat('y can not wrap around 0!! Ending.\n')
                return(NA)
            }
            
            lonInter <- pmin(x.lon.max, y.max)-pmax(x.lon.min, y.min)            
            lonInter <- lonInter/(x.lon.max-x.lon.min)
            lonInter[lonInter<0] <- 0
            
            if(extremeVerbose) {
                cat('dim x:', dim(x),
                    ' dim of lonInter ', length(lonInter),
                    ' dim(latInter) ', length(latInter),'\n')
                cat('\n')
                cat('x lat min: [', head(x.lat.min, n=3),
                    '...] max:[...', tail(x.lat.max, n=3),
                    '] lon min: [', head(x.lon.min, n=3),
                    '...] max:[...', tail(x.lon.max, n=3),']\n')
                cat('y lon max [',y.max, ']\n')
                cat('y lon min [',y.min, ']\n')
                cat('lon maxs: ',pmin(x.lon.max, y.max), 'lon mins: ',pmax(x.lon.min, y.min),'\n')
                cat('lonInter: [', lonInter, ']\n')
            }
            
            ##############################################
            # Pull the x grid cells which contribute to y
            ##############################################
            
            contributingGrids <- x[lonInter>0, latInter>0]
            areaGrids <- x_area[lonInter>0, latInter>0]
            
            # Resize the grid since the grid structure was lost in the logical indexing
            #...(R is evil like that).
            dim(contributingGrids) <- c(length(lonInter[lonInter>0]),
                                        length(latInter[latInter>0]) )
            dim(areaGrids) <- c(length(lonInter[lonInter>0]),
                                length(latInter[latInter>0]) )
            
            if(all(!is.finite(contributingGrids))) {
                y[jj, ii] <- NaN
            } else {
                contributingGrids[!is.finite(contributingGrids)] <- 0
                
                # Calculate the projected grid value
                # More accurate to sum the lat first instead of the lon
                y[jj, ii] <-
                    (t(contributingGrids%*%as.matrix(latInter[latInter>0]))
                     %*%as.matrix(lonInter[lonInter>0]))
            }
            y_area[jj, ii] <-
                (t(areaGrids%*%as.matrix(latInter[latInter>0]))
                 %*%as.matrix(lonInter[lonInter>0]))
            
            if(extremeVerbose) {
                cat(" ii: [", ii, "] jj: [", jj,"]\n")
                temp <- x_lat
                cat("--lat-- class(temp) [", class(temp),
                    "], dim(temp), [", length(temp),"] temp [", temp, "]\n")
                temp <- y_lat[ii]
                cat("--p.lat considering-- class(temp) [",
                    class(temp), "] temp [", temp, "]\n")
                temp <- latInter
                cat("--latInter-- class(temp) [", class(temp), "] temp [", temp, "]\n")
                temp <- latInter
                cat("latInter class(temp) [", class(temp), "] dim(temp) [", length(temp),"] temp [",temp, "]\n")
                temp <- x
                cat("x class(temp) [", class(temp), "] dim(temp) [", dim(temp),"]\n")
                temp <- contributingGrids
                cat("pulled x: class(temp) [", class(temp),
                    "] dim(temp) [", dim(temp),"]\n")
                cat("projected$val[",jj,",", ii,"]: [", y[jj, ii], "]\n")
            } # if(extremeVerbose)
        }
    }
    
    if(extremeVerbose) {
        cat('x lat[', x_lat, ']\n')
        cat('x lon[', x_lon, ']\n')
        cat('x val[', x, ']\n')
    }
    if(verbose) {
        cat('\n compare sums: [', sum(y, na.rm=TRUE), '] =?= [',
            sum(x, na.rm=TRUE), ']\n') # x val has been normalized
    }
    
    y    
} # regridVal


checkGridAbnormalities <- function(x) {
    
}