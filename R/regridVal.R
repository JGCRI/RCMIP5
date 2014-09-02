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
    
    ###################################################
    # Check for grid abnormalities in original data
    ###################################################
    
    results <- checkGridAbnormalities(x, x_lon, x_lat, x_area,
                                      y_lon, y_lat,
                                      minLat, maxLat, minLon, maxLon)
    x <- results$x   # ... and copy back (possibly changed) data
    x_lon <- results$x_lon
    x_lon_min <- results$x_lon_min
    x_lon_max <- results$x_lon_max
    x_lat <- results$x_lat
    x_lat_min <- results$x_lat_min
    x_lat_max <- results$x_lat_max
    x_area <- results$x_area
    
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
 
    yMid <- (y_lat[2:y_nlat] + y_lat[1:(y_nlat-1)]) / 2
    y_lat_min <- c(-90, yMid)
    y_lat_max <- c(yMid, 90)
    
    for(jj in 1:y_nlon) { # main loop
        cat( jj, " ")
        for(ii in 1:y_nlat) {
            ##############################################
            # Calculate latitude intersections
            ##############################################
            y_min <- y_lat_min[ii]#y_lat[ii] - y$deltaLat/2
            y_max <- y_lat_max[ii]#y_lat[ii] + y$deltaLat/2
            
            # Calcuate the length of the intersection between the x grid and
            # projected grid cell. Take the minimum of the max point in the
            # segment and subtract it from the maximum of the min point in the
            # segments.
            latInter <- pmin(x_lat_max, y_max)-pmax(x_lat_min, y_min)
            
            # Normalize this to the size of the x grid to get a
            # percentage contribution of the x grid to the projected grid cell
            latInter <- latInter/(x_lat_max-x_lat_min)
            
            # Remove all negitive values which indicate non-intersecting segments
            latInter[latInter<0] <- 0
            
            ##############################################
            # Calculate for the longitude
            ##############################################
            if(jj==1) {
                y_min <- 0
            } else {
                y_min <- mean(y_lon[(jj-1):jj])#y_lon[jj] - y$deltaLon/2
            }
            if(jj==y_nlon) {
                y_max <- 360
            } else {
                y_max <- mean(y_lon[jj:(jj+1)])#y_lon[jj] + y$deltaLon/2
            }
            # Dealing with longitude wrapping which was not an issue for latitude
            if(y_min < 0 || y_max > maxLon) {
                cat('y can not wrap around 0!! Ending.\n')
                return(NA)
            }
            
            lonInter <- pmin(x_lon_max, y_max)-pmax(x_lon_min, y_min)            
            lonInter <- lonInter/(x_lon_max-x_lon_min)
            lonInter[lonInter<0] <- 0
            
            if(extremeVerbose) {
                cat('dim x:', dim(x),
                    ' dim of lonInter ', length(lonInter),
                    ' dim(latInter) ', length(latInter),'\n')
                cat('\n')
                cat('x lat min: [', head(x_lat_min, n=3),
                    '...] max:[...', tail(x_lat_max, n=3),
                    '] lon min: [', head(x_lon_min, n=3),
                    '...] max:[...', tail(x_lon_max, n=3),']\n')
                cat('y lon max [',y_max, ']\n')
                cat('y lon min [',y_min, ']\n')
                cat('lon maxs: ',pmin(x_lon_max, y_max), 'lon mins: ',pmax(x_lon_min, y_min),'\n')
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
        } # for ii
    } # for jj
    
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


#' Check for grid abnormalities.
#' 
#' Check for grid abnormalities. TODO - describe more.
#'
#' @param x an array containing original data; rows=lon and columns=lat
#' @param x_area a matrix, same dimensions as x, containing area data
#' @param y_area a matrix of the new (projected) dimensions, containing area data
#' @param verbose logical. Print info as we go?
#' @return TODO
#' @note This is an internal RCMIP5 function and not exported.
checkGridAbnormalities <- function(x, x_lon, x_lat, x_area,
                                   y_lon, y_lat, 
                                   minLat, maxLat, minLon, maxLon) {

    y_nlat <- length(y_lat)
    y_nlon <- length(y_lon)
    x_nlat <- length(x_lat)
    x_nlon <- length(x_lon)
        
    # Pull the boundries of the grid cells
    xMid <- (x_lat[2:x_nlat]+x_lat[1:(x_nlat-1)])/2
    x_lat_min <- c(-90, xMid) #x_lat-x$deltaLat/2
    x_lat_max <- c(xMid, 90) #x_lat+x$deltaLat/2
    deltaLon <- unique(x_lon[2:x_nlon] - x_lon[1:(x_nlon-1)])
    
    if(max(deltaLon)-min(deltaLon) > 1e-8) {
        print(max(deltaLon) - min(deltaLon))
        warning('ERROR: non uniform longitude... returning NA.\n')
        return(NA)
    } else {
        deltaLon <- signif(deltaLon[1], 8) # make the array a scalar... hackish I know
    }
    
    x_lon_min <- x_lon-deltaLon/2
    x_lon_max <- x_lon+deltaLon/2
    cat('x lat min: [', head(x_lat_min, n=3),
        '...] max:[', tail(x_lat_max, n=3),
        '] lon min: [', head(x_lon_min, n=3),
        '] max:[', tail(x_lon_max, n=3),']\n')
    
    # Check to see if these boundries fall over the min/max
    cat('check lat boundry\n')
    
    if((x_lat_min[1] < minLat) | (x_lat_max[length(x_lat_max)] > maxLat)) {
        
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
        x_lat_min <- c(-90, xMid)#x_lat-x$deltaLat/2
        x_lat_max <- c(xMid, 90) #x_lat+x$deltaLat/2
        
        #org.lat.min <- x_lat-x$deltaLat/2
        #org.lat.max <- x_lat+x$deltaLat/2
        x_lon_min <- x_lon-deltaLon/2
        x_lon_max <- x_lon+deltaLon/2
        
        if((x_lat_min[1] < minLat) || (x_lat_max[length(x_lat_max)] > maxLat)){
            cat(x_lat_min[1], '> ', minLat)
            cat('\n minLat:[', minLat, ']: test [', (x_lat_min[1] < minLat)
                , '] maxLat:[', maxLat, ']: test [', (x_lat_max[length(x_lat_max)] > maxLat), '] Trimming unsucessful. Exiting\n')
            return(NA)
        } else {
            cat('Trimming sucessful. Moving on. \n !!!!!\n')
        }
    } # if
    
    # Move any lon grids which are below/above the minimum to the other side
    cat('wrap lon boundries\n')
    if(x_lon_min[1] < minLon) {
        x_lon_min <- c(0, x_lon_min[2:length(x_lon_min)], x_lon_min[1]+maxLon)
        x_lon_max <- c(x_lon_max, maxLon)
        x_area <- rbind(x_area, x_area[1,])*
            (x_lon_max-x_lon_min)/deltaLon
        x <- rbind(x, x[1,])*
            (x_lon_max-x_lon_min)/deltaLon
        
        if(verbose) cat('Expanding lon min points to include: [', min(x_lon_min), 
                        '] dimensions: , [', length(x_lon_min), ']\n')
        cat('data values are [', dim(x), ']\n')
    } # if
    
    if(x_lon_max[length(x_lon_max)] > maxLon) {
        cat('longitude is too high', x_lon_max[length(x_lon_max)],'>', maxLon,'\n')
        x_lon_max <- c(x_lon_max[length(x_lon_max)]-maxLon,
                       x_lon_max[1:(length(x_lon_max)-1)], maxLon)
        x_lon_min <- c(0, x_lon_min)
        x_area <- rbind(x_area[length(x_lon_max),] , x) * (x_lon_max-x_lon_min)/deltaLon
        x <- rbind(x[length(x_lon_max),] , x) * (x_lon_max-x_lon_min)/deltaLon
        
        if(verbose) cat('Expanding lon max points\n')
    }
    
    cat('x lat min: [', head(x_lat_min, n=3),
        '...] max:[...', tail(x_lat_max, n=3),
        '] lon min: [', head(x_lon_min, n=3),
        '...] max:[...', tail(x_lon_max, n=3),']\n')
    
    return(list(x=x, x_lon=x_lon, x_lon_min=x_lon_min, x_lon_max=x_lon_max,
                x_lat=x_lat, x_lat_min=x_lat_min, x_lat_max=x_lat_max,
                x_area=x_area))
}
