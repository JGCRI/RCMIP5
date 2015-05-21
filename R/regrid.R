#' Calculate projection matrix to translate one grid to another
#' 
#' The bulk of the computational time for regridding is in calculating the 
#' area-weighted projection matrix. This functions allows you to pre-calculate the 
#' projection matrix to speed up regridding.
#' 
#' @param orgArea A \code{\link{cmip5data}} object or list with lat (latitude) and log (longitude) matrices
#' of the orginal grid
#' @param projArea A \code{\link{cmip5data}} object or list with lat (latitude) and log (longitude) matrices
#' of the projection grid
#' @details This function calculates the projection matrix to shift one global grid to a second.
#' The relative contribution of an old grid to the new grid is calculated via an area weighting scheme
#' where the area of a grid cell is assumed to be proportional to the degree area of that cell and
#' neighboring cells are assumed to have the same area to degree ratios. This will NOT hold in large grids.
#' Nor is the area weighting scheme appropriate for all variable types and grid shifts. Use with caution.
#' @examples
#' numOrgLon <- 3
#' numOrgLat <- 3
#' orgLon <- matrix(seq(0, 360-360/numOrgLon, by=360/numOrgLon) + 360/numOrgLon/2, nrow=numOrgLon, ncol=numOrgLat)
#' orgLat <- matrix(seq(-90, 90-180/numOrgLat, by=180/numOrgLat) + 180/numOrgLat/2, nrow=numOrgLon, ncol=numOrgLat, byrow=TRUE)
#' orgArea <- list(lon = orgLon, lat=orgLat)
#' 
#' numProjLon <- 2
#' numProjLat <- 2
#' projLon <- matrix(seq(0, 360-360/numProjLon, by=360/numProjLon) + 360/numProjLon/2, nrow=numProjLon, ncol=numProjLat)
#' projLat <- matrix(seq(-90, 90-180/numProjLon, by=180/numProjLon) + 180/numProjLon/2, nrow=numProjLon, ncol=numProjLat, byrow=TRUE)
#' projArea <- list(lon = projLon, lat=projLat)
#' 
#' transferMatrix <- getProjectionMatrix(orgArea = orgArea, projArea=projArea)
#' @seealso \code{\link{regrid}}
#' @export
getProjectionMatrix <- function(orgArea, projArea) {
    
    assert_that(all(!apply(orgArea$lon, c(2), is.unsorted)), all(!apply(orgArea$lat, c(1), is.unsorted)))
    assert_that(all(!apply(projArea$lon, c(2), is.unsorted)), all(!apply(projArea$lat, c(1), is.unsorted)))
    
    extractBounds <- function(lat, lon) {
        ans <- list(lat=lat, lon=lon)
        ans$maxLat <- cbind((lat[,(2:dim(lat)[2])] + lat[,(2:dim(lat)[2])-1])/2, 90)
        ans$minLat <- cbind(-90, (lat[,(2:dim(lat)[2])] + lat[,(2:dim(lat)[2])-1])/2)
        ans$maxLon <- rbind((lon[(2:dim(lon)[1]), ] + lon[(2:dim(lon)[1])-1, ])/2, 
                            lon[dim(lon)[1], ]+(360-lon[dim(lon)[1], ] + lon[1,])/2)
        ans$minLon <- rbind(lon[1, ]-(360-lon[dim(lon)[1], ] + lon[1,])/2,
                            (lon[(2:dim(lon)[1]), ] + lon[(2:dim(lon)[1])-1, ])/2)
        assert_that(all(ans$maxLat >= ans$lat), all(ans$minLat <= ans$lat), 
                    all(ans$maxLon >= ans$lon), all(ans$minLon <= ans$lon))
        return(ans)
    }
    
    orgEnds <- extractBounds(lat=orgArea$lat, lon=orgArea$lon)
    projEnds <- extractBounds(lat=projArea$lat, lon=projArea$lon)
    
    lodf <- list()  # List Of Data Frames (initially empty)
    dimprod <- prod(dim(projArea$lat))
    pb <- txtProgressBar(min = 1, max = dimprod, style = 3) # for sanity
#    tf <- tempfile()
#    first <- TRUE
    for(projIndex in seq_len(dimprod)) {
        latOverlap <- (pmin.int(projEnds$maxLat[projIndex], orgEnds$maxLat[TRUE]) - 
                           pmax.int(projEnds$minLat[projIndex], orgEnds$minLat[TRUE])) /
            (orgEnds$maxLat[TRUE]-orgEnds$minLat[TRUE])
        latOverlap[latOverlap < 0] <- 0
        
        lonOverlap <- (pmin.int(projEnds$maxLon[projIndex], orgEnds$maxLon[TRUE]) - 
                           pmax.int(projEnds$minLon[projIndex], orgEnds$minLon[TRUE])) /
            (orgEnds$maxLon[TRUE]-orgEnds$minLon[TRUE])
        lonOverlap[lonOverlap < 0] <- 0
        
        areaFrac <- latOverlap * lonOverlap
        
        # Third method - dplyr
        lodf[[projIndex]] <- data.frame(projIndex=projIndex,
                                        orgIndex=which(areaFrac != 0), 
                                        value=areaFrac[which(areaFrac != 0)])
        
        # Second method - tempfile
        #         write.table(data.frame(projIndex =projIndex,
        #                                orgIndex=which(areaFrac != 0), 
        #                                value=areaFrac[which(areaFrac != 0)]),
        #                     file=tf, append=!first, col.names=first, sep=",", row.names=FALSE)
        #         first <- FALSE
        
        # Old method - rbind'ing data frames
        #         projectionMatrix <- rbind(projectionMatrix, 
        #                       data.frame(projIndex =projIndex,
        #                                         orgIndex=which(areaFrac != 0), 
        #                                         value=areaFrac[which(areaFrac != 0)]))
        setTxtProgressBar(pb, projIndex)
    }
    
    # Third method - dplyr
    projectionMatrix <- rbind_all(lodf)
    
    # Second method - tempfile
    #projectionMatrix <- read.csv(tf)
    Matrix::sparseMatrix(j = projectionMatrix$projIndex, 
                         i = projectionMatrix$orgIndex, 
                         x = projectionMatrix$value)
} # getProjectionMatrix

#' Project the values of one \code{\link{cmip5data}} object to a new grid
#' 
#' @param orgVar A \code{\link{cmip5data}} object to be regridded
#' @param projLat TODO
#' @param projLon TODO
#' @param orgArea TODO
#' @param projArea A \code{\link{cmip5data}} object or list with lat (latitude) and log (longitude) matricies
#' of the projection grid
#' @param projectionMatrix TODO
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object, whose \code{val} is the area-weighted regrided
#' variable passed in \code{orgVar} parameter. A \code{projectionMatrix} field is also added
#' recording projection matrix used in regridding; this can be reused for later variables with the same regridding.
#' @details This function calculates the projection matrix to shift one global grid to a second.
#' The relative contribution of an old grid to the new grid is calculated via an area weighting scheme
#' where the area of a grid cell is assumed to be proportional to the degree area of that cell and
#' neighboring cells are assumed to have the same area to degree ratios. This will NOT hold in large grids.
#' Nor is the area weighting scheme appropreate for all variable types and grid shifts. Use with caution.
#' @seealso \code{\link{getProjectionMatrix}}
#' @export
regrid <- function(orgVar, projLat, projLon, 
                   orgArea=NULL, projArea=NULL,
                   projectionMatrix=NULL, verbose=FALSE) {
    
    numOrgLat <- dim(orgVar$val)[2]
    numOrgLon <- dim(orgVar$val)[1]
    
    # Deal with legacy where the lat/lon were stored as vectors not arrays
    if(identical(class(orgArea$lon), 'numeric')) {
        orgArea$lon <- matrix(orgArea$lon, nrow=numOrgLon, ncol=numOrgLat)
        orgArea$lat <- matrix(orgArea$lat, nrow=numOrgLon, ncol=numOrgLat, byrow=TRUE)
    }
    if(identical(class(orgVar$lon), 'numeric')) {
        orgVar$lon <- matrix(orgVar$lon, nrow=numOrgLon, ncol=numOrgLat)
        orgVar$lat <- matrix(orgVar$lat, nrow=numOrgLon, ncol=numOrgLat, byrow=TRUE)
    }
    
    # Check that relevant lat/lon are matricies
    assert_that(is.matrix(orgVar$lon), is.matrix(orgVar$lat), is.matrix(projLat), is.matrix(projLon))
    
    # Pull the orginal area if it isn't provided
    if(is.null(orgArea)) {
        orgArea <- orgVar
        orgArea$val <- calcGridArea(lon=orgVar$lon[,1], lat=orgVar$lat[1,])
        if(length(dim(orgArea$val)) == 2) dim(orgArea$val) <- c(dim(orgArea$val), 1,1)
    }
    
    # Check that the area is for the correct variable
    assert_that(identical(orgArea$lat, orgVar$lat), identical(orgArea$lon, orgVar$lon))
    
    # Force lon to 0-360 if it's on -180-180
    orgArea$lon[orgArea$lon < 0] <- orgArea$lon[orgArea$lon < 0] + 360 
    
    # Check that the lat/lon is sorted
    assert_that(all(!apply(orgArea$lon, c(2), is.unsorted)), all(!apply(orgArea$lat, c(1), is.unsorted)))
    
    # Copy the orginal variable for historical purposes
    projVar <- orgVar[setdiff(names(orgVar), c('val', 'lon', 'lat'))]
    
    projVar$lon <- projLon
    projVar$lat <- projLat
    if(is.null(projArea)) {
        projArea <- list(lon=projLon, lat=projLat, val= calcGridArea(lon=projLon[,1], lat=projVar$lat[1,]))
    }
    
    # Force a 4D array for the area
    ifelse(length(dim(projArea$val)) == 2,  dim(projArea$val) <- c(dim(projArea$val), 1, 1), dim(projArea$val) <- dim(projArea$val))
    ifelse(length(dim(orgArea$val)) == 2,  dim(orgArea$val) <- c(dim(orgArea$val), 1, 1), dim(orgArea$val) <- dim(orgArea$val))
    
    # If the projection matrix is undefined then pull it
    if(is.null(projectionMatrix) | 
           !all(dim(projectionMatrix) == c(prod(dim(orgArea$lat)[1:2]), prod(dim(projVar$lat)[1:2])))) {
        projectionMatrix <- getProjectionMatrix(orgArea, projVar)   
    }
    
    # Project the lat/lon for each level/time slice
    projVar$val <- apply(orgVar$val, c(3,4), function(myMap) {
        temp <- myMap*orgArea$val[,,1,1]
        temp <- temp[TRUE]
        ans <- as.numeric(temp%*%projectionMatrix)
        dim(ans) <- dim(projVar$lat)
        ans <- ans/projArea$val[,,1,1]
        return(ans)
    })
    dim(projVar$val) <- c(dim(projVar$lat), dim(orgVar$val)[c(3,4)])
    
    projVar$projectionMatrix <- projectionMatrix
    addProvenance(cmip5data(projVar), 
                  paste('Shifting grid size from [', 
                        paste(dim(orgVar$val), collapse = ', '), '] to [', paste(dim(projVar$val), collapse = ', '), ']'))
} # regrid

