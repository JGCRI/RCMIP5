#This is the alternate regridding algorithm
#library(Matrix) #sparse matrix
#library(plyr) 
getProjectionMatrix <- function(orgArea, projArea, verbose=FALSE){
    
    assert_that(all(!apply(orgArea$lon, c(2), is.unsorted)), all(!apply(orgArea$lat, c(1), is.unsorted)))
    assert_that(all(!apply(projArea$lon, c(2), is.unsorted)), all(!apply(projArea$lat, c(1), is.unsorted)))

    extractBounds <- function(lat, lon){
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

    projectionMatrix <- ldply(as.list(1:prod(dim(projArea$lat))), 
                      function(projIndex){
        latOverlap <- (pmin(projEnds$maxLat[projIndex], orgEnds$maxLat[TRUE]) - 
                            pmax(projEnds$minLat[projIndex], orgEnds$minLat[TRUE])) /
                                (orgEnds$maxLat[TRUE]-orgEnds$minLat[TRUE])
        latOverlap[latOverlap < 0] <- 0
        
        lonOverlap <- (pmin(projEnds$maxLon[projIndex], orgEnds$maxLon[TRUE]) - 
                           pmax(projEnds$minLon[projIndex], orgEnds$minLon[TRUE])) /
            (orgEnds$maxLon[TRUE]-orgEnds$minLon[TRUE])
        lonOverlap[lonOverlap < 0] <- 0
        
        areaFrac <- latOverlap * lonOverlap
        
        return(data.frame(projIndex =projIndex,
                        orgIndex=which(areaFrac != 0), 
                          value=areaFrac[which(areaFrac != 0)]))
    } )
        
    projectionMatrix <- sparseMatrix(j = projectionMatrix$projIndex, i = projectionMatrix$orgIndex, 
                                         x=projectionMatrix$value)
    
    return(projectionMatrix)   
}


regrid <- function(orgVar, projLat, projLon, 
                   orgArea=NULL, projArea=NULL,
                   projectionMatrix=NULL, verbose=FALSE){
    
    if(is.null(orgArea)){
        orgArea <- orgVar
        orgArea$val <- calcGridArea(lon=orgVar$lon[,1], lat=orgVar$lat[1,])
        dim(orgArea$val) <- c(dim(orgArea$val), 1,1)
    }
    
    assert_that(identical(orgArea$lat, orgVar$lat), identical(orgArea$lon, orgVar$lon))
    
    if(all(orgArea$lon <= 180 & orgArea$lon >= -180)){
        orgArea$lon <- orgArea$lon + 180
    }
    
    numOrgLat <- dim(orgArea$val)[2]
    numOrgLon <- dim(orgArea$val)[1]
    
    #Deal with legacy where the lat/lon were stored as vectors not arrays
    if(identical(class(orgArea$lon), 'numeric')){
        orgArea$lon <- matrix(orgArea$lon, nrow=numOrgLon, ncol=numOrgLat)
        orgArea$lat <- matrix(orgArea$lat, nrow=numOrgLon, ncol=numOrgLat, byrow=TRUE)
    }
    
    assert_that(all(!apply(orgArea$lon, c(2), is.unsorted)), all(!apply(orgArea$lat, c(1), is.unsorted)))
    
    stepLon <- 360/numProjLon
    stepLat <- 180/numProjLat
    projVar <- orgVar
    projVar$lon <- projLon
    projVar$lat <- projLat
    if(is.null(projArea)){
        projArea <- list(lon=projLon, lat=projLat, val= calcGridArea(lon=projLon[,1], lat=projVar$lat[1,]))
    }
    
    ifelse(length(dim(projArea$val)) == 2,  dim(projArea$val) <- c(dim(projArea$val), 1, 1), dim(projArea$val) <- dim(projArea$val))
    ifelse(length(dim(orgArea$val)) == 2,  dim(orgArea$val) <- c(dim(orgArea$val), 1, 1), dim(orgArea$val) <- dim(orgArea$val))
    
    if(is.null(projectionMatrix) | 
           !all(dim(projectionMatrix) == c(prod(dim(orgArea$lat)[1:2]), prod(dim(projVar$lat)[1:2])))){
        projectionMatrix <- getProjectionMatrix(orgArea, projVar)   
    }
    
    projVar$val <- apply(orgVar$val, c(3,4), function(myMap){
        temp <- myMap*orgArea$val[,,1,1]
        dim(temp) <- c(1, prod(dim(orgArea$val)))
        ans <- as.numeric(temp%*%projectionMatrix)
        #print(sum(temp[TRUE], na.rm=TRUE))
        #print(sum(ans[TRUE], na.rm=TRUE))
        #assert_that(sum(temp[TRUE], na.rm=TRUE) - sum(ans[TRUE], na.rm=TRUE) < 1e-6*sum(temp[TRUE], na.rm=TRUE))
        return(ans/projArea$val[TRUE])
    })
    dim(projVar$val) <- c(dim(projVar$lat), dim(orgVar$val)[c(3,4)])
    
    projVar$projectionMatrix <- projectionMatrix
    projVar <- addProvenance(projVar, paste('Shifting grid size from [', dim(orgVar$val), '] to [', dim(projVar$val), ']', collapse = ', '))
    return(projVar)
}