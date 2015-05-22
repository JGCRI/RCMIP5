# Testing code for the RCMIP5 'loadCMIP5.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("regrid.R")
#   library(testthat)
#   test_file("tests/testthat/test_regrid.R")

context("regrid")

test_that("regrid handles bad input", {
    # TODO
})

test_that('regrid returns expected values for simple case', {
    numOrgLon <- 3
    numOrgLat <- 2
    orgLon <- matrix(seq(0, 360-360/numOrgLon, by=360/numOrgLon) + 360/numOrgLon/2, 
                     nrow=numOrgLon, ncol=numOrgLat)
    orgLat <- matrix(seq(-90, 90-180/numOrgLat, by=180/numOrgLat) + 180/numOrgLat/2, 
                     nrow=numOrgLon, ncol=numOrgLat, byrow=TRUE)
    
    orgArea <- list(lon = orgLon, lat=orgLat, val= matrix(1, nrow=numOrgLon, ncol=numOrgLat))
    orgVar <- cmip5data(x=list(lon = orgLon, lat=orgLat, val=rep(1:numOrgLon, times=numOrgLat)))
    dim(orgVar$val) <- c(numOrgLon, numOrgLat, 1, 1)
   
    numProjLon <- 2
    numProjLat <- 3
    projLon <- matrix(seq(0, 360-360/numProjLon, by=360/numProjLon) + 360/numProjLon/2, 
                      nrow=numProjLon, ncol=numProjLat)
    projLat <- matrix(seq(-90, 90-180/numProjLat, by=180/numProjLat) + 180/numProjLat/2, 
                      nrow=numProjLon, ncol=numProjLat, byrow=TRUE)
    projArea <- list(lon = projLon, lat=projLat, val= matrix(1, nrow=numProjLon, ncol=numProjLat))
    projArea$val <- projArea$val/sum(projArea$val, na.rm=TRUE)*sum(orgArea$val, na.rm=TRUE)
    transferMatrix <- getProjectionMatrix(orgArea = orgArea, projArea=projArea)
    
    test <- regrid(orgVar, projLat, projLon, orgArea=orgArea, projArea=projArea)
    
    #given specific values, test that we get the right answer
    expect_equal(as.numeric(orgVar$val), c(1,2,3,1,2,3))
    expect_equal(as.numeric(test$val), c(1,2,1,2,1,2) + 1/3*c(1,2,1,2,1,2))
    
    expect_equal(sum(as.numeric(test$val[,,1,1]*projArea$val), na.rm=TRUE), 
                 sum(as.numeric(orgVar$val[,,1,1]*orgArea$val, na.rm=TRUE)))
})

test_that('regrid test for data', {
    skip_on_cran()
    path <- "../../sampledata"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    numProjLon <- 360
    numProjLat <- 180
    projLon <- matrix(seq(0, 360-360/numProjLon, by=360/numProjLon) + 360/numProjLon/2, nrow=numProjLon, ncol=numProjLat)
    projLat <- matrix(seq(-90, 90-180/numProjLat, by=180/numProjLat) + 180/numProjLat/2, nrow=numProjLon, ncol=numProjLat, byrow=TRUE)
    projArea <- list(lon = projLon, lat=projLat, val= calcGridArea(lon = projLon, lat=projLat))
    
    orgVar <- loadCMIP5(path=path, experiment='historical', variable='nbp', model='HadGEM2-ES', loadAs='array')
    orgArea <- loadCMIP5(path=path, experiment='historical', variable='areacella', model='HadGEM2-ES', loadAs='array')
    orgVar$val[is.na(orgVar$val)] <- 0
    orgArea$val[is.na(orgArea$val)] <- 0
    
    transferMatrix <- getProjectionMatrix(orgArea = orgArea, projArea=projArea)
    d <- regrid(orgVar = orgVar, projLat=projLat, projLon=projLon, 
           orgArea = orgArea, projectionMatrix = transferMatrix,
           verbose=FALSE)
    
    ##debug looking at image
    #image(d$val[,,1,1])
    #image(orgVar$val[,,1,1])
    orgTotal <- sum(as.numeric(orgVar$val[,,1,1]*orgArea$val[,,1,1]), na.rm=TRUE)
    projTotal <- sum(as.numeric(d$val[,,1,1]*projArea$val), na.rm=TRUE)
    expect_less_than(abs(projTotal-orgTotal)/abs(orgTotal), 1e-2)
})