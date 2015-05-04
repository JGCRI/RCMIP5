# Testing code for the RCMIP5 'loadCMIP5.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("regrid.R")
#   library(testthat)
#   test_file("tests/testthat/test_regrid.R")

context("regrid")

test_that("regrid handles bad input", {})

test_that('regrid returns expected values for simple case', {
    numOrgLon <- 3
    numOrgLat <- 3
    orgLon <- matrix(seq(0, 360-360/numOrgLon, by=360/numOrgLon) + 360/numOrgLon/2, nrow=numOrgLon, ncol=numOrgLat)
    orgLat <- matrix(seq(-90, 90-180/numOrgLat, by=180/numOrgLat) + 180/numOrgLat/2, nrow=numOrgLon, ncol=numOrgLat, byrow=TRUE)
    
    orgArea <- list(lon = orgLon, lat=orgLat, val= matrix(1, nrow=numOrgLon, ncol=numOrgLat))
    orgVar <- list(lon = orgLon, lat=orgLat, val=rep(1:numOrgLat, times=numOrgLon))
    dim(orgVar$val) <- c(numOrgLon, numOrgLat, 1, 1)
   
    numProjLon <- 2
    numProjLat <- 2
    projLon <- matrix(seq(0, 360-360/numProjLon, by=360/numProjLon) + 360/numProjLon/2, nrow=numProjLon, ncol=numProjLat)
    projLat <- matrix(seq(-90, 90-180/numProjLon, by=180/numProjLon) + 180/numProjLon/2, nrow=numProjLon, ncol=numProjLat, byrow=TRUE)
    projArea <- list(lon = projLon, lat=projLat, val= matrix(1, nrow=numProjLon, ncol=numProjLat))
    transferMatrix <- getProjectionMatrix(orgArea = orgArea, projArea=projArea)
    test <- regrid(orgVar, projLat, projLon)
})

test_that('regrid test for data', {
    d <- regrid(orgVar = loadCMIP5(experiment='historical', variable='nbp', model='HadGEM2-ES', loadAs='array'),
           projLat=143, projLon=190, 
           orgArea = loadCMIP5(experiment='historical', variable='areacella', model='HadGEM2-ES', loadAs='array'),
           verbose=FALSE)
    

})