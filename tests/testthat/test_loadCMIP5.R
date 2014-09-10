# Testing code for the RCMIP5 'loadCMIP5.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("loadCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_loadCMIP5.R")

context("loadCMIP5")

test_that("loadCMIP5 handles bad input", {
    path <- normalizePath("../../sampledata/")
    expect_error(loadCMIP5("",,"",path="does_not_exist"))  # path does not exist
    expect_error(loadCMIP5("","","",path=c(path,path)))       # multi-value path
    expect_error(loadCMIP5("","","",path=1))                  # non-character path
    expect_error(loadCMIP5(1,"",""))                          # non-character
    expect_error(loadCMIP5("",1,""))                          # non-character
    expect_error(loadCMIP5("","",1))                          # non-character
    expect_error(loadCMIP5("","","",domain=1))                # non-character
    expect_error(loadCMIP5(c("",""),"",""))                   # multi-value
    expect_error(loadCMIP5("",c("",""),""))                   # multi-value
    expect_error(loadCMIP5("","",c("","")))                   # multi-value
    expect_error(loadCMIP5("","","",domain=c("","")))         # multi-value
    expect_error(loadCMIP5("","","",verbose=1))               # non-logical verbose
    expect_error(loadCMIP5("","","",recursive=1))             # non-logical recursive
    expect_error(loadCMIP5("","","",force.ncdf=1))             # non-logical force.ncdf
    expect_error(loadCMIP5("","","",yearRange=T))             # non-numeric yearRange
    expect_error(loadCMIP5("","","",yearRange=1))             # non-numeric yearRange
})

test_that("loadCMIP5 handles no files found", {            # no netcdf files found
    w <- getOption('warn')
    options(warn=-1)
    expect_warning(loadCMIP5("","","",path=normalizePath("testdata_none/")))
    expect_is(loadCMIP5("","","",path=normalizePath("testdata_none/")),"NULL")
    options(warn=w)
})

test_that("loadCMIP5 loads monthly data", {
    path <- "../../sampledata/monthly/"
    d <- loadCMIP5('nbp','HadGEM2-ES','rcp85',path=path,verbose=F)     # test data set
    expect_is(d,"cmip5data")
    expect_equal(length(d$files), 4)                                 # should be four files
})

test_that("loadCMIP5 loads annual data", {
    path <- "../../sampledata/annual/"
    d <- loadCMIP5('co3','HadGEM2-ES','rcp85',path=path,verbose=F)
    expect_is(d,"cmip5data")
    expect_equal(length(d$files),1)                                 # should be one file
})

test_that("loadEnsemble checks unique domain", {
    expect_error(loadCMIP5("co3","fakemodel1-ES","rcp85",path='testdata_twodomains/',verbose=F))
})

test_that("loadCMIP5 handles spatial mismatches between ensembles", {
    path <- "testdata_mismatch/"
    
    # Test data created by
    # d1 <- cmip5data(1850,lonsize=10,latsize=10)
    # d2 <- cmip5data(1851,lonsize=10,latsize=8)
    # d2$ensemble <- "dummyensemble2"
    # saveNetCDF(d1) and then d2
    # Rename files to avoid R CMD CHECK warning
    expect_warning(loadCMIP5("dummyvar", "b", "c", domain="d", path=path, verbose=F))
})

test_that("loadCMIP5 can load using both ncdf and ncdf4", {
    path <- "../../sampledata/monthly/"
    d1 <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F)     # test data set
    d2 <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, force.ncdf=TRUE)     # test data set
    expect_equal(d1$val, d2$val)
    expect_equal(names(d1), names(d2))
})