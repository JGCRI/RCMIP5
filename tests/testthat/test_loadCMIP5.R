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
    expect_error(loadCMIP5("","","",yearRange=1))             # yearRange wrong length
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
    d1 <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F)  # ncdf4
    d2 <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, force.ncdf=TRUE) # ncdf
    expect_equal(d1$val, d2$val)
    expect_equal(names(d1), names(d2))
})

test_that("loadCMIP5 handles YearRange", {
    path <- "../../sampledata/monthly/"
    
    # These sample data are 200512-203011 and 203012-205511 (with 2 ensembles)
    # yearRange in first file only
    d <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, yearRange=c(2006, 2007))
    expect_equal(length(d$time), 24)
    expect_equal(dim(d$val)[3], 24)
    d <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, yearRange=c(1, 2007))
    expect_equal(length(d$time), 25)
    expect_equal(dim(d$val)[3], 25)
    
    # yearRange in second file only
    d <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, yearRange=c(2036, 2037))
    expect_equal(length(d$time), 24)
    expect_equal(dim(d$val)[3], 24)
    d <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, yearRange=c(2054, 9999))
    expect_equal(length(d$time), 23)
    expect_equal(dim(d$val)[3], 23)
    
    # yearRange spans files
    d <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, yearRange=c(2030, 2031))
    expect_equal(length(d$time), 24)
    expect_equal(dim(d$val)[3], 24)
    
    # yearRange doesn't overlap with files
    d <- loadCMIP5('nbp', 'HadGEM2-ES', 'rcp85', path=path, verbose=F, yearRange=c(1995, 1996))
    expect_null(d$time)
    expect_null(dim(d$val))
})