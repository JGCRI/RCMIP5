# Testing code for the RCMIP5 'loadEnsemble.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("loadEnsemble.R")
#   library(testthat)
#   test_file("tests/testthat/test_loadEnsemble.R")

context("loadEnsemble")

test_that("loadEnsemble handles bad input", {
    expect_error(RCMIP5:::loadEnsemble("","","","","",path="does_not_exist"))  # path does not exist
    expect_error(RCMIP5:::loadEnsemble("","","","","",path=c("path1","path2")))       # multi-value path
    expect_error(RCMIP5:::loadEnsemble("","","","","",path=1))                  # non-character path
    expect_error(RCMIP5:::loadEnsemble(variable=1,"","","",""))                          # non-character
    expect_error(RCMIP5:::loadEnsemble("",model=1,"","",""))                          # non-character
    expect_error(RCMIP5:::loadEnsemble("","",experiment=1,"",""))                          # non-character
    expect_error(RCMIP5:::loadEnsemble("","","",ensemble=1,""))                          # non-character
    expect_error(RCMIP5:::loadEnsemble("","","","",domain=1))                # non-character
    expect_error(RCMIP5:::loadEnsemble(variable=c("",""),"","","",""))                   # multi-value
    expect_error(RCMIP5:::loadEnsemble("",model=c("",""),"","",""))                   # multi-value
    expect_error(RCMIP5:::loadEnsemble("","",experiment=c("",""),"",""))                   # multi-value
    expect_error(RCMIP5:::loadEnsemble("","","",ensemble=c("",""),""))                   # multi-value
    expect_error(RCMIP5:::loadEnsemble("","","","",domain=c("","")))         # multi-value
    expect_error(RCMIP5:::loadEnsemble("","","","",verbose=1))               # non-logical verbose
    expect_error(RCMIP5:::loadEnsemble("","","","",recursive=1))             # non-logical recursive
})

test_that("loadEnsemble handles no files found", {            # no NetCDF files found
    w <- getOption('warn')
    options(warn=-1)
    expect_warning(RCMIP5:::loadEnsemble("","","","","", path=("testdata_none")))
    options(warn=w)
})

test_that("loadEnsemble loads monthly data", {
    
    skip_on_cran()
    
    path <- "../../sampledata/monthly"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    d <- RCMIP5:::loadEnsemble('prc','GFDL-CM3', 'rcp85', 'r1i1p1', '[^_]+', path=path, verbose=F)
    expect_is(d, "cmip5data")
    d <- RCMIP5:::loadEnsemble('prc','GFDL-CM3','rcp85','r1i1p1','[^_]+', path=path, verbose=F)     
    expect_is(d, "cmip5data")
    expect_equal(length(d$files), 1)                                 # should be one file
})

test_that("loadEnsemble loads annual data", {
    
    skip_on_cran()
    
    path <- "../../sampledata/annual"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    d <- RCMIP5:::loadEnsemble('co3', 'HadGEM2-ES', 'rcp85', 'r1i1p1', '[^_]+', path=path, verbose=F)
    expect_is(d, "cmip5data")
})

test_that("loadEnsemble loads 4D data", {
    
    skip_on_cran()
    
    path <- "../../sampledata/annual"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    d <- RCMIP5:::loadEnsemble('co3','HadGEM2-ES','rcp85','r1i1p1', '[^_]+', 
                      path=path, verbose=F)     # test data set
    expect_is(d,"cmip5data")
    expect_is(d$Z, "numeric")
    
    path <- "../../sampledata/monthly"
    d <- RCMIP5:::loadEnsemble('tsl','GFDL-CM3','historicalGHG','r1i1p1', '[^_]+', 
                      path=path, verbose=F)     # test data set
    expect_is(d,"cmip5data")
    expect_is(d$Z, "numeric")
})

test_that("loadEnsemble checks unique domain", {
    expect_error(RCMIP5:::loadEnsemble("co3","fakemodel1-ES","rcp85","r1i1p1", '[^_]+',
                              path='testdata_twodomains/', verbose=F))
})

test_that("loadEnsemble assigns ancillary data", {
    
    skip_on_cran()
    
    path <- "../../sampledata/annual"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    d <- RCMIP5:::loadEnsemble('co3','HadGEM2-ES','rcp85','r1i1p1', '[^_]+', path=path,verbose=F)
    expect_is(d, "cmip5data")
    expect_true(!is.null(d$provenance))
})

test_that("loadEnsemble handles 2D lon and lat", {
    
    skip_on_cran()
    
    path <- "../../sampledata"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    d <- RCMIP5:::loadEnsemble('tos','GFDL-ESM2G', 'historical', 'r1i1p1', '[^_]+', path=path, verbose=F)
    expect_is(d, "cmip5data")
    expect_is(d$lon, "matrix")
    expect_is(d$lat, "matrix")
})

test_that("loadEnsemble handles data with time length=1", {
    
    skip_on_cran()
    
    path <- "../../sampledata"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    # This is a real CMIP5 file with one single month
    # loadEnsemble should add an extra dimension (of length 1) to the data
    d <- RCMIP5:::loadEnsemble("spco2", "HadGEM2-ES", "rcp85", domain="Omon",
                      ensemble="r1i1p1", path=path, verbose=F)    
    expect_is(d, "cmip5data")
    # TODO: check dimensions
})


test_that("loadEnsemble handles time-only data", {
    
    skip_on_cran()
    
    path <- "../../sampledata"
    if(!file.exists(path)) skip("Path doesn't exist")
    
    # This is a real CMIP5 file with no lon or lat, just time
    # loadEnsemble should read OK, and add extra lon/lat dimensions of length 1
    d <- RCMIP5:::loadEnsemble("co2mass", "GFDL-ESM2M", "historical", domain="Amon", ensemble="r1i1p1",
                      path=path, verbose=F)
    
    expect_is(d, "cmip5data")
    # TODO: check dimensions
})

test_that("loadEnsemble detects overlapping files", {
    
    path <- "testdata_overlap"
    # These two files (saved by saveNetCDF) have overlapping time periods
    
    expect_error(RCMIP5:::loadEnsemble("var", "m", "ex", path=path))
})

