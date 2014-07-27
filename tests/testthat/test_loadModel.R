# Testing code for the RCMIP5 (?) 'loadModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("loadModel.R")    # can't do this here

# To run this code: 
#   source("loadModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_loadModel.R")

context("loadModel")

test_that("loadModel handles bad input", {
    path <- normalizePath("../../sampledata/")
    expect_error(loadModel("",,"",path="does_not_exist"))  # path does not exist
    expect_error(loadModel("","","",path=c(path,path)))       # multi-value path
    expect_error(loadModel("","","",path=1))                  # non-character path
    expect_error(loadModel(1,"",""))                          # non-character
    expect_error(loadModel("",1,""))                          # non-character
    expect_error(loadModel("","",1))                          # non-character
    expect_error(loadModel(c("",""),"",""))                   # multi-value
    expect_error(loadModel("",c("",""),""))                   # multi-value
    expect_error(loadModel("","",c("","")))                   # multi-value
    expect_error(loadModel("","","",verbose=1))               # non-logical verbose
    expect_error(loadModel("","","",recursive=1))             # non-logical recursive
})

test_that("loadModel handles no files found", {            # no netcdf files found
    w <- getOption('warn')
    options(warn=-1)
    expect_warning(loadModel("","","",path=normalizePath("testdata_none/")))
    expect_is(loadModel("","","",path=normalizePath("testdata_none/")),"NULL")
    options(warn=w)
})

test_that("loadModel loads monthly data", {
    path <- "../../sampledata/monthly/"
    d <- loadModel('nbp','HadGEM2-ES','rcp85',path=path)     # test data set
    expect_is(d,"list")
    expect_equal(length(d$files),2)                                 # should be two files
    d <- loadEnsemble('prc','GFDL-CM3','rcp85','r1i1p1',path=path)     
    expect_is(d,"list")
    expect_equal(length(d$files),1)                                 # should be one file
})

test_that("loadModel handles lat-lon-time mismatches", {
    path <- "testdata_mismatch/"
    expect_warning(loadModel("nbp","HadGEM2-ES","historical",path=path))
})

test_that("demo mode works", {
    # TODO
})
