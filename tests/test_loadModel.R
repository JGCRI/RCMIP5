# Testing code for the RCMIP5 (?) 'loadModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("loadModel.R")    # can't do this here

# To run this code: 
#   source("loadModel.R")
#   library(testthat)
#   test_dir("tests/")
# which will run everything in the 'tests' directory

context("loadModel")

test_that("loadModel handles bad input", {
    path <- normalizePath("testdata/")
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

test_that("loadModel loads data", {
    # TODO
})