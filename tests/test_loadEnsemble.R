# Testing code for the RCMIP5 (?) 'loadEnsemble.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("loadEnsemble.R")    # can't do this here

# To run this code: 
#   source("loadEnsemble.R")
#   library(testthat)
#   test_dir("tests/")
# which will run everything in the 'tests' directory

context("loadEnsemble")

test_that("loadEnsemble handles bad input", {
    path <- normalizePath("testdata/")
    expect_error(loadEnsemble("","","","",path="does_not_exist"))  # path does not exist
    expect_error(loadEnsemble("","","","",path=c(path,path)))       # multi-value path
    expect_error(loadEnsemble("","","","",path=1))                  # non-character path
    expect_error(loadEnsemble(1,"","",""))                          # non-character
    expect_error(loadEnsemble("",1,"",""))                          # non-character
    expect_error(loadEnsemble("","",1,""))                          # non-character
    expect_error(loadEnsemble("","","",1))                          # non-character
    expect_error(loadEnsemble(c("",""),"","",""))                   # multi-value
    expect_error(loadEnsemble("",c("",""),"",""))                   # multi-value
    expect_error(loadEnsemble("","",c("",""),""))                   # multi-value
    expect_error(loadEnsemble("","","",c("","")))                   # multi-value
    expect_error(loadEnsemble("","","","",verbose=1))               # non-logical verbose
    expect_error(loadEnsemble("","","","",recursive=1))             # non-logical recursive
})

test_that("loadEnsemble handles no files found", {            # no netcdf files found
    w <- getOption('warn')
    options(warn=-1)
    expect_warning(loadEnsemble("","","","",path=normalizePath("testdata_none/")))
    expect_is(loadEnsemble("","","","",path=normalizePath("testdata_none/")),"NULL")
    options(warn=w)
})

test_that("loadEnsemble loads data", {
    # TODO
})