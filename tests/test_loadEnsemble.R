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
    expect_error(loadEnsemble(path="does_not_exist"))  # path does not exist
    expect_error(loadEnsemble(path=c(path,path)))       # multi-value path
    expect_error(loadEnsemble(path=1))                  # non-character path
    
    # TODO tests for other parameters
})

test_that("loadEnsemble handles no input", {            # no netcdf files found
    d <- loadEnsemble(path=normalizePath("testdata_none/"))
    expect_is(d,"list")
    expect_equal(length(names(d)),8)
    expect_equal(length(d$files),0)
    expect_equal(sum(unlist(lapply(d,is.null))),7)      # should have seven NULLs
})
