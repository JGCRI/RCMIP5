# Testing code for the RCMIP5 (?) 'getFileInfo.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("getFileInfo.R")    # can't do this here

# To run this code: 
#   test_dir("tests/")
# which will run everything in the 'tests' directory

context("getFileInfo")

test_that("getFileInfo handles bad input", {
    expect_error(getFileInfo("does_not_exist"),"file.exists\\(path) is not TRUE")
                                                            # path does not exist
    expect_error(getFileInfo(path=c("1","2")))              # multi-value path
    expect_error(getFileInfo(recursive=c(T,F)))             # multi-value recursive
    expect_error(getFileInfo(12))                           # non-character path
    expect_error(getFileInfo(recursive=1))                  # non-logical recursive
})

test_that("getFileInfo handles no input", {                 # no netcdf files found
    w <- getOption('warn')
    options(warn=-1)
    expect_warning(getFileInfo("testdata_none/"),"No netcdf files found")   
    expect_is(getFileInfo("testdata_none/"),'NULL')
    options(warn=w)
})

test_that("getFileInfo handles non-CMIP5 netcdfs", {        # improper netcdf filenames
    w <- getOption('warn')
    options(warn=-1)
    expect_warning(getFileInfo("testdata_badfilename/"),"No netcdf files found")   
    expect_is(getFileInfo("testdata_badfilename/"),'NULL')
    options(warn=w)
})

test_that("getFileInfo handles annual netcdfs", { 
    # TODO
})

test_that("getFileInfo handles monthly netcdfs", { 
    # TODO
})
