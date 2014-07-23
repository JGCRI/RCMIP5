# Testing code for the RCMIP5 (?) scripts

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   test_dir("tests/")
# which will run everything in the 'tests' directory

context("getFileInfo")

test_that("getFileInfo handles bad input", {
    expect_error(getFileInfo("dskjfjhkds"),"file.exists\\(path) is not TRUE")
                                                            # path does not exist
    expect_error(getFileInfo(path=c("1","2")))              # multi-value path
    expect_error(getFileInfo(recursive=c(T,F)))             # multi-value recursive
    expect_error(getFileInfo(12))                           # non-character path
    expect_error(getFileInfo(recursive=1))                  # non-logical recursive
})

test_that("getFileInfo handles no input", {
    options(warn=-1)
    expect_warning(getFileInfo(),"No netcdf files found")   # no netcdf files found
    expect_is(getFileInfo(),'NULL')                         # no netcdf files found
    options(warn=0)
})

