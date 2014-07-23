# Testing code for the RCMIP5 (?) 'checkTimePeriod.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("checkTimePeriod.R")    # can't do this here

# To run this code: 
#   source("checkTimePeriod.R")
#   test_dir("tests/")
# which will run everything in the 'tests' directory

context("checkTimePeriod")

test_that("checkTimePeriod handles bad input", {
    d <- checkTimePeriod(getFileInfo("testdata/"))
    expect_error(checkTimePeriod())                     # no input
    expect_error(checkTimePeriod(c(d,d)))               # multi-value input
    expect_error(checkTimePeriod(12))                   # non-data.frame input
    expect_error(checkTimePeriod(data.frame()))         # incorrect data.frame input
})

test_that("checkTimePeriod correctly finds missing files", { 
    d <- checkTimePeriod(getFileInfo("testdata_missingfile"))
    expect_is(d,"data.frame")
    expect_equal(nrow(d),2)     # should be two cases
    expect_equal(ncol(d),10)
    expect_false(d$allHere[1])
    expect_false(d$allHere[2])
})
