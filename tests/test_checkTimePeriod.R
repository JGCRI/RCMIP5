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
    d <- checkTimePeriod(getFileInfo("testdata_missingfile/"))
    expect_is(d,"data.frame")
    expect_equal(nrow(d),2)     # should be two cases
    expect_equal(ncol(d),10)
    expect_false(d$allHere[1])  # monthly data case is not complete
    expect_false(d$allHere[2])  # annual data case is not complete
    expect_true(all(d$files > 1))
})

test_that("checkTimePeriod correctly sees continuous files", { 
    d <- checkTimePeriod(getFileInfo("testdata/"))
    expect_is(d,"data.frame")
    expect_equal(nrow(d),4)     # should be four cases
    expect_equal(ncol(d),10)
    expect_true(all(d$allHere))
})

test_that("checkTimePeriod correctly parses dates", { 
    d <- checkTimePeriod(getFileInfo("testdata/"))
    expect_is(d,"data.frame")
    expect_is(d$startDate,"numeric")
    expect_is(d$endDate,"numeric")
    expect_true(all(d$endDate > d$startDate))
    expect_true(all(d$startDate >= 1850))
    expect_true(all(d$endDate <= 2300))
})
