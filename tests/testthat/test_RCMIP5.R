# Testing code for the RCMIP5 scripts in 'RCMIP5.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("RCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_RCMIP5.R")

context("RCMIP5")

test_that("cmip5data print method works", {
    d <- cmip5data(2000:2005)
    expect_output(print(d), "CMIP5")
    # not sure what to test here, except that no error
})

test_that("cmip5data summary method works", {
    d <- cmip5data(2000:2005)
    expect_output(print(summary(d)), "CMIP5")
    # not sure what to test here, except that no error
})

test_that("as.data.frame works", {
    df <- as.data.frame(cmip5data(2000:2002, Z=T))
    expect_is(df, "data.frame")
    expect_equal(names(df), c("lon", "lat", "Z", "time", "value"))
})

test_that("as.array works", {
    arr <- as.array(cmip5data(2000:2002, Z=T))
    expect_is(arr, "array")
    expect_equal(dim(arr), c(10, 10, 5, 36))

    arr <- as.array(cmip5data(2000:2002))
    expect_is(arr, "array")
    expect_equal(dim(arr), c(10, 10, 36))
})
