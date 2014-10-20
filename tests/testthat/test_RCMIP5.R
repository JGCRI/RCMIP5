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

    df <- as.data.frame(makeAnnualStat(cmip5data(2000:2002), verbose=F))
    expect_is(df, "data.frame")
    expect_equal(names(df), c("lon", "lat", "time", "value"))

    df <- as.data.frame(makeMonthlyStat(cmip5data(2000:2002), verbose=F))
    expect_is(df, "data.frame")
    expect_equal(names(df), c("lon", "lat", "time", "value"))

    df <- as.data.frame(makeGlobalStat(cmip5data(2000:2002), verbose=F))
    expect_is(df, "data.frame")
    expect_equal(names(df), c("time", "value"))

    df <- as.data.frame(makeGlobalStat(cmip5data(2000:2002, Z=T), verbose=F))
    expect_is(df, "data.frame")
    expect_equal(names(df), c("Z", "time", "value"))

    df <- as.data.frame(makeZStat(cmip5data(2000:2002, Z=T), verbose=F))
    expect_is(df, "data.frame")
    expect_equal(names(df), c("lon", "lat", "time", "value"))

    d <- cmip5data(2000:2002, Z=T)
    d$dimNames <- c("a", "b", "c", "d")
    expect_equal(names(as.data.frame(d, verbose=F)),
                 c("lon", "lat", "Z", "time", "value"))
    expect_equal(names(as.data.frame(d, verbose=F, originalNames=T)),
                 c("a", "b", "c", "d", "value"))
})

test_that("as.array works", {
    arr <- as.array(cmip5data(2000:2002, Z=T))
    expect_is(arr, "array")
    expect_equal(dim(arr), c(10, 10, 5, 36))

    arr <- as.array(cmip5data(2000:2002))
    expect_is(arr, "array")
    expect_equal(dim(arr), c(10, 10, 1, 36))

})
