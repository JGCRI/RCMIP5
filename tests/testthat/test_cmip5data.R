# Testing code for the RCMIP5 scripts in 'RCMIP5.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("RCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_internalHelpers.R")

context("cmip5data")

test_that("cmip5data handles bad input", {
    expect_error(cmip5data("hi"))   
    expect_error(cmip5data(1, monthly=123))   
    expect_error(cmip5data(1, depth=123))
    expect_error(cmip5data(1, lev=123))
    expect_error(cmip5data(1, randomize="hi"))   
})

test_that("cmip5data generates annual and monthly data", {
    d <- cmip5data(1)
    expect_is(d, "cmip5data")
    expect_equal(length(dim(d$val)), 3)
    expect_equal(dim(d$val)[3], 12)
    expect_equal(length(d$time), 12)
    expect_equal(d$timeFreqStr, "mon")
    expect_equal(dim(d$val)[1], length(d$lon))
    expect_equal(dim(d$val)[2], length(d$lat))
    expect_equal(dim(d$val)[3], length(d$time))
    
    d <- cmip5data(1, monthly=F)
    expect_equal(length(dim(d$val)), 3)
    expect_equal(dim(d$val)[3], 1)
    expect_equal(length(d$time), 1)
    expect_equal(d$timeFreqStr, "yr")
    expect_equal(dim(d$val)[3], length(d$time))
})

test_that("cmip5data fills in ancillary data", {
    d <- cmip5data(1)
    expect_is(d$model, "character")
    expect_is(d$variable, "character")
    expect_is(d$experiment, "character")
    expect_is(d$valUnit, "character")
    expect_is(d$timeFreqStr, "character")
    expect_is(d$debug, "list")
    expect_is(d$debug$calendarStr, "character")
    expect_is(d$debug$timeUnit, "character")
})

test_that("cmip5data obeys depth and lev", {
    d <- cmip5data(1, depth=T)
    expect_equal(length(dim(d$val)), 4)
    expect_false(is.null(d$depth))
    
    d <- cmip5data(1, lev=T)
    expect_equal(length(dim(d$val)), 4)
    expect_false(is.null(d$lev))
    
    d <- cmip5data(1, depth=T, lev=T)
    expect_equal(length(dim(d$val)), 5)
    expect_false(is.null(d$depth))
    expect_false(is.null(d$lev))
})

test_that("cmip5data obeys randomize", {
    expect_true(sum(cmip5data(1, randomize=T)$val) != sum(cmip5data(1, randomize=T)$val))
})

test_that("cmip5data generates an area file", {
    d <- cmip5data(0) # 0 signals area only
    expect_is(d, "cmip5data")
    expect_is(d$model, "character")
    expect_is(d$domain, "character")
    expect_is(d$variable, "character")
    expect_is(d$experiment, "character")
    expect_is(d$valUnit, "character")
    expect_is(d$debug, "list")

    # Should be spatial information
    expect_equal(dim(d$val)[1], length(d$lon))
    expect_equal(dim(d$val)[2], length(d$lat))
    
    # Shouldn't be any time information
    expect_equal(length(dim(d$val)), 2)
    expect_null(d$time)
    expect_null(d$timeFreqStr, "character")
    expect_null(d$debug$calendarStr)
})
