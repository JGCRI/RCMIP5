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
    expect_error(cmip5data(1, Z=123))
    expect_error(cmip5data(1, lev=123))
    expect_error(cmip5data(1, randomize="hi"))   
})

test_that("cmip5data generates annual and monthly data", {
    d <- cmip5data(1)
    expect_is(d, "cmip5data")
    expect_equal(length(dim(d$val)), 4)
    expect_equal(dim(d$val)[4], 12)
    expect_equal(length(d$time), 12)
    expect_equal(d$debug$timeFreqStr, "mon")
    expect_equal(dim(d$val)[1], length(d$lon))
    expect_equal(dim(d$val)[2], length(d$lat))
    expect_equal(dim(d$val)[3], 1)
    expect_equal(dim(d$val)[4], length(d$time))
    
    d <- cmip5data(1, monthly=F)
    expect_equal(length(dim(d$val)), 4)
    expect_equal(dim(d$val)[4], 1)
    expect_equal(length(d$time), 1)
    expect_equal(d$debug$timeFreqStr, "yr")
    expect_equal(dim(d$val)[4], length(d$time))
})

test_that("cmip5data fills in ancillary data", {
    d <- cmip5data(1)
    expect_is(d$model, "character")
    expect_is(d$variable, "character")
    expect_is(d$experiment, "character")
    expect_is(d$valUnit, "character")
    expect_is(d$debug, "list")
    expect_is(d$debug$timeFreqStr, "character")
    expect_is(d$debug$calendarStr, "character")
    expect_is(d$debug$timeUnit, "character")
})

test_that("cmip5data obeys randomize", {
    expect_true(sum(cmip5data(1, randomize=T)$val) != sum(cmip5data(1, randomize=T)$val))
})

test_that("cmip5data creates area-only data", {
    lonsize <- 10
    latsize <- 10
    d <- cmip5data(1, lonsize=lonsize, latsize=latsize, time=F, verbose=F)
    expect_equal(length(dim(d$val)), 4)
    expect_equal(dim(d$val)[1], lonsize)
    expect_equal(dim(d$val)[2], latsize)
    expect_equal(dim(d$val)[3], 1)
    expect_equal(dim(d$val)[4], 1)
    expect_is(d$lon, "numeric")
    expect_is(d$lat, "numeric")
    expect_null(d$Z)
    expect_null(d$time)
})

test_that("cmip5data creates area and Z data", {
    lonsize <- 10
    latsize <- 10
    Zsize <- 5
    d <- cmip5data(1, lonsize=lonsize, latsize=latsize, Z=T, Zsize=Zsize, time=F, verbose=F)
    expect_equal(length(dim(d$val)), 4)
    expect_equal(dim(d$val)[1], lonsize)
    expect_equal(dim(d$val)[2], latsize)
    expect_equal(dim(d$val)[3], Zsize)
    expect_equal(dim(d$val)[4], 1)    
    expect_is(d$lon, "numeric")
    expect_is(d$lat, "numeric")
    expect_is(d$Z, "integer")
    expect_null(d$time)
})

test_that("cmip5data creates time-only data", {
    d <- cmip5data(1, lonlat=F, Z=F, verbose=F)
    expect_equal(length(dim(d$val)), 4)
    expect_equal(dim(d$val)[1], 1)
    expect_equal(dim(d$val)[2], 1)
    expect_equal(dim(d$val)[3], 1)
    expect_equal(dim(d$val)[4], 12)  # 1 year above * 12 months
    expect_null(d$lon)
    expect_null(d$lat)
    expect_null(d$Z)
})
