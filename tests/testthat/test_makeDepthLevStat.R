# Testing code for the RCMIP5 'makeDepthLevStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
library(plyr)
library(abind)

# To run this code: 
#   source("makeDepthLevStat.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_makeDepthLevStat.R")

context("makeDepthLevStat")

test_that("makeDepthLevStat handles bad input", {
    expect_error(makeDepthLevStat(1))                         # non-list d
    expect_error(makeDepthLevStat(cmpi5data()))               # wrong size list d
    expect_error(makeDepthLevStat(d,verbose=1))               # non-logical verbose
    expect_error(makeDepthLevStat(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeDepthLevStat(d,parallel=1))              # non-logical parallel
    expect_error(makeDepthLevStat(d,parallel=c(T, T)))         # multiple parallel values
    expect_error(makeDepthLevStat(d,FUN=1))                   # non-function FUN
    expect_error(makeDepthLevStat(d,FUN=c(mean, mean)))        # multiple FUN values
})

test_that("makeDepthLevStat computes depth means", {
    years <- 1850:1851
    d <- cmip5data(years, depth=T)
    res <- makeDepthLevStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res, "cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # Depth removed, and other info updated?
    expect_null(res$depths)
    expect_is(res$numDepths, "integer")
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
        
    # Is the answer value array correctly sized?
    ndims <- length(dim(res$val))
    expect_equal(ndims, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[3], 1) #  all spatial dimensions should be 1
    expect_equal(dim(res$val)[c(1,2,4)], dim(d$val)[c(1,2,4)]) # time should match    
    
    # Are the answer values numerically correct?
    expect_equal(mean(res$val), mean(d$val))  # no weighting
})

test_that("makeDepthLevStat computes lev means", {
    years <- 1850:1851
    d <- cmip5data(years, lev=T)
    res <- makeDepthLevStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res, "cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # Depth removed, and other info updated?
    expect_null(res$levs)
    expect_is(res$numLevs, "integer")
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
    
    # Is the answer value array correctly sized?
    ndims <- length(dim(res$val))
    expect_equal(ndims, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[3], 1) #  all spatial dimensions should be 1
    expect_equal(dim(res$val)[c(1,2,4)], dim(d$val)[c(1,2,4)]) # time should match    
    
    # Are the answer values numerically correct?
    expect_equal(mean(res$val), mean(d$val))  # no weighting
})

test_that("makeDepthLevStat parallel results == serial result", {
    years <- 1850:1851
    d <- cmip5data(years, depth=T, randomize=T)
    res_s <- makeDepthLevStat(d, verbose=F, parallel=F)
    res_p <- makeDepthLevStat(d, verbose=F, parallel=T)
    expect_equal(res_s$val, res_p$val)
    expect_equal(res_s$numDepths, res_p$numDepths)
})

test_that("makeDepthLevStat handles 3-dimensional data", {
    years <- 1850:1851
    d <- cmip5data(years, depth=F, lev=F)
    expect_warning(makeDepthLevStat(d))
})
