# Testing code for the RCMIP5 'makeZStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
library(plyr)
library(abind)

# To run this code: 
#   source("makeZStat.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_makeZStat.R")

context("makeZStat")

test_that("makeZStat handles bad input", {
    expect_error(makeZStat(1))                         # non-list d
    expect_error(makeZStat(cmpi5data()))               # wrong size list d
    expect_error(makeZStat(d,verbose=1))               # non-logical verbose
    expect_error(makeZStat(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeZStat(d,parallel=1))              # non-logical parallel
    expect_error(makeZStat(d,parallel=c(T, T)))         # multiple parallel values
    expect_error(makeZStat(d,FUN=1))                   # non-function FUN
    expect_error(makeZStat(d,FUN=c(mean, mean)))        # multiple FUN values
})

test_that("makeZStat computes Z means", {
    years <- 1850:1851
    d <- cmip5data(years, Z=T)
    res <- makeZStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res, "cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # Depth removed, and other info updated?
    expect_null(res$Z)
    expect_is(res$numZs, "integer")
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
    
    # Is the answer value array correctly sized?
    ndims <- length(dim(res$val))
    expect_equal(ndims, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[3], 1) #  all spatial dimensions should be 1
    expect_equal(dim(res$val)[c(1,2,4)], dim(d$val)[c(1,2,4)]) # time should match    
    
    # Are the answer values numerically correct?
    expect_equal(mean(res$val), mean(d$val))  # no weighting
})

test_that("makeZStat parallel results == serial result", {
    skip_on_cran()
    
    library(doParallel)
    registerDoParallel(cores=2)  # CRAN policy is 2 cores max
    years <- 1850:1851
    d <- cmip5data(years, Z=T, randomize=T)
    res_s <- makeZStat(d, verbose=F, parallel=F)
    res_p <- makeZStat(d, verbose=F, parallel=T)
    expect_equal(res_s$val, res_p$val)
    expect_equal(res_s$numZs, res_p$numZs)
})

test_that("makeZStat handles 3-dimensional data", {
    years <- 1850:1851
    d <- cmip5data(years, Z=F)
    expect_warning(makeZStat(d))
})
