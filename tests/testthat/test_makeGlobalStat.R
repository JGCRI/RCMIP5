# Testing code for the RCMIP5 'makeGlobalStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
library(plyr)
library(abind)

# To run this code: 
#   source("makeGlobalStat.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_makeGlobalStat.R")

context("makeGlobalStat")

test_that("makeGlobalStat handles bad input", {
    expect_error(makeGlobalStat(1))                         # non-list d
    expect_error(makeGlobalStat(cmpi5data()))               # wrong size list d
    expect_error(makeGlobalStat(d,verbose=1))               # non-logical verbose
    expect_error(makeGlobalStat(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeGlobalStat(d,parallel=1))              # non-logical parallel
    expect_error(makeGlobalStat(d,parallel=c(T, T)))         # multiple parallel values
    expect_error(makeGlobalStat(d,FUN=1))                   # non-function FUN
    expect_error(makeGlobalStat(d,FUN=c(mean, mean)))        # multiple FUN values
})

test_that("makeGlobalStat handles monthly data", {
    years <- 1850:1851
    d <- cmip5data(years)
    res <- makeGlobalStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res,"cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # Lon/lat removed, numCells set, and provenance updated?
    expect_null(res$lon)
    expect_null(res$lat)
    expect_is(res$numCells, "integer")
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
    
    # Does time match what we expect?
    expect_equal(res$time, d$time)
    
    # Is the answer value array correctly sized?
    ndims <- length(dim(res$val))
    expect_equal(ndims, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[1:2], c(1, 1)) #  all spatial dimensions should be 1
    expect_equal(dim(res$val)[3:ndims], dim(d$val)[3:ndims]) # time should match    
    
    # Are the answer values numerically correct?
    expect_equal(mean(res$val), mean(d$val))  # no weighting
})

test_that("makeGlobalStat weights correctly", {
    d <- cmip5data(1850, randomize=T, monthly=F)
    darea <- cmip5data(0, randomize=T)

    res <- makeGlobalStat(d, area=darea, verbose=F)
    
    # Are the answer values numerically correct?
    dummyans <- weighted.mean(d$val[,,1], w=darea$val)
    expect_equal(dummyans, res$val[,,1])
})

test_that("weighted.sum works correctly", {
    d <- cmip5data(1850, randomize=T, monthly=F)
    darea <- cmip5data(0, randomize=T)
    res <- makeGlobalStat(d, area=darea, verbose=F, FUN=weighted.sum)
    
    # Are the answer values numerically correct?
    dummyans <- weighted.sum(d$val[,,1], w=darea$val)
    expect_equal(dummyans, res$val[,,1])
    
    # Make sure the function itself is OK
    expect_equal(weighted.sum(1:4), 10)
    expect_equal(weighted.sum(1:4, 1:4), 30) # 4*4 + 3*3 + 2*2 + 1*1
})

test_that("makeGlobalStat parallel results == serial result", {
    years <- 1850:1851
    d <- cmip5data(years, randomize=T)
    res_s <- makeGlobalStat(d, verbose=F, parallel=F)
    res_p <- makeGlobalStat(d, verbose=F, parallel=T)
    expect_equal(res_s$val, res_p$val)
    expect_equal(res_s$time, res_p$time)
    expect_equal(res_s$timeUnit, res_p$timeUnit)
    expect_equal(res_s$numMonths, res_p$numMonths)
})

test_that("makeGlobalStat handles 4-dimensional data", {
    years <- 1850:1851
    d <- cmip5data(years, depth=T)
    res <- makeGlobalStat(d, verbose=F)
    
    # Do years match what we expect?
    expect_equal(res$time, d$time)
    
    # Is the answer value array correctly sized?
    ndims <- length(dim(res$val))
    expect_equal(ndims, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[1:2], c(1, 1)) #  all spatial dimensions should be 1
    expect_equal(dim(res$val)[3:(ndims-1)], dim(d$val)[3:(ndims-1)]) # time should match    
    expect_equal(dim(res$val)[ndims], dim(d$val)[ndims]) # time should match    
    
    # Same tests, but with lev
    d <- cmip5data(years, lev=T)
    res <- makeGlobalStat(d, verbose=F)
    ndims <- length(dim(res$val))
    expect_equal(res$time, d$time)
    expect_equal(ndims, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[1:2], c(1, 1)) #  all spatial dimensions should be 1
    expect_equal(dim(res$val)[3:(ndims-1)], dim(d$val)[3:(ndims-1)]) # time should match    
    expect_equal(dim(res$val)[ndims], dim(d$val)[ndims]) # time should match    

    # lev and depth cannot both be present
    expect_error(makeAnnualStat(cmip5data(years, depth=T, lev=T), verbose=F))
})
