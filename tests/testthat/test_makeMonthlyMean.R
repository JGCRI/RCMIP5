# TODO: test new level-aware code!

# Testing code for the RCMIP5 'makeMonthlyMean.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeMonthlyMean.R")
#   source("internalHelpers.R") # for dummyData
#   library(testthat)
#   test_file("tests/testthat/test_makeMonthlyMean.R")

context("makeMonthlyMean")

test_that("makeMonthlyMean handles bad input", {
    expect_error(makeMonthlyMean(1))                         # non-list d
    expect_error(makeMonthlyMean(cmpi5data()))               # wrong size list d
    expect_error(makeMonthlyMean(d,yearRange=1))             # non-vector yearRange
    expect_error(makeMonthlyMean(d,yearRange=c(-1, 1)))       # illegal value yearRange
    expect_error(makeMonthlyMean(d,yearRange=c(1, 2, 3)))      # wrong vector size yearRange
    expect_error(makeMonthlyMean(d,verbose=1))               # non-logical verbose
    expect_error(makeMonthlyMean(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeMonthlyMean(d,parallel=1))              # non-logical parallel
    expect_error(makeMonthlyMean(d,parallel=c(T, T)))         # multiple parallel values
    expect_error(makeMonthlyMean(d,FUN=1))                   # non-function FUN
    expect_error(makeMonthlyMean(d,FUN=c(mean, mean)))        # multiple FUN values
    
    d <- dummydata(1850)
    d$val <- array(1:2, dim=dim(d$val)-1)
    expect_error(makeMonthlyMean(d, verbose=F))        # corrupt value array
    
    # TODO: annual data should error
})

test_that("makeMonthlyMean handles monthly data", {
    years <- 1850:1851
    d <- dummydata(years)
    res <- makeMonthlyMean(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res,"cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$lon, d$lon)
    expect_equal(res$lat, d$lat)
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # Provenance updated?
    expect_more_than(length(res$provenance), length(d$provenance))
    
    # Does time match what we expect?
    expect_equal(res$time, 1:12)
    
    # Is the answer value array correctly sized?
    expect_equal(dim(res$val)[1:2], dim(d$val)[1:2])   # spatial size match
    expect_equal(dim(res$val)[length(dim(res$val))], 12)  # temporal size match
    
    # Are the answer values numerically correct?
    ans <- list()
    monthIndex <- floor((d$time %% 1) * 12 + 1)
    for(i in 1:12) {
        ans[[i]] <- aaply(asub(d$val, idx=(i == monthIndex), dims=3), c(1:2), mean)
    }
    dummyans <- unname(abind(ans, along=3))
    expect_equal(res$val, dummyans)
})

test_that("makeMonthlyMean parallel results == serial result", {
    years <- 1850:1851
    d <- dummydata(years)
    res_s <- makeMonthlyMean(d, verbose=F, parallel=F)
    res_p <- makeMonthlyMean(d, verbose=F, parallel=T)
    expect_equal(res_s$val, res_p$val)
    expect_equal(res_s$time, res_p$time)
    expect_equal(res_s$timeUnit, res_p$timeUnit)
    expect_equal(res_s$numMonths, res_p$numMonths)
})

test_that("makeMonthlyMean handles 4-dimensional data", {
    years <- 1850:1851
    d <- dummydata(years, depth=T)
    res <- makeMonthlyMean(d, verbose=F)
    
    # Do years match what we expect?
    expect_equal(res$time, 1:12)
    
    # Is the answer value array correctly sized?
    expect_equal(dim(res$val)[1:3], dim(d$val)[1:3])   # spatial size match
    expect_equal(dim(res$val)[4], 12)  # temporal size match
    
    # Same tests, but with lev
    d <- dummydata(years, lev=T)
    res <- makeMonthlyMean(d, verbose=F)
    expect_equal(res$time, 1:12)
    expect_equal(dim(res$val)[1:3], dim(d$val)[1:3])   # spatial size match
    expect_equal(dim(res$val)[4], 12)  # temporal size match
    
    # Don't know if this ever will occur, but need to handle lev AND depth
    d <- dummydata(years, depth=T, lev=T)
    res <- makeMonthlyMean(d, verbose=F)
    expect_equal(res$time, 1:12)
    expect_equal(dim(res$val)[1:4], dim(d$val)[1:4])   # spatial size match
    expect_equal(dim(res$val)[5], 12)  # temporal size match
})
