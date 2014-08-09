# Testing code for the RCMIP5 'makeMonthlyStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeMonthlyStat.R")
#   source("internalHelpers.R") # for dummyData
#   library(testthat)
#   test_file("tests/testthat/test_makeMonthlyStat.R")

context("makeMonthlyStat")

test_that("makeMonthlyStat handles bad input", {
    expect_error(makeMonthlyStat(1))                         # non-list d
    expect_error(makeMonthlyStat(cmpi5data()))               # wrong size list d
    expect_error(makeMonthlyStat(d,verbose=1))               # non-logical verbose
    expect_error(makeMonthlyStat(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeMonthlyStat(d,parallel=1))              # non-logical parallel
    expect_error(makeMonthlyStat(d,parallel=c(T, T)))         # multiple parallel values
    expect_error(makeMonthlyStat(d,FUN=1))                   # non-function FUN
    expect_error(makeMonthlyStat(d,FUN=c(mean, mean)))        # multiple FUN values
    
    d <- dummydata(1850)
    d$val <- array(1:2, dim=dim(d$val)-1)
    expect_error(makeMonthlyStat(d, verbose=F))        # corrupt value array
    
    # TODO: annual data should error
})

test_that("makeMonthlyStat handles monthly data", {
    years <- 1850:1851
    d <- dummydata(years)
    res <- makeMonthlyStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res,"cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$lon, d$lon)
    expect_equal(res$lat, d$lat)
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # numYears set and Provenance updated?
    expect_is(res$numYears, "table")
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

test_that("makeMonthlyStat parallel results == serial result", {
    years <- 1850:1851
    d <- dummydata(years)
    res_s <- makeMonthlyStat(d, verbose=F, parallel=F)
    res_p <- makeMonthlyStat(d, verbose=F, parallel=T)
    expect_equal(res_s$val, res_p$val)
    expect_equal(res_s$time, res_p$time)
    expect_equal(res_s$timeUnit, res_p$timeUnit)
    expect_equal(res_s$numMonths, res_p$numMonths)
})

test_that("makeAnnualStat handles annual data", {
    years <- 1850:1851
    d <- dummydata(years, monthly=F)
    expect_error(makeMonthlyStat(d, verbose=F))
})

test_that("makeMonthlyStat handles 4-dimensional data", {
    years <- 1850:1851
    d <- dummydata(years, depth=T)
    res <- makeMonthlyStat(d, verbose=F)
    
    # Do years match what we expect?
    expect_equal(res$time, 1:12)
    
    # Is the answer value array correctly sized?
    expect_equal(dim(res$val)[1:3], dim(d$val)[1:3])   # spatial size match
    expect_equal(dim(res$val)[4], 12)  # temporal size match
    
    # Same tests, but with lev
    d <- dummydata(years, lev=T)
    res <- makeMonthlyStat(d, verbose=F)
    expect_equal(res$time, 1:12)
    expect_equal(dim(res$val)[1:3], dim(d$val)[1:3])   # spatial size match
    expect_equal(dim(res$val)[4], 12)  # temporal size match
    
    # Don't know if this ever will occur, but need to handle lev AND depth
    d <- dummydata(years, depth=T, lev=T)
    res <- makeMonthlyStat(d, verbose=F)
    expect_equal(res$time, 1:12)
    expect_equal(dim(res$val)[1:4], dim(d$val)[1:4])   # spatial size match
    expect_equal(dim(res$val)[5], 12)  # temporal size match
})
