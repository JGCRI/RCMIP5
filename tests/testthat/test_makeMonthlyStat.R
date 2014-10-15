# Testing code for the RCMIP5 'makeMonthlyStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeMonthlyStat.R")
#   source("RCMIP5.R") # for cmip5data
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
    
    d <- cmip5data(1850)
    d$val <- array(1:2, dim=dim(d$val)-1)
    expect_error(makeMonthlyStat(d, verbose=F))        # corrupt value array
})

test_that("makeMonthlyStat handles monthly data", {
    years <- 1850:1851
    d <- cmip5data(years)
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
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
    
    # Does time match what we expect?
    expect_equal(res$time, 1:12)
    
    # Is the answer value array correctly sized?
    dl <- length(dim(res$val))
    expect_equal(dl, length(dim(d$val)))  # same number of dimensions
    expect_equal(dim(res$val)[1:2], dim(d$val)[1:2])   # spatial size should match
    expect_equal(dim(res$val)[dl], 12)  # temporal size set to # of months
    
    # Are the answer values numerically correct?
    ans <- list()
    monthIndex <- floor((d$time %% 1) * 12 + 1)
    for(i in 1:12) {
        ans[[i]] <- aaply(asub(d$val, idx=(i == monthIndex), dims=4, drop=F), c(1:3), mean, .drop=F)
    }
    dummyans <- unname(abind(ans, along=4))
    expect_equal(res$val, dummyans)
})

test_that("makeMonthlyStat parallel results == serial result", {
    library(doParallel)
    registerDoParallel(cores=2)  # CRAN policy is 2 cores max
    years <- 1850:1851
    d <- cmip5data(years)
    res_s <- makeMonthlyStat(d, verbose=F, parallel=F)
    res_p <- makeMonthlyStat(d, verbose=F, parallel=T)
    expect_equal(res_s$val, res_p$val)
    expect_equal(res_s$time, res_p$time)
    expect_equal(res_s$timeUnit, res_p$timeUnit)
    expect_equal(res_s$numMonths, res_p$numMonths)
})

test_that("makeMonthlyStat handles annual data", {
    years <- 1850:1851
    d <- cmip5data(years, monthly=F)
    expect_error(makeMonthlyStat(d, verbose=F))
})

test_that("makeMonthlyStat handles 4-dimensional data", {
    years <- 1850:1851
    d <- cmip5data(years, Z=T)
    res <- makeMonthlyStat(d, verbose=F)
    
    # Do years match what we expect?
    expect_equal(res$time, 1:12)
    
    # Is the answer value array correctly sized?
    expect_equal(dim(res$val)[1:3], dim(d$val)[1:3])   # spatial size match
    expect_equal(dim(res$val)[4], 12)  # temporal size match
})

test_that("makeMonthlyStat handles custom function", {
    years <- 1850:1851
    d <- cmip5data(years)
    
    res <- makeMonthlyStat(d, verbose=F, FUN=function(x) {
        a <- mean(x)
        b <- a
        b
    }
    )
    
    # Do years match what we expect?
    expect_equal(res$time, 1:12)
})
