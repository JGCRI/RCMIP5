# Testing code for the RCMIP5 'makeAnnualStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("makeAnnualStat.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_makeAnnualStat.R")

context("makeAnnualStat")

test_that("makeAnnualStat handles bad input", {
    expect_error(makeAnnualStat(1))                         # non-list d
    expect_error(makeAnnualStat(cmpi5data()))               # wrong size list d
    expect_error(makeAnnualStat(d,verbose=1))               # non-logical verbose
    expect_error(makeAnnualStat(d,verbose=c(F, F)))          # multiple verbose values
    expect_error(makeAnnualStat(d,FUN=1))                   # non-function FUN
    expect_error(makeAnnualStat(d,FUN=c(mean, mean)))        # multiple FUN values
})

test_that("makeAnnualStat handles monthly data", {
    years <- 1850:1851
    d <- cmip5data(years)
    res <- makeAnnualStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res,"cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$lon, d$lon)
    expect_equal(res$lat, d$lat)
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # numPerYear set and provenance updated?
    expect_is(res$numPerYear, "integer")
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
    
    # Do years match what we expect?
    expect_equal(res$time, years)
    
    # Is the answer value data frame correctly sized?
    expect_equal(nrow(res$val), nrow(d$val)/12)
    expect_equal(length(res$time), length(d$time)/12)
        
    # Are the answer values numerically correct?    
    d$val$time <- floor(d$val$time)
    dummyans <- aggregate(value~lon+lat+time, data=d, FUN=mean)
    expect_equal(dummyans$value, res$val$value)
})

test_that("makeAnnualStat handles annual data", {
    years <- 1850:1851
    d <- cmip5data(years, monthly=F)
    res <- makeAnnualStat(d, verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res, "cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$lon, d$lon)
    expect_equal(res$lat, d$lat)
    expect_equal(res$valUnit, d$valUnit)
    expect_equal(res$files, d$files)
    
    # Provenance updated?
    expect_more_than(nrow(res$provenance), nrow(d$provenance))
    
    # Do years match what we expect?
    expect_equal(res$time, years)
    
    # Is the answer value array correct (unchanged except for time)?
    expect_equal(d$val[c("lon", "lat", "Z", "value")], 
                 res$val[c("lon", "lat", "Z", "value")])
})

test_that("makeAnnualStat handles 4-dimensional data", {
    years <- 1850:1851
    d <- cmip5data(years, Z=T)
    res <- makeAnnualStat(d, verbose=F)
    
    # Do years match what we expect?
    expect_equal(res$time, years)
    
    # Is the answer value data frame correctly sized?
    expect_equal(nrow(res$val), nrow(d$val)/12)
    expect_equal(length(res$time), length(d$time)/12)
    
    # Are the answer values numerically correct?    
    d$val$time <- floor(d$val$time)
    dummyans <- aggregate(value~lon+lat+Z+time, data=d, FUN=mean)
    expect_equal(dummyans$value, res$val$value)
})

test_that("makeAnnualStat handles custom function and dots", {
    years <- 1850:1851
    d <- cmip5data(years, lonsize=2, latsize=2)
    
    # All data 1, except December, which is 2
    d$val$value <- 1
    d$val$value[round(d$val$time %% 1, 3)==.958] <- 2
    
    # Weights are all 1 except December, which is 11
    w <- c(rep(1, 11), 11)
    
    res1 <- makeAnnualStat(d, verbose=F, FUN=weighted.mean, w)
    expect_is(res1, "cmip5data")
    
    myfunc <- function(x, w, ...) weighted.mean(x, w, ...)
    res2 <- makeAnnualStat(d, verbose=F, FUN=myfunc, w)
    expect_is(res1, "cmip5data")
    
    # Are the answer values numerically correct?    
    # Eleven 1's and one 11, weighted by 1's and an 11 respectively
    # (11*1 + 11*2) / (11*1 + 1*11) = 1.5
    expect_true(all(res1$val$value == 1.5))    
    expect_true(all(res2$val$value == 1.5))    
})
