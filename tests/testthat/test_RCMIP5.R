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
    expect_is(as.data.frame(cmip5data(2000:2005)), "data.frame")
    expect_is(as.data.frame(cmip5data(2000:2005), depth=T), "data.frame")
    expect_is(as.data.frame(cmip5data(2000:2005), lev=T), "data.frame")
    
    d <- makeAnnualStat(cmip5data(2000:2005), verbose=F)
    expect_is(as.data.frame(d), "data.frame")
    d <- makeGlobalStat(cmip5data(2000:2005), verbose=F)
    expect_is(as.data.frame(d), "data.frame")
    d <- makeMonthlyStat(cmip5data(2000:2005), verbose=F)
    expect_is(as.data.frame(d), "data.frame")
    d <- makeDepthLevStat(cmip5data(2000:2005, depth=T), verbose=F)
    expect_is(as.data.frame(d), "data.frame")
})


