# Testing code for the RCMIP5 'filterDimensions.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   library(testthat)
#   source("filterDimensions.R")
#   source("RCMIP5.R") # for cmip5data
#   test_file("tests/testthat/test_filterDimensions.R")
    
context("filterDimensions")

test_that("filterDimensions handles bad input", {
    expect_error(filterDimensions(1))                       # non-cmip5data x
    d <- cmip5data(1)
    expect_error(filterDimensions(d, lons='1'))              # non-numeric lons
    expect_error(filterDimensions(d, lats='1'))              # non-numeric lats
    expect_error(filterDimensions(d, depths='1'))              # non-numeric depths
    expect_error(filterDimensions(d, levs='1'))              # non-numeric levs
    expect_error(filterDimensions(d, years='1'))              # non-numeric years
    expect_error(filterDimensions(d, months='1'))              # non-numeric months
    expect_error(filterDimensions(d, verbose=1))            # non-logical verbose
    expect_error(filterDimensions(d, verbose=c(T, T)))      # multiple verbose values
})

test_that("filterDimensions filters depth", {
    expect_warning(filterDimensions(cmip5data(1), depths=1))

    d <- cmip5data(1, depth=T)
    df <- d$depth[1:(length(d$depth)-1)] # the filter
    res <- filterDimensions(d, depths=df)
    expect_equal(res$depth, df)
    expect_true(dim(res$val)[3] == dim(d$val)[3]-1)  # stripped off one depth
    expect_more_than(length(res$provenance), length(d$provenance))     
})

test_that("filterDimensions filters lev", {
    expect_warning(filterDimensions(cmip5data(1), levs=1))
    
    d <- cmip5data(1, lev=T)
    lf <- d$lev[1:(length(d$lev)-1)] # the filter
    res <- filterDimensions(d, levs=lf)
    expect_equal(res$lev, lf)
    expect_true(dim(res$val)[3] == dim(d$val)[3]-1)  # stripped off one lev
    expect_more_than(length(res$provenance), length(d$provenance))     
})

test_that("filterDimensions filters time (years)", {
    d <- cmip5data(1:5, monthly=F)
    d$time <- NULL
    expect_warning(filterDimensions(d, years=1))
    
    d <- cmip5data(1:5, monthly=F)
    yf <- d$time[1:(length(d$time)-1)] # the filter
    res <- filterDimensions(d, years=yf)
    expect_equal(res$time, yf)
    expect_true(dim(res$val)[3] == dim(d$val)[3]-1)  # stripped off one year
    expect_more_than(length(res$provenance), length(d$provenance))     
})
