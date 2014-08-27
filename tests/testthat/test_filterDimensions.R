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

test_that("filterDimensions filters lon", {
    d <- cmip5data(1)
    d$lon <- NULL
    expect_warning(filterDimensions(d, lons=1))
    
    d <- cmip5data(1)
    lf <- d$lon[1:(length(d$lon)-1)] # the filter
    res <- filterDimensions(d, lons=lf)
    expect_equal(res$lon, lf)
    expect_true(dim(res$val)[1] == dim(d$val)[1]-1)  # stripped off one lon
    expect_more_than(length(res$provenance), length(d$provenance))     
})

test_that("filterDimensions filters lat", {
    d <- cmip5data(1)
    d$lat <- NULL
    expect_warning(filterDimensions(d, lats=1))
    
    d <- cmip5data(1)
    lf <- d$lat[1:(length(d$lat)-1)] # the filter
    res <- filterDimensions(d, lats=lf)
    expect_equal(res$lat, lf)
    expect_true(dim(res$val)[2] == dim(d$val)[2]-1)  # stripped off one lat
    expect_more_than(length(res$provenance), length(d$provenance))     
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
    
    # Annual data
    d <- cmip5data(1:5, monthly=F)
    d$time <- NULL
    expect_warning(filterDimensions(d, years=1))
    
    d <- cmip5data(1:5, monthly=F)
    yf <- d$time[1:(length(d$time)-1)] # the filter
    res <- filterDimensions(d, years=yf)
    expect_equal(res$time, yf)                                      # only years in filter
    expect_true(dim(res$val)[3] == dim(d$val)[3]-1)                 # stripped off one year
    expect_more_than(length(res$provenance), length(d$provenance))  # provenance bigger
    
    # Monthly data
    d <- cmip5data(1:5)
    yf <- d$time[1:(length(d$time)/2)] # the filter
    res <- filterDimensions(d, years=yf)
    expect_equal(unique(floor(res$time)), unique(floor(yf)))        # only years in filter
    expect_more_than(dim(d$val)[3], dim(res$val)[3])                # data should be smaller
    expect_more_than(length(res$provenance), length(d$provenance))  # and provenance bigger
    
})

test_that("filterDimensions filters time (months)", {
    
    # Annual data
    d <- cmip5data(1:5, monthly=F)
    expect_warning(filterDimensions(d, months=1))                   # monthly data required
    
    # Monthly data
    d <- cmip5data(1:5)
    mf <- 1:2
    res <- filterDimensions(d, months=mf)
    expect_equal(length(unique(round(res$time %% 1, 2))), length(mf))  # only months in filter
    expect_more_than(dim(d$val)[3], dim(res$val)[3])                # data should be smaller
    expect_more_than(length(res$provenance), length(d$provenance))  # and provenance bigger
   
    expect_error(filterDimensions(d, months=13))                    # illegal months value
})

test_that("filterDimensions handles multiple operations", {
    d <- cmip5data(1:5, depth=T)
    res <- filterDimensions(d, lons=d$lon[1], lats=d$lat[1], 
                            depths=d$depth[1], years=d$time[1])
    expect_equal(dim(res$val)[1:3], c(1, 1, 1))
})
