# Testing code for the RCMIP5 scripts in 'RCMIP5.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("RCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_internalHelpers.R")

context("cmip5data")

implementations <- c("data.frame", "array")

test_that("cmip5data handles bad input", {
    expect_error(cmip5data("hi"))   
    expect_error(cmip5data(1, monthly=123))   
    expect_error(cmip5data(1, Z=123))
    expect_error(cmip5data(1, lev=123))
    expect_error(cmip5data(1, randomize="hi"))   
})

# helper function: test basic structural integrity
structuretest <- function(d, i) {  # d is cmip5data object, i is info
    expect_is(d, "cmip5data")
    expect_false(xor(is.null(d$lon), is.null(d$lat)))  # both NULL, or both not
    if(!is.null(d$lon)) {
        expect_is(d$lon, "matrix", info=i)
        expect_is(d$lat, "matrix", info=i)        
    }
    expect_is(d$model, "character", info=i)
    expect_is(d$variable, "character", info=i)
    expect_is(d$experiment, "character", info=i)
    expect_is(d$valUnit, "character", info=i)
    expect_is(d$debug, "list", info=i)
    if(!is.null(d$time)) {
        expect_is(d$debug$timeFreqStr, "character", info=i)
        expect_is(d$debug$calendarStr, "character", info=i)
        expect_is(d$debug$timeUnit, "character", info=i)
    }
    
    if(is.data.frame(d$val)) {        # Data-frame specific tests
        expect_equal(ncol(d$val), 5, info=i)
        if(is.null(d$lon)) {
            expect_true(all(is.na(d$val$lon)))
            expect_true(all(is.na(d$val$lat)))
        }
        else {
            expect_equal(length(unique(d$val$lon)), length(unique(as.numeric(d$lon))), info=i)            
            expect_equal(length(unique(d$val$lat)), length(unique(as.numeric(d$lat))), info=i)            
        }
        if(is.null(d$Z)) {
            expect_true(all(is.na(d$val$Z)), info=i)            
        }
        else {
            expect_equal(length(unique(d$val$Z)), length(unique(as.numeric(d$Z))), info=i)            
        }
        if(is.null(d$time)) {
            expect_true(all(is.na(d$val$time)), info=i)            
        }
        else {
            expect_equal(length(unique(d$val$time)), length(unique(as.numeric(d$time))), info=i)
        }
    } else if(is.array(d$val)) {        # Array-frame specific tests
        dm <- dim(d$val)
        expect_equal(length(dm), 4, info=i)
        if(is.null(d$lon)) {
            expect_equal(dm[1], 1, info=i)
            expect_equal(dm[2], 1, info=i)
        } else {
            expect_equal(dm[1], length(unique(as.numeric(d$lon))), info=i)
            expect_equal(dm[2], length(unique(as.numeric(d$lat))), info=i)            
        }
        if(is.null(d$Z)) {
            expect_equal(dm[3], 1, info=i)            
        } else {
            expect_equal(dm[3], length(unique(as.numeric(d$Z))), info=i)
        }
        if(is.null(d$time)) {
            expect_equal(dm[4], 1, info=i)            
        } else {
            expect_equal(dm[4], length(unique(as.numeric(d$time))), info=i)
        }
    } else stop("Unknown val type")
}

test_that("cmip5data generates annual and monthly data", {
    for(i in implementations) {
        d <- cmip5data(1, loadAs=i)
        structuretest(d, paste("A1", i))
        expect_equal(length(dim(d$lon)), 2, info=i)
        expect_equal(length(dim(d$lat)), 2, info=i)
        expect_equal(length(d$time), 12, info=i)
        expect_equal(d$debug$timeFreqStr, "mon", info=i)
        
        d <- cmip5data(1, monthly=F, loadAs=i)
        structuretest(d, paste("A2", i))
        expect_equal(length(dim(d$lon)), 2, info=i)
        expect_equal(length(dim(d$lat)), 2, info=i)
        expect_equal(length(d$time), 1, info=i)
        expect_equal(d$debug$timeFreqStr, "yr", info=i)
    }
})

test_that("cmip5data obeys randomize", {
    for(i in implementations) {
        d1 <- cmip5data(1, randomize=T, loadAs=i)
        d2 <- cmip5data(1, randomize=T, loadAs=i)
        expect_false(all(RCMIP5:::vals(d1) == RCMIP5:::vals(d2)), info=i)
    }
})

test_that("cmip5data creates area-only data", {
    lonsize <- 10
    latsize <- 10
    
    for(i in implementations) {
        d <- cmip5data(1, lonsize=lonsize, latsize=latsize, time=F, verbose=F, loadAs=i)
        structuretest(d, paste("D1", i))
        expect_is(d$lon, "matrix", info=i)
        expect_is(d$lat, "matrix", info=i)
        expect_null(d$Z, info=i)
        expect_null(d$time, info=i)
    }
})

test_that("cmip5data creates area and Z data", {
    lonsize <- 10
    latsize <- 10
    Zsize <- 5
    
    for(i in implementations) {
        d <- cmip5data(1, lonsize=lonsize, latsize=latsize, Z=T, Zsize=Zsize, time=F, verbose=F, loadAs=i)    
        structuretest(d, paste("E1", i))
        expect_is(d$lon, "matrix", info=i)
        expect_is(d$lat, "matrix", info=i)
        expect_is(d$Z, "integer", info=i)
        expect_null(d$time, info=i) 
    }
})

test_that("cmip5data creates time-only data", {
    for(i in implementations) {
        d <- cmip5data(1, lonlat=F, Z=F, verbose=F, loadAs=i)
        structuretest(d, paste("F1", i))
        expect_null(d$lon, info=i)
        expect_null(d$lat, info=i)
        expect_null(d$Z, info=i)
        expect_is(d$time, "numeric", info=i)
    }
})
