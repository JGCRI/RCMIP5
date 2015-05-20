# Testing code for the RCMIP5 'makeZStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("makeZStat.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_makeZStat.R")

context("makeZStat")

implementations <- c("data.frame") #, "array")

test_that("makeZStat handles bad input", {
    expect_error(makeZStat(1))                         # non-list d
    expect_error(makeZStat(cmpi5data()))               # wrong size list d
    expect_error(makeZStat(d,verbose=1))               # non-logical verbose
    expect_error(makeZStat(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeZStat(d,FUN=1))                   # non-function FUN
    expect_error(makeZStat(d,FUN=c(mean, mean)))        # multiple FUN values
})

test_that("makeZStat computes Z means", {
    years <- 1850:1851
    
    for(i in implementations) {
        d <- cmip5data(years, Z=T, loadAs=i)
        res <- makeZStat(d, verbose=F)
        
        # Is 'res' correct type and size?
        expect_is(res, "cmip5data", info=i)
        
        # Did unchanging info get copied correctly?
        expect_equal(res$valUnit, d$valUnit, info=i)
        expect_equal(res$files, d$files, info=i)
        expect_equal(res$lon, d$lon, info=i)
        expect_equal(res$lat, d$lat, info=i)
        expect_equal(res$time, d$time, info=i)
        
        # Depth removed, and other info updated?
        expect_null(res$Z, info=i)
        expect_is(res$numZs, "integer", info=i)
        expect_more_than(nrow(res$provenance), nrow(d$provenance), info=i)
        
        # Is the answer value array correctly sized?
        expect_equal(RCMIP5:::nvals(res), RCMIP5:::nvals(d)/length(d$Z), info=i)
        
        # Are the answer values numerically correct?
        expect_equal(mean(RCMIP5:::vals(res)), mean(RCMIP5:::vals(d)), info=i)
    }
})

test_that("makeZStat handles custom function and dots", {
    years <- 1850:1851
    llsize <- 2
    
    for(i in implementations) {
        d <- cmip5data(years, lonsize=llsize, latsize=llsize, Z=T, loadAs=i)
        
        # All data 1, except max Z is 2
        if(is.data.frame(d$val)) {
            d$val$value <- 1
            d$val$value[d$val$Z ==max(d$Z)] <- 2                
        } else if(is.array(d$val)) {
            d$val <- array(1, dim=dim(d$val))
            d$val[,,length(d$Z),] <- 2          
        }
        w <- c(rep(1, length(d$Z)-1), length(d$Z)-1)
        
        # Compute correct answer
        ans <- aggregate(value~lon+lat+time, data=d$val, FUN=weighted.mean, w=w)
        
        res1 <- makeZStat(d, verbose=F, sortData=F, FUN=weighted.mean, w)
        expect_is(res1, "cmip5data", info=i)
        
        myfunc <- function(x, w, ...) weighted.mean(x, w, ...)
        res2 <- makeZStat(d, verbose=F, sortData=F, FUN=myfunc, w)
        expect_is(res1, "cmip5data", info=i)
        
        # Are the result values correct?    
        expect_equal(RCMIP5:::vals(res1), ans$value, info=i)
        expect_equal(RCMIP5:::vals(res2), ans$value, info=i)
    }
})
