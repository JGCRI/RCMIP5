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

implementations <- c("data.frame", "array")

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
    
    for(i in implementations) {
        
        d <- cmip5data(years, randomize=TRUE, loadAs=i)
        res <- makeAnnualStat(d, verbose=F)
        
        # Is 'res' correct type and size?
        expect_is(res,"cmip5data", info=i)
        
        # Did unchanging info get copied correctly?
        expect_equal(res$lon, d$lon, info=i)
        expect_equal(res$lat, d$lat, info=i)
        expect_equal(res$valUnit, d$valUnit, info=i)
        expect_equal(res$files, d$files, info=i)
        
        # numPerYear set and provenance updated?
        expect_is(res$numPerYear, "integer") #, info=i)
        expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=i)
        
        # Do years match what we expect?
        expect_equal(res$time, years, info=i)
        
        # Is the answer data correctly sized?
        expect_equal(RCMIP5:::nvals(res), RCMIP5:::nvals(d)/12, info=i)
        expect_equal(length(res$time), length(d$time)/12, info=i)
        
        # Are the answer values numerically correct?    
        ref <- as.data.frame(d)
        ref$time <- floor(ref$time)
        refans <- aggregate(value~lon+lat+time, data=ref, FUN=mean)
        refans <- merge(refans, as.data.frame(res), by=c('lon', 'lat', 'time'))
        expect_equal(refans$value.x, refans$value.y, info=i)
    }
})

test_that("makeAnnualStat handles annual data", {
    years <- 1850:1851
    
    for(i in implementations) {
        
        d <- cmip5data(years, monthly=F, randomize=TRUE, loadAs=i)
        res <- makeAnnualStat(d, verbose=F)
        
        # Is 'res' correct type and size?
        expect_is(res, "cmip5data", info=i)
        
        # Did unchanging info get copied correctly?
        expect_equal(res$lon, d$lon, info=i)
        expect_equal(res$lat, d$lat, info=i)
        expect_equal(res$valUnit, d$valUnit, info=i)
        expect_equal(res$files, d$files, info=i)
        
        # Provenance updated?
        expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=i)
        
        # Do years match what we expect?
        expect_equal(res$time, years, info=i)
        
        # Are the answer values numerically correct?    
        ref <- as.data.frame(d)
        ref$time <- floor(ref$time)
        refans <- aggregate(value~lon+lat+time, data=ref, FUN=mean)
        refans <- merge(refans, as.data.frame(res), by=c('lon', 'lat', 'time'))
        expect_equal(refans$value.x, refans$value.y, info=i)
    }
})

test_that("makeAnnualStat handles 4-dimensional data", {
    years <- 1850:1851
    
    for(i in implementations) {
        d <- cmip5data(years, Z=T, randomize=TRUE, loadAs=i)
        res <- makeAnnualStat(d, verbose=F)
        
        # Do years match what we expect?
        expect_equal(res$time, years, info=i)
        
        # Is the answer value data correctly sized?
        expect_equal(RCMIP5:::nvals(res), RCMIP5:::nvals(d)/12, info=i)
        expect_equal(length(res$time), length(d$time)/12, info=i)
        
        # Are the answer values numerically correct?    
        ref <- as.data.frame(d)
        ref$time <- floor(ref$time)
        refans <- aggregate(value~lon+lat+Z+time, data=ref, FUN=mean)
        refans <- merge(refans, as.data.frame(res), by=c('lon', 'lat', 'Z', 'time'))
        expect_equal(refans$value.x, refans$value.y, info=i)
        
    }
})

test_that("makeAnnualStat handles custom function and dots", {
    years <- 1850:1851
    llsize <- 2
    
    for(i in implementations) {
        d <- cmip5data(years, lonsize=llsize, latsize=llsize, loadAs=i)
        
        # All data 1, except December, which is 2
        if(is.data.frame(d$val)) {
            d$val$value <- 1
            d$val$value[round(d$val$time %% 1, 3)==.958] <- 2        
        } else if(is.array(d$val)) {
            d$val <- array(1, dim=dim(d$val))
            d$val[,,,round(d$time %% 1, 3)==.958] <- 2                
        }
        
        # Weights are all 1 except December, which is 11
        w <- c(rep(1, 11), 11)
        
        # Compute correct answer
        ref <- cmip5data(years, lonsize=llsize, latsize=llsize, loadAs="data.frame")
        ref$val$value <- 1
        ref$val$value[round(ref$val$time %% 1, 3)==.958] <- 2        
        ref$val$year <- floor(ref$val$time)
        ans <- aggregate(value~lon+lat+year, data=ref$val, FUN=weighted.mean, w=w)
        
        res1 <- makeAnnualStat(d, verbose=F, sortData=F, filterNum=T, FUN=weighted.mean, w)
        expect_is(res1, "cmip5data", info=i)
        expect_is(res1$val, i, info=i)
        
        myfunc <- function(x, w, ...) weighted.mean(x, w, ...)
        res2 <- makeAnnualStat(d, verbose=F, sortData=F, filterNum=T, FUN=myfunc, w)
        expect_is(res2, "cmip5data", info=i)
        expect_is(res2$val, i, info=i)
        
        # Are the result values correct?    
        expect_equal(RCMIP5:::vals(res1), ans$value, info=i)
        expect_equal(RCMIP5:::vals(res2), ans$value, info=i)
    }
})

test_that("makeAnnualStat computes numPerYear correctly", {
    years <- 1850:1851
    
    for(i in implementations) {
        d <- cmip5data(years, loadAs=i)
        
        res1 <- makeAnnualStat(d)
        expect_equal(res1$numPerYear, c(12, 12), info=i)
        
        # Get rid of first month. makeAnnualStat should drop this month when completeYears is TRUE
        if(i == "data.frame") {  # can't really run this test with arrays TODO: KTB?
            d$val <- d$val[d$val$time != d$val$time[1],] 
            res2 <- makeAnnualStat(d, filterNum=TRUE)
            expect_equal(res2$numPerYear, c(12), info=i)
            res3 <- makeAnnualStat(d, filterNum=FALSE)
            expect_equal(res3$numPerYear, c(11, 12), info=i)            
        }
    }
})
