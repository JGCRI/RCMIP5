# Testing code for chained operations with the RCMIP5 'makeXxxxxStat' functions

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeAnnualStat.R") # ...and all other functions
#   library(testthat)
#   test_file("tests/testthat/test_makeAnnualStat.R")

context("Chained operations")

implementations <- c("data.frame") #, "array")

test_that("monthly data", {
    years <- 1851:1855
    ysize <- length(years)
    lsize <- 10
    
    for(i in implementations) {
        d <- cmip5data(years, monthly=T, lonsize=lsize, latsize=lsize, loadAs=i)
        expect_equal(nrow(d$val), prod(lsize, lsize, 1, ysize*12), info=i)
        
        # filter, annual stat, global stat
        d10 <- filterDimensions(d, yearRange=range(years[-1]), verbose=F)
        expect_equal(nrow(d10$val), prod(lsize, lsize, 1, (ysize-1)*12), info=i)
        d11 <- makeAnnualStat(d10, verbose=F)
        expect_equal(nrow(d11$val), prod(lsize, lsize, 1, ysize-1), info=i)
        d12 <- makeGlobalStat(d11, verbose=F)
        expect_equal(nrow(d12$val), prod(1, 1, 1, ysize-1), info=i)
        
        # filter, global stat, annual stat
        d20 <- filterDimensions(d, yearRange=range(years[-1]), verbose=F)
        expect_equal(nrow(d20$val), prod(lsize, lsize, 1, (ysize-1)*12), info=i)
        d21 <- makeGlobalStat(d20, verbose=F)
        expect_equal(nrow(d21$val), prod(1, 1, 1, (ysize-1)*12), info=i)
        d22 <- makeAnnualStat(d21, verbose=F)
        expect_equal(nrow(d22$val), prod(1, 1, 1, (ysize-1)), info=i)
        
        # order of operations shouldn't matter
        expect_equal(d12$val, d22$val, info=i)
        if(exists("last_d12")) {  # ...nor should implementation
            expect_equal(as.data.frame(d12), last_d12)
        }
        last_d12 <- as.data.frame(d12)
        
        # filter, monthly stat, global stat
        d30 <- filterDimensions(d, yearRange=range(years[-1]), verbose=F)
        d31 <- makeMonthlyStat(d30, verbose=F)
        expect_equal(nrow(d31$val), prod(lsize, lsize, 1, 12), info=i)
        d32 <- makeGlobalStat(d31, verbose=F)
        expect_equal(nrow(d32$val), prod(1, 1, 1, 12), info=i)
        
        # filter, global stat, annual stat
        d40 <- filterDimensions(d, yearRange=range(years[-1]), verbose=F)
        d41 <- makeGlobalStat(d40, verbose=F)
        expect_equal(nrow(d41$val), prod(1, 1, 1, (ysize-1)*12), info=i)
        d42 <- makeMonthlyStat(d41, verbose=F)
        expect_equal(nrow(d42$val), prod(1, 1, 1, 12), info=i)
        
        # order of operations shouldn't matter
        expect_equal(d32$val, d42$val, info=i)
        if(exists("last_d32")) {  # ...nor should implementation
            expect_equal(as.data.frame(d32), last_d32)
        }
        last_d32 <- as.data.frame(d32)
    }
})

test_that("annual data", {
    years <- 1851:1855
    ysize <- length(years)
    lsize <- 10
    
    for(i in implementations) {
        d <- cmip5data(years, monthly=F, lonsize=lsize, latsize=lsize)
        expect_equal(nrow(d$val), prod(lsize, lsize, 1, ysize), info=i)
        
        # filter, annual stat, global stat
        d10 <- filterDimensions(d, yearRange=range(years[-1]), verbose=F)
        expect_equal(nrow(d10$val), prod(lsize, lsize, 1, ysize-1), info=i)
        d11 <- makeAnnualStat(d10, verbose=F)
        expect_equal(nrow(d11$val), prod(lsize, lsize, 1, ysize-1), info=i)
        d12 <- makeGlobalStat(d11, verbose=F)
        expect_equal(nrow(d12$val), prod(1, 1, 1, ysize-1), info=i)
        
        # filter, global stat, annual stat
        d20 <- filterDimensions(d, yearRange=range(years[-1]), verbose=F)
        expect_equal(nrow(d20$val), prod(lsize, lsize, 1, ysize-1), info=i)
        d21 <- makeGlobalStat(d20, verbose=F)
        expect_equal(nrow(d21$val), prod(1, 1, 1, ysize-1), info=i)
        d22 <- makeAnnualStat(d21, verbose=F)
        expect_equal(nrow(d22$val), prod(1, 1, 1, ysize-1), info=i)
        
        # order of operations shouldn't matter
        expect_equal(d12$val, d22$val, info=i)
        if(exists("last_d12")) {  # ...nor should implementation
            expect_equal(as.data.frame(d12), last_d12)
        }
        last_d12 <- as.data.frame(d12)
    }
})

test_that("four-D data", {
    years <- 1851:1855
    ysize <- length(years)
    lsize <- 10
    zsize <- 5
    
    for(i in implementations) {
        d <- cmip5data(years, monthly=T, lonsize=lsize, latsize=lsize, Z=T, Zsize=zsize)
        expect_equal(nrow(d$val), prod(lsize, lsize, zsize, ysize*12), info=i)
        
        # filter, annual stat, global stat
        d10 <- filterDimensions(d, ZRange=range(d$Z[-1]), verbose=F)
        expect_equal(nrow(d10$val), prod(lsize, lsize, zsize-1, ysize*12), info=i)
        d11 <- makeAnnualStat(d10, verbose=F)
        expect_equal(nrow(d11$val), prod(lsize, lsize, zsize-1, ysize), info=i)
        d12 <- makeGlobalStat(d11, verbose=F)
        expect_equal(nrow(d12$val), prod(1, 1, zsize-1, ysize), info=i)
        
        # filter, global stat, annual stat
        d20 <- filterDimensions(d, ZRange=range(d$Z[-1]), verbose=F)
        expect_equal(nrow(d20$val), prod(lsize, lsize, zsize-1, ysize*12), info=i)
        d21 <- makeGlobalStat(d20, verbose=F)
        expect_equal(nrow(d21$val), prod(1, 1, zsize-1, ysize*12), info=i)
        d22 <- makeAnnualStat(d21, verbose=F)
        expect_equal(nrow(d22$val), prod(1, 1, zsize-1, ysize), info=i)
        
        # order of operations shouldn't matter
        expect_equal(d12$val, d22$val, info=i)
        if(exists("last_d12")) {  # ...nor should implementation
            expect_equal(as.data.frame(d12), last_d12)
        }
        last_d12 <- as.data.frame(d12)
        
        # filter, monthly stat, global stat
        d30 <- filterDimensions(d, ZRange=range(d$Z[-1]), verbose=F)    
        expect_equal(nrow(d30$val), prod(lsize, lsize, zsize-1, ysize*12), info=i)    
        d31 <- makeMonthlyStat(d30, verbose=F)
        expect_equal(nrow(d31$val), prod(lsize, lsize, zsize-1, 12), info=i)
        d32 <- makeZStat(d31, verbose=F)
        expect_equal(nrow(d32$val), prod(lsize, lsize, 1, 12), info=i)
        d33 <- makeGlobalStat(d32, verbose=F)
        expect_equal(nrow(d33$val), prod(1, 1, 1, 12), info=i)
        
        # filter, global stat, annual stat
        d40 <- filterDimensions(d, ZRange=range(d$Z[-1]), verbose=F)    
        expect_equal(nrow(d40$val), prod(lsize, lsize, zsize-1, ysize*12), info=i)    
        d41 <- makeGlobalStat(d40, verbose=F)
        expect_equal(nrow(d41$val), prod(1, 1, zsize-1, ysize*12), info=i)        
        d42 <- makeZStat(d41, verbose=F)
        expect_equal(nrow(d42$val), prod(1, 1, 1, ysize*12), info=i)
        d43 <- makeMonthlyStat(d42, verbose=F)
        expect_equal(nrow(d43$val), prod(1, 1, 1, 12), info=i)
        
        # order of operations shouldn't matter
        expect_equal(d33$val, d43$val, info=i)
        if(exists("last_d33")) {  # ...nor should implementation
            expect_equal(as.data.frame(d33), last_d33)
        }
        last_d12 <- as.data.frame(d33)
    }
})
