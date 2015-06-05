# Testing code for the RCMIP5 'mergeExperiments.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("mergeExperiments.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_mergeExperiments.R")

context("mergeExperiments")

implementations <- c("data.frame", "array")

test_that("RCMIP5:::mergeExperiments.R handles bad input", {  
    for(i in implementations){
        x <- cmip5data(loadAs=i)
        expect_error(RCMIP5:::mergeExperiments())                             # non-cmip5data
        expect_error(RCMIP5:::mergeExperiments(1))                            # non-cmip5data
        expect_error(RCMIP5:::mergeExperiments(1, x))                         # non-cmip5data
        expect_error(RCMIP5:::mergeExperiments(x, 1))                         # non-cmip5data
        expect_error(RCMIP5:::mergeExperiments(x, x, verbose=1))              # non-logical verbose
        expect_error(RCMIP5:::mergeExperiments(x, x, verbose=c(T, T)))        # multiple verbose values
    }
})

test_that("RCMIP5:::mergeExperiments identifies ancillary problems", {
    for(i in implementations){
        y <- cmip5data(2, loadAs=i)
        
        x <- cmip5data(1, loadAs=i)
        x$domain <- paste0(y$domain, "x")
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        x$variable <- paste0(y$domain, "x")
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        x$model <- paste0(y$domain, "x")
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        x$valUnit <- paste0(y$valUnit, "x")
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        x$lat <- c(y$lat, 0)
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        x$lon <- c(y$lon, 0)
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, Z=T, loadAs=i)
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        x$ensembles <- paste0(y$ensembles, "x")
        expect_warning(RCMIP5:::mergeExperiments(x, y, verbose=F))
    }
})

test_that("RCMIP5:::mergeExperiments identifies time problems", {
    for(i in implementations){
        x <- cmip5data(1, loadAs=i)
        y <- cmip5data(2, loadAs=i)    
        x$debug$timeFreqStr <- paste0(y$debug$timeFreqStr, "x")
        expect_error(RCMIP5:::mergeExperiments(x, y, verbose=F))
        
        x <- cmip5data(1, loadAs=i)
        expect_error(RCMIP5:::mergeExperiments(x, x, verbose=F)) # identical times
        x$time[length(x$time)] <- mean(y$time[1:2]) 
        expect_error(RCMIP5:::mergeExperiments(x, x, verbose=F)) # identical times
        
        x <- cmip5data(4, loadAs=i)
        expect_warning(RCMIP5:::mergeExperiments(x, y, verbose=F)) # time gap
    }
})

test_that("RCMIP5:::mergeExperiments merges monthly data", {
    for(i in implementations){
        x <- cmip5data(1:5, randomize=TRUE, loadAs=i)
        y <- cmip5data(6:10, randomize=TRUE, loadAs=i)    
        res <- RCMIP5:::mergeExperiments(x, y, verbose=F)
        
        expect_equal(RCMIP5:::nvals(x) + RCMIP5:::nvals(y), RCMIP5:::nvals(res))
        expect_equal(length(x$time) + length(y$time), length(res$time))
        expect_equal(c(x$files, y$files), res$files)
        expect_true(grepl(x$experiment, res$experiment)) # each experiment name should appear
        expect_true(grepl(y$experiment, res$experiment)) # each experiment name should appear
        
        # Order of x and y should make no difference
        xy <- RCMIP5:::mergeExperiments(x, y, verbose=F)
        yx <- RCMIP5:::mergeExperiments(x, y, verbose=F)
        expect_equal(xy$val, yx$val, loadAs=i)
        expect_equal(xy$time, yx$time, loadAs=i)
    }
})

test_that("RCMIP5:::mergeExperiments merges annual data", {
    for(i in implementations){
        x <- cmip5data(1:5, monthly=F, randomize=TRUE, loadAs=i)
        y <- cmip5data(6:10, monthly=F, randomize=TRUE, loadAs=i)    
        res <- RCMIP5:::mergeExperiments(x, y, verbose=F)
        
        expect_equal(RCMIP5:::nvals(x) + RCMIP5:::nvals(y), RCMIP5:::nvals(res))
        expect_equal(length(x$time) + length(y$time), length(res$time))
    }
})

test_that("RCMIP5:::mergeExperiments merges 4-dimensional data", {
    for(i in implementations){
        x <- cmip5data(1:5, Z=T, randomize=TRUE, loadAs=i)
        y <- cmip5data(6:10, Z=T, randomize=TRUE, loadAs=i)    
        res <- RCMIP5:::mergeExperiments(x, y, verbose=F)
        
        expect_equal(RCMIP5:::nvals(x) + RCMIP5:::nvals(y), RCMIP5:::nvals(res))
        expect_equal(length(x$time) + length(y$time), length(res$time))
    }
})
