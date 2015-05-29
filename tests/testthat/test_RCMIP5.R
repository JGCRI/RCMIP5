# Testing code for the RCMIP5 scripts in 'RCMIP5.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("RCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_RCMIP5.R")

context("RCMIP5")

implementations <- c("data.frame", "array")

test_that("cmip5data print method works", {
    for(i in implementations) {
        d <- cmip5data(2000:2005, loadAs=i)
        expect_output(print(d), "CMIP5", info=i)
        # not sure what to test here, except that no error
    }
})

test_that("cmip5data summary method detects summaries", {
i <- "data.frame"
    #    for(i in implementations)  TODO
{
        d <- cmip5data(2000:2005, Z=T, loadAs=i)
        expect_output(print(summary(d)), "CMIP5")
        
        # Summary should let user know data have been run through stat fn
        da <- makeAnnualStat(d)
        expect_output(print(summary(da)), "annual summary", info=i)
        dm <- makeMonthlyStat(d)
        expect_output(print(summary(dm)), "monthly summary", info=i)
        dz <- makeZStat(d)
        expect_output(print(summary(dz)), "Z summary", info=i)
        dg <- makeGlobalStat(d)
        expect_output(print(summary(dg)), "spatial summary", info=i)
        
        # Multiple stat functions should be detected
        dag <- makeGlobalStat(da)
        expect_output(print(summary(dag)), "annual summary", info=i)
        expect_output(print(summary(dag)), "spatial summary", info=i)
        daz <- makeZStat(da)
        expect_output(print(summary(daz)), "annual summary", info=i)
        expect_output(print(summary(daz)), "Z summary", info=i)
        dmg <- makeGlobalStat(dm)
        expect_output(print(summary(dmg)), "monthly summary", info=i)
        expect_output(print(summary(dmg)), "spatial summary", info=i)
        dmz <- makeZStat(dm)
        expect_output(print(summary(dmz)), "monthly summary", info=i)
        expect_output(print(summary(dmz)), "Z summary", info=i)
        
        # All filter functions should be detected
        df <- filterDimensions(d, yearRange=floor(range(d$time)))
        expect_output(print(summary(df)), "filtered", info=i)
        df <- filterDimensions(d, monthRange=c(1,2))
        expect_output(print(summary(df)), "filtered", info=i)
        df <- filterDimensions(d, lonRange=range(d$lon))
        expect_output(print(summary(df)), "filtered", info=i)
        df <- filterDimensions(d, latRange=range(d$lat))
        expect_output(print(summary(df)), "filtered", info=i)
        df <- filterDimensions(d, ZRange=range(d$Z))
        expect_output(print(summary(df)), "filtered", info=i)
    }
})

test_that("as.data.frame works", {
    for(i in implementations) {
        df <- as.data.frame(cmip5data(2000:2002, Z=T, loadAs=i))
        expect_is(df, "data.frame", info=i)
        expect_equal(names(df), c("lon", "lat", "Z", "time", "value"), info=i)
    }
})

test_that("as.array works", {
    for(i in implementations) {
        arr <- as.array(cmip5data(2000:2002, Z=T))
        expect_is(arr, "array", info=i)
        expect_equal(dim(arr), c(10, 10, 5, 36), info=i)
        
        arr <- as.array(cmip5data(2000:2002), info=i)
        expect_is(arr, "array", info=i)
        expect_equal(dim(arr), c(10, 10, 36), info=i)
        
        arr <- as.array(cmip5data(2000:2002), drop=FALSE)
        expect_is(arr, "array", info=i)
        expect_equal(dim(arr), c(10, 10, 1, 36), info=i)
    }
})

test_that("weighted.mean works", {
    for(i in implementations) {
    }
})

