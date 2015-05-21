# Testing code for the RCMIP5 'makeGlobalStat.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeGlobalStat.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_makeGlobalStat.R")

context("makeGlobalStat")

implementations <- c("data.frame", "array")

test_that("makeGlobalStat handles bad input", {
    expect_error(makeGlobalStat(1))                         # non-list d
    expect_error(makeGlobalStat(cmpi5data()))               # wrong size list d
    expect_error(makeGlobalStat(d,verbose=1))               # non-logical verbose
    expect_error(makeGlobalStat(d,verbose=c(T, T)))          # multiple verbose values
    expect_error(makeGlobalStat(d,FUN=1))                   # non-function FUN
    expect_error(makeGlobalStat(d,FUN=c(mean, mean)))        # multiple FUN values
})

test_that("makeGlobalStat handles monthly data", {
    years <- 1850:1851
    
    for(i in implementations) {
        d <- cmip5data(years, loadAs=i)
        res <- makeGlobalStat(d, verbose=F)
        
        # Is 'res' correct type and size?
        expect_is(res,"cmip5data", info=i)
        
        # Did unchanging info get copied correctly?
        expect_equal(res$valUnit, d$valUnit, info=i)
        expect_equal(res$files, d$files, info=i)
        
        # Lon/lat removed, numCells set, and provenance updated?
        expect_null(res$lon, info=i)
        expect_null(res$lat, info=i)
        expect_is(res$numCells, "integer", info=i)
        expect_more_than(nrow(res$provenance), nrow(d$provenance), info=i)
        
        # Does time match what we expect?
        expect_equal(res$time, d$time, info=i)
        
        # Is the answer value data frame correctly sized?
        expect_equal(RCMIP5:::nvals(res), RCMIP5:::nvals(d)/length(d$lon), info=i)
        
        # Are the answer values numerically correct?
        expect_equal(mean(RCMIP5:::vals(res)), mean(RCMIP5:::vals(d)), info=i)  # no weighting
    }
})

test_that("makeGlobalStat weights correctly", {
    for(i in implementations) {
        d <- cmip5data(1850, randomize=T, monthly=F, loadAs=i)
        darea <- cmip5data(0, time=F, randomize=T, loadAs=i)  # create an area file
        
        res <- makeGlobalStat(d, area=darea, sortData=F, verbose=F)
        
        # Are the answer values numerically correct?
        dummyans <- weighted.mean(RCMIP5:::vals(d), w=RCMIP5:::vals(darea))
        expect_equal(dummyans, RCMIP5:::vals(res), info=i)
    }
})

test_that("weighted.sum works correctly", {
    #    d <- cmip5data(1850, randomize=T, monthly=F)
    #    darea <- cmip5data(0, time=F, randomize=T)
    #    res <- makeGlobalStat(d, area=darea, verbose=F, sortData=F, FUN=weighted.sum)
    
    # Are the answer values numerically correct?
    #    dummyans <- weighted.sum(d$val$value, w=darea$val$value)
    #    expect_equal(dummyans, res$val$value)
    
    # Make sure the function itself is OK
    #    expect_equal(weighted.sum(1:4), 10)
    #    expect_equal(weighted.sum(1:4, 1:4), 30) # 4*4 + 3*3 + 2*2 + 1*1
})

test_that("makeGlobalStat handles 4-dimensional data", {
    for(i in implementations) {
        years <- 1850:1851
        d <- cmip5data(years, Z=T, loadAs=i)
        res <- makeGlobalStat(d, verbose=F)
        
        # Do years match what we expect?
        expect_equal(res$time, d$time, info=i)
        
        # Is the answer value array correctly sized?
        expect_equal(RCMIP5:::nvals(res), RCMIP5:::nvals(d)/length(d$lon), info=i)
    }
})

test_that("makeGlobalStat handles custom function and dots", {
    years <- 1850:1851
    llsize <- 2
    
    for(i in implementations) {
        d <- cmip5data(years, lonsize=llsize, latsize=llsize, loadAs=i)
        darea <- cmip5data(0, time=F, lonsize=llsize, latsize=llsize, loadAs=i)
        
        # All data 1 except for max lon/lat is 2        
        if(is.data.frame(d$val)) {
            d$val$value <- 1
            d$val$value[d$val$lon == max(d$lon) & d$val$lat == max(d$lat)] <- 2    
            darea$val$value <- c(rep(1, llsize*llsize-1), llsize*llsize-1)
        } else if(is.array(d$val)) {
            d$val <- array(1, dim=dim(d$val))
            d$val[ncol(d$lon),nrow(d$lat),,] <- 2
            darea$val <- array(c(rep(1, llsize*llsize-1), llsize*llsize-1), dim=dim(darea$val))
        }
        
        # Compute correct answer
        ref <- cmip5data(years, lonsize=llsize, latsize=llsize, loadAs="data.frame")
        ref$val$value <- 1
        ref$val$value[ref$val$lon == max(ref$lon) & ref$val$lat == max(ref$lat)] <- 2    
        refarea <- cmip5data(0, time=F, lonsize=llsize, latsize=llsize, loadAs="data.frame")
        refarea$val$value <- c(rep(1, llsize*llsize-1), llsize*llsize-1)
        ans <- aggregate(value~time, data=ref$val, FUN=weighted.mean, w=RCMIP5:::vals(refarea))
        
        res1 <- makeGlobalStat(d, darea, verbose=F, sortData=F, FUN=weighted.mean)
        expect_is(res1, "cmip5data", info=i)
        
        myfunc <- function(x, w, ...) weighted.mean(x, w, ...)
        res2 <- makeGlobalStat(d, darea, verbose=F, sortData=F, FUN=myfunc)
        expect_is(res2, "cmip5data", info=i)
        
        # Are the result values correct?    
        expect_equal(RCMIP5:::vals(res1), ans$value, info=i)    
        expect_equal(RCMIP5:::vals(res2), ans$value, info=i)
    }
})

test_that("makeGlobalStat sorts before computing", {
    years <- 1850:1851
    llsize <- 2
    
    # Note this test is only applicable to the data.frame implementation
    d <- cmip5data(years, lonsize=llsize, latsize=llsize, monthly=F, loadAs="data.frame")
    darea <- cmip5data(0, time=F, lonsize=llsize, latsize=llsize, loadAs="data.frame")
    
    # All data 1 except for max lon/lat is 2
    d$val$value <- 1
    d$val$value[d$val$lon == max(d$lon) & d$val$lat == max(d$lat)] <- 2    
    darea$val$value <- c(rep(1, llsize*llsize-1), llsize*llsize-1)
    
    # Compute correct answer
    ans <- aggregate(value~time, data=d$val, FUN=weighted.mean, w=darea$val$value)
    
    # Now we put `darea` out of order and call makeGlobalStat
    darea$val <- dplyr::arrange(darea$val, desc(lon), desc(lat))
    res1 <- makeGlobalStat(d, darea, verbose=F, sortData=TRUE)
    expect_is(res1, "cmip5data")
    
    # makeGlobalStat should be sorted darea correctly before calculating
    # Are the result values correct?    
    expect_equal(res1$val$value, ans$value)
    
    # This should generate a warning, because sortData not specified
    expect_warning(makeGlobalStat(d, darea, verbose=F))
    
    # Put data out of order and test again
    #d$val <- d$val[order(d$val$lon, d$val$lat, decreasing=TRUE),]
    d$val <- dplyr::arrange(d$val, desc(lon), desc(lat))
    res2 <- makeGlobalStat(d, darea, verbose=F, sortData=TRUE)
    expect_equal(res2$val$value, ans$value)
})


