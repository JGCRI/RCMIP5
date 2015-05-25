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

implementations <- c("data.frame", "array")

test_that("filterDimensions handles bad input", {
    expect_error(filterDimensions(1))                       # non-cmip5data x
    d <- cmip5data(1, Z=TRUE)
    expect_error(filterDimensions(d, lonRange='1'))              # non-numeric lonRange
    expect_error(filterDimensions(d, latRange='1'))              # non-numeric latRange
    expect_error(filterDimensions(d, ZRange='1'))              # non-numeric ZRange
    expect_error(filterDimensions(d, yearRange='1'))              # non-numeric yearRange
    expect_error(filterDimensions(d, monthRange='1'))              # non-numeric monthRange
    expect_error(filterDimensions(d, lonRange=1))              # wrong length lonRange
    expect_error(filterDimensions(d, latRange=1))              # wrong length latRange
    expect_error(filterDimensions(d, ZRange=1))              # wrong length ZRange
    expect_error(filterDimensions(d, yearRange=1))              # wrong length yearRange
    expect_error(filterDimensions(d, monthRange=1))              # wrong length monthRange
    expect_error(filterDimensions(d, monthRange=c(1, 13)))  # illegal monthRange value
    expect_error(filterDimensions(d, verbose=1))            # non-logical verbose
    expect_error(filterDimensions(d, verbose=c(T, T)))      # multiple verbose values
})

test_that("filterDimensions filters lon", {
    d <- cmip5data(1)
    d$lon <- NULL
    expect_warning(filterDimensions(d, lonRange=1))
    
    d <- cmip5data(1)
    lf <- d$lon[1:(length(d$lon)-1)] # the filter
    res <- filterDimensions(d, lonRange=lf)
    #    expect_equal(res$lon, lf)
    #    expect_equal(nrow(res$val), prod(length(res$lon), length(res$lat), length(res$time)))
    expect_more_than(nrow(res$provenance), nrow(d$provenance))     
})

test_that("filterDimensions filters lat", {
    d <- cmip5data(1)
    d$lat <- NULL
    expect_warning(filterDimensions(d, latRange=1))
    
    d <- cmip5data(1)
    lf <- d$lat[-length(d$lat)] # the filter
    res <- filterDimensions(d, latRange=lf)
    #    expect_equal(res$lat, lf)
    #    expect_equal(nrow(res$val), prod(length(res$lon), length(res$lat), length(res$time)))
    expect_more_than(nrow(res$provenance), nrow(d$provenance))     
})

test_that("filterDimensions filters Z", {
    for(i in implementations) {
        # Does filterDimensions warn if there's no Z data?
        expect_warning(filterDimensions(cmip5data(1, loadAs=i), ZRange=c(1, 2)))
        
        # Do the filtered flag, Z data, provenance all get set?
        d <- cmip5data(1, Z=T, loadAs=i)
        zf <- range(d$Z[-1]) # the filter
        res <- filterDimensions(d, ZRange=zf)
        expect_true(res$filtered, info=i)
        expect_equal(range(res$Z), zf, info=i)
        expect_more_than(nrow(res$provenance), nrow(d$provenance), info=i)
        
        # Were data filtered correctly?
        if(i == "data.frame") {
            expect_true(all(res$val$Z >= min(zf) & res$val$Z <= max(zf)), info=i)
        } else if(i == "array") {
            expect_equal(dim(res$val)[3], length(res$Z), info=i)
        }
    }
})

test_that("filterDimensions filters time (yearRange)", {
    for(i in implementations) {
        for(mon in c(TRUE, FALSE)) { # test monthly and annual data
            info <- paste(i, "monthly =", mon) # test info
            
            # Does filterDimensions warn if there's no time data?
            d <- cmip5data(1:5, monthly=mon, loadAs=i)
            d$time <- NULL
            expect_warning(filterDimensions(d, yearRange=c(1, 2)), info=info)
            
            # Do the filtered flag, Z data, provenance all get set?
            d <- cmip5data(1:5, monthly=mon, loadAs=i)
            yf <- range(unique(floor(d$time))[-1]) # the filter
            res <- filterDimensions(d, yearRange=yf)
            expect_true(res$filtered, info=i)
            expect_equal(range(floor(res$time)), yf, info=info)  # only yearRange in filter
            expect_more_than(nrow(res$provenance), nrow(d$provenance), info=info)  # provenance bigger
            
            # Were data filtered correctly?
            if(i == "data.frame") {
                expect_true(all(floor(res$val$time) >= min(yf) & floor(res$val$time) <= max(yf)), info=info)
            } else if(i == "array") {
                expect_equal(dim(res$val)[4], length(res$time), info=info)
            }            
        }        
    }
})

test_that("filterDimensions filters time (monthRange)", {
    
    for(i in implementations) {
        # Does filterDimensions warn if there's no monthly data?
        d <- cmip5data(1:5, monthly=FALSE, loadAs=i)
        mf <- c(1, 2)
        expect_warning(filterDimensions(d, monthRange=mf))  # monthly data required
        
        # Do the filtered flag, Z data, provenance all get set?
        d <- cmip5data(1:5, monthly=TRUE, loadAs=i)
        res <- filterDimensions(d, monthRange=mf)
        expect_true(res$filtered, info=i)
        expect_equal(length(unique(round(res$time %% 1, 2))), length(mf), info=i)  # only monthRange in filter
        expect_more_than(nrow(res$provenance), nrow(d$provenance), info=i)  # and provenance bigger
        
        # Were data filtered correctly?
        if(i == "data.frame") {
            m <- round(res$val$time %% 1, 2)
            fracmonths <- round((mf - 0.5) / 12, 2)
            expect_true(all(m >= min(fracmonths) & m <= max(fracmonths)), info=i)
        } else if(i == "array") {
            expect_equal(dim(res$val)[4], length(res$time), info=i)
        }            
    }
})

test_that("filterDimensions handles multiple operations", {
    for(i in implementations) {
        d <- cmip5data(1:5, Z=T, loadAs=i)
        lor <- c(d$lon[1], d$lon[2])
        lar <- c(d$lat[1], d$lat[2])
        zr <- c(d$Z[1], d$Z[2])
        yr <- c(d$time[1], d$time[2])
        
        res1 <- filterDimensions(d, lonRange=lor, latRange=lar, ZRange=zr, yearRange=yr)
        res2 <- filterDimensions(d, lonRange=lor) %>%
            filterDimensions(latRange=lar) %>%
            filterDimensions(ZRange=zr) %>%
            filterDimensions(yearRange=yr)
        
        expect_identical(res1$lon, res2$lon)
        expect_identical(res1$lat, res2$lat)
        expect_identical(res1$Z, res2$Z)
        expect_identical(res1$time, res2$time)
        expect_identical(res1$val, res2$val)
    }
})

test_that("filterDimensions handles nonstandard structures", {
    d <- cmip5data(0, time=F)  # area-only data
    expect_warning(filterDimensions(d, yearRange=c(1,2)))
    expect_warning(filterDimensions(d, monthRange=c(1,2)))
    expect_warning(filterDimensions(d, ZRange=c(1,2)))
    res <- filterDimensions(d, lonRange=d$lon[1], latRange=d$lat[1])
    expect_is(res, "cmip5data")
    
    d <- cmip5data(0, Z=T, time=F)  # area and Z-only data
    expect_warning(filterDimensions(d, yearRange=c(1,2)))
    expect_warning(filterDimensions(d, monthRange=c(1,2)))
    res <- filterDimensions(d, lonRange=d$lon[1], latRange=d$lat[1], ZRange=d$Z[1])
    expect_is(res, "cmip5data")
    
    d <- cmip5data(1:5, lonlat=F)  # time-only data
    expect_warning(filterDimensions(d, lonRange=c(1,2)))
    expect_warning(filterDimensions(d, latRange=c(1,2)))
    expect_warning(filterDimensions(d, ZRange=c(1,2)))
    res <- filterDimensions(d, yearRange=d$time[1])
    expect_is(res, "cmip5data")    
})

