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
    for(i in implementations) {
        d <- cmip5data(1, loadAs=i)
        d$lon <- NULL
        expect_warning(filterDimensions(d, lonRange=c(1,2)), info=i)
        
        d <- cmip5data(1, irregular=FALSE, loadAs=i)
        lf <- range(unique(as.numeric(d$lon))[-1]) # the filter
        res <- filterDimensions(d, lonRange=lf)
        expect_equal(range(res$lon), lf, info=i) # all lon values should be in filter
        expect_equal(dim(d$lon)[1]-1, dim(res$lon)[1], info=i) #  one row gone
        expect_equal(dim(d$lat)[1]-1, dim(res$lat)[1], info=i) #  one row gone
        expect_equal(d$Z, res$Z, info=i)
        expect_equal(d$time, res$time, info=i)
        expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=i)     
        
        # Were data filtered correctly?
        if(i == "data.frame") {
            expect_true(all(res$val$lon >= min(lf) & res$val$lon <= max(lf)))
        } else if(i == "array") {
            expect_equal(dim(res$val)[1], nrow(res$lon), info=i)
            expect_equal(dim(res$val)[2], ncol(res$lon), info=i)
        }
    }
})

test_that("filterDimensions handles irregular grids", {
    # For the array implementation, we have to 'punch holes' in the
    # array when filtering, because if the grid is irregular, can't
    # rely just on excluding rows/columns. Makes sure this works.
    d <- cmip5data(1, lonsize=10, latsize=10, irregular=TRUE, loadAs="array")
    # Construct filter such that we're guaranteed to have some values above 
    # AND below the bounds within rows
    lf <- c(mean(d$lon[3,]), mean(d$lon[7,])) 
    
    res <- filterDimensions(d, lonRange=lf)
    expect_true(any(is.na(res$val[1,,,]))) # should be some NAs in first row
    expect_true(!all(is.na(res$val[1,,,]))) # but not all
    expect_true(any(is.na(res$val[nrow(res$lon),,,]))) # ...same true for last row
    expect_true(!all(is.na(res$val[nrow(res$lon),,,]))) # but not all
    expect_equal(dim(res$val)[1], nrow(res$lon))
    expect_equal(dim(res$val)[2], ncol(res$lon))
    
    # Same as above but for latitude
    lf <- c(mean(d$lat[,3]), mean(d$lat[,7])) 
    res <- filterDimensions(d, latRange=lf)
    expect_true(any(is.na(res$val[,1,,]))) # should be some NAs in first col
    expect_true(!all(is.na(res$val[,1,,]))) # but not all
    expect_true(any(is.na(res$val[,ncol(res$lat),,]))) # ...same true for last col
    expect_true(!all(is.na(res$val[,ncol(res$lat),,]))) # but not all
    expect_equal(dim(res$val)[1], nrow(res$lon))
    expect_equal(dim(res$val)[2], ncol(res$lon))
})

test_that("filterDimensions filters lat", {
    for(i in implementations) {
        d <- cmip5data(1, loadAs=i)
        d$lat <- NULL
        expect_warning(filterDimensions(d, latRange=c(1,2)), info=i)
        
        d <- cmip5data(1, irregular=FALSE, loadAs=i)
        lf <- range(unique(as.numeric(d$lat))[-1]) # the filter
        res <- filterDimensions(d, latRange=lf)
        expect_equal(range(res$lat), lf, info=i) # all lat values should be in filter
        expect_equal(dim(d$lon)[2]-1, dim(res$lon)[2], info=i) #  one row gone
        expect_equal(dim(d$lat)[2]-1, dim(res$lat)[2], info=i) #  one row gone
        expect_equal(d$Z, res$Z, info=i)
        expect_equal(d$time, res$time, info=i)
        expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=i)     
        
        # Were data filtered correctly?
        if(i == "data.frame") {
            expect_true(all(res$val$lat >= min(lf) & res$val$lat <= max(lf)))
        } else if(i == "array") {
            expect_equal(dim(res$val)[1], nrow(res$lon), info=i)
            expect_equal(dim(res$val)[2], ncol(res$lon), info=i)
        }
    }
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
        expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=i)
        
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
            expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=info)  # provenance bigger
            
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
        expect_gt(nrow(res$provenance), nrow(d$provenance)) #, info=i)  # and provenance bigger
        
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
        lor <- c(mean(d$lon) - sd(d$lon), mean(d$lon) + sd(d$lon))
        lar <- c(mean(d$lat) - sd(d$lat), mean(d$lat) + sd(d$lat))
        zr <- c(mean(d$Z) - sd(d$Z), mean(d$Z) + sd(d$Z))
        yr <- c(mean(d$time) - sd(d$time), mean(d$time) + sd(d$time))
        
        res1 <- filterDimensions(d, lonRange=lor, latRange=lar, ZRange=zr, yearRange=yr)
        res2 <- filterDimensions(d, lonRange=lor) %>%
            filterDimensions(latRange=lar) %>%
            filterDimensions(ZRange=zr) %>%
            filterDimensions(yearRange=yr)
        
        expect_identical(res1$lon, res2$lon, info=i)
        expect_identical(res1$lat, res2$lat, info=i)
        expect_identical(res1$Z, res2$Z, info=i)
        expect_identical(res1$time, res2$time, info=i)
        expect_identical(res1$val, res2$val, info=i)
    }
})

test_that("filterDimensions handles nonstandard structures", {
    d <- cmip5data(0, time=F)  # area-only data
    expect_warning(filterDimensions(d, yearRange=c(1,2)))
    expect_warning(filterDimensions(d, monthRange=c(1,2)))
    expect_warning(filterDimensions(d, ZRange=c(1,2)))
    res <- filterDimensions(d, lonRange=c(1,2), latRange=c(1,2))
    expect_is(res, "cmip5data")
    
    d <- cmip5data(0, Z=T, time=F)  # area and Z-only data
    expect_warning(filterDimensions(d, yearRange=c(1,2)))
    expect_warning(filterDimensions(d, monthRange=c(1,2)))
    res <- filterDimensions(d, lonRange=c(1,2), latRange=c(1,2), ZRange=c(1,2))
    expect_is(res, "cmip5data")
    
    d <- cmip5data(1:5, lonlat=F)  # time-only data
    expect_warning(filterDimensions(d, lonRange=c(1,2)))
    expect_warning(filterDimensions(d, latRange=c(1,2)))
    expect_warning(filterDimensions(d, ZRange=c(1,2)))
    res <- filterDimensions(d, yearRange=c(mean(d$time) - sd(d$time), 
                                           mean(d$time) + sd(d$time)))
    expect_is(res, "cmip5data")    
})

