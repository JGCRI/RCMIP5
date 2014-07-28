# Testing code for the RCMIP5 (?) 'makeMonthlyMean.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeMonthlyMean.R")
#   library(testthat)
#   test_file("tests/testthat/test_makeMonthlyMean.R")

dummydata <- function(years) {
    cmip5data(list(files="", 
         val=array(runif(10*10*12*length(years)), dim=c(10,10,12*length(years))),
         valUnit="",
         timeUnit=paste0("days since ",years[1],"-01-01"),
         calendarStr="360_day",  # don't change this
         lat=c(0:9),
         lon=c(0:9),
         time=30*c(0:(length(years)*12-1) ) ))
} # dummydata

context("makeMonthlyMean")

test_that("makeMonthlyMean handles bad input", {
    expect_error(makeMonthlyMean(1))                         # non-list d
    expect_error(makeMonthlyMean(cmip5data()))               # wrong size list d
    expect_error(makeMonthlyMean(d,yearRange=1))             # non-vector yearRange
    expect_error(makeMonthlyMean(d,yearRange=c(-1,1)))       # illegal value yearRange
    expect_error(makeMonthlyMean(d,yearRange=c(1,2,3)))      # wrong vector size yearRange
    expect_error(makeMonthlyMean(d,verbose=1))               # non-logical verbose
    expect_error(makeMonthlyMean(d,verbose=c(T,T)))          # multiple verbose values
    expect_error(makeMonthlyMean(d,parallel=1))              # non-logical parallel
    expect_error(makeMonthlyMean(d,parallel=c(T,T)))         # multiple parallel values
    expect_error(makeMonthlyMean(d,FUN=1))                   # non-function FUN
    expect_error(makeMonthlyMean(d,FUN=c(mean,mean)))        # multiple FUN values
    
    d <- dummydata(1850)
    d$calendarStr <- ""
    expect_error(makeMonthlyMean(d))        # corrupt calendarStr
    d <- dummydata(1850)
    d$timeUnit <- "days since 8767-31-54"
    expect_error(makeMonthlyMean(d))        # corrupt timeUnit
    d <- dummydata(1850)
    d$timeUnit <- ""
    expect_error(makeMonthlyMean(d))        # empty timeUnit
    d <- dummydata(1850)
    d$val <- array(1:2, dim=dim(d$val)-1)
    expect_error(makeMonthlyMean(d))        # corrupt value array
})

test_that("makeMonthlyMean handles monthly data", {
    #    d <- loadModel('nbp','HadGEM2-ES','rcp85',path=SAMPLEDATA)
    years <- 1850:1851
    d <- dummydata(years)
    res <- makeMonthlyMean(d,verbose=F)
    
    # Is 'res' correct type?
    expect_is(res,"cmip5data")
    
    # Did unchanging info get copied correctly?
    expect_equal(res$lon,d$lon)
    expect_equal(res$lat,d$lat)
    expect_equal(res$valUnit,d$valUnit)
    expect_equal(res$files,d$files)
    
    # Do years match what we expect?
    #    expect_equal(res$year,years)
    
    # Is the answer value array correctly sized?
    expect_equal(dim(res$val)[1:2],dim(d$val)[1:2])   # spatial size match
    expect_equal(dim(res$val)[3],12)  # temporal size match
    
    # Are the answer values numerically correct?
    yearIndex <- d$time/360 + years[1]
    yearFilter <- T
    monthIndex <- floor((yearIndex %% 1) * 12 + 1)
    dummyans <- array(NA_real_, dim=c(dim(d$val)[c(1,2)], 12))
    for(i in 1:12) {
        dummyans[,,i] <- aaply(d$val[,,(i == monthIndex) & yearFilter], c(1,2), mean)
    }
    expect_equal(res$val, dummyans)
})

test_that("makeMonthlyMean yearRange filter works", {
    years <- 1850:1853
    yearRange <- c(1851,1852)
    d <- dummydata(years)
    
    yearIndex <- d$time/360 + years[1]
    yearFilter <- floor(yearIndex) >= min(yearRange) & floor(yearIndex) <= max(yearRange)
    monthIndex <- floor((yearIndex %% 1) * 12 + 1)
    dummyans <- array(NA_real_, dim=c(dim(d$val)[c(1,2)], 12))
    for(i in 1:12) {
        dummyans[,,i] <- aaply(d$val[,,(i == monthIndex) & yearFilter], c(1,2), mean)
    }
    
    # Testing the test: check our choice of years above
    expect_that(range(years),not(equals(range(yearRange))))
    
    # makeMonthlyMean should return exactly the array calculated above
    expect_equal(makeMonthlyMean(d,yearRange=yearRange,verbose=F)$val, dummyans)

    # ...unless the time ranges don't match
    expect_that(makeMonthlyMean(d,verbose=F)$val,not(equals(dummyans)))
})

test_that("makeMonthlyMean parallel produces same answer", {
    #    d <- loadModel('nbp','HadGEM2-ES','rcp85',path=SAMPLEDATA)
    years <- 1850:1851
    d <- dummydata(years)
    res_s <- makeMonthlyMean(d,verbose=F,parallel=F)
    res_p <- makeMonthlyMean(d,verbose=F,parallel=T)
    expect_equal(res_s,res_p)
})

test_that("makeMonthlyMean handles annual data", {
    # TODO
})
