# Testing code for the RCMIP5 (?) 'makeAnnualMean.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeAnnualMean.R")
#   library(testthat)
#   test_file("tests/testthat/test_makeAnnualMean.R")

dummydata <- function(years) {
    list(files="", 
         val=array(runif(10*10*12*length(years)), dim=c(10,10,12*length(years))),
         valUnit="",
         timeUnit=paste0("days since ",years[1],"-01-01"),
         calendarStr="360_day",  # don't change this
         lat=c(0:9),
         lon=c(0:9),
         time=30*c(0:(length(years)*12-1) ) )
} # dummydata

context("makeAnnualMean")

test_that("makeAnnualMean handles bad input", {
    expect_error(makeAnnualMean(1))                         # non-list d
    expect_error(makeAnnualMean(list()))                    # wrong size list d
    expect_error(makeAnnualMean(d,yearRange=1))             # non-vector yearRange
    expect_error(makeAnnualMean(d,yearRange=c(-1,1)))       # illegal value yearRange
    expect_error(makeAnnualMean(d,yearRange=c(1,2,3)))      # wrong vector size yearRange
    expect_error(makeAnnualMean(d,verbose=1))               # non-logical verbose
    expect_error(makeAnnualMean(d,verbose=c(T,T)))          # multiple verbose values
    expect_error(makeAnnualMean(d,parallel=1))              # non-logical parallel
    expect_error(makeAnnualMean(d,parallel=c(T,T)))         # multiple parallel values
    expect_error(makeAnnualMean(d,FUN=1))                   # non-function FUN
    expect_error(makeAnnualMean(d,FUN=c(mean,mean)))        # multiple FUN values
    
    d <- dummydata(1850)
    d$calendarStr <- ""
    expect_error(makeAnnualMean(d))        # corrupt calendarStr
    d <- dummydata(1850)
    d$timeUnit <- "days since 8767-31-54"
    expect_error(makeAnnualMean(d))        # corrupt timeUnit
    d <- dummydata(1850)
    d$timeUnit <- ""
    expect_error(makeAnnualMean(d))        # empty timeUnit
    d <- dummydata(1850)
    d$val <- array(1:2, dim=dim(d$val)-1)
    expect_error(makeAnnualMean(d))        # corrupt value array
})

test_that("makeAnnualMean handles monthly data", {
    #    d <- loadModel('nbp','HadGEM2-ES','rcp85',path=SAMPLEDATA)
    years <- 1850:1851
    d <- dummydata(years)
    res <- makeAnnualMean(d,verbose=F)
    
    # Is 'res' correct type and size?
    expect_is(res,"list")
    expect_equal(length(res),7)
    
    # Did unchanging info get copied correctly?
    expect_equal(res$lon,d$lon)
    expect_equal(res$lat,d$lat)
    expect_equal(res$valUnit,d$valUnit)
    expect_equal(res$files,d$files)
    
    # Do years match what we expect?
    expect_equal(res$year,years)
    
    # Is the answer value array correctly sized?
    expect_equal(dim(res$val)[1:2],dim(d$val)[1:2])   # spatial size match
    expect_equal(dim(res$val)[3],length(years))  # temporal size match
    
    # Are the answer values numerically correct?
    yearIndex <- d$time/360 + years[1]
    dummyans <- array(NA_real_, dim=c(dim(d$val)[c(1,2)], length(years)))
    for(i in 1:length(years)) {
        dummyans[,,i] <- aaply(d$val[,,years[i] == floor(yearIndex)], c(1,2), mean)
    }
    expect_equal(res$val, dummyans)
})

test_that("makeAnnualMean yearRange filter works", {
    years <- 1850:1853
    yearRange <- 1851:1852
    d <- dummydata(years)
    
    yearIndex <- d$time/360 + years[1]
    uniqueYears <- unique(floor(yearIndex))
    uniqueYears <- uniqueYears[uniqueYears >= min(yearRange) & uniqueYears <= max(yearRange)]
    dummyans <- array(NA_real_, dim=c(dim(d$val)[c(1,2)], length(uniqueYears)))
    for(i in 1:length(uniqueYears)) {
        dummyans[,,i] <- aaply(d$val[,,uniqueYears[i] == floor(yearIndex)], c(1,2), mean)
    }

    # Testing the test: check our choice of years above
    expect_that(range(years),not(equals(range(yearRange))))
    
    # makeMonthlyMean should return exactly the array calculated above
    expect_equal(makeAnnualMean(d,yearRange=yearRange,verbose=F)$val, dummyans)
    
    # ...unless the time ranges don't match
    expect_that(makeAnnualMean(d,verbose=F)$val,not(equals(dummyans)))
})

test_that("makeAnnualMean parallel produces same answer", {
    #    d <- loadModel('nbp','HadGEM2-ES','rcp85',path=SAMPLEDATA)
    years <- 1850:1851
    d <- dummydata(years)
    res_s <- makeAnnualMean(d,verbose=F,parallel=F)
    res_p <- makeAnnualMean(d,verbose=F,parallel=T)
    expect_equal(res_s,res_p)
})

test_that("makeAnnualMean handles annual data", {
    # TODO
})
