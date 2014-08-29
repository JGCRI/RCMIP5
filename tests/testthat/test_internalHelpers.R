# Testing code for the RCMIP5 scripts in 'internalHelpers.R' and 'RCMIP5.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("internalHelpers.R")    # can't do this here

# To run this code: 
#   source("internalHelpers.R")
#   source("RCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_internalHelpers.R")

context("addProvenance")

test_that("addProvenance handles bad input", {
    expect_error(addProvenance(prov=3))
    expect_error(addProvenance(msg=3))
})

test_that("addProvenance initializes", {
    expect_is(addProvenance(), "character")
    expect_is(addProvenance(msg="test"), "character")
})

test_that("addProvenance adds messages", {
    expect_equal(length(addProvenance()), 2)            # One line for timestamp, one for function
    expect_equal(length(addProvenance(msg="test")), 3)  # Timestamp, function, msg
    
    p1 <- addProvenance(msg="test1")
    p2 <- addProvenance(prov=p1, msg="test2")
    expect_true(grepl("test1", p2[3]))   # 'test2' should appear in final line
    expect_true(grepl("test2", p2[4]))   # 'test2' should appear in final line
    expect_equal(length(p2), 4)          # One line for function, two for msgs
    new.env()
    expect_equal(length(addProvenance(p2, msg="test3")), 6)  # Should add two new lines
})


# ===============================================

context("cmip5data")

test_that("cmip5data handles bad input", {
    expect_error(cmip5data("hi"))   
    expect_error(cmip5data(1, monthly=123))   
    expect_error(cmip5data(1, depth=123))
    expect_error(cmip5data(1, lev=123))
    expect_error(cmip5data(1, randomize="hi"))   
})

test_that("cmip5data generates annual and monthly data", {
    d <- cmip5data(1)
    expect_equal(length(dim(d$val)), 3)
    expect_equal(dim(d$val)[3], 12)
    expect_equal(length(d$time), 12)
    expect_equal(d$timeFreqStr, "mon")
    expect_equal(dim(d$val)[1], length(d$lon))
    expect_equal(dim(d$val)[2], length(d$lat))
    expect_equal(dim(d$val)[3], length(d$time))
    
    d <- cmip5data(1, monthly=F)
    expect_equal(length(dim(d$val)), 3)
    expect_equal(dim(d$val)[3], 1)
    expect_equal(length(d$time), 1)
    expect_equal(d$timeFreqStr, "yr")
    expect_equal(dim(d$val)[3], length(d$time))
})

test_that("cmip5data fills in ancillary data", {
    d <- cmip5data(1)
    expect_is(d$model, "character")
    expect_is(d$variable, "character")
    expect_is(d$experiment, "character")
    expect_is(d$valUnit, "character")
    expect_is(d$timeFreqStr, "character")
    expect_is(d$debug, "list")
    expect_is(d$debug$calendarStr, "character")
    expect_is(d$debug$timeUnit, "character")
})

test_that("cmip5data obeys depth and lev", {
    d <- cmip5data(1, depth=T)
    expect_equal(length(dim(d$val)), 4)
    expect_false(is.null(d$depth))
    
    d <- cmip5data(1, lev=T)
    expect_equal(length(dim(d$val)), 4)
    expect_false(is.null(d$lev))
    
    d <- cmip5data(1, depth=T, lev=T)
    expect_equal(length(dim(d$val)), 5)
    expect_false(is.null(d$depth))
    expect_false(is.null(d$lev))
})

test_that("cmip5data obeys randomize", {
    expect_true(sum(cmip5data(1, randomize=T)$val) != sum(cmip5data(1, randomize=T)$val))
})
