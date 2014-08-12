# Testing code for the RCMIP5 scripts in 'internalHelpers.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)
#source("internalHelpers.R")    # can't do this here

# To run this code: 
#   source("internalHelpers.R")
#   library(testthat)
#   test_file("tests/testthat/test_loadModel.R")

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
    expect_equal(length(addProvenance()), 1)            # One line for function
    expect_equal(length(addProvenance(msg="test")), 2)  # One line for function, one for msg
    
    p1 <- addProvenance(msg="test1")
    p2 <- addProvenance(prov=p1, msg="test2")
    expect_true(grepl("test1", p2[2]))   # 'test2' should appear in final line
    expect_true(grepl("test2", p2[3]))   # 'test2' should appear in final line
    expect_equal(length(p2), 3)          # One line for function, two for msgs
    new.env()
    expect_equal(length(addProvenance(p2, msg="test3")), 5)  # Should add two new lines
})


# ===============================================

context("dummydata")

test_that("dummydata handles bad input", {
    expect_error(dummydata())   
    expect_error(dummydata("hi"))   
    expect_error(dummydata(1, monthly=123))   
    expect_error(dummydata(1, depth=123))
    expect_error(dummydata(1, lev=123))
    expect_error(dummydata(1, randomize="hi"))   
})

test_that("dummydata generates annual and monthly data", {
    d <- dummydata(1)
    expect_equal(length(dim(d$val)), 3)
    expect_equal(dim(d$val)[3], 12)
    expect_equal(length(d$time), 12)
    expect_equal(d$timeFreqStr, "mon")
    expect_equal(dim(d$val)[1], length(d$lon))
    expect_equal(dim(d$val)[2], length(d$lat))
    expect_equal(dim(d$val)[3], length(d$time))
    
    d <- dummydata(1, monthly=F)
    expect_equal(length(dim(d$val)), 3)
    expect_equal(dim(d$val)[3], 1)
    expect_equal(length(d$time), 1)
    expect_equal(d$timeFreqStr, "yr")
    expect_equal(dim(d$val)[3], length(d$time))
})

test_that("dummydata fills in ancillary data", {
    d <- dummydata(1)
    expect_is(d$model, "character")
    expect_is(d$variable, "character")
    expect_is(d$experiment, "character")
    expect_is(d$valUnit, "character")
    expect_is(d$timeUnit, "character")
    expect_is(d$timeFreqStr, "character")
    expect_is(d$calendarStr, "character")
})

test_that("dummydata obeys depth and lev", {
    d <- dummydata(1, depth=T)
    expect_equal(length(dim(d$val)), 4)
    expect_false(is.null(d$depth))
    
    d <- dummydata(1, lev=T)
    expect_equal(length(dim(d$val)), 4)
    expect_false(is.null(d$lev))

    d <- dummydata(1, depth=T, lev=T)
    expect_equal(length(dim(d$val)), 5)
    expect_false(is.null(d$depth))
    expect_false(is.null(d$lev))
})

test_that("dummydata obeys randomize", {
    expect_true(sum(dummydata(1, randomize=T)$val) != sum(dummydata(1, randomize=T)$val))
})
