# Testing code for the RCMIP5 scripts in 'addProvenance.R' and 'RCMIP5.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("addProvenance.R")
#   source("RCMIP5.R")
#   library(testthat)
#   test_file("tests/testthat/test_internalHelpers.R")

context("addProvenance")

test_that("addProvenance handles bad input", {
    expect_error(addProvenance())
    expect_error(addProvenance(x=3))
    expect_error(addProvenance(cmip5data(), msg=3))
})

test_that("addProvenance initializes", {
    d <- addProvenance(cmip5data(1), msg="test")
    expect_is(d, "cmip5data")
    expect_is(d$provenance, "data.frame")
})

test_that("addProvenance adds messages", {
    d <- cmip5data(1)
    expect_equal(nrow(d$provenance), 2)            # One line for software specs, one for message
    d <- addProvenance(d, "test23")
    expect_equal(nrow(d$provenance), 3)           # One line for software specs, one for message
    expect_true(any(grepl("test23", d$provenance[3,])))   # 'test23' should appear in final line
})

test_that("addProvenance merges provenances", {
    d1 <- cmip5data(1)
    d2 <- cmip5data(2)
    d3 <- addProvenance(d1, d2)
    
    expect_equal(nrow(d1$provenance)+nrow(d2$provenance), nrow(d3$provenance))
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
