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
    expect_error(addProvenance(msg=3))
    expect_error(addProvenance(msg=c("foo","bar")))
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
