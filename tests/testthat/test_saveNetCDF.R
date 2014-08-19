# Testing code for the RCMIP5 'saveNetCDF.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("saveNetCDF.R")
#   source("RCMIP5.R") # for cmip5data
#   library(testthat)
#   test_file("tests/testthat/test_saveNetCDF.R")

context("saveNetCDF")

test_that("saveNetCDF handles bad input", {
    expect_error(makeAnnualStat(1))                         # non-list x
    expect_error(makeAnnualStat(d,verbose=1))               # non-logical verbose
    expect_error(makeAnnualStat(d,verbose=c(T, T)))          # multiple verbose values
    
})

test_that("saveNetCDF - TODO", {
})
