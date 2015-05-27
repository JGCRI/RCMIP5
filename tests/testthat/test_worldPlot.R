# Testing code for worldPlot function

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("calcGridArea.R") # ...and all other functions
#   library(testthat)
#   test_file("tests/testthat/test_calcGridArea.R")

context("worldPlot")

implementations <- c("data.frame", "array")

test_that("worldPlot generates ggplot object", {
    for(i in implementations) {
        d <- cmip5data(1:3, loadAs=i)
        p <- worldPlot(d)
        expect_is(p, "ggplot", info=i)
    }
})
