# Testing code for the RCMIP5 scripts in 'addProvenance.R'

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

context("addProvenance")

implementations <- c("data.frame", "array")

test_that("addProvenance handles bad input", {
    expect_error(addProvenance())
    expect_error(addProvenance(x=3))
    expect_error(addProvenance(cmip5data(), msg=3))    
})

test_that("addProvenance initializes", {
    for(i in implementations) {
        d <- RCMIP5:::addProvenance(cmip5data(1, loadAs=i), msg="test")
        expect_is(d, "cmip5data", info=i)
        expect_is(d$provenance, "data.frame", info=i)
    }
})

test_that("addProvenance adds messages", {
    for(i in implementations) {
        d <- cmip5data(1, loadAs=i)
        expect_equal(nrow(d$provenance), 2, info=i)            # One line for software specs, one for message
        d <- RCMIP5:::addProvenance(d, "test23")
        expect_equal(nrow(d$provenance), 3, info=i)           # One line for software specs, one for message
        expect_true(any(grepl("test23", d$provenance[3,])), info=i)   # 'test23' should appear in final line
    }
})

test_that("addProvenance merges provenances", {
    for(i in implementations) {
        d1 <- cmip5data(1, loadAs=i)
        d2 <- cmip5data(2, loadAs=i)
        d3 <- RCMIP5:::addProvenance(d1, d2)
        
        expect_equal(nrow(d1$provenance)+nrow(d2$provenance), nrow(d3$provenance), info=i)
    }
})
