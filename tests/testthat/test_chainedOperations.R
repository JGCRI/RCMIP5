# Testing code for chained operations with the RCMIP5 'makeXxxxxStat' functions

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("makeAnnualStat.R") # ...and all other functions
#   library(testthat)
#   test_file("tests/testthat/test_makeAnnualStat.R")

context("Chained operations")

test_that("monthly data", {
    years <- 1851:1855
    lsize <- 10
    d <- cmip5data(years, monthly=T, lonsize=lsize, latsize=lsize)
    expect_equal(dim(d$val), c(lsize, lsize, 1, length(years)*12))

    # filter, annual stat, global stat
    d10 <- filterDimensions(d, years=years[1:(length(years)-1)], verbose=F)
    expect_equal(dim(d10$val), c(lsize, lsize, 1, (length(years)-1)*12))
    d11 <- makeAnnualStat(d10, verbose=F)
    expect_equal(dim(d11$val), c(lsize, lsize, 1, length(years)-1))
    d12 <- makeGlobalStat(d11, verbose=F)
    expect_equal(dim(d12$val), c(1, 1, 1, length(years)-1))
    
    # filter, global stat, annual stat
    d20 <- filterDimensions(d, years=years[1:(length(years)-1)], verbose=F)
    expect_equal(dim(d20$val), c(lsize, lsize, 1, (length(years)-1)*12))
    d21 <- makeGlobalStat(d20, verbose=F)
    expect_equal(dim(d21$val), c(1, 1, 1, (length(years)-1)*12))
    d22 <- makeAnnualStat(d21, verbose=F)
    expect_equal(dim(d22$val), c(1, 1, 1, (length(years)-1)))
    
    # order of operations shouldn't matter
    expect_equal(d12$val, d22$val)
    
    # filter, monthly stat, global stat
    d30 <- filterDimensions(d, years=years[1:(length(years)-1)], verbose=F)
    d31 <- makeMonthlyStat(d30, verbose=F)
    expect_equal(dim(d31$val), c(lsize, lsize, 1, 12))
    d32 <- makeGlobalStat(d31, verbose=F)
    expect_equal(dim(d32$val), c(1, 1, 1, 12))
    
    # filter, global stat, annual stat
    d40 <- filterDimensions(d, years=years[1:(length(years)-1)], verbose=F)
    d41 <- makeGlobalStat(d40, verbose=F)
    expect_equal(dim(d41$val), c(1, 1, 1, (length(years)-1)*12))
    d42 <- makeMonthlyStat(d41, verbose=F)
    expect_equal(dim(d42$val), c(1, 1, 1, 12))
    
    # order of operations shouldn't matter
    expect_equal(d32$val, d42$val)
})

test_that("annual data", {
    years <- 1851:1855
    lsize <- 10
    d <- cmip5data(years, monthly=F, lonsize=lsize, latsize=lsize)
    expect_equal(dim(d$val), c(lsize, lsize, 1, length(years)))
    
    # filter, annual stat, global stat
    d10 <- filterDimensions(d, years=years[1:(length(years)-1)], verbose=F)
    expect_equal(dim(d10$val), c(lsize, lsize, 1, (length(years)-1)))
    d11 <- makeAnnualStat(d10, verbose=F)
    expect_equal(dim(d11$val), c(lsize, lsize, 1, length(years)-1))
    d12 <- makeGlobalStat(d11, verbose=F)
    expect_equal(dim(d12$val), c(1, 1, 1, length(years)-1))
    
    # filter, global stat, annual stat
    d20 <- filterDimensions(d, years=years[1:(length(years)-1)], verbose=F)
    expect_equal(dim(d20$val), c(lsize, lsize, 1, (length(years)-1)))
    d21 <- makeGlobalStat(d20, verbose=F)
    expect_equal(dim(d21$val), c(1, 1, 1, (length(years)-1)))
    d22 <- makeAnnualStat(d21, verbose=F)
    expect_equal(dim(d22$val), c(1, 1, 1, (length(years)-1)))
    
    # order of operations shouldn't matter
    expect_equal(d12$val, d22$val)
})

test_that("four-D data", {
    years <- 1851:1855
    lsize <- 10
    zsize <- 5
    d <- cmip5data(years, monthly=T, lonsize=lsize, latsize=lsize, Z=T, Zsize=zsize)
    expect_equal(dim(d$val), c(lsize, lsize, zsize, length(years)*12))
    
    # filter, annual stat, global stat
    d10 <- filterDimensions(d, Zs=d$Z[1:(length(d$Z)-1)], verbose=F)
    expect_equal(dim(d10$val), c(lsize, lsize, zsize-1, (length(years))*12))
    d11 <- makeAnnualStat(d10, verbose=F)
    expect_equal(dim(d11$val), c(lsize, lsize, zsize-1, length(years)))
    d12 <- makeGlobalStat(d11, verbose=F)
    expect_equal(dim(d12$val), c(1, 1, zsize-1, length(years)))
    
    # filter, global stat, annual stat
    d20 <- filterDimensions(d, Zs=d$Z[1:(length(d$Z)-1)], verbose=F)
    expect_equal(dim(d20$val), c(lsize, lsize, zsize-1, (length(years))*12))
    d21 <- makeGlobalStat(d20, verbose=F)
    expect_equal(dim(d21$val), c(1, 1, zsize-1, (length(years))*12))
    d22 <- makeAnnualStat(d21, verbose=F)
    expect_equal(dim(d22$val), c(1, 1, zsize-1, (length(years))))
    
    # order of operations shouldn't matter
    expect_equal(d12$val, d22$val)
    
    # filter, monthly stat, global stat
    d30 <- filterDimensions(d, Zs=d$Z[1:(length(d$Z)-1)], verbose=F)    
    expect_equal(dim(d30$val), c(lsize, lsize, zsize-1, (length(years))*12))    
    d31 <- makeMonthlyStat(d30, verbose=F)
    expect_equal(dim(d31$val), c(lsize, lsize, zsize-1, 12))
    d32 <- makeZStat(d31, verbose=F)
    expect_equal(dim(d32$val), c(lsize, lsize, 1, 12))
    d33 <- makeGlobalStat(d32, verbose=F)
    expect_equal(dim(d33$val), c(1, 1, 1, 12))
    
    # filter, global stat, annual stat
    d40 <- filterDimensions(d, Zs=d$Z[1:(length(d$Z)-1)], verbose=F)    
    expect_equal(dim(d40$val), c(lsize, lsize, zsize-1, (length(years))*12))    
    d41 <- makeGlobalStat(d40, verbose=F)
    expect_equal(dim(d41$val), c(1, 1, zsize-1, (length(years))*12))        
    d42 <- makeZStat(d41, verbose=F)
    expect_equal(dim(d42$val), c(1, 1, 1, (length(years))*12))
    d43 <- makeMonthlyStat(d42, verbose=F)
    expect_equal(dim(d43$val), c(1, 1, 1, 12))
    
    # order of operations shouldn't matter
    expect_equal(d33$val, d43$val)
})
