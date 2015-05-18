# Testing code calcGridArea function

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code: 
#   source("calcGridArea.R") # ...and all other functions
#   library(testthat)
#   test_file("tests/testthat/test_calcGridArea.R")

context("Calculating global grid areas")

test_that("Test 1 degree grids", {
    area <- calcGridArea(lon=0:359+0.5, lat=-90:89+0.5)
    
    #check global area
    expect_less_than(abs(sum(area[TRUE])-5.10072e14)/5.10072e14, 1e-5)
    
    #check grid area against arc lengths
    #       LAT 	     LONG
    # 0° 	110.574 km 	111.320 km
    # 15° 	110.649 km 	107.550 km
    # 30° 	110.852 km 	96.486 km
    # 45° 	111.132 km 	78.847 km
    # 60° 	111.412 km 	55.800 km
    # 75° 	111.618 km 	28.902 km
    # 90° 	111.694 km 	0.000 km

    # 0 - 0 will have an area of (110.574 * 111.320)*10^6 = 1.23091e10
    expect_less_than((abs(110.574 * 111.320)*10^6 - max(area))/(110.574 * 111.320 *10^6), 1e-2)
    # an area at 75 lat will be 111.618 * 28.902 * 1e6
    expect_less_than((abs(111.618 * 28.902 * 1e6) - mean(c(area[1, -90:89+0.5 == 74.5], area[1, -90:89+0.5 == 75.5])))/ abs(111.618 * 28.902 * 1e6), 1e-2)
    
})

test_that("Test global totals with real data", {
    path <- "../../sampledata"
    d <- loadCMIP5('areacella', 'GFDL-CM3', 'historical', path=path, verbose=F, loadAs='array')
    
    area <- calcGridArea(lat=d$lat, lon=d$lon)
    expect_less_than(max(as.numeric(abs(area-d$val[,,1,1])/d$val[,,1,1])), 1e-3)
})