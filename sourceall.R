# File to source if not using package-development mode

library(testthat)
library(plyr)
library(abind)
library(ggplot2)
library(reshape2)
#library(ncdf4)
library(digest)
library(foreach)
library(dplyr)

# Load sample data, as package would
#load("data/tas_Amon_CMCC-CESM_historical_r1i1p1.Rdata")
#load("data/tas_Amon_CMCC-CESM_rcp85_r1i1p1.Rdata")

source("R/getFileInfo.R")
source("R/checkTimePeriod.R")

source("R/loadCMIP5.R")
source("R/loadEnsemble.R")

source("R/RCMIP5.R")
source("R/addProvenance.R")
source("R/makeAnnualStat.R")
source("R/makeZStat.R")
source("R/makeGlobalStat.R")
source("R/makeMonthlyStat.R")
source("R/mergeExperiments.R")
source("R/filterDimensions.R")
source("R/saveNetCDF.R")

#KTB old area weighted regridding functions
#...these are bloated and have lots of room
#...for optimization but do provide a starting point.
source("R/calcGridArea.R")
#source("R/regridVal.R")
#source("R/regridData.R")

#source('R/world.plot.R')
source("R/worldPlot.R")
