if(!'RDirPath' %in% ls()) {
    RDirPath <- 'R'
}

# File to source if not using package-development mode

library(testthat)
library(ggplot2)
#library(ncdf4)
library(digest)
library(plyr)
library(dplyr)
library(abind)
library(devtools)
library(assertthat)

# Load sample data, as package would
#load("data/tas_Amon_CMCC-CESM_historical_r1i1p1.Rdata")
#load("data/tas_Amon_CMCC-CESM_rcp85_r1i1p1.Rdata")

source(sprintf("%s/getFileInfo.R", RDirPath))
source(sprintf("%s/checkTimePeriod.R", RDirPath))

source(sprintf("%s/loadCMIP5.R", RDirPath))
source(sprintf("%s/loadEnsemble.R", RDirPath))

source(sprintf("%s/RCMIP5.R", RDirPath))
source(sprintf("%s/addProvenance.R", RDirPath))
source(sprintf("%s/makeAnnualStat.R", RDirPath))
source(sprintf("%s/makeZStat.R", RDirPath))
source(sprintf("%s/makeGlobalStat.R", RDirPath))
source(sprintf("%s/makeMonthlyStat.R", RDirPath))
source(sprintf("%s/mergeExperiments.R", RDirPath))
source(sprintf("%s/filterDimensions.R", RDirPath))
source(sprintf("%s/saveNetCDF.R", RDirPath))

#KTB old area weighted regridding functions
#...these are bloated and have lots of room
#...for optimization but do provide a starting point.
source(sprintf("%s/calcGridArea.R", RDirPath))
#source(sprintf("%s/regridVal.R", RDirPath))
#source(sprintf("%s/regridData.R", RDirPath))

#source('%s/world.plot.R')
source(sprintf("%s/worldPlot.R", RDirPath))
