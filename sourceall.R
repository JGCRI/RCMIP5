library(testthat)

# Load sample data, as package
load("data/tas_Amon_CMCC-CESM_historical_r1i1p1.Rdata")
load("data/tas_Amon_CMCC-CESM_rcp85_r1i1p1.Rdata")

source("R/getFileInfo.R")
source("R/checkTimePeriod.R")

source("R/loadEnsemble.R")
source("R/loadModel.R")

source("R/RCMIP5.R")
source("R/internalHelpers.R")
source("R/makeAnnualStat.R")
source("R/makeGlobalStat.R")
source("R/makeMonthlyStat.R")

source("R/devMain.R")
