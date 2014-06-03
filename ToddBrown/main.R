##Purpose: Run a test case to demonstrate makeAnnualGrid and loadmodel

CMIPDir <- '../JGCRI/sampledata'
anuRstDir <- 'anuGrid'
modelToConsider <- c('CMCC-CESM')

source('makeAnnualGrid.R') ##this will error out with the test data because it'strying to merge the historical and rcp runs but the years don't match

modelStr <-  'CMCC-CESM'
commonStr <- '_historical'
varStrArr <- c('tas')
source('loadmodel.R')

## there is now a variable 'tas' in your workspace which is the annual mean for the CMCC-CESM historical run
