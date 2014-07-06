library(plyr) #required in checkTimePeriod
library(reshape) #required in devMain
library(raster) #required in loadEnsemble,
library(ncdf4) #required in loadEnsemble,

##Manage download functions
source('getFileInfo.R')
source('checkTimePeriod.R')

##read in varaibles
source('loadEnsemble.R')


if(TRUE){ ##run the management functions
######################################################
##Get the information on the files already downloaded
######################################################
#fileInfo <- getFileInfo('/Volumes/DATAFILES/downloads') #peers into your soul
               ##This script was tested on a directory that had >10K files
               ##...downloaded. Runtime took a few seconds
fileInfo <- getFileInfo() #runs on the current directory

#Checks that the times match for multi-file ensembles
checkTime <- checkTimePeriod(fileInfo)


######################################################
##Examples of useful oneliners that we may want to consider
##...formalizing in a package
######################################################

##example remove the empty files that failed to download, could be extended to
##...other booleans. Be careful with this! Removing a file like this is
##...permanent and the ESFG is a pain to download from
#file.remove(as.character(fileInfo[fileInfo$fileSize == 0, 1]))

##Check that the time period for a model-experiment-variable is continuous
checkVar <- ddply(checkTime, .(experiment, model, variable), summarize, startSame=all(startDate==startDate[1]), endSame= all(endDate==endDate[1]))

##Check that the time period for a model-experiment match across variables
checkModel <- ddply(checkTime, .(experiment, model), summarize, startSame=all(startDate==startDate[1]), endSame= all(endDate==endDate[1]))

##count the number of ensembles for a given e-m-v
countEnsemble <- ddply(fileInfo, .(experiment, model, variable), summarize, numEnsembles=length(unique(ensemble)))

##Report the number of ensembles of variables for model-experiment in a different way (better for tabular reporting)
countEnsembleAlt <- cast(countEnsemble, experiment+model~variable, value='numEnsembles')
}

######################################################
##Load variables
######################################################
prcTemp <- loadEnsemble(experiment='rcp85', variable='prc', model='GFDL-CM3', ensemble='r1i1p1')

#cSoilTemp <- loadEnsemble(CMIP5dir='/Volumes/DATAFILES/downloads', experiment='historical', variable='cSoil', model='CanESM2', #model='GISS-E2-R',
                          ensemble='r1i1p1')


#cSoilTemp <- loadEnsemble(CMIP5dir='/Volumes/DATAFILES/downloads', experiment='historical', variable='cSoil', model='CanESM2')
