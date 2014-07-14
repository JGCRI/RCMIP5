library(plyr) #required in checkTimePeriod
library(reshape) #required in devMain
library(raster) #required in loadEnsemble,
library(ncdf4) #required in loadEnsemble,

##Manage download functions
source('getFileInfo.R')
source('checkTimePeriod.R')

##read in varaibles
source('loadEnsemble.R')
source('loadModel.R')
source('makeAnnualMean.R')
source('makeSeasonalMean.R')


if(!TRUE){ ##run the management functions
######################################################
##Get the information on the files already downloaded
######################################################
if(file.exists('/Volumes/DATAFILES/downloads')){
    cat('getting file information from [/Volumes/DATAFILES/downloads]...')
    fileInfo <- getFileInfo('/Volumes/DATAFILES/downloads')
               ##This script was tested on a directory that had >10K files
               ##...downloaded. Runtime took a few seconds
}else{
    cat('getting file inforation from current directory...')
    fileInfo <- getFileInfo() #runs on the current directory
}
cat('done\n')

##Checks that the times match for multi-file ensembles
cat('checking that multi-file ensembles have matching time periods...')
checkTime <- checkTimePeriod(fileInfo)
cat('done\n')

######################################################
##Examples of useful oneliners that we may want to consider
##...formalizing in a package
######################################################

cat('going through various useful examples of how to use information from file names we just constructed...')
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
cat('done\n')
}

######################################################
##Load variables
######################################################

if(file.exists('/Volumes/DATAFILES/downloads')){
    cat('loading ensemble... 1...')
    cSoilEns <- loadEnsemble(path='/Volumes/DATAFILES/downloads', experiment='historical', variable='cSoil', model='GISS-E2-R',#model='CanESM2',
                          ensemble='r1i1p1')
    cat('2...')
    cSoilEns_CanESM2 <- loadEnsemble(path='/Volumes/DATAFILES/downloads', experiment='historical', variable='cSoil', model='CanESM2',
                          ensemble='r1i1p1')
    cat('done\n')

    cat('loading model mean...')
    cSoilModel_CanESM2 <- loadModel(path='/Volumes/DATAFILES/downloads', experiment='historical', variable='cSoil', model='CanESM2')
    cat('done\n')

    cat('making gridded annual mean...')
    meanTemp <- makeAnnualMean(cSoilModel_CanESM2)
    cat('done\n')

    cat('make seasonal average...')
    seasonalTemp <- makeSeasonalMean(cSoilModel_CanESM2, yrRange=list(min=1850, max=1860))
    cat('done\n')
#temp <- rasterToArray(seasonalTemp)
}else{
    prcTemp <- loadEnsemble(experiment='rcp85', variable='prc', model='GFDL-CM3', ensemble='r1i1p1')
}
