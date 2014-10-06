library(RCMIP5)
library(reshape2)

myPath <- '/Volumes/DATAFILES/downloads/'

allDownloads <- getFileInfo(myPath)

#check empty downloads
cat("Empty downloads:\n")
print(allDownloads[allDownloads$fileSize == 0,])

##example remove the empty files that failed to download, could be extended to
##...other booleans. Be careful with this! Removing a file like this is
##...permanent
#file.remove(as.character(allDownloads[allDownloads$fileSize == 0, 1]))

yrCheck <- checkTimePeriod(allDownloads)

##pull the years that require hand checking
handCheck <- yrCheck[is.na(yrCheck$allHere),]

##pull the years that are clearly bad
badCheck <- yrCheck[!yrCheck$allHere & is.finite(yrCheck$allHere),]

print("Files to hand check are in variable: 'handCheck'\nFiles flaged as incomplete years are in 'badCheck'\n")

##Generate download table for variables of interest
varArr <- c('cSoil', 'rh', 'tsl', 'cLitter', 'cCwd', 'pr', 'evspsbl', #soils
            'cVeg', 'ra', 'gpp', 'tas', 'pr', 'evspsbl', 'lai', 'rsds', #veg
            'areacella', 'sftlf') # needed fixed
varArr <- unique(varArr)
downloadSummary <- allDownloads[allDownloads$variable %in% varArr &
                                allDownloads$experiment %in% '1pctCO2',
                                c('variable', 'model')]

downloadSummary <- dcast(downloadSummary, model ~ variable,
                         fun.aggregate=length)


#Check that the correct models are downloaded based on what's on the ESFG
modelsWithSoil <- downloadSummary$cSoil > 0
modelsWithVeg <- downloadSummary$cVeg > 0

cat('Summary of model-variables for 1pctCO2\n')
print(downloadSummary[modelsWithSoil & modelsWithVeg,])



