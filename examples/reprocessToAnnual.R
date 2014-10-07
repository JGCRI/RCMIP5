library(RCMIP5)

downloadPath <-  '/Volumes/DATAFILES/downloads'

allDownloads <- getFileInfo(downloadPath)

outputPath <- '/Volumes/DATAFILES/RCMIP5output'

expStr <- '1pctCO2'

##get all the models with both soil and veg carbon
allModels <- intersect(
              unique(allDownloads$model[allDownloads$experiment %in% expStr &
                                        allDownloads$variable %in% 'cSoil']),
              unique(allDownloads$model[allDownloads$experiment %in% expStr &
                                        allDownloads$variable %in% 'cVeg']))

carbonStocks <- c('cSoil', 'cVeg', 'cCwd', 'cLitter')
carbonFlux <- c('gpp', 'rh', 'ra')
environ <- c('tas', 'pr', 'evspsbl', 'rsds', 'lai')
for(modelStr in allModels){
    for(varStr in c(carbonStocks, carbonFlux, environ)){
        outputFile <- file.path(outputPath, paste(expStr, varStr, modelStr, 'yearly.RData', sep='_'))
        loadExists <- any(allDownloads$experiment %in% expStr &
                          allDownloads$model %in% modelStr &
                          allDownloads$variable %in% varStr)
        if(!loadExists){
            cat('no simulation to load for [', basename(outputFile), ']\n')
            next
        }

        if(file.exists(outputFile) ){
            cat('File exists: [', basename(outputFile), '] moving on\n')
            next
        }

        cat('making file: [', outputFile, ']...')
        CMIP5sim <- makeAnnualStat(loadCMIP5(experiment=expStr,
                                             variable=varStr,
                                             model=modelStr, path=downloadPath))
        save(file=outputFile, CMIP5sim)
        cat('\n')

    }
}

#Deal with tsl which has 4D
