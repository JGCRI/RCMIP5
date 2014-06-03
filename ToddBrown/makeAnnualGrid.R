##Purpose: This script processes CMIP5 outputs by
##         1) merging ensembles from each model to
##         2) generate annual gridded means (on both orginal and 1x1 grid) and
##         3) global annual totals.
##Notes: [[TODO]] - flags desired features of bugs to fix
##Precondition: Several folders are required to be defined before this script is run (see below).
##Outputs: This file writes out raster files
##Org dev date: Febuary 2014
##Orginal programer: Kathe Todd-Brown (ktoddbrown@gmail.com)

##set the librarys
library('raster')
library('ncdf')

##########################################
##Define the directories
##!! Change this for your specifics
##########################################
#CMIPDir <- '/Volumes/DATAFILES/downloads'
#anuRstDir <- '/Volumes/DATAFILES/anuNCDF'

##Define the raster format
RstSaveExt <- 'grd'

forceAnnualCalc <- FALSE #force the annual multi-ensemble average calculation
forceUnion <- FALSE #force the joining of the historical and rcp
forceCommonScaling <- FALSE #force the rescaling to a 1x1 grid

##Definine the models we are interested in if not previously defined
if(!'modelToConsider' %in% ls()){
    modelToConsider <- c( "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CESM1-BGC",
                         "GFDL-ESM2G", "HadGEM2-ES", "NorESM1-ME", "NorESM1-M",
                         "inmcm4", "IPSL-CM5A-MR", "MIROC-ESM", "MPI-ESM-MR")
}
yrIndex <- rep(1:250, each=12)

##create the common 1x1 templet for our regridding
newproj <- '+proj=longlat'
templetCommon <- raster(nrows=180, ncols=360,
                        xmn=-180, xmx=180, ymn=-90, ymx=90, crs=newproj)

for(modelStr in modelToConsider){
    for(varStr in c('tas', 'gpp', 'ra', 'tsl10', 'cSoil', 'npp', 'rh', 'cLitter', 'cVeg', 'cCwd', 'rsds', 'pr', 'evspsbl')[1]){
        cat('*********\n\nProcessing', varStr,'\n**************\n')

        ##Load historical, rcp, and other experimental runs
        for(expStr in c('historical', 'rcp85', 'historicalGHG', '1pctCO2')){
            cat('***', expStr, '***\n')

            meanRst <- NULL
            counter <- 0

            ##ncdf can't track the layer names yet so save it to default grd
            ##Check that we haven't already processed this file
            meanFile <- sprintf('%s/%s_%s_%s.%s', anuRstDir, varStr, modelStr, expStr, RstSaveExt)
            if(!forceAnnualCalc & file.exists(meanFile)){
                cat('[', meanFile, '] already exists... moving on.\n')
                next
            }

            ##Get the files we are interested in
            fullFileList <- list.files(CMIPDir, sprintf('%s_.*mon_%s_%s_.*(nc|grd)$', varStr, modelStr, expStr), full.names=TRUE)

            ##Load and average the ensembles
            oldYMStr <- NULL
            if(length(fullFileList) == 0){
                cat('No files found... moving on\n')
                next
            }


             ##Tell the user how long this should take
            cat('Processing [', expStr, '] should 2 minutes a file for', length(list.files(CMIPDir, sprintf('%s_.*mon_%s_%s_.*(nc|grd)$', varStr, modelStr, expStr), full.names=TRUE)), 'files\n')

            ensembleArr <- unique(unlist(lapply(strsplit(fullFileList, '_'), function(x){x[5]})))
            for(ensembleStr in ensembleArr){
                ensembleRst <- NULL

                for(filename in list.files(CMIPDir, sprintf('%s_.*mon_%s_%s_%s_.*(nc|grd)$', varStr, modelStr, expStr, ensembleStr), full.names=TRUE)){
                    cat('[', filename, '] \n')
                    cat('loading ', format(Sys.time(), '%H:%M:%OS3...'))
                    if(is.null(ensembleRst)){ ##Set the orginal ensemble
                        ensembleRst <- brick(filename, varname=varStr, lvar=3)
                        yrIndex <- as.numeric(unlist(regmatches(names(ensembleRst),
                                                                gregexpr("\\d+{4}",names(ensembleRst)))))
                    }else{ ##Add next layers to the ensemble
                        tempRst <- brick(filename, varname=varStr, lvar=3)
                        ensembleRst <- addLayer(ensembleRst, tempRst)
                        yrIndex <- c(yrIndex,
                                     as.numeric(unlist(regmatches(names(tempRst),
                                      gregexpr("\\d+{4}",names(tempRst))))))
                }
                cat('done\n')
            }
                ##pull the number of months for each year
                numMonth <- table(yrIndex)
                ##make a year identifier with the year and number of months
                YMstr <- paste(names(numMonth), numMonth, sep='.')

                ##Set the number of years.months to check against in the future
                if(is.null(oldYMStr)){
                    oldYMStr <- YMstr
                }else{ ##check that we have the same time frame to compare against
                    if(!all(YMstr %in% oldYMStr)){
                        stop('Time strings do not match.')
                    }
                }
                cat(format(Sys.time(), 'calc anu means %H:%M:%OS3...'))
                ##Calculate the annual means
                if(!('meanRst' %in% ls()) || is.null(meanRst)){
                    meanRst <- stackApply(ensembleRst, yrIndex-yrIndex[1]+1,
                                          fun=mean, na.rm=FALSE)
                    meanRstCount <- 1
                }else{
                    ##Add the annual means to previous ensembles
                    meanRst <- overlay(meanRst,
                                       stackApply(ensembleRst, yrIndex-yrIndex[1]+1,
                                                  fun=mean, na.rm=FALSE),
                                       fun=sum, na.rm=FALSE)
                }
            cat(format(Sys.time(), '%H:%M:%OS3\n'))
            counter <- counter + 1 ##keep track of the number of ensembles
        }
            ##take the mean from the sums
            meanRst <- calc(meanRst, fun=function(x){x/counter})
            ##name the years
            names(meanRst) <- YMstr

            ##Save the gridded annual means
            writeRaster(meanRst, filename=meanFile, overwrite=TRUE)
        }


        ##Keep things clean
        closeAllConnections()
        cat('flag2')

        #####################################
        ##Merge the historical and rcp runs
        #####################################

        ##file name for the merged gridded run
        fullFilename <- sprintf('%s/%s_%s.%s', anuRstDir, varStr, modelStr,  RstSaveExt)
        histFilename <- sprintf('%s/%s_%s_%s.%s', anuRstDir, varStr, modelStr, 'historical', RstSaveExt)
        rcp85Filename <- sprintf('%s/%s_%s_%s.%s', anuRstDir, varStr, modelStr, 'rcp85', RstSaveExt)
        ##CHeck that we haven't already processed
        if((forceAnnualCalc | forceUnion | !(file.exists(fullFilename))) &
           file.exists(histFilename) & file.exists(rcp85Filename)){

            histMean <- brick(histFilename)
            rcpMean  <- brick(rcp85Filename)
             cat('flag1')
            ##Drop not full years, we've recorded the number of months that contribute to the annual means so drop the ones that aren't 'full'
            ##[[TODO]] we could be a bit more clever about this and merge fractional years at the start/end of the runs but haven't implemented this year
            cat('Checking for full years.\n')
            histYr <- as.numeric(substr(names(histMean), 2,5))
            histMnt <-  as.numeric(substring(names(histMean), 7))
            rcpYr <-  as.numeric(substr(names(rcpMean), 2,5))
            rcpMnt <-  as.numeric(substring(names(rcpMean), 7))

            shortHist <- histMnt != 12
            shortRcp <- rcpMnt != 12

            if(length(intersect(rcpYr[shortRcp] , histYr[shortHist])) != 0){
                stop('have not implimented merging of fractional years yet')
            }

            cat('Dropping [', names(histMean)[shortHist], '] as fractional years from historical means.\n')
            histMean <- dropLayer(histMean, (1:length(shortHist))[shortHist])
            cat('Dropping [', names(rcpMean)[shortRcp], '] as fractional years from rcp means.\n')
            rcpMean <- dropLayer(rcpMean, (1:length(shortRcp))[shortRcp])

            ##Drop overlapping years from hist
            histDrops <- names(histMean) %in% names(rcpMean)
            histDrops <- (1:(dim(histMean)[3]))[histDrops]
            cat('Dropping overlaying full years from the historical run [', names(histMean)[histDrops],']\n')
            histMean <- dropLayer(histMean, histDrops)

            ##merge the two trimmed layers
            anuMean <- addLayer(histMean, rcpMean)


            cat('Final check...')
            anuYr <- as.numeric(substr(names(anuMean), 2,5))
            anuMnt <-  as.numeric(substring(names(anuMean), 7))
            if(any(anuMnt != 12)){
                stop('There are fractional years in the final annual mean!')
            }
            if(any((anuYr[2:length(anuYr)]-anuYr[(2:length(anuYr))-1]) != 1)){
                stop('There are non-sequential years in the final annual mean!')
            }
            cat('Annual mean years are whole and sequencial[',anuYr[1:3], '...',rev(rev(anuYr)[1:3]),']\n')

            ##Save on orginal grid
            writeRaster(anuMean, fullFilename, overwrite=TRUE)
        }

        ##File name for the common merged annual grid
        commonFilename <- sprintf('%s/%s_%s_commmon.%s', anuRstDir, varStr, modelStr,  RstSaveExt)

        if(!file.exists(commonFilename) && file.exists(fullFilename) &&
           !(forceAnnualCalc | forceUnion | forceCommonScaling) ){
            ##Save on common 1x1 grid
            temp <- brick(fullFilename)
            cat('rotating longitude reference...')
            temp <- rotate(temp) #shift from lon:0,360 to -180, 180
            cat('done\n')

            cat('projecting values from [',res(temp),'] to common grid [', res(templetCommon), '] ')
            commonVal <- projectRaster(temp,  templetCommon, filename=commonFilename, overwrite=TRUE)
            cat('done\n')
        }
    }
}
