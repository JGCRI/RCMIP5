setwd(gsub('/examples$', '', getwd()))
source('sourceall.R')

cat('starting plotModernNEE.R\n')
expStr <- 'historical'
yearArr <- as.numeric(1995:2005)

modelArr <- c('bcc-csm1-1-m', 'BNU-ESM', 'CanESM2', 'CESM1-BGC',  'HadGEM2-ES',
              'inmcm4', 'IPSL-CM5A-LR', 'MIROC-ESM', 'MPI-ESM-LR')

CMIP5Path <- '/Volumes/DATAFILES/downloads/'
load('examples/plotModernNEE.RData')
if(! 'landC.ls' %in% ls()){
    landC.ls <- list()
}

if(! 'gpp.ls' %in% ls()){
    gpp.ls <- list()
}
for(modelStr in modelArr){
    cat('loading variables for [', modelStr, ']\n')
    cat('loading grid area...')
    gridArea <- loadModel(experiment=expStr, model=modelStr, variable='areacella', path=CMIP5Path, verbose=TRUE)
    #dim(gridArea$val) <- dim(gridArea$val)[1:2]
    print(summary(gridArea))
    cat('\nloading land percentage...')
    landPer <- loadModel(experiment=expStr, model=modelStr, variable='sftlf', path=CMIP5Path)
    #dim(landPer$val) <- dim(landPer$val)[1:2]
    print(summary(landPer))

    if(is.null(landPer)){
        cat('Setting land percentage to one\n')
        landPer$val <- 100
    }

    if(max(as.vector(landPer$val), na.rm=TRUE) != 100){
        cat('bad land percentage... moving on\n')
        next
    }

    #Load soil
    if(!modelStr %in% names(landC.ls)){
        landC <- NULL
        cat('loading land carbon\n')
        for(varStr in c('cSoil', 'cCwd', 'cLitter', 'cVeg')){
            cat('loading [', varStr, ']...')
            temp <- loadModel(experiment=expStr, model=modelStr,
                              variable=varStr, path=CMIP5Path)
            if(is.null(gridArea)){
                gridArea <- list(val=calcGridArea(lat=temp$lat,
                                 lon=temp$lon))
            }
            if(is.null(temp)){
                cat('no', varStr, 'found... moving on\n')
            }else{
                cat('pulling dC...')
                temp10yr <- makeAnnualStat(filterDimensions(temp, years=yearArr),
                                           FUN=mean)
                tempdC <- temp10yr$val[,,2:11] - temp10yr$val[,,1:10]
                tempModern <- list(val=aaply(tempdC, 1:2, mean),
                                   area=gridArea$val*landPer$val/100,
                                   lat=temp10yr$lat, lon=temp10yr$lon,
                                   valUnit=paste(temp10yr$valUnit, 'yr^-1'))
                tempRegrid <- regrid_data(tempModern, 1)
                if(is.null(landC)){
                    landC <- tempRegrid
                }else{
                    landC$val <- landC$val + tempRegrid$val
                }
                cat(sprintf('%.2f Pg-C yr^-1\n',
                            sum(as.vector(tempModern$val *
                                          gridArea$val*landPer$val/100),
                                na.rm=TRUE)/1e12))

            }
        }
        cat('done and saving\n')

        landC.ls[[modelStr]] <- landC
        save(file='examples/plotModernNEE.RData', landC.ls, gpp.ls)
    }
    if(!modelStr %in% names(gpp.ls)){
        gpp <- NULL
        cat('loading gpp\n')
        varStr <- 'gpp'
        cat('loading [', varStr, ']...')
        temp <- loadModel(experiment=expStr, model=modelStr,
                          variable=varStr, path=CMIP5Path)
        if(is.null(gridArea)){
            gridArea <- list(val=calcGridArea(lat=temp$lat,
                             lon=temp$lon))
        }
        if(is.null(temp)){
            cat('no', varStr, 'found... moving on\n')
        }else{
            cat('pulling modern...')
            temp10yr <- makeAnnualStat(filterDimensions(temp, years=yearArr),
                                       FUN=mean)
            tempModern <- list(val=aaply(tempdC, 1:2, mean),
                               area=gridArea$val*landPer$val/100,
                               lat=temp10yr$lat, lon=temp10yr$lon,
                               valUnit=paste(temp10yr$valUnit, 'yr^-1'))
            tempRegrid <- regrid_data(tempModern, 1)
            if(is.null(gpp)){
                gpp <- tempRegrid
            }else{
                gpp$val <- gpp$val + tempRegrid$val
            }
            cat(sprintf('%.2f Pg-C yr^-1\n',
                        sum(as.vector(tempModern$val *
                                      gridArea$val*landPer$val/100),
                            na.rm=TRUE)/1e12))

        }

        cat('done and saving\n')

        gpp.ls[[modelStr]] <- gpp
        save(file='examples/plotModernNEE.RData', landC.ls, gpp.ls)
    }
}


cat('finding min/max and NEE\n')
minGrid <- landC.ls[[1]]$val
maxGrid <- landC.ls[[1]]$val
for(modelStr in names(landC.ls)){
    minGrid <- pmin(minGrid, landC.ls[[modelStr]]$val)
    maxGrid <- pmax(maxGrid, landC.ls[[modelStr]]$val)
    cat(sprintf('%s: %0.2f Pg-C yr^-1\n', modelStr, sum(landC.ls[[modelStr]]$val*landC.ls[[modelStr]]$area, na.rm=TRUE)/1e12))
}
cat('done\n')

cat('finding GPP\n')
for(modelStr in names(gpp.ls)){
    cat(sprintf('%s: %0.2f Pg-C yr^-1\n', modelStr, sum(gpp.ls[[modelStr]]$val*landC.ls[[modelStr]]$area, na.rm=TRUE)/1e12))
}
cat('done\n')

pdf('examples/plotModernNEE.pdf', height=2*3, width=2*5)
par(mfrow=c(2,2), mar=c(1,3,3,1), oma=c(0,0,0,3) )
world.plot(lon=landC.ls[[1]]$lon, lat=landC.ls[[1]]$lat, x=minGrid,
           main='minimum [kg m^-2 yr^-1]', centerZero=TRUE,
           absNum=c(-0.25, 0.25))

world.plot(lon=landC.ls[[1]]$lon, lat=landC.ls[[1]]$lat, x=maxGrid,
           main='maximum [kg m^-2 yr^-1]', centerZero=TRUE, absNum=c(-0.25, 0.25))

world.plot(lon=landC.ls[[1]]$lon, lat=landC.ls[[1]]$lat, x=maxGrid-minGrid,
           main='range [kg m^-2 yr^-1]')
graphics.off()

globalMin <- sum(minGrid*landC.ls[[1]]$area, na.rm=TRUE)/1e12
globalMax <- sum(maxGrid*landC.ls[[1]]$area, na.rm=TRUE)/1e12
