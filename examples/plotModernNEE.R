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
for(modelStr in setdiff(modelArr, names(landC.ls))){
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
    save(file='examples/plotModernNEE.RData', landC.ls)

}

cat('finding min/max...')
minGrid <- landC.ls[[1]]$val
maxGrid <- landC.ls[[1]]$val
for(modelStr in names(landC.ls)){
    minGrid <- pmin(minGrid, landC.ls[[modelStr]]$val)
    maxGrid <- pmax(minGrid, landC.ls[[modelStr]]$val)
}

world.plot(lon=landC.ls[[1]]$lon, lat=landC.ls[[1]]$lat, x=minGrid,
           main='minimum [kg m^-2 yr^-1]', centerZero=TRUE, absNum=c(-0.5, 0.5))
dev.new()
world.plot(lon=landC.ls[[1]]$lon, lat=landC.ls[[1]]$lat, x=maxGrid,
           main='maximum [kg m^-2 yr^-1]', centerZero=TRUE, absNum=c(-0.5, 0.5))
dev.new()
world.plot(lon=landC.ls[[1]]$lon, lat=landC.ls[[1]]$lat, x=maxGrid-minGrid,
           main='range [kg m^-2 yr^-1]', absNum=c(0, 0.1))

globalMin <- sum(minGrid*landC.ls[[1]]$area, na.rm=TRUE)/1e12
globalMax <- sum(maxGrid*landC.ls[[1]]$area, na.rm=TRUE)/1e12
