setwd(gsub('/examples$', '', getwd()))
source('sourceall.R')

cat('starting plotModernNEE.R\n')
expStr <- 'historical'
yearArr <- as.numeric(1995:2005)

modelArr <- c('bcc-csm1-1-m')

CMIP5Path <- '/Volumes/DATAFILES/downloads/'

landC.ls <- list()
for(modelStr in modelArr){
    cat('loading variables for [', modelStr, ']\n')
    cat('loading grid area...')
    gridArea <- loadModel(experiment=expStr, model=modelStr, variable='areacella', path=CMIP5Path, verbose=TRUE)
    print(summary(gridArea))
    cat('\nloading land percentage...')
    landPer <- loadModel(experiment=expStr, model=modelStr, variable='sftlf', path=CMIP5Path)

    print(summary(landPer))

    #Load soil
    landC <- NULL
    cat('loading land carbon\n')
    for(varStr in c('cSoil', 'cCwd', 'cLitter', 'cVeg')){
        cat('loading [', varStr, ']...')
        temp <- loadModel(experiment=expStr, model=modelStr,
                          variable=varStr, path=CMIP5Path)
        if(is.null(temp)){
            cat('no', varStr, 'found... moving on\n')
        }else{
            cat('pulling dC...')
            temp10yr <- makeAnnualStat(filterDimensions(temp, years=yearArr), FUN=mean)
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
        }
    }
    cat('done and saving\n')

    landC.ls[[modelStr]] <- landC


}
