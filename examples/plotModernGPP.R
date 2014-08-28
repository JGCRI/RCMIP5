#This code is designed to run on

setwd(gsub('/examples$', '', getwd()))
source('sourceall.R')

cat('starting plotModernNEE.R\n')
expStr <- 'historical'
yearArr <- as.numeric(1995:2005)

modelArr <- c('bcc-csm1-1-m', 'BNU-ESM', 'CanESM2', 'CESM1-BGC',  'HadGEM2-ES',
              'inmcm4', 'IPSL-CM5A-LR', 'MIROC-ESM', 'MPI-ESM-LR')

CMIP5Path <- '/Volumes/DATAFILES/downloads/'
if(file.exists('examples/plotModernGPP.RData')){
    load('examples/plotModernGPP.RData')
}
if(! 'gpp.ls' %in% ls()){
    gpp.ls <- list()
}
for(modelStr in setdiff(modelArr, names(gpp.ls))){
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
        cat('Setting land percentage to 100\n')
        landPer$val <- 100
    }

    if(max(as.vector(landPer$val), na.rm=TRUE) != 100){
        cat('bad land percentage... moving on\n')
        next
    }

    cat('loading gpp\n')
    varStr <- 'gpp'

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
        ##convert from s-1 to yr-1
        temp$val <- temp$val*3.15569e7
        #We know that gpp variables must be in [kg m-2 s-1]
        temp$valUnit <- gsub('s', 'yr', temp$valUnit)
        tempModern <- list(val=aaply(temp$val, 1:2, mean),
                           area=gridArea$val*landPer$val/100,
                           lat=temp$lat, lon=temp$lon,
                           valUnit=temp$valUnit)
        gpp <- regrid_data(tempModern, 1)

        cat(sprintf('%.2f Pg-C yr^-1\n',
                    sum(as.vector(tempModern$val*gridArea$val*landPer$val/100),
                        na.rm=TRUE)/1e12))

    }

    cat('done and saving\n')

    gpp.ls[[modelStr]] <- gpp
    save(file='examples/plotModernGPP.RData', gpp.ls)
}

cat('finding GPP\n')
minGrid <- gpp.ls[[1]]$val
maxGrid <- gpp.ls[[1]]$val
for(modelStr in names(gpp.ls)){
    if(all(as.vector(gpp.ls[[modelStr]]$val) <= 0, na.rm=TRUE)){
        cat('reseting flux direction for ', modelStr,'\n')
        gpp.ls[[modelStr]]$val <- gpp.ls[[modelStr]]$val * -1
    }
    minGrid <- pmin(minGrid, gpp.ls[[modelStr]]$val)
    maxGrid <- pmax(maxGrid, gpp.ls[[modelStr]]$val)
    cat(sprintf('%s: %0.2f Pg-C yr^-1\n', modelStr, sum(gpp.ls[[modelStr]]$val*gpp.ls[[modelStr]]$area, na.rm=TRUE)/1e12))
}
cat('done\n')

pdf('examples/plotModernGPP.pdf', height=2*3, width=2*5)
par(mfrow=c(2,2), mar=c(1,3,3,1), oma=c(0,0,0,3) )
world.plot(lon=gpp.ls[[1]]$lon, lat=gpp.ls[[1]]$lat, x=minGrid,
           main='minimum [kg m^-2 yr^-1]', centerZero=TRUE,
           absNum=c(0, 4))

world.plot(lon=gpp.ls[[1]]$lon, lat=gpp.ls[[1]]$lat, x=maxGrid,
           main='maximum [kg m^-2 yr^-1]', centerZero=TRUE, absNum=c(0, 4))

world.plot(lon=gpp.ls[[1]]$lon, lat=gpp.ls[[1]]$lat, x=maxGrid-minGrid,
           main='range [kg m^-2 yr^-1]')
graphics.off()

globalMin <- sum(minGrid*gpp.ls[[1]]$area, na.rm=TRUE)/1e12
globalMax <- sum(maxGrid*gpp.ls[[1]]$area, na.rm=TRUE)/1e12
