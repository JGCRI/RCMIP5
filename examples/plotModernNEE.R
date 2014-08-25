setwd(gsub('/examples$', '', getwd()))
source('sourceall.R')

cat('starting plotModernNEE.R\n')
expStr <- 'historical'
yearArr <- as.numeric(1995:2004)

modelArr <- c('bcc-csm1-1-m')

CMIP5Path <- '/Volumes/DATAFILES/downloads/'

for(modelStr in modelArr){
    gridArea <- loadModel(experiment=expStr, model=modelStr, variable='areacella', path=CMIP5Path)
    landPer <- loadModel(experiment=expStr, model=modelStr, variable='sftlf', path=CMIP5Path)
    soil <- loadModel(experiment=expStr, model=modelStr, variable='cSoil', path=CMIP5Path)
    soil10yr <- filterDimensions(soil, years=yearArr)
    soilModern <- list(val=aaply(soil10yr$val, 1:2, mean),
                       area=gridArea$val*landPer$val/100,
                       lat=soil10yr$lat, lon=soil10yr$lon,
                       valUnit=soil10yr$valUnit)
    soilRegrid <- regrid_data(soilModern, 1)
    #cwd <-  loadModel(experiment=expStr, model=modelStr, variable='cCwd')
    #litter <-  loadModel(experiment=expStr, model=modelStr, variable='cLitter')
    #veg <-  loadModel(experiment=expStr, model=modelStr, variable='cVeg')
    #gpp <-  loadModel(experiment=expStr, model=modelStr, variable='gpp')


}
