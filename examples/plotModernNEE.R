source('../R/RCMIP5.R')
source('../sourceall.R')

expStr <- 'historical'
yearArr <- 1995:2004

modelArr <- 'bcc-csm1-1'

for(modelStr in modelArr){
    soil <- loadModel(experiment=expStr, model=modelStr, variable='cSoil')
    cwd <-  loadModel(experiment=expStr, model=modelStr, variable='cCwd')
    litter <-  loadModel(experiment=expStr, model=modelStr, variable='cLitter')
    veg <-  loadModel(experiment=expStr, model=modelStr, variable='cVeg')
    gpp <-  loadModel(experiment=expStr, model=modelStr, variable='gpp')


}
