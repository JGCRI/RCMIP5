library('raster')
library('ncdf4')

makeAnnualMean <- function(rasterObj){
    ##Input: Takes a raster object like the one returned by either loadEnsemble
    ##       and loadModel
    yrIndex <- as.numeric(substr(names(rasterObj), 2, 5))

    meanRst <- stackApply(rasterObj, yrIndex-yrIndex[1] + 1,
                          fun=mean, na.rm=FALSE)

    #names(meanRst) <- unique(yrIndex)
    return(meanRst)
}
