library('raster')
library('ncdf')

makeSeasonalMean <- function(rasterObj, yrRange){

    yrIndex <- as.numeric(substr(names(rasterObj), 2, 5))
    yrSubset <- yrIndex >= yrRange$min & yrIndex <= yrRange$max
    mntIndex <- as.numeric(substr(names(rasterObj), 7, 8))

    meanRst <- subset(rasterObj, subset=yrIndex[yrSubset])

    meanRst <- stackApply(meanRst, mntIndex[yrSubset],
                          fun=mean, na.rm=FALSE)

    names(meanRst) <- unique(mntIndex)
    return(meanRst)

}
