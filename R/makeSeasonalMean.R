#library('plyr')
makeSeasonalMean <- function(temp.ls, yrRange){

    numDays <- as.numeric(substr(temp.ls$calendarStr, 1, 3))

    startYr <- as.numeric(unlist(strsplit(regmatches(temp.ls$timeUnit,
                          regexpr('\\d+.\\d+.\\d+', temp.ls$timeUnit)), '-')))
    startYr <- startYr[1]+(startYr[2]-1)/12+(startYr[3]-1)/numDays

    yrIndex <- temp.ls$time/numDays + startYr
    mntIndex <- floor((yrIndex%% 1)*12+1)


    ans <- array(NA, dim=c(dim(temp.ls$val)[c(1,2)], 12))
    counter <- 1
    for(ii in 1:12){
        #cat(ii, ': ', which((ii == mntIndex) & floor(yrIndex) >= yrRange$min & floor(yrIndex) <= yrRange$max), '\n')
        ans[,,counter] <- aaply(temp.ls$val[,,(ii == mntIndex) &
                                            floor(yrIndex) >= yrRange$min &
                                            floor(yrIndex) <= yrRange$max],
                                c(1,2), mean)
        counter <- counter + 1
    }


    return(list(val=ans, files=temp.ls$files,
                lat=temp.ls$lat, lon=temp.ls$lon,
                valUnit=temp.ls$valUnit))


}
