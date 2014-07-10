makeAnnualMean <- function(temp.ls){
    ##Input: Takes a raster object like the one returned by either loadEnsemble
    ##       and loadModel

    numDays <- as.numeric(substr(temp.ls$calendarStr, 1, 3))

    startYr <- as.numeric(unlist(strsplit(regmatches(temp.ls$timeUnit,
                          regexpr('\\d+.\\d+.\\d+', temp.ls$timeUnit)), '-')))
    startYr <- startYr[1]+(startYr[2]-1)/12+(startYr[3]-1)/numDays

    yrIndex <- temp.ls$time/numDays + startYr

    ans <- vapply(unique(floor(yrIndex)), function(x){
        aaply(temp.ls$val[,,x==floor(yrIndex)], c(1,2), mean)
    }, FUN.VALUE=temp.ls$val[,,1])

    #ans <- apply(temp.ls$val, c(1,2), function(x){
    #     vapply(unique(floor(yrIndex)), function(y){
    #         mean(x[y==floor(yrIndex)])
    #}, FUN.VALUE=1.0)})

    #ans <- a_ply(temp.ls$val, c(1,2), function(x){
    #     vapply(unique(floor(yrIndex)), function(y){
    #         mean(x[y==floor(yrIndex)])
    #}, FUN.VALUE=1.0)})

    return(list(val=ans, files=temp.ls$files,
                year=unique(floor(yrIndex)),
                lat=temp.ls$lat, lon=temp.ls$lon,
                valUnit=temp.ls$valUnit))
}
