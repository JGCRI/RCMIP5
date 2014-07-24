#library('plyr')

#' Compute annual mean of a variable
#'
#' @param temp.ls TODO
#' @param verbose logical. Print info as we go?
#' @return TODO
makeAnnualMean <- function(temp.ls,verbose=TRUE) {
    
    # TODO: this line doesn't seem to work, as calendarStr is "noleap" in example files
    numDays <- as.numeric(substr(temp.ls$calendarStr, 1, 3))

    startYr <- as.numeric(unlist(strsplit(regmatches(temp.ls$timeUnit,
                          regexpr('\\d+.\\d+.\\d+', temp.ls$timeUnit)), '-')))
    startYr <- startYr[1]+(startYr[2]-1)/12+(startYr[3]-1)/numDays

    yrIndex <- temp.ls$time/numDays + startYr

    #ptm <- proc.time()
    ##user  system elapsed 79.655   0.301  80.285 for apply
    ##user  system elapsed 438.284   1.423 441.075 for aaply
    ans <- array(NA, dim=c(dim(temp.ls$val)[c(1,2)], length(unique(floor(yrIndex)))))
    counter <- 1
    for(ii in unique(floor(yrIndex))){
        #cat(ii, ' ')
        ans[,,counter] <- aaply(temp.ls$val[,,ii == floor(yrIndex)], c(1,2), mean)
        counter <- counter + 1
    }

    ##user  system elapsed  78.618   0.275  79.111
    ##This function is a little faster but changes the order of the dimentions
    ##...from [lon, lat, time] to [time, lon, lat], recasting would negate
    ##...advantage probably
    #ans <- vapply(unique(floor(yrIndex)), function(x){
    #    cat(x, ' ')
    #    return(apply(temp.ls$val[,,x==floor(yrIndex)], c(1,2), mean))
    #}, FUN.VALUE=temp.ls$val[,,1])

    ##user  system elapsed 202.290   7.138 210.332
    #ans <- apply(temp.ls$val, c(1,2), function(x){
    #     vapply(unique(floor(yrIndex)), function(y){
    #         mean(x[y==floor(yrIndex)])
    #}, FUN.VALUE=1.0)})

    #print(proc.time()-ptm)
    return(list(val=ans, files=temp.ls$files,
                year=unique(floor(yrIndex)),
                numMonths=table(floor(yrIndex)),
                lat=temp.ls$lat, lon=temp.ls$lon,
                valUnit=temp.ls$valUnit))
}
