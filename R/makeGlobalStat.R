makeGlobalStat <- function(val, area=NULL, stat.fun=NULL,
                           unitStr='', variable='', ...){

    #Sanity check val
    stopifnot(class(val) %in% 'cmip5data')
    #check that val only has 3 dimentions
    if(length(dim(val$val)) != 3){
        stop('Val$val must be 3 dimentions')
    }
    varProv <- c('(val provenance)', val$provenance)

    if(!is.null(area)){
        stopifnot(class(area) %in% 'cmip5data')
        ##some of the area have a 3rd dummy dimention (ie [,,1]) deal with this
        if(length(as.vector(area$val)) != prod(dim(area$val)[1:2])){
            stop('Area$val can not be more then 2 dimentions')
        }
        dim(area$val) <- dim(area$val)[1:2]

        ##check that the val and area dimentions match
        if(! (identical(val$lat, area$lat) &
              identical(val$lon, area$lon))){
            stop('lat-lon does not match')
        }
        varProv <- c(varProv, '(area provenance)', area$provenance)
    }

    ans <- val
    #remove all the spatial info
    ans[c('depth', 'lat', 'lev', 'lon')] <- NULL
    if(!is.null(area) & is.null(stat.fun)){
        ans$unitStr <- val$unitStr
        ans$variable <- val$variable
        ans$val <- apply(val$val, c(3), function(x){
            sum(x*area$val, na.rm=TRUE)/sum(area$val[is.finite(x)], na.rm=TRUE)
        })
    }else if(!is.null(area)){
        ans$valUnit <- unitStr
        ans$variable <- variable
        ans$val <- apply(val$val, c(3), function(x){
            stat.fun(x*area$val, ...)})
    }else{
        cat('flag1')
        ans$valUnit <- unitStr
        ans$variable <- variable
        ans$val <- apply(val$val, c(3), stat.fun, ...)
    }

    ans$provenance <-  addProvenance(NULL,
                              c("Computed global stat", varProv))
    return(ans)
}
