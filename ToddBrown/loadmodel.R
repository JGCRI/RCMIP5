##modelStr is previously defined

##set the librarys
library('raster')
library('ncdf4')
library('abind')

source('rasterToArray.R') #convert raster files to our array format

##Where is everything located
#CMIPDir <- '/Volumes/DATAFILES/downloads'
#anuRstDir <- '/Volumes/DATAFILES/anuNCDF'

CToK <- 273.15 ##Convert Celcius to Kalvin
yrToSec <- 3.15569e7 ##convert seconds to years using this constant
RstSaveExt <- 'grd' ##which raster format is everything saved to?

if(! 'commonStr' %in% ls()){
    commonStr <- '' #alternative is '_commmon' (yes, it's misspelled)
}

if(! 'varStrArr' %in% ls()){
    varStrArr <- c('npp', 'gpp', 'ra', 'rh',
                   'cSoil', 'cVeg', 'tsl10')
}

cat('loading: ', varStrArr, '\n')
cat('common string is: [', commonStr, ']\n')

multiFactor <- list(npp=yrToSec, gpp=yrToSec, ra=yrToSec, rh=yrToSec,
                         cSoil=1, cVeg=1, tsl10=1, tas=1)
addFactor <-  list(npp=0, gpp=0, ra=0, rh=0,
                         cSoil=0, cVeg=0, tsl10=-1*CToK, tas=-1*CToK)
yrStr <- NULL
for(varStr in setdiff(varStrArr, c('cLitter', 'cCwd'))){
    cat('loading',varStr,'for', modelStr, '...')
    eval(parse(text=sprintf('%s <- NULL', varStr)))
    varFilename <- sprintf('%s/%s_%s%s.%s',
                       anuRstDir, varStr, modelStr,  commonStr, RstSaveExt)
    if(file.exists(varFilename)){
        var <- brick(varFilename)
        Rstlandarea <- raster::area(var)
        yrStr.var <- names(var)
        var <- rasterToArray(var)*multiFactor[[varStr]]+addFactor[[varStr]]
    }else if(varStr %in% 'npp'){
        ##Load gpp and ra to construct NPP
        cat('constructing from gpp...')
        gppFilename <- sprintf('%s/%s_%s%s.%s', anuRstDir, 'gpp', modelStr,  commonStr, RstSaveExt)
        gpp <- brick(gppFilename)
        Rstlandarea <- raster::area(gpp)
        yrStr.var <- names(gpp)
        gpp <- rasterToArray(gpp)
        if(all(gpp <=0, na.rm=TRUE)){
            gpp <- gpp*-1
        }
        cat('rh...')
        raFilename <- sprintf('%s/%s_%s%s.%s', anuRstDir, 'ra', modelStr,  commonStr, RstSaveExt)
        ra <- brick(raFilename)
        ra <- rasterToArray(ra)
        cat('differences is npp...')
        var <- (gpp-ra)*multiFactor[[varStr]]+addFactor[[varStr]]

    }else{
        cat('no file found, moving on\n')
        next
    }

    cat('applying absolute tollerenances of 1e10 and 1e-4...')
    var[abs(var) > 1e10] <- NA
    var[abs(var) < 1e-4] <- 0
    cat('done\n')

    var[!is.finite(var)] <- NA

    eval(parse(text=sprintf('%s <- var', varStr)))
    eval(parse(text=sprintf('yrStr.%s <- yrStr.var', varStr)))

    if(is.null(yrStr)){
        yrStr <- yrStr.var
    }else{
        yrStr <- intersect(yrStr, yrStr.var)
    }
}

if('cCwd' %in% ls() && !is.na(cCwd)){
    cVeg <- cVeg + cCwd
}

if('cLitter' %in% ls() && !is.na(cLitter)){
    cSoil <- cSoil + cLitter
}

##Take the intersecting years only
#yrStr <- intersect(intersect(intersect(intersect(yrStr.npp, yrStr.rh),
#                                       yrStr.cSoil), yrStr.cVeg), yrStr.tsl10)
cat('loading area...')
years <- as.numeric(substr(yrStr, 2,5))

areaFile <- sprintf('%s/areacella_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr)
sftlfFile <- sprintf('%s/sftlf_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr)
if(commonStr %in% '' &&
   file.exists(sftlfFile) && file.exists(areaFile)){
    cat('loading land area from CMIP output...')

    cellarea <- raster( sprintf('%s/areacella_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr), varname='areacella')
    modelRes <- res(cellarea)

    sftlf <- rasterToArray(raster( sprintf('%s/sftlf_fx_%s_historical_r0i0p0.nc', CMIPDir,  modelStr), varname='sftlf'))
    if(max(as.vector(sftlf), na.rm=TRUE)==100){
        cat('correcting percentage to land fraction...')
        sftlf <- sftlf/100
    }
    landarea <- rasterToArray(cellarea)*sftlf

    if(is.null(dim(landarea))){
        cat('load failed for some reason... falling back on raster::area')
        modelRes <- res(Rstlandarea)
        landarea <- rasterToArray(Rstlandarea)
        cat('converting from km to m...')
        landarea <- landarea * 1e6
    }
}else{
    cat('using raster::area for land area...')
    modelRes <- c(1,1)

    landarea <- rasterToArray(Rstlandarea)
    cat('converting from km to m...')
    landarea <- landarea * 1e6
}

lon <- seq(0, 360, length=dim(landarea)[1] + 1)
lon <- (lon[2:length(lon)]+lon[(2:length(lon))-1])/2
lat <- seq(0, 360, length=dim(landarea)[2] + 1)
lat <- (lat[2:length(lat)]+lat[(2:length(lat))-1])/2

cat('done\n')

cat('trimming and aggrigating...')
if('npp' %in% ls()){
    npp <- npp[,,yrStr.npp %in% yrStr]
    npp_tot <- apply(npp, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
}
if('gpp' %in% ls() && !is.null(gpp)){
    gpp <- gpp[,,yrStr.gpp %in% yrStr]
    gpp_tot <- apply(gpp, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
}

if('rh' %in% ls()){
    rh <- rh[,,yrStr.rh %in% yrStr]
    rh_tot <- apply(rh, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12

}

if('ra' %in% ls() && !is.null(ra)){
    ra <- ra[,,yrStr.ra %in% yrStr]
    ra_tot <- apply(ra, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12

}
if('cSoil' %in% ls()){
    cSoil <- cSoil[,,yrStr.cSoil %in% yrStr]
    Soil_tot <- apply(cSoil, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
    if('rh' %in% ls()){
        k_tot <- rh_tot/Soil_tot
    }
}
if('cVeg' %in% ls()){
    Veg_tot <- apply(cVeg, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
    cVeg <- cVeg[,,yrStr.cVeg %in% yrStr]
}
if('tsl10' %in% ls()){
    tsl <- tsl10[,,yrStr.tsl10 %in% yrStr]
    tsl_tot <- apply(tsl, c(3), mean, weight=landarea, na.rm=TRUE)
}
if('tsl10' %in% ls()){tsl10 <- NULL}

if('cSoil' %in% ls()){
    dCs <- abind(cSoil[,,2:(dim(cSoil)[3])] - cSoil[,,(2:(dim(cSoil)[3])) - 1], array(NA, dim(landarea)), along=3)
    dCs_tot <- apply(dCs, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
}
if('cVeg' %in% ls()){
    dCv <-  abind(cVeg[,,2:(dim(cSoil)[3])] - cVeg[,,(2:(dim(cSoil)[3])) - 1], array(NA, dim(landarea)), along=3)
    dCv_tot <- apply(dCv, c(3), function(x){sum(as.vector(x*landarea), na.rm=TRUE)})/1e12
}

cat('done\n')
