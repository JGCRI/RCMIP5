
# We want to build a performance table

# Log machine specs (cores, speed, memory)


# Filename
# File size

# Time to load one ensemble
# Size of loaded variable
# Time to merge 2 ensembles
# Time to calculate annual mean
# Time to calc global mean
# Time to calc annual -> global mean

#library(RCMIP5)

profile_file <- function(...) {
    
    on.exit(NA)
    
    version <- packageVersion("RCMIP5")
    
    ef <- function(e) { NA }

    cat("Loading...\n")
    load_time <- tryCatch(system.time(x <- loadCMIP5(...))[3], error=ef)
    
    size <- format(object.size(x), units="Mb")
    
    cat("Annual stat...\n")
    astat_time <- tryCatch(system.time(makeAnnualStat(x, sortData=TRUE))[3], error=ef)
    cat("Global stat...\n")
    gstat_time <- tryCatch(system.time(makeGlobalStat(x))[3], error=ef)
    
    list('version'=version, 'load_time'=load_time, 'size'=size, 'astat_time'=astat_time, 'gstat_time'=gstat_time)
}



#x2 <- RCMIP5:::loadEnsemble(variable="nbp",model="HadGEM2-ES",experiment="historical",path="sampledata/monthly/",ensemble="r3i1p1",domain="Lmon",verbose=T, yearRange=c(1970,1971))

#x1 <- loadCMIP5(variable="nbp",model="HadGEM2-ES",experiment="historical",path="sampledata/monthly/",ensemble="r3i1p1",domain="Lmon",verbose=T, yearRange=c(1970,1971))

#x3 <- expand.grid(x1$lon, x1$lat, x1$Z, x1$time)
