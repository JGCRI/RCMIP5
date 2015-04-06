
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
    
    load_time <- system.time(try(x <- loadCMIP5(...)))[3]
    
    # TODO: change to using tryCatch and return NA if any error
    
    size <- as.numeric(object.size(x))
    
    astat_time <- system.time(try(makeAnnualStat(x)))[3]
    gstat_time <- system.time(try(makeGlobalStat(x)))[3]
    
    list(version, load_time, size, astat_time, gstat_time)
}



#x2 <- RCMIP5:::loadEnsemble(variable="nbp",model="HadGEM2-ES",experiment="historical",path="sampledata/monthly/",ensemble="r3i1p1",domain="Lmon",verbose=T, yearRange=c(1970,1971))

#x1 <- loadCMIP5(variable="nbp",model="HadGEM2-ES",experiment="historical",path="sampledata/monthly/",ensemble="r3i1p1",domain="Lmon",verbose=T, yearRange=c(1970,1971))

#x3 <- expand.grid(x1$lon, x1$lat, x1$Z, x1$time)
