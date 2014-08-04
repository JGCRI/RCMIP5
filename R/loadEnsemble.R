library(ncdf4)
library(abind)

#' Load data for a particular set of experiment/variable/model/ensemble
#'
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param experiment CMIP5 experiment to load
#' @param ensemble CMIP5 ensemble to load
#' @param domain CMIP5 domain to load
#' @param path root of directory tree
#' @param recursive logical. Recurse into directories?
#' @param verbose logical. Print info as we go?
#' @param demo logical. Demo mode (reading data from global environment, not disk)?
#' @return A \code{\link{cmip5data}} object.
#' @details This function is the core of RCMIP5's data-loading. It loads all files matching
#' the experiment, variable, model, and ensemble supplied by the caller. We can also load
#' from the package datasets by specifying DEMO=TRUE.
#' @export
#' @examples
#' loadEnsemble('nbp','HadGEM2-ES','rcp85','r3i1p1',verbose=TRUE,demo=TRUE)
loadEnsemble <- function(variable='[^_]+', model='[^_]+',
                         experiment='[^_]+', ensemble='[^_]+', domain='[^_]+',
                         path='.', recursive=TRUE, verbose=FALSE, demo=FALSE) {

    ##Sanity check
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(file.exists(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(demo)==1 & is.logical(demo))

    # Sanity checks
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(ensemble)==1 & is.character(ensemble))

    # List all files that match specifications
    if(demo) {
        fileList <- ls(envir=.GlobalEnv)  # in demo mode pull from environment, not disk
    } else {
        fileList <- list.files(path=path, full.names=TRUE, recursive=recursive)
    }
    fileList <- fileList[grepl(pattern=sprintf('%s_%s_%s_%s_%s[_\\.]',
                               variable, domain, model, experiment, ensemble),
                               fileList)]
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }

    # Need to deal with mix of fx and other domains which don't split to the
    # ...same number of strings
    domainCheck <- unname(vapply(unlist(fileList),
                          function(x){unlist(strsplit(basename(x), '_'))[2]},
                          FUN.VALUE=''))
    if(length(unique(domainCheck)) > 1){
        stop('Domain is not unique: [', paste(unique(domainCheck), collapse=' '), ']\n')
    }

    # Check that there are unique variables, domain, model, exierments
    # ...and ensemble specified, reset them as needed
    numSplits <- length(unlist(strsplit(basename(fileList[1]), '_')))
    cmipName <- unname(vapply(unlist(fileList),
                       function(x){unlist(strsplit(basename(x), '_'))},
                       FUN.VALUE=rep('', length=numSplits)))

    checkField <- list(variable=1, domain=2, model=3, experiment=4, ensemble=5)

    for(checkStr in names(checkField)){
        tempStr <- unique(cmipName[checkField[[checkStr]],])
        if(length(tempStr) > 1){
             stop('[',checkStr, '] is not unique: [', paste(tempStr, collapse=' '), ']\n')
        }else{
            eval(parse(text=paste(checkStr, ' <- "', tempStr[1], '"', sep='')))
        }
    }

    # Go through and load the data
    temp <- c()
    timeArr <- c()
    for(fileStr in fileList) {
        if(demo) {
            if(verbose) cat("DEMO: loading", fileStr, "from package data")
            return(get(fileStr, envir=.GlobalEnv))
        } else {

            if(verbose) cat('Loading', fileStr)
            temp.nc <- nc_open(fileStr, write=FALSE)

            temp <- abind(temp, ncvar_get(temp.nc, varid=variable), along=3)
            varUnit <- ncatt_get(temp.nc, variable, 'units')$value

            varnames <- names(temp.nc$var)

            # Load these guaranteed data
            stopifnot(any(c("lon", "lon_bnds") %in% varnames))
            stopifnot(any(c("lat", "lat_bnds") %in% varnames))
            latArr <- ncvar_get(temp.nc, varid='lat')
            lonArr <- ncvar_get(temp.nc, varid='lon')

            # pull the time frequency
            timeFreqStr <- ncatt_get(temp.nc, varid=0)$frequency

            # Non-fixed files have times to load
            if(! timeFreqStr %in% 'fx'){

                timeUnit <- ncatt_get(temp.nc, 'time', 'units')$value
                calendarStr <- ncatt_get(temp.nc, 'time', 'calendar')$value
                calendarUnitsStr <- ncatt_get(temp.nc, 'time', 'units')$value
                # Pull the number of days in a year
                if(grepl('^[^\\d]*\\d{3}[^\\d]day', calendarStr)){
                    calendarDayLength <- as.numeric(regmatches(calendarStr, regexpr('\\d{3}', calendarStr)))
                }else{
                    calendarDayLength <- 365
                }

                # Pull the start decimal year
                # assume that the starting year is specified by
                # YYYY-MM-DD hh:mm:ss
                if(grepl('\\d{4}-\\d{2}-\\d{2}[^\\d]\\d{2}:\\d{2}:\\d{2}',
                         calendarUnitsStr)){
                    dateStr <- regmatches(calendarUnitsStr, regexpr('\\d{4}-\\d{2}-\\d{2}[^\\d]\\d{2}:\\d{2}:\\d{2}', calendarUnitsStr))
                    startYr <- as.numeric(substr(dateStr, 1, 4))+ #YYYY
                        (as.numeric(substr(dateStr, 6, 7))-1)/12 + #MM
                        (as.numeric(substr(dateStr, 9, 10))-1)/calendarDayLength+#DD
                        as.numeric(substr(dateStr, 12, 13))/(calendarDayLength*24)+#hh
                        as.numeric(substr(dateStr, 15, 16))/(calendarDayLength*24*60)+#mm
                        as.numeric(substr(dateStr, 18, 19))/(calendarDayLength*24*60*60)#ss
                # Alternatively YYYY-MM-DD
                }else if(grepl('\\d{4}-\\d{2}-\\d{2}', calendarUnitsStr)){
                    dateStr <- regmatches(calendarUnitsStr, regexpr('\\d{4}-\\d{2}-\\d{2}', calendarUnitsStr))
                    startYr <- as.numeric(substr(dateStr, 1, 4))+ #YYYY
                        (as.numeric(substr(dateStr, 6, 7))-1)/12 + #MM
                        (as.numeric(substr(dateStr, 9, 10))-1)/calendarDayLength#DD
                }else{
                    startYr <- 0
                }

                # Pull the actual time
                timeArr <- c(timeArr,
                             ncvar_get(temp.nc, varid='time')/calendarDayLength
                                       + startYr)
            }else{ #this is a fx variable
                startYr <- NULL
                timeArr <- NULL
                timeUnit <- NULL
                timeFreqStr <- 'fx'
                calendarStr <- NULL
                calendarDayLength <- NULL
            }

            # Load the 4th dimentions: lev (atmospheric levels)
            #                          depth (ocean/land depths)
            levArr <- NULL
            if(any(c("lev", "lev_bnds") %in% varnames))
                levArr <- ncvar_get(temp.nc, varid='lev')
            depthArr <- NULL
            if(any(c("depth", "depth_bnds") %in% varnames))
                depthArr <- ncvar_get(temp.nc, varid='depth')

            nc_close(temp.nc)
        }
    }

    cmip5data(list(files=fileList, val=unname(temp), valUnit=varUnit,
                   lat=latArr, lon=lonArr, lev=levArr, depth=depthArr,
                   time=timeArr,
                   variable=variable, model=model,
                   experiment=experiment, ensembles=ensemble,
                   debug=list(startYr=startYr, timeUnit=timeUnit,
                              timeFreqStr=timeFreqStr, calendarStr=calendarStr,
                              calendarDayLength=calendarDayLength)
                   ))
} # loadEnsemble
