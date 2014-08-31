#' Load a model-variable-experiment-ensemble.
#'
#' Loads the data for a particular CMIP5 experiment/variable/model/ensemble
#' combination (one or more files). Returns NULL and a warning if nothing matches.
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
#' the experiment, variable, model, ensemble, and perhaps domain supplied by the caller.
#' We can also load from the package datasets by specifying DEMO=TRUE.
#' @export
loadEnsemble <- function(variable='[^_]+', model='[^_]+',
                         experiment='[^_]+', ensemble='[^_]+', domain='[^_]+',
                         path='.', recursive=TRUE, verbose=TRUE, demo=FALSE) {
    
    # Sanity checks - make sure all parameters are correct class and length
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(ensemble)==1 & is.character(ensemble))
    stopifnot(length(domain)==1 & is.character(domain))
    stopifnot(length(path)==1 & is.character(path)) # valid path?
    stopifnot(file.exists(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(demo)==1 & is.logical(demo))
    
    # List all files that match specifications
    if(demo) {
        # in demo mode pull list of files from environment, not disk
        fileList <- ls(envir=.GlobalEnv)
    } else {
        # otherwise pull the file list
        fileList <- list.files(path=path, full.names=TRUE, recursive=recursive)
    }
    # Match file names with valid CMIP5 patterns:
    # ...variable_domain_model_experiment_ensemble followed by either
    # ...a '_' or a '.' depending on whether a time period is specified or not.
    # ...Note that the '[_\\.]' match differentiates between
    # ...ensembles like 'r1i1p1' and 'r1i1p11', an unlikely but possible case.
    fileList <- fileList[grepl(pattern=sprintf('^%s_%s_%s_%s_%s[_\\.]',
                                               variable, domain, model,
                                               experiment, ensemble),
                               basename(fileList))]
    
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }
    
    # Get the domains of all files we want to load
    domainCheck <- unname(vapply(unlist(fileList),
                                 function(x){
                                     unlist(strsplit(basename(x), '_'))[2]
                                 },
                                 FUN.VALUE=''))
    # Check that we are only loading one domain. We check this before checking
    # other CMIP5 specifications because 'fx' domains will split on '_' to a
    # different number of strings then temporal domains.
    if(length(unique(domainCheck)) > 1){
        stop('Domain is not unique: [', paste(unique(domainCheck), collapse=' '), ']\n')
    }
    
    # Get the number of pieces of CMIP5 information strings
    numSplits <- length(unlist(strsplit(basename(fileList[1]), '_')))
    
    # Split all file names based on '_' or '.'
    cmipName <- unname(vapply(unlist(fileList),
                              function(x){
                                  unlist(strsplit(basename(x), '[_\\.]'))
                              },
                              FUN.VALUE=rep('', length=numSplits+1)))
    
    # Key in on what order the CMIP5 information is presented in the filename
    # TODO BBL: why is this taking place? I don't understand. Comments not very helpful
    checkField <- list(variable=1, domain=2, model=3, experiment=4, ensemble=5)
    for(checkStr in names(checkField)){
        # Pull all unique strings
        tempStr <- unique(cmipName[checkField[[checkStr]],])
        if(length(tempStr) > 1) { # there should only be one!
            stop('[',checkStr, '] is not unique: [', paste(tempStr, collapse=' '), ']\n')
        } else {
            # Set a variable by the field name to the correct string for this ensemble
            eval(parse(text=paste(checkStr, ' <- "', tempStr[1], '"', sep='')))
        }
    }
    
    # Go through and load the data
    val <- c() # variable to temporarily holds main data
    timeRaw <- c()
    timeArr <- c()
    depthUnit <- NULL
    levUnit <- NULL
    prov <- NULL # provenance
    # Note that list.files returns a sorted list so these file should already
    # be in temporal order if the ensemble is split over multiple files.
    for(fileStr in fileList) {
        if(demo) { # KTB Does demo load any data??
            # BBL: Yes, but see issue on github. It may not be worth it to ship package
            # with any test data, and in that case, can remove this 'demo' mode
            if(verbose) cat("DEMO: loading", fileStr, "from package data\n")
            return(get(fileStr, envir=.GlobalEnv))
        } else {
            if(verbose) cat('Loading', fileStr, "\n")
            temp.nc <- nc_open(fileStr, write=FALSE)
            temp <- ncvar_get(temp.nc, varid=variable)  # load data array
            if(verbose) cat("- data", dim(temp), "\n")
            
            # Test that spatial dimensions are identical across files
            if(length(val) > 0) {
                stopifnot(all(dim(val)[1:(length(dim(val))-1)] == dim(temp)[1:(length(dim(temp))-1)]))
            }
            
            # Bind the main variable along time dimension to previously loaded data
            # Note that the time dimensions is guaranteed to be last - see ncdf4 documentation
            val <- abind(val, temp, along=length(dim(temp)))
            
            valUnit <- ncatt_get(temp.nc, variable, 'units')$value  # load units
            
            # Get all the variables stored in the netcdf file so that we
            # ...can load the lon, lat and time variables if appropriate
            varnames <- names(temp.nc$var)
            dimensionnames <- unlist(lapply(temp.nc$dim, FUN=function(x) { x$name }))
            
            if(verbose) cat("- var names:", varnames, "\n")
            if(verbose) cat("- dimension names:", dimensionnames, "\n")
            # TODO: this is a hack. The files written by saveNetCDF look the same in Panoply
            # to CMIP5 files, but loadEnsemble isn't seeing their dimension names in the
            # variable name list, the way it does with CMIP5 data. So merge the two here,
            # so we can use these (small) files in testing code.
            varnames <- union(varnames, dimensionnames)
            
            # Load these guaranteed data; latitude and longitude
            stopifnot(any(c("lon", "lon_bnds") %in% varnames))
            stopifnot(any(c("lat", "lat_bnds") %in% varnames))
            latArr <- ncvar_get(temp.nc, varid='lat')
            lonArr <- ncvar_get(temp.nc, varid='lon')
            latUnit <- ncatt_get(temp.nc, 'lat', 'units')$value
            lonUnit <- ncatt_get(temp.nc, 'lon', 'units')$value
            
            # Get the time frequency. Note that this should be related to
            # ...the domain but really we are looking for 'fx'/fixed variables
            # ...where we don't have to deal with time.
            # TODO BBL: what does this comment mean? Clarify if possible
            timeFreqStr <- ncatt_get(temp.nc, varid=0)$frequency
            
            # Non-fixed files have a time dimension to deal with:
            if(! timeFreqStr %in% 'fx') {
                # Get the time unit (e.g. 'days since 1860')
                timeUnit <- ncatt_get(temp.nc, 'time', 'units')$value
                # Get the type of calendar used (e.g. 'noleap')
                calendarStr <- ncatt_get(temp.nc, 'time', 'calendar')$value
                calendarUnitsStr <- ncatt_get(temp.nc, 'time', 'units')$value
                # Pull the number of days in a year
                if(grepl('^[^\\d]*\\d{3}[^\\d]day', calendarStr)) {
                    calendarDayLength <- as.numeric(regmatches(calendarStr, regexpr('\\d{3}', calendarStr)))
                } else {
                    calendarDayLength <- 365
                }
                
                # Calculate the start decimal year, assuming YYYY-MM-DD hh:mm:ss
                if(grepl('\\d{4}-\\d{2}-\\d{2}[^\\d]\\d{2}:\\d{2}:\\d{2}',
                         calendarUnitsStr)) {
                    dateStr <- regmatches(calendarUnitsStr, regexpr('\\d{4}-\\d{2}-\\d{2}[^\\d]\\d{2}:\\d{2}:\\d{2}', calendarUnitsStr))
                    startYr <- as.numeric(substr(dateStr, 1, 4)) + # YYYY
                        (as.numeric(substr(dateStr, 6, 7))-1)/12 + # MM
                        (as.numeric(substr(dateStr, 9, 10))-1)/calendarDayLength + # DD
                        as.numeric(substr(dateStr, 12, 13))/(calendarDayLength*24) + # hh
                        as.numeric(substr(dateStr, 15, 16))/(calendarDayLength*24*60) + # mm
                        as.numeric(substr(dateStr, 18, 19))/(calendarDayLength*24*60*60) # ss
                    ##TODO: Should really split this based on '-' instead
                    # Alternatively YYYY-MM-DD
                } else if(grepl('\\d{4}-\\d{2}-\\d{2}', calendarUnitsStr)) { 
                    dateStr <- regmatches(calendarUnitsStr, regexpr('\\d{4}-\\d{2}-\\d{2}', calendarUnitsStr))
                    startYr <- as.numeric(substr(dateStr, 1, 4))+ # YYYY
                        (as.numeric(substr(dateStr, 6, 7))-1)/12 + # MM
                        (as.numeric(substr(dateStr, 9, 10))-1)/calendarDayLength # DD
                    # Alternatively YYYY-M-D      
                } else if(grepl('\\d{4}-\\d{1}-\\d{1}', calendarUnitsStr)) { 
                    dateStr <- regmatches(calendarUnitsStr, regexpr('\\d{4}-\\d{1}-\\d{1}', calendarUnitsStr))
                    startYr <- as.numeric(substr(dateStr, 1, 4))+ # YYYY
                        (as.numeric(substr(dateStr, 6, 6))-1)/12 + # M
                        (as.numeric(substr(dateStr, 8, 8))-1)/calendarDayLength # D
                } else {
                    startYr <- 0
                }
                
                # Pull the actual time
                thisTimeRaw <- ncvar_get(temp.nc, varid='time')
                timeRaw <- c(timeRaw, thisTimeRaw)
                timeArr <- c(timeArr, thisTimeRaw / calendarDayLength + startYr)
            } else { # this is a fx variable. Set most things to NULL
                startYr <- NULL
                timeArr <- NULL
                timeUnit <- NULL
                calendarStr <- NULL
                calendarDayLength <- NULL
                calendarUnitsStr <- NULL
                dim(val) <- dim(val)[1:2]
            }
            
            # Load the 4th dimension identifiers, if present:
            # depth (ocean/land depths) and lev (atmospheric levels)
            depthArr <- NULL
            if(any(c("depth", "depth_bnds") %in% varnames)){
                depthArr <- ncvar_get(temp.nc, varid='depth')
                depthUnit <- ncatt_get(temp.nc, 'depth', 'units')$value
            }
            
            levArr <- NULL
            if(any(c("lev", "lev_bnds") %in% varnames)){
                levArr <- ncvar_get(temp.nc, varid='lev')
                levUnit <- ncatt_get(temp.nc, 'lev', 'units')$value
            }
            
            nc_close(temp.nc)
        }
    } # for
    
    x <- cmip5data(list(files=fileList, val=unname(val), valUnit=valUnit,
                        lat=latArr, lon=lonArr, lev=levArr, depth=depthArr,
                        time=timeArr, timeFreqStr=timeFreqStr,
                        variable=variable, model=model, domain=domain,
                        experiment=experiment, ensembles=ensemble,
                        
                        debug=list(startYr=startYr, 
                                   lonUnit=lonUnit, latUnit=latUnit,
                                   depthUnit=depthUnit, levUnit=levUnit, 
                                   timeUnit=timeUnit,
                                   calendarUnitsStr=calendarUnitsStr,
                                   calendarStr=calendarStr, timeRaw=timeRaw,
                                   calendarDayLength=calendarDayLength)
    ))

    for(f in fileList) {
        x <- addProvenance(x, paste("Loaded", basename(f)))     
    }
    
    x
} # loadEnsemble
