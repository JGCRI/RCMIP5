library(ncdf4)
library(abind)

#' Load data for a particular set of experiment/variable/model/ensemble
#'
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param experiment CMIP5 experiment to load
#' @param ensemble CMIP5 ensemble to load
#' @param path root of directory tree
#' @param recursive logical. Should we recurse into directories?
#' @param verbose logical. Print info as we go?
#' @return list with elements 'files', 'val', 'valUnit', timeUnit', 'calendarStr',
#'      'lat', 'lon', and 'time'. If no files match the requested criteria function
#'      will return NULL with a warning.
#' @examples
#' loadEnsemble(model="GFDL-CM3",variable="prc",experiment="rcp85",ensemble="r1i1p1")
loadEnsemble <- function(variable, model, experiment, ensemble,
                         path='.', recursive=TRUE, verbose=FALSE) {
    
    # Sanity checks
    stopifnot(length(variable==1) & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(ensemble)==1 & is.character(ensemble))
    stopifnot(file.exists(path))
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    
    # List all files that match specifications
    fileList <- list.files(path=path,
                           pattern=sprintf('%s_[a-zA-Z]+_%s_%s_%s_',
                                           variable, model, experiment, ensemble),
                           full.names=TRUE, recursive=recursive)
    
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }
    
    temp <- c()
    timeArr <- c()
    for(fileStr in fileList) {
        if(verbose) cat('Loading...',fileStr)
        temp.nc <- nc_open(fileStr, write=FALSE)
        
        temp <- abind(temp, ncvar_get(temp.nc, varid=variable), along=3)
        varUnit <- ncatt_get(temp.nc, variable, 'units')$value
        
        timeArr <- c(timeArr, ncvar_get(temp.nc, varid='time'))
        timeUnit <- ncatt_get(temp.nc, 'time', 'units')$value
        calendarStr <- ncatt_get(temp.nc, 'time', 'calendar')$value
        
        latArr <- ncvar_get(temp.nc, varid='lat')
        lonArr <- ncvar_get(temp.nc, varid='lon')
        
        nc_close(temp.nc)
        if(verbose) cat('\n')
    }
    
    list(files=fileList, val=temp, valUnit=varUnit, timeUnit=timeUnit, 
         calendarStr=calendarStr, lat=latArr, lon=lonArr, time=timeArr)
} # loadEnsemble
