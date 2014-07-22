library('ncdf4')
library('abind')

#' List CMIP5 files matching an  experiment/variable/model/ensemble combination
#' 
#' @param path root of directory tree
#' @param experiment CMIP5 experiment to load
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param ensemble CMIP5 ensemble to load
#' @param recursive logical. Should we recurse into directories?
#' @return vector of filenames
list.cmip5.files <- function(path,experiment,variable,model,ensemble,recursive) {
    f <- list.files(path=path,
               pattern=sprintf('%s_[a-zA-Z]+_%s_%s_%s_',
                               variable, model, experiment, ensemble),
               full.names=TRUE, recursive=recursive)
    if(length(f)==0) warning("No matching CMIP5 files")
    f
} # list.cmip5.files

#' Load data for a particular set of experiment/variable/model/ensemble
#'
#' @param path root of directory tree
#' @param experiment CMIP5 experiment to load
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param ensemble CMIP5 ensemble to load
#' @param recursive logical. Should we recurse into directories?
#' @param verbose logical. Print info as we go?
#' @return list with elements 'files', 'val', 'valUnit', timeUnit', 'calendarStr',
#'      'lat', 'lon', and 'time'. If no files match the requested criteria these
#'      will all be NULL.
#' @examples
#' loadEnsemble(model="GFDL-CM3",variable="prc",experiment="rcp85",ensemble="r1i1p1")
loadEnsemble <- function(path='.', experiment='[a-zA-Z0-9-]+', variable='[a-zA-Z0-9-]+',
                         model='[a-zA-Z0-9-]+', ensemble='[a-zA-Z0-9-]+',
                         recursive=TRUE, verbose=FALSE) {
    
    # List all files that match specifications
    fileList <- list.cmip5.files(path,experiment,variable,model,ensemble,recursive)
    
    # TODO: warn user if lots of files specified
    
    # Initialize outputs
    temp <- c()
    varUnit <- c()
    timeUnit <- c()
    calendarStr <- c()
    latArr <- c()
    lonArr <- c()
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
