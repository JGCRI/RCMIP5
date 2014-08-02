library(ncdf4)
library(abind)

if(!exists("compute_yearIndex") | !exists("cmip5data")) {
    source('internalHelpers.R')     # TODO: KTB is running code in R directory,
    source('RCMIP5.R')              # while BBL is running one level up. Should standardize.
}

#' Load data for a particular set of experiment/variable/model/ensemble
#'
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param experiment CMIP5 experiment to load
#' @param ensemble CMIP5 ensemble to load
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
loadEnsemble <- function(variable, model, experiment, ensemble,
                         path='.', recursive=TRUE, verbose=FALSE, demo=FALSE) {

    # Sanity checks
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(ensemble)==1 & is.character(ensemble))
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(file.exists(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(demo)==1 & is.logical(demo))

    # List all files that match specifications
    if(demo) {
        fileList <- ls(envir=.GlobalEnv)  # in demo mode pull from environment, not disk
    } else {
        fileList <- list.files(path=path, full.names=TRUE, recursive=recursive)
    }
    fileList <- fileList[grepl(pattern=sprintf('%s_[a-zA-Z]+_%s_%s_%s_',
                                               variable, model, experiment, ensemble), fileList)]
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }

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
            if(verbose) cat(' [',dim(temp),']\n')
            varUnit <- ncatt_get(temp.nc, variable, 'units')$value
            timeArr <- c(timeArr, ncvar_get(temp.nc, varid='time'))
            timeUnit <- ncatt_get(temp.nc, 'time', 'units')$value
            calendarStr <- ncatt_get(temp.nc, 'time', 'calendar')$value
            latArr <- ncvar_get(temp.nc, varid='lat')
            lonArr <- ncvar_get(temp.nc, varid='lon')
            levArr <- NULL
            if(temp.nc$nvars==5)  # TODO: would be better to search variable names
                levArr <- ncvar_get(temp.nc, varid='lev')

            nc_close(temp.nc)
        }
    }

    cmip5data(list(files=fileList, val=unname(temp), valUnit=varUnit, timeUnit=timeUnit,
                   calendarStr=calendarStr, lat=latArr, lon=lonArr, lev=levArr, time=timeArr,
                   variable=variable, model=model, experiment=experiment, ensembles=ensemble))
} # loadEnsemble
