library(ncdf4)
#source('loadEnsemble.R')

#' Average all ensemble members of the specified experiment-variable-model combination
#'
#' @param variable CMIP5 variable of interest
#' @param model CMIP5 model of interest
#' @param experiment CMIP5 experiment of interest
#' @param path root of directory tree
#' @param recursive logical. Should we recurse into directories?
#' @param verbose logical. Print info as we go?
#' @param demo logical. Demo mode (reading data from global environment, not disk)?
#' @return list with elements 'files', 'val', 'valUnit', timeUnit', 'calendarStr',
#'      'lat', 'lon', and 'time'. If no files match the requested criteria function
#'      will return NULL with a warning.
#' @export
#' @examples
#' loadModel('nbp','HadGEM2-ES','rcp85',verbose=TRUE)
loadModel <- function(variable, model, experiment, 
                      path='.', recursive=TRUE, verbose=FALSE, demo=FALSE) {
    
    # Sanity checks
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
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
    fileList <- fileList[grepl(pattern=sprintf('%s_[a-zA-Z]+_%s_%s_', 
                                               variable, model, experiment), fileList)]
#     fileList <- list.files(path=path,
#                            pattern=sprintf('%s_[a-zA-Z]+_%s_%s_', variable, model, experiment),
#                            full.names=TRUE, recursive=recursive)
    
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }
    
    # Parse out the ensemble strings
    ensembleArr <- unique(unlist(lapply(strsplit(basename(fileList), '_'),
                                        function(x){x[5]})))
    
    if(verbose) cat('Averaging ensembles:', ensembleArr, '\n')
    
    model.ls <- NULL
    for(ensemble in ensembleArr) {
        temp <- loadEnsemble(variable, model, experiment, ensemble, 
                             path=path, verbose=verbose, recursive=recursive, demo=demo)
        # If this is the first model
        if(is.null(model.ls)) {
            model.ls <- temp                # initialize the results with the first ensemble
            model.ls$files <- list()
            model.ls$files[[ensemble]] <- temp$files
            ensembleNames <- c(ensemble)
        } else {
            if(all(temp$lat==model.ls$lat) &            # Check that lat-lon-time match
                   all(temp$lon==model.ls$on) &
                   all(temp$time==model.ls$time)) {
                model.ls$val <- model.ls$val + temp$val # if so, add values and record successful load
                model.ls$files[[ensemble]] <- temp$files
                ensembleNames <- c(ensembleNames, ensemble)
            } else {                                    # ...if not, fail
                warning(ensemble, 'does not match previous ensembles\' lon-lat-time.\n')
            }
        }
    }
    # convert the sum to an average
    stopifnot(length(ensembleNames)>0)
    model.ls$val <- model.ls$val / length(ensembleNames)
    
    invisible(model.ls)
} # loadModel
