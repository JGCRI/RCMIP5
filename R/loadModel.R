library(ncdf4)

#' Average all ensemble members of the specified experiment-variable-model combination
#'
#' @param variable CMIP5 variable of interest
#' @param model CMIP5 model of interest
#' @param experiment CMIP5 experiment of interest
#' @param path root of directory tree
#' @param recursive logical. Should we recurse into directories?
#' @param verbose logical. Print info as we go?
#' @param demo logical. Demo mode (reading data from global environment, not disk)?
#' @return A \code{\link{cmip5data}} object.
#' @export
#' @examples
#' loadModel('nbp','HadGEM2-ES','rcp85',verbose=TRUE,demo=TRUE)
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
            model.ls <- temp
        } else {
            if(identical(temp$lat, model.ls$lat) &            # Check that lat-lon-lev-time match
                   identical(temp$lon, model.ls$lon) &
                   identical(temp$lev, model.ls$lev) &
                   identical(temp$time, model.ls$time)) {
                model.ls$val <- model.ls$val + temp$val # if so, add values and record successful load
                model.ls$files <- c( model.ls$files, temp$files )
                model.ls$ensembles <- c(model.ls$ensembles, ensemble)
            } else {                                    # ...if not, don't load
                warning(ensemble, 'not loaded: does not match previous lon-lat-lev-time.\n')
            }
        }
    }
    # convert the sum to an average
    stopifnot(length(model.ls$ensembles)>0)
    model.ls$val <- unname(model.ls$val / length(model.ls$ensembles))
    
    return(model.ls)
} # loadModel
