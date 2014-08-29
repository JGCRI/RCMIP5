#' Load a model-variable-experiment.
#'
#' Averages all ensemble members of the specified CMIP5 experiment-variable-model combination.
#'
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param experiment CMIP5 experiment to load
#' @param domain CMIP5 domain to load
#' @param path root of directory tree
#' @param recursive logical. Should we recurse into directories?
#' @param verbose logical. Print info as we go?
#' @param demo logical. Demo mode (reading data from global environment, not disk)?
#' @return A \code{\link{cmip5data}} object
#' @export
loadModel <- function(variable, model, experiment, domain='[^_]+',
                      path='.', recursive=TRUE, verbose=TRUE, demo=FALSE) {
    
    # Match the path conventions to the operating system
    w <- getOption('warn')
    options(warn=-1)
    path <- normalizePath(path)
    options(warn=w)
    
    # Sanity checks########
    # Check that the model specifiers are strings
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(domain)==1 & is.character(domain))
    # Check that the path is a string and exists
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(file.exists(path))
    # Check the boolean flags
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(demo)==1 & is.logical(demo))
    
    # List all files that match specifications
    if(demo) {
        fileList <- ls(envir=.GlobalEnv) # in demo mode pull from environment, not disk
    } else {
        fileList <- list.files(path=path, full.names=TRUE, recursive=recursive)
    }
    
    # Only pull the files which are specified by the model strings
    fileList <- fileList[grepl(pattern=sprintf('^%s_%s_%s_%s_',
                                               variable, domain, model,
                                               experiment),
                               basename(fileList))]
    
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }
    
    #strip the .nc out of the file list
    fileList <- gsub('\\.nc$', '', fileList)
    
    # Parse out the ensemble strings according to CMIP5 specifications
    ensembleArr <- unique(unlist(lapply(strsplit(basename(fileList), '_'),
                                        function(x){x[5]})))
    
    if(verbose) cat('Averaging ensembles:', ensembleArr, '\n')
    
    modelTemp <- NULL                   # Initalize the return data structure
    for(ensemble in ensembleArr) { # for each ensemble...
        
        # load the entire ensemble
        temp <- loadEnsemble(variable, model, experiment, ensemble, path=path,
                             verbose=verbose, recursive=recursive, demo=demo)
        
        if(is.null(modelTemp)) {         # If first model, just copy
            modelTemp <- temp 
        } else {
            # Make sure lat-lon-depth|lev-time match
            if(all(dim(temp) == dim(modelTemp)) &
                   identical(temp$lat, modelTemp$lat) &
                   identical(temp$lon, modelTemp$lon) &
                   identical(temp$depth, modelTemp$depth) &
                   identical(temp$lev, modelTemp$lev) &
                   identical(temp$time, modelTemp$time)) {
                # Add this ensemble's data and record file and ensemble loaded
                modelTemp$val <- modelTemp$val + temp$val
                modelTemp$files <- c( modelTemp$files, temp$files )
                modelTemp$ensembles <- c(modelTemp$ensembles, ensemble)
            } else { # ...if dimensions don't match, don't load and warn user
                warning(ensemble,
                        'Not loaded: data dimensions do not match those of previous ensemble(s)\n')
            }
        } # is.null(modelTemp)
    } # for
    
    # Make sure at least one ensemble was actually loaded
    if(length(modelTemp$ensembles) == 0) {
        warning("No ensembles were loaded.")
        return(NULL)
    }
    
    # compute mean over all ensembles, update provenance, return
    modelTemp$val <- unname(modelTemp$val / length(modelTemp$ensembles))
    modelTemp$provenance <- addProvenance(NULL,
                                          c(paste("Computed mean of ensembles:",
                                                  paste(ensembleArr, collapse=' '))))
    modelTemp$provenance <- addProvenance(modelTemp$provenance,
                                          c(paste("From files:",
                                                  paste(fileList, collapse=' '))))
    return(modelTemp)
} # loadModel
