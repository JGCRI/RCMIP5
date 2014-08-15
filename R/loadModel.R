library(ncdf4)

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
    
    # Sanity checks
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(domain)==1 & is.character(domain))
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
    fileList <- fileList[grepl(pattern=sprintf('^%s_%s_%s_%s_', # '%s_[a-zA-Z]+_%s_%s_'
                                               variable, domain, model, experiment),
                               basename(fileList))]
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

        if(is.null(model.ls)) {         # If this is the first model
            model.ls <- temp
            ensembleProv <- temp$provenance
        } else {
            ensembleProv <- c(ensembleProv, temp$provenance)
                                        # Check that lat-lon-lev-time match
            if(identical(temp$lat, model.ls$lat) &
                   identical(temp$lon, model.ls$lon) &
                   identical(temp$lev, model.ls$lev) &
                   identical(temp$time, model.ls$time)) {
                                        # add values and record successful load
                model.ls$val <- model.ls$val + temp$val
                model.ls$files <- c( model.ls$files, temp$files )
                model.ls$ensembles <- c(model.ls$ensembles, ensemble)
            } else {
                                        # ...if not, don't load
                warning(ensemble,
                     'not loaded: does not match previous lon-lat-lev-time.\n')
            }
        }
    }
    # convert the sum to an average
    stopifnot(length(model.ls$ensembles)>0)
    model.ls$val <- unname(model.ls$val / length(model.ls$ensembles))
    model.ls$provenance <- addProvenance(NULL,
                              c(paste("Computed mean of ensembles", paste(model.ls$ensembles, collapse=' ')), ensembleProv))
    #model.ls$provenance <- addProvenance(model.ls$provenance, )

    return(model.ls)
} # loadModel
