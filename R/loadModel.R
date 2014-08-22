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
        # in demo mode pull from environment, not disk
        fileList <- ls(envir=.GlobalEnv)
    } else {
        fileList <- list.files(path=path, full.names=TRUE, recursive=recursive)
    }

    # Only pull the files which are specified by the model strings
    fileList <- fileList[grepl(pattern=sprintf('^%s_%s_%s_%s_',
                                               variable, domain, model,
                                               experiment),
                               basename(fileList))]

    # Exit if we don't have any files to load
    if(length(fileList)==0) {
        warning("Could not find any matching files")
        return(NULL)
    }

    # Parse out the ensemble strings according to CMIP5 specifications
    ensembleArr <- unique(unlist(lapply(strsplit(basename(fileList), '_'),
                                        function(x){x[5]})))

    if(verbose) cat('Averaging ensembles:', ensembleArr, '\n')

    modelTemp <- NULL                   # Initalize the return data structure

    # Go through each of the ensembles that make up the specified model
    for(ensemble in ensembleArr) {

        # load the entire ensemble
        temp <- loadEnsemble(variable, model, experiment, ensemble, path=path,
                             verbose=verbose, recursive=recursive, demo=demo)

        if(is.null(modelTemp)) {         # If this is the first model
            modelTemp <- temp            # set the return structure
            # Copy the provenance so we can add to it later
            ensembleProv <- temp$provenance
        } else {
            # Add this ensemble proveanace to the existing provenance
            ensembleProv <- c(ensembleProv, temp$provenance)

            # Check that lat-lon-lev-time match
            if(identical(temp$lat, modelTemp$lat) &
                   identical(temp$lon, modelTemp$lon) &
                   identical(temp$lev, modelTemp$lev) &
                   identical(temp$time, modelTemp$time)) {
                # add values which will later be nomalized to the number of
                # ...ensembles that make up this model
                modelTemp$val <- modelTemp$val + temp$val
                # record the files loaded
                # KTB: possibly redundent given provenance??
                modelTemp$files <- c( modelTemp$files, temp$files )
                # add the ensemble to those sucessfully loaded
                modelTemp$ensembles <- c(modelTemp$ensembles, ensemble)
            } else { # ...if dimentions don't match, don't load and warn user
                warning(ensemble,
                     'not loaded: does not match previous lon-lat-lev-time.\n')
            } #dimentions check
        } #is.null(modelTemp)
    } #ensemble for-loop

    # check that an ensemble was actually loaded
    stopifnot(length(modelTemp$ensembles)>0)

    # convert the sum to an average
    modelTemp$val <- unname(modelTemp$val / length(modelTemp$ensembles))
    # record the provenance
    modelTemp$provenance <- addProvenance(NULL,
                              c(paste("Computed mean of ensembles",
                                     paste(modelTemp$ensembles, collapse=' ')),
                                ensembleProv))

    return(modelTemp)
} # loadModel
