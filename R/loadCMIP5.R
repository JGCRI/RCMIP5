#' Load CMIP5 data
#'
#' Loads CMIP5 data from disk. \code{loadCMIP5} will return a unique model ensemble,
#' or will apply a function across all ensemble members of a 
#' specified experiment-variable-model combination.
#'
#' @param variable CMIP5 variable to load (required)
#' @param model CMIP5 model to load (required)
#' @param experiment CMIP5 experiment to load (required)
#' @param ensemble optional CMIP5 ensemble to load
#' @param domain optional CMIP5 domain to load
#' @param path root of directory tree
#' @param recursive logical. Should we recurse into directories?
#' @param verbose logical. Print info as we go?
#' @param force.ncdf Force use of the less-desirable ncdf package for testing?
#' @param FUN function. Function (mean, min, max, or sum) to apply across ensembles
#' @param yearRange numeric of length 2. If supplied, load only these years of data
#' @return A \code{\link{cmip5data}} object
#' @note The \code{yearRange} parameter is intended to help users deal with large
#' CMIP5 data files on memory-limited machines, e.g. by allowing them to process
#' smaller chunks of such files.
#' @note FUN is limited to min, max, sum, and mean (the default), because the 
#' memory costs of keeping all ensembles in memory is too high. Be warned that 
#' min and max are quite slow!
#' @examples
#' \dontrun{
#' loadCMIP5(experiment='rcp85', variable='prc', model='GFDL-CM3', ensemble='r1i1p1')
#' }
#' @export
loadCMIP5 <- function(variable, model, experiment, ensemble=NULL, domain='[^_]+',
                      path='.', recursive=TRUE, verbose=FALSE, force.ncdf=FALSE,
                      FUN=mean, yearRange=NULL) {
    
    # Sanity checks - parameters are correct type and length
    stopifnot(length(variable)==1 & is.character(variable))
    stopifnot(length(model)==1 & is.character(model))
    stopifnot(length(experiment)==1 & is.character(experiment))
    stopifnot(length(ensemble)==1 & is.character(ensemble) | is.null(ensemble))
    stopifnot(length(domain)==1 & is.character(domain))
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(file.exists(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(length(verbose)==1 & is.logical(verbose))
    stopifnot(length(force.ncdf)==1 & is.logical(force.ncdf))
    stopifnot(is.null(yearRange) | length(yearRange)==2 & is.numeric(yearRange))
    FUNstr <- as.character(substitute(FUN))
    stopifnot(FUNstr %in% c("mean", "min", "max", "sum"))
    
    # If a unique ensemble is specified, jump right to loadEnsemble()
    if(!is.null(variable) & !is.null(model)
       &!is.null(experiment) & !is.null(ensemble)) {
        return(loadEnsemble(variable, model, experiment, ensemble, domain,
                            path=path, recursive=recursive, verbose=verbose,
                            force.ncdf=force.ncdf, yearRange=yearRange))
    }
    
    # List all files that match specifications
    fileList <- list.files(path=path, full.names=TRUE, recursive=recursive)
    
    # Only pull the files which are specified by the id strings
    fileList <- fileList[grepl(pattern=sprintf('^%s_%s_%s_%s_',
                                               variable, domain, model, experiment),
                               basename(fileList))]
    
    if(length(fileList) == 0) {
        warning("Could not find any matching files")
        return(NULL)
    }
    
    # Strip the .nc out of the file list
    fileList <- gsub('\\.nc$', '', fileList)
    
    # Parse out the ensemble strings according to CMIP5 specifications for
    # ...file naming conventions
    ensembleArr <- unique(unlist(lapply(strsplit(basename(fileList), '_'),
                                        function(x){x[5]})))
    
    if(verbose) cat('Averaging ensembles:', ensembleArr, '\n')
    
    modelTemp <- NULL              # Initalize the return data structure
    for(ensemble in ensembleArr) { # for each ensemble...
        
        # load the entire ensemble
        temp <- loadEnsemble(variable, model, experiment, ensemble, domain,
                             path=path, verbose=verbose, recursive=recursive,
                             force.ncdf=force.ncdf, yearRange=yearRange)
        
        if(is.null(modelTemp)) {         # If first model, just copy
            modelTemp <- temp
        } else {
            # Make sure lat-lon-Z-time match
            if(all(dim(temp) == dim(modelTemp)) &
                   identical(temp$lat, modelTemp$lat) &
                   identical(temp$lon, modelTemp$lon) &
                   identical(temp$Z, modelTemp$Z) &
                   identical(temp$time, modelTemp$time)) {
                
                # Add this ensemble's data and record file and ensemble loaded
                if(FUNstr %in% c("min", "max")) { # for min and max, compute as we go
                    if(verbose) cat("Computing", FUNstr)
                    combined <- array(c(modelTemp$val, temp$val), dim=c(dim(modelTemp$val), 2))
                    modelTemp$val <- aaply(combined, 
                                           .margins=1:length(dim(modelTemp$val)),
                                           .fun=FUN,
                                           .progress=ifelse(verbose, "text", "none"))
                } else { # mean and sum are easier, and much faster
                    modelTemp$val <- modelTemp$val + temp$val
                }
                
                modelTemp$files <- c( modelTemp$files, temp$files )
                modelTemp$ensembles <- c(modelTemp$ensembles, ensemble)
                modelTemp <- addProvenance(modelTemp, temp)
                modelTemp <- addProvenance(modelTemp, paste("Added ensemble", ensemble))
            } else { # ...if dimensions don't match, don't load
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
    
    # If taking the mean, calculate over all ensembles
    if(FUNstr == "mean")
        modelTemp$val <- unname(modelTemp$val / length(modelTemp$ensembles))
    
    # Update provenance and return
    addProvenance(modelTemp, c(paste("Computed", FUNstr, "of ensembles:",
                                                  paste(ensembleArr, collapse=' '))))
} # loadCMIP5

