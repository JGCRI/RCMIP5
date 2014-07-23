library('ncdf4')
source('loadEnsemble.R')

#' Average all ensemble members of the specified experiment-variable-model combination
#'
#' @param path root of directory tree
#' @param experiment CMIP5 experiment of interest
#' @param variable CMIP5 variable of interest
#' @param model CMIP5 model of interest
#' @param recursive logical. Should we recurse into directories?
#' @return TODO
#' @examples
#  cSoilModel_CanESM2 <- loadModel(path='/Volumes/DATAFILES/downloads', experiment='historical', variable='cSoil', model='CanESM2')
#  prcTemp <- loadEnsemble(experiment='rcp85', variable='prc', model='GFDL-CM3')
loadModel <- function(path='.', experiment='[a-zA-Z0-9-]+', variable='[a-zA-Z0-9-]+',
                      model='[a-zA-Z0-9-]+', recursive=TRUE, verbose=FALSE) {
 
    # Sanity checks
    stopifnot(is.character(path))
    stopifnot(is.character(experiment))
    stopifnot(is.character(variable))
    stopifnot(is.character(model))
    stopifnot(is.logical(recursive))
    stopifnot(is.logica(verbose))
    
    # List all files that match specifications
    fileList <- list.cmip5.files(path,experiment,variable,model,ensemble='[a-zA-Z0-9-]+',recursive)
    
    # Parse out the ensemble strings
    ensembleArr <- unique(unlist(lapply(strsplit(fileList, '_'),
                                        function(x){x[5]})))
    
    if(verbose) cat('Averaging ensembles:', ensembleArr, '\n')
    
    model.ls <- NULL
    for(ensemble in ensembleArr){
        #If this is the first model
        if(is.null(model.ls)){
            #initialize the results with the first ensemble
            model.ls <- loadEnsemble(path=path,
                                     experiment=experiment, variable=variable,
                                     model=model, ensemble=ensemble,
                                     recursive=recursive)
            #record the files for the ensembles
            model.ls$files <- list(a=model.ls$files)
            #record the sucessful ensemble load
            ensembleNames <- c(ensemble)
        } else {
            #load the new ensemble
            temp <- loadEnsemble(path=path,
                                 experiment=experiment, variable=variable,
                                 model=model, ensemble=ensemble,
                                 recursive=recursive)
            #If the lat-lon-time match
            if(all(temp$lat==model.ls$lat) &
                   all(temp$lon==model.ls$on) &
                   all(temp$time==model.ls$time)){
                #add them to the values
                model.ls$val <- model.ls$val + temp$val
                #record the files loaded
                model.ls$files <- c(model.ls$files, a=temp$files)
                #record a sucessful ensemble load
                ensembleNames <- c(ensembleNames, ensemble)
            }else{
                #notify user of failed load
                cat(ensemble, 'does not match previous ensembles lon-lat-time.\n')
            }
        }
    }
    #name the files with the ensemble load
    names(model.ls$files) <- ensembleNames
    
    #convert the sum to an average
    if(length(ensembleNames > 0))
        model.ls$val <- model.ls$val / length(ensembleNames)
    
    return(model.ls)
} # loadModel
