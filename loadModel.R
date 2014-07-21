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
loadModel <- function(path='.', experiment=NULL, variable=NULL,
                          model=NULL, recursive=TRUE){

    ##Pull a list of all files that match specifications
    ##Match file types of
    ##...variable_domain_model_experiment_ensemble_startYr_endYr.nc
    fileList <- list.files(path=path,
                           pattern=sprintf('%s_[a-zA-Z]+_%s_%s_',
                                     variable, model, experiment),
                           recursive=recursive)

    #cat('loading:\n')
    #print(fileList)

    ##Pull the ensemble strings
    ensembleArr <- unique(unlist(lapply(strsplit(fileList, '_'),
                                        function(x){x[5]})))

    #cat('averaging ensembles:', ensembleArr, '\n')

    #Initialize the variable to return model data
    model.ls <- NULL

    #Go through each ensemble
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
        }else{
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
    model.ls$val <- model.ls$val / length(ensembleNames)

    return(model.ls)
}
