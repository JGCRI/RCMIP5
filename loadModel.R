library('ncdf4')

source('loadEnsemble.R')

loadModel <- function(path='.', experiment=NULL, variable=NULL,
                          model=NULL, recursive=TRUE){

    ##Pull a list of all files that match specifications
    fileList <- list.files(path=path,
                           pattern=sprintf('%s_[a-zA-Z]+_%s_%s_',
                                     variable, model, experiment),
                           recursive=recursive)

    #cat('loading:\n')
    #print(fileList)

    ensembleArr <- unique(unlist(lapply(strsplit(fileList, '_'),
                                        function(x){x[5]})))

    cat('averaging ensembles:', ensembleArr, '\n')

    model.ls <- NULL

    for(ensemble in ensembleArr){
        if(is.null(model.ls)){
            model.ls <- loadEnsemble(path=path,
                                     experiment=experiment, variable=variable,
                                     model=model, ensemble=ensemble,
                                     recursive=recursive)
            model.ls$files <- list(a=model.ls$files)
            ensembleNames <- c(ensemble)
        }else{
            temp <- loadEnsemble(path=path,
                                     experiment=experiment, variable=variable,
                                     model=model, ensemble=ensemble,
                                     recursive=recursive)
            if(all(temp$lat==model.ls$lat) &
               all(temp$lon==model.ls$on) &
               all(temp$time==model.ls$time)){

                model.ls$val <- model.ls$val + temp$val
                model.ls$files <- c(model.ls$files, a=temp$files)
                ensembleNames <- c(ensembleNames, ensemble)
            }else{
                cat(ensemble, 'does not match previous ensembles\n')
            }
        }
    }
    names(model.ls$files) <- ensembleNames

    model.ls$val <- model.ls$val / length(ensembleNames)

    return(model.ls)

}
