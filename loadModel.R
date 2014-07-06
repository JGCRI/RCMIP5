library('raster')
library('ncdf4')

source('loadEnsemble.R')

loadModel <- function(CMIP5dir='.', experiment=NULL, variable=NULL,
                          model=NULL, recursive=TRUE){

    ##Pull a list of all files that match specifications
    fileList <- list.files(path=CMIP5dir, pattern=sprintf('%s_[a-zA-Z]+_%s_%s_', variable, model, experiment), recursive=recursive)

    #cat('loading:\n')
    #print(fileList)

    ensembleArr <- unique(unlist(lapply(strsplit(fileList, '_'),
                                        function(x){x[5]})))

    cat('averaging ensembles:', ensembleArr, '\n')

    modelRst <- NULL
    for(ensemble in ensembleArr){
        if(is.null(modelRst)){
            modelRst <- loadEnsemble(CMIP5dir=CMIP5dir,
                                     experiment=experiment, variable=variable,
                                     model=model, ensemble=ensemble,
                                     recursive=recursive)
        }else{
            modelRst <- modelRst +
                        loadEnsemble(CMIP5dir=CMIP5dir,
                                     experiment=experiment, variable=variable,
                                     model=model, ensemble=ensemble,
                                     recursive=recursive)
        }
    }

    modelRst <- modelRst / length(ensembleArr)


    return(modelRst)

}
