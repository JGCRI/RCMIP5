library('raster')
library('ncdf4')

loadEnsemble <- function(CMIP5dir='.', experiment=NULL, variable=NULL,
                          model=NULL, ensemble=NULL, recursive=TRUE){

    ##Pull a list of all files that match specifications
    fileList <- list.files(path=CMIP5dir, pattern=sprintf('%s_[a-zA-Z]+_%s_%s_%s_', variable, model, experiment, ensemble), full.names=TRUE, recursive=recursive)

    cat('loading:\n')
    print(fileList)

    ensembleRst <- NULL

    for(filename in fileList){
        if(is.null(ensembleRst)){
            ensembleRst <- brick(filename, varname=variable, lvar=3) #may also need to id lvar=3
        }else{
            ##TODO This doesn't work with GFDL; they used non-uniform grids
            ##     Possible reason to switch to ncdf4 directly
            ##TODO Append data more elegantly, we loose all the header info
            ##     currently.
            ensembleRst <- addLayer(ensembleRst,
                                    brick(filename, varname=variable, lvar=3))
        }
    }

    return(ensembleRst)
}
