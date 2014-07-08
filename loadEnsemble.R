library('raster')
library('ncdf4')

#' Load data for a particular experiment/variable/model/ensemble combination
#'
#' @param path root of directory tree
#' @param experiment CMIP5 experiment to load
#' @param variable CMIP5 variable to load
#' @param model CMIP5 model to load
#' @param ensemble CMIP5 ensemble to load
#' @param recursive logical. Should we recurse into directories?
#' @return RasterStack object with all loaded data
#' @examples
#' loadEnsemble(model="GFDL-CM3",variable="prc",experiment="rcp85",ensemble="r1i1p1")
loadEnsemble <- function(path='.', experiment='[a-zA-Z0-9-]+', variable='[a-zA-Z0-9-]+',
                          model='[a-zA-Z0-9-]+', ensemble='[a-zA-Z0-9-]+', recursive=TRUE) {

    # List all files that match specifications
    fileList <- list.files(path=path, pattern=sprintf('%s_[a-zA-Z]+_%s_%s_%s_', variable, model, experiment, ensemble), full.names=TRUE, recursive=recursive)
    
    ensembleRst <- NULL
    for(f in fileList) {
        cat('Loading ',f)
        if(is.null(ensembleRst)) {
            ensembleRst <- brick(f, varname=variable, lvar=3) # may also need to id lvar=3
        } else {
            ##TODO This doesn't work with GFDL; they used non-uniform grids
            ##     Possible reason to switch to ncdf4 directly
            ##TODO Append data more elegantly, we lose all the header info
            ensembleRst <- addLayer(ensembleRst,
                                    brick(f, varname=variable, lvar=3))
        }
    }

    return(ensembleRst)
}
