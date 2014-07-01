#' List all CMIP5 file in a directory tree and parse their filenames for CMIP5
#' information like experiment, model, and variable names.
#'
#' @param path root of directory tree
#' @param recursive logical. Should the listing recurse into directories?
#' @return  data frame containing the following parsed from file names.
#'         1) full file name, 2) filename without the directory or '.nc'
#'         3) variable, 4) domain, 5) model, 6) experiment, 7) ensemble
#'         8) time period, 9) filesize in KB
#' @examples
#' getFileInfo()
#' getFileInfo('just_this_dir',recursive=F)
#' @author Kathe Todd-Brown and Ben Bond-Lamberty
getFileInfo <- function(path='.', recursive=TRUE){

    ## Sanity checks
    ##CMIP5Dir <- normalizePath(CMIP5Dir)
    stopifnot(length(path)==1)
    stopifnot(is.character(path))
    stopifnot(file.exists(path))
    stopifnot(is.logical(recursive))

    ## Pull the full filenames and extract short (no path or extension) names
    fullFile <- list.files(path=path, pattern='nc$',
                               full.names=TRUE, recursive=recursive)
    if(length(fullFile) == 0){
        warning('No netcdf files found')
        return(NULL)
    }

    ##Pull the file name w/o directory and take off the '.nc',
    ##...this is the informative part of the naming convention.
    shortFile <- gsub(".nc$", "", basename(fullFile))

    ##split out the various components of the file name
    fileInfo <- strsplit(shortFile, split='_')

    ## Check how many pieces of information we have
    infoSize <- unlist(lapply(fileInfo, length))

    ## TODO: shouldn't abort here; either ignore file, or have blank row in data frame
    valid <- infoSize %in% c(5,6)
    if(!all(valid)){
        stop('Unexpected info found in files: ',fullFile[!valid],'... aborting')
    }
    
    fixedInfo <- t(as.data.frame(fileInfo[infoSize == 5], row.names=NULL))
    fixedInfo <- cbind(fixedInfo, rep('', length=sum(infoSize == 5))) ##Deal with the fixed variables like areacella
    if( !length(fixedInfo)) fixedInfo <- NULL
    
    #print(str(fixedInfo))
    #print(fixedInfo)

    temporalInfo <-  t(as.data.frame(fileInfo[infoSize==6], row.names=NULL)) ##Deal with temporal variables
    if( !length(temporalInfo)) temporalInfo <- NULL
    
    sizeInfo <- unlist(lapply(fullFile, function(x){paste0(round(file.info(x)$size/1024),"K")}))
    
    ##Put everything together
    fileInfo.df <- data.frame(row.names=NULL,
    						path=dirname(fullFile),
                            filename=shortFile,
                            rbind(fixedInfo, temporalInfo),
							sizeInfo)

    ##add useful column names
    names(fileInfo.df) <- c('path', 'filename', 'variable', 'domain', 'model', 'experiment', 'ensemble', 'time', 'size')

    return(fileInfo.df)
}
