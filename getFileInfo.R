#' List all CMIP5 file in a directory tree and parse their filenames for CMIP5
#' information like experiment, model, and variable names.
#'
#'
#' @param CMIP5Dir root of directory tree
#' @param logical. Should the listing recurse into directories?
#' @return  data frame containing the following parsed from file names.
#'         1) full file name, 2) filename without the directory or '.nc'
#'         3) variable, 4) domain, 5) model, 6) experiment, 7) ensemble
#'         8) time period, 9) filesize in bytes
#' @examples
#' getFileInfo()
#' @author Kathe Todd-Brown and Ben Bond-Lamberty

getFileInfo <- function(CMIP5Dir='.', recursive=TRUE){


    ##Debug defaults
    ##CMIP5Dir <- '/Volumes/DATAFILES/downloads'
    ##recursive <- TRUE

    ## Sanity checks
    ##CMIP5Dir <- normalizePath(CMIP5Dir)
    stopifnot(length(CMIP5Dir)==1)
    stopifnot(is.character(CMIP5Dir))
    stopifnot(file.exists(CMIP5Dir))
    stopifnot(is.logical(recursive))

    ## Pull the full filenames and extract short (no path or extension) names
        fullFile <- list.files(path=CMIP5Dir, pattern='nc$',
                               full.names=TRUE, recursive=recursive)
    if(length(fullFile) == 0){
        stop('No netcdf files found in specified directory.')
    }


    ##Pull the file name w/o directory and take off the '.nc',
    ##...this is the informative part of the naming convention.
    shortFile <- unlist(lapply(strsplit(fullFile, '[\\/]'),
                               function(x){sub('\\.nc', '', rev(x)[1])}))

    ##split out the various components of the file name
    fileInfo <- strsplit(shortFile, split='_')

    ## Check how many pieces of information we have
    infoSize <- unlist(lapply(fileInfo, length))

    ## TODO: shouldn't abort here; either ignore file, or have blank row in data frame
    if(!all(infoSize %in% c(5,6))){
        ##if they are an unexpected length then abort
        stop('Unexpected info found in file name [',unique(infoSize),']... aborting')
    }

    fixedInfo <- t(as.data.frame(fileInfo[infoSize == 5]))
    fixedInfo <- cbind(fixedInfo, rep('', length=sum(infoSize == 5))) ##Deal with the fixed variables like areacella
    row.names(fixedInfo) <-NULL ##Be sure to strip out the long row names otherwise it trips up other functions

    #print(str(fixedInfo))
    #print(fixedInfo)

    temporalInfo <-  t(as.data.frame(fileInfo[infoSize==6])) ##Deal with temporal variables
    row.names(temporalInfo) <- NULL


    ##Put everything together
    fileInfo.df <- data.frame(fullFilename=fullFile, filename=shortFile,
                              rbind(fixedInfo, temporalInfo),
                              unlist(lapply(fullFile, function(x){file.info(x)$size})))

    ##strip the non-informative row names
    row.names(fileInfo.df) <- NULL

    ##add useful column names
    names(fileInfo.df) <- c('fullFilename', 'filename', 'variable', 'domain', 'model', 'experiment', 'ensemble', 'time', 'fileSize')

    return(fileInfo.df)
}
