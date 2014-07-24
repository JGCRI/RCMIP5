#' List all CMIP5 file in a directory tree and parse their filenames for CMIP5
#' information like experiment, model, and variable names.
#'
#' @param path root of directory tree
#' @param recursive logical. Should the listing recurse into directories?
#' @return  data frame containing the following parsed from file names:
#'         1) full file name, 2) filename without path or '.nc' extension,
#'         3) variable, 4) domain, 5) model, 6) experiment, 7) ensemble
#'         8) time period, 9) filesize in KB
#' @examples
#' getFileInfo()
#' getFileInfo('just_this_dir',recursive=F)
getFileInfo <- function(path='.', recursive=TRUE) {
    
    # Sanity checks
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(file.exists(path))
    
    # Pull the full filenames and extract short (no path or extension) names
    path <- normalizePath(path)
    fullFile <- list.files(path=path, pattern='nc$',
                           full.names=TRUE, recursive=recursive)
    if(!length(fullFile)) {
        warning('No netcdf files found')
        return(NULL)
    }
    
    # Pull the file name w/o directory and take off the '.nc',
    # and split out the various components of the file name
    shortFile <- gsub(".nc$", "", basename(fullFile))
    fileInfo <- strsplit(shortFile, split='_')
    
    # Check how many pieces of information we have
    infoSize <- unlist(lapply(fileInfo, length))
    valid <- infoSize %in% c(5,6)
    if(!all(valid)){
        warning('Unexpected (not correctly formatted) files: ',fullFile[!valid])
        fullFile <- fullFile[valid]
        shortFile <- shortFile[valid]
        fileInfo <- fileInfo[valid]
        infoSize <- infoSize[valid]
        if(length(fullFile) == 0) return(NULL)
    }
    
    fixedInfo <- t(as.data.frame(fileInfo[infoSize == 5], row.names=NULL))
    fixedInfo <- cbind(fixedInfo, rep('', length=sum(infoSize == 5))) # Deal with the fixed variables like areacella
    row.names(fixedInfo) <- NULL ##Strip the to remove embeded atributes in the data frame
    
    if(length(fixedInfo) == 0) fixedInfo <- NULL
    
    #print(str(fixedInfo))
    #print(fixedInfo)
    
    temporalInfo <-  t(as.data.frame(fileInfo[infoSize==6], row.names=NULL)) # Deal with temporal variables
    row.names(temporalInfo) <- NULL
    if(length(temporalInfo) == 0) temporalInfo <- NULL
    
    sizeInfo <- unlist(lapply(fullFile, function(x){paste0(round(file.info(x)$size/1024),"K")}))
    
    # Put everything together
    fileInfo.df <- data.frame(row.names=NULL,
                              path=dirname(fullFile),
                              filename=shortFile,
                              rbind(fixedInfo, temporalInfo),
                              sizeInfo)
    
    # Add useful column names and convert everything to character
    names(fileInfo.df) <- c('path', 'filename', 'variable', 'domain', 'model', 'experiment', 'ensemble', 'time', 'size')
    fileInfo.df <- data.frame(lapply(fileInfo.df, as.character), stringsAsFactors=FALSE)
    
    return(fileInfo.df)
}
