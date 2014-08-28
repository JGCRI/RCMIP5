#' List all CMIP5 files in a directory tree.
#' 
#' List all CMIP5 files in a directory tree, parsing their filenames for
#' information like experiment, model, and variable names.
#'
#' @param path string root of directory tree
#' @param recursive logical. Should the listing recurse into directories?
#' @return  data.frame containing the following parsed from file names:
#' \item{filename}{Full filename, including path}
#' \item{variable}{File variable}
#' \item{domain}{File domain}
#' \item{model}{Model that produced this file}
#' \item{experiment}{File experiment}
#' \item{ensemble}{File ensemble}
#' \item{time}{year (and often month) range of file}
#' \item{size}{File size, in kilobytes}
#' @details For more information on CMIP5 filename structure and data description, 
#' see \url{http://cmip-pcmdi.llnl.gov/cmip5/data_description.html}
#' @export
#' @examples
#' getFileInfo()
#' getFileInfo('.',recursive=FALSE)
getFileInfo <- function(path='.', recursive=TRUE) {

    # Sanity checks
    stopifnot(length(path)==1 & is.character(path))
    stopifnot(length(recursive)==1 & is.logical(recursive))
    stopifnot(file.exists(path))

    # Match the path conventions to the operating system
    w <- getOption('warn')
    options(warn=-1)
    path <- normalizePath(path)
    options(warn=w)
    
    # Pull all nc files from the directory
    fullFile <- list.files(path=path, pattern='nc$',
                           full.names=TRUE, recursive=recursive)

    # Check that there are nc files to process
    if(!length(fullFile)) {
        warning('No netcdf files found')
        return(NULL)
    }

    # Pull the file name w/o directory and take off the '.nc',
    shortFile <- gsub(".nc$", "", basename(fullFile))

    # Split out the various components of the file name based on CMIP5 convention
    fileInfo <- strsplit(shortFile, split='_')

    # Check how many components (pieces of info) we have
    infoSize <- unlist(lapply(fileInfo, length))
    valid <- infoSize %in% c(5,6)
    if(!all(valid)) {
        warning('Unexpected (not correctly formatted) files. Cutting the following files from the list: ', fullFile[!valid])
        fullFile <- fullFile[valid]
        shortFile <- shortFile[valid]
        fileInfo <- fileInfo[valid]
        infoSize <- infoSize[valid]
        if(length(fullFile) == 0) {
            warning('No files to process.')
            return(NULL)
        }
    }

    # Pull the file size
    sizeInfo <- unlist(lapply(fullFile, function(x){paste0(round(file.info(x)$size/1024),"K")}))

    # Deal with the 'fixed' variables  (example: areacella)
    if(any(infoSize == 5)) {
        fixedInfo <- t(as.data.frame(fileInfo[infoSize == 5], row.names=NULL))
        fixedInfo <- cbind(fixedInfo, rep('', length=sum(infoSize == 5)))
        fixedInfo <- data.frame(path=dirname(fullFile[infoSize == 5]),
                                filename=shortFile[infoSize==5],
                                fixedInfo,
                                size=sizeInfo[infoSize==5],
                                row.names=NULL)
    } else {
        fixedInfo <- NULL
    }

    # Deal with the 'temporal' variables (example: tas)
    if(any(infoSize == 6)) {
        temporalInfo <-  t(as.data.frame(fileInfo[infoSize==6], row.names=NULL))
        temporalInfo <- data.frame(path=dirname(fullFile[infoSize == 6]),
                                filename=shortFile[infoSize==6],
                                temporalInfo,
                                size=sizeInfo[infoSize==6],
                                row.names=NULL)
    } else {
        temporalInfo <- NULL
    }

    # Put everything together
    fileInfo.df <- rbind(fixedInfo, temporalInfo)
    names(fileInfo.df) <- c('path', 'filename', 'variable', 'domain', 'model', 'experiment', 'ensemble', 'time', 'size')
    fileInfo.df <- data.frame(lapply(fileInfo.df, as.character),
                              stringsAsFactors=FALSE)

    return(fileInfo.df)
}
