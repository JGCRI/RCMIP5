getFileInfo <- function(CMIP5Dir='.', checkSubdirectory=TRUE){
    ##Purpose: List all CMIP5 files in a directory with their file size
    ##         and break down the information contained in their file names.
    ##Input: CMIP5Dir - a string identifing a valid directory
    ##       checkSubdirectory - a boolean flagging whether or not to
    ##                           search sub-directories
    ##Outputs: This function returns a data frame containing the following.
    ##         1) full file name, 2) filename without the directory or '.nc'
    ##         3) variable, 4) domain, 5) model, 6) experiment, 7) ensemble
    ##         8) time period, 9) filesize in bytes
    ##Date: 27 June 2014

    ##Pull the full file names
    fullFile <- list.files(path=CMIP5Dir, pattern='nc$', full.names=TRUE,
                           recursive=checkSubdirectory)

    ##Pull the file name w/o directory and take off the '.nc',
    ##...this is the informative part of the naming convention.
    shortFile <- unlist(lapply(strsplit(fullFile, '[\\/]'),
                               function(x){sub('\\.nc', '', rev(x)[1])}))

    ##split out the various components of the file name
    fileInfo <- strsplit(shortFile, '[_]')

    ##check how many pieces of information we have
    infoSize <- unlist(lapply(fileInfo, length))

    if(all(unique(infoSize) == c(5,6))){
        ##if they are an unexpected length then abort
        stop('Unexpected info found in file name... aborting')
    }

    ##Put everything together
    fileInfo.df <- data.frame(fullFilename=fullFile, filename=shortFile,
                              rbind( cbind(t(as.data.frame(fileInfo[infoSize == 5])), rep('', length=sum(infoSize == 5))), ##Deal with the fixed variables like areacella
                     t(as.data.frame(fileInfo[infoSize==6]))), ##Deal with temporal variables
                              unlist(lapply(fullFile, function(x){file.info(x)$size})))

    ##strip the non-informative row names
    row.names(fileInfo.df) <- NULL

    ##add useful column names
    names(fileInfo.df) <- c('fullFilename', 'filename', 'variable', 'domain', 'model', 'experiment', 'ensemble', 'time', 'fileSize')

    return(fileInfo.df)
}
