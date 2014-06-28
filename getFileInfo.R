#' List all CMIP5 file in a directory tree and parse their filenames
#'
#'
#' @param CMIP5Dir root of directory tree
#' @param logical. Should the listing recurse into directories? 
#' @return data frame holding parsed filename data
#' @examples
#' getFileInfo()
#' @author Kathe Todd-Brown and Ben Bond-Lamberty
getFileInfo <- function(CMIP5Dir='.', recursive=TRUE){

	# Sanity checks
	#CMIP5Dir <- normalizePath(CMIP5Dir)
	stopifnot(length(CMIP5Dir)==1)
	stopifnot(is.character(CMIP5Dir))
	stopifnot(file.exists(CMIP5Dir))
	stopifnot(is.logical(recursive))
	
    # Pull the full filenames and extract short (no path or extension) names
    fullFile <- list.files(path=CMIP5Dir, pattern='nc$', full.names=T, recursive=recursive)
    shortFile <- gsub(".nc$", "", basename(fullFile))

	if(!length(fullFile)) return(NULL)
	
    # Split out the various components of the file name
    fileInfo <- strsplit(shortFile, '[_]')

    # Check how many pieces of information we have
    infoSize <- unlist(lapply(fileInfo, length))

	# TODO: shouldn't abort here; either ignore file, or have blank row in data frame
	valid <- infoSize %in% c(5,6)
    if(!all(valid)){
        stop('Unexpected info found in files: ',fullFile[!valid],'... aborting')
    }

	# Parse information from the two kinds of files: fixed and temporal variables
	fivesInfo <- cbind(t(as.data.frame(fileInfo[infoSize == 5])), 
			rep('', length=sum(infoSize == 5)))	# Deal with the fixed variables like areacella
	if(!length(fivesInfo)) fivesInfo <- NULL
	sixesInfo <- t(as.data.frame(fileInfo[infoSize==6]))
	if(!length(sixesInfo)) sixesInfo <- NULL # Deal with temporal variables
	
    # Put everything together
    fileInfo.df <- data.frame( row.names=NULL,
    	fullFilename=dirname(fullFile),
    	filename=shortFile,
        rbind(fivesInfo,sixesInfo),
        unlist(lapply(fullFile, function(x){paste0(round(file.info(x)$size/1024),"K")}))
        )

    # Add useful column names
    names(fileInfo.df) <- c('path', 'filename', 'variable', 'domain', 'model', 
    							'experiment', 'ensemble', 'time', 'filesize')
    return(fileInfo.df)
}
