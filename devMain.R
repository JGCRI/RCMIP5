source('getFileInfo.R')

fileInfo <- getFileInfo('/Volumes/DATAFILES/downloads')
##example remove the empty files that failed to download
#file.remove(as.character(fileInfo[fileInfo$fileSize == 0, 1]))

