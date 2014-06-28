require(plyr) #for BIG data.frames we want to do this fast

checkTimePeriod <- function(fileInfo_df){
    ##Purpose: Check that all time periods match for multi-file ensembles
    ##Input: fileInfo_df - data.frame from getFileInfo
    ##Outputs: data.frame with
    ##         from fileInfo_df: domain, experiment, model, variable, ensemble, and
    ##         yrStr (string - all year boundries in relevent files)
    ##         allHere (boolean - do the year boundries match up?)
    ##         startYr (numeric - decemal time of minimum year
    ##         endYr (numeric - decemal time of maximum year)
    ##Note: Decemal time is (year + (month-1)/12).
    ##      Non-monthly time intervals and temporally fixed varaibles are the
    ##      ...only ones delt with anything else will throw an error.
    ##Date: 28 June 2014

    #fileInfo_df <- fileInfo     #Debugging

    ##Check that we only have monthly and fixed variables
    if(!all(grepl('mon', fileInfo_df$domain) |
            grepl('fx', fileInfo_df$domain))){
        stop('Time checks for non-monthly variables not currently coded. Can not handle [',
             unique(fileInfo_df[!all(grepl('mon', fileInfo_df$domain) |
                                     grepl('fx', fileInfo_df$domain))]), ']')
    }

    ##This is the bulk of the function here. We return expModelVarEns.
    expModelVarEns <- ddply(fileInfo_df,
                            .(domain, experiment, model, variable, ensemble),
            function(x){
                  curCombo <-x$time

                  ##find the starting and ending decimal year
                  ##...time string is of the form 'YYYYMM-YYYYMM'
                  endMnt <- as.numeric(substr(curCombo, 12, 13))
                  endYr <- as.numeric(substr(curCombo, 8, 11)) + (endMnt-1)/12
                  startMnt <- as.numeric(substr(curCombo, 5, 6))
                  startYr <- as.numeric(substr(curCombo, 1, 4)) + (startMnt-1)/12
                  ##what year are we shoting for to link up with the next file
                  nextYr <- endYr + 1/12

                  ##If there are multiple files specified
                  if(length(startYr) > 1){
                      ##shift the indexes to compare the right start/stop
                      startInd <- c(2:length(startYr))
                      endInd <- c((2:length(startYr))-1)

                      ##construct the answering data frame which contains
                      ##yrStr - All orginal year strings for reference
                      ##       (useful if something is wrong).
                      ##allHere - boolean saying if the strings match up
                      ##startDate - earliest time stamp
                      ##endDate - latest time stamp
                      ans <- data.frame(yrStr=paste(curCombo, collapse='_'),
                                        allHere=all((nextYr[endInd] - startYr[startInd]) < 1e-6),
                                        startDate=min(startYr), endDate=max(endYr))
                  }else{
                      ##otherwise there is only one file and the entire time
                      ##period is by definition here!
                      ans <- data.frame(yrStr=paste(curCombo, collapse='_'),
                                        allHere=TRUE,
                                        startDate=min(startYr), endDate=max(endYr))
                  }

                  return(ans)
              })

    return(expModelVarEns)
}
