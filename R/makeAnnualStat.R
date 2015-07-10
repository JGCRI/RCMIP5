#' Compute annual statistic of a variable
#'
#' Most CMIP5 data are monthly, and we frequently want to summarize these to annual
#' numbers. This function does that (although annual files also occur, and will be
#' handled as well). The default statistic is \link{mean}, but any summary
#' function that returns a numeric result can be used.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param verbose logical. Print info as we go?
#' @param sortData logical. Sort \code{x} and \code{area} before computing?
#' @param filterNum logical. Only keep the years which share the most common count.
#' For example, only keep years with 12 months and discard those with 11 or fewer.
#' @param FUN function. Function to apply across months of year
#' @param ... Other arguments passed on to \code{FUN}
#' @return A \code{\link{cmip5data}} object, whose \code{val} field is the annual
#' mean of the variable. A \code{numMonths} field is also added
#' recording the number of months averaged for each year.
#' @details The stat function is calculated for all combinations of lon,
#' lat, and Z (if present).
#' @note If \code{x} is not in a needed order (for example, \code{FUN} uses
#' weights in a different order), be sure to specify \code{sortData=TRUE}.
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' makeAnnualStat(d)
#' summary(makeAnnualStat(d))
#' summary(makeAnnualStat(d, FUN=sd))
#' @seealso \code{\link{makeZStat}} \code{\link{makeGlobalStat}} \code{\link{makeMonthlyStat}}
#' @export
makeAnnualStat <- function(x, verbose=FALSE, sortData=FALSE, filterNum=TRUE, FUN=mean, ...) {
    # Sanity checks
    assert_that(class(x)=="cmip5data")
    assert_that(is.flag(verbose))
    assert_that(is.flag(sortData))
    assert_that(is.function(FUN))
    
    
    # Main computation code
    timer <- system.time({  # time the main computation, below
        if(is.array(x$val)) {
            rawYrs <- table(floor(x$time))
            yrs <- as.numeric(names(rawYrs))
            mostCommonCount <- as.numeric(names(table(rawYrs))[table(rawYrs) == max(table(rawYrs))])
            if(filterNum) {
                if(verbose) {
                    print('Filtering based on number in annual aggregation: ')
                    print(rawYrs)
                    print('count required: ')
                    print(mostCommonCount)
                }
                yrs <- as.numeric(names(rawYrs)[rawYrs == mostCommonCount])    
                if(verbose) {
                    print('new years:')
                    print(yrs)
                }
            }
            myDim <- dim(x$val)
            x$val <- vapply(yrs, 
                            FUN=function(yrNum, ...) {
                                temp <- x$val[,,,yrNum == floor(x$time)]
                                myDim[4] <- sum(yrNum == floor(x$time))
                                dim(temp) <- myDim
                                temp <- apply(temp, c(1,2,3), FUN, ...)
                                myDim[4] <- 1
                                dim(temp) <- myDim
                                return(temp)}, 
                            FUN.VALUE=x$val[,,,1], ...)
            myDim[4] <- length(yrs)
            dim(x$val) <- myDim
            x$time <- yrs
            x$numPerYear <- as.integer(rawYrs[as.numeric(names(rawYrs)) %in% yrs])
            x$debug$AnnualFreqTable <- rawYrs
        } else {
            # Suppress stupid NOTEs from R CMD CHECK
            lon <- lat <- Z <- time <- year <- value <- `.` <- NULL
            
            # Put data in consistent order BEFORE overwriting time
            if(sortData) {
                if(verbose) cat("Sorting data...\n")
                x$val <- group_by(x$val, lon, lat, Z, time) %>%
                    arrange()            
            }
            
            # Instead of "summarise(value=FUN(value, ...))", we use the do()
            # call below, because the former doesn't work (as of dplyr 0.3.0.9000):
            # the ellipses cause big problems. This solution thanks to Dennis
            # Murphy on the manipulatr listesrv.
            x$val <- x$val %>%
                # start by taking spatial mean, in case there are multiple data per spatial point
                # looking at you, IPSL-CM5A-MR
                group_by(lon, lat, Z, time) %>%
                summarise(value=mean(value, na.rm=TRUE)) %>% 
                # now move on to year summary
                mutate(year = floor(time)) %>%
                group_by(lon, lat, Z, year) %>%
                do(data.frame(value = FUN(.$value, ...),
                              counts = length(.$value))) %>%
                ungroup()
            freqTable <- unique(x$val[,c('year', 'counts')])
            assert_that(max(freqTable$counts) <= 12) # shouldn't be more than 12 months per year
            
            if(filterNum) {
                if(verbose) {
                    print('Filtering based on number in annual aggregation: ')
                    print(freqTable)
                    print('number required: ')
                    print(freqTable$year[which.max(freqTable$counts)] )
                }
                x$val <- x$val[x$val$count == max(freqTable$counts),]
                x$numPerYear <- freqTable$counts[freqTable$counts == max(freqTable$counts)]
            } else {
                x$numPerYear <- freqTable$counts[order(freqTable$year)]
            }
            x$val <- x$val[c('lon', 'lat', 'Z', 'year', 'value')]
            x$val$time <- x$val$year
            x$val$year <- NULL
            
            if(verbose) {
                print(head(x$val))
            }
            
            # dplyr doesn't (yet) have a 'drop=FALSE' option, and the summarise
            # command above may have removed some lon/lat combinations
            if(length(unique(x$val$lon)) < length(unique(as.numeric(x$lon))) |
                   length(unique(x$val$lat)) < length(unique(as.numeric(x$lat)))) {
                if(verbose) cat("Replacing missing lon/lat combinations\n")
                
                # Fix this by generating all lon/lat pairs and combining with answer
                full_data <- tbl_df(data.frame(lon=as.vector(x$lon), lat=as.vector(x$lat)))
                x$val <- left_join(full_data, x$val, by=c("lon", "lat"))
            }
            x$time <- sort(freqTable$year)
            x$debug$AnnualFreqTable <- freqTable
        }
    }) # system.time
    
    if(verbose) cat('\nTook', timer[3], 's\n')
    
    x$debug$timeFreqStr <- "years (summarized)"
    addProvenance(x, paste("Calculated", 
                           paste(deparse(substitute(FUN)), collapse="; "),
                           "for years", min(x$time), "-", max(x$time)))
} # makeAnnualStat
