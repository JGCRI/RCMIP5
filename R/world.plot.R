library(fields)

#' Plot global data.
#' 
#' Plot a quick world map with reasonable coloring.
#'
#' @param x x
#' @param time time
#' @param main main
#' @param parList parList
#' @param centerZero centerZero
#' @param absNum absNum
#' @param axisFlag axisFlag
#' @param showRange showRange
#' @param verbose logical. Print info as we go?
#' @param simple simple
#' @param col col
#' @param latAxis latAxis
#' @param lonAxis lonAxis
#' @export
world.plot <- function(x, time=1, main=NULL, parList=NULL,
                       centerZero=FALSE,  absNum=NULL, axisFlag=TRUE,
                       showRange=TRUE, verbose=FALSE, simple=FALSE,
                       col=NULL, latAxis=TRUE, lonAxis=TRUE) {
    ##        main - string that is the title of the plot
    ##        parList - a list of par values
    ##        centerZero - coloring of the map is centered around 0
    ##        absNum - absolute c(min, max) for the coloring
    ##        showRange - If true then show the max value in the color labels otherwise just show +/-
    ##        verbose - true, then this code is broken and you are trying to fix it.
    ##        simple - don't show the color key and just print the maps
    
    if(verbose) cat('******world.plot starting************\n')

    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    
    # Pull the lat/lon/val/main from the cmip5 object
    lon <- x$lon
    lat <- x$lat
    if(length(dim(x$val)) > 2) {
        val <- x$val[,,time]  # TODO: won't handle levels/depths correctly
    }
    
    if(is.null(main)) {
        main <- sprintf('%s %s [%s]', x$model, x$variable, x$valUnit)
    }
    
    val[!is.finite(val)] <- NA           # clean up the values to plot
    
    # Pull the real max/min before we change anything, just in case
    trueMax <-max(val, na.rm=TRUE)
    trueMin <-min(val, na.rm=TRUE)
    
    #    numBreaks <- 13 # make sure that the breakIndex plays nicely with numBreaks
    numBreaks <- 65
    ###################################################################
    # Figure out the color scheme and what the min/max we are plotting
    ###################################################################
    if(!is.null(absNum)) { ##are there min/max cut off's provided?
        if(verbose) cat('setting abs min/max \n')
        # set the min/max to some absolute number
        minNum <- absNum[1]
        maxNum <- absNum[2]
        if(centerZero) {
            maxNum <- max(abs(absNum))
            minNum <- -maxNum
        }
    } else if(centerZero) { # set the min/max to be symmetric about 0
        if(verbose) cat('setting 0-centered min/max')
        quant <- quantile(abs(val), na.rm=TRUE)
        maxDiv <- quant[[3]]+10*(quant[[4]]-quant[[3]])*1.05
        trueDiv <- max(abs(c(trueMax, trueMin)))
        if(trueDiv < maxDiv) maxDiv <- trueDiv
        minNum <- -maxDiv
        maxNum <- maxDiv
    } else { # set the min/max relitive to the quantiles
        if(verbose) cat('setting quant derived min/max')
        quant <- quantile(val, na.rm=TRUE)
        minNum <- quant[[3]]-10*(quant[[3]]-quant[[2]])
        maxNum <- quant[[3]]+10*(quant[[4]]-quant[[3]])
        minNum <- max(minNum, quant[[1]])
        maxNum <- min(maxNum, quant[[5]])
    }
    
    ##############################################################
    # Figure out what colors we are using
    ##############################################################
    if(is.null(col)) {
        if(centerZero) {
            # be smart about the color map if we want it zero centered
            if(verbose){cat('centering colors \n')}
            # col.gen <- colorRampPalette(c('violet', 'dark blue', 'beige',
            #                                'dark red', 'yellow'), space='rgb')
            col.gen <- colorRampPalette(c('dark red', 'beige', 'dark green'),
                                        space='rgb')
            col <- col.gen(numBreaks-1)
        } else {
            # Use default tim.colors otherwise
            col <- tim.colors(numBreaks-1)
        }
    } else {
        col <- col(numBreaks-1)   # TODO: this doesn't work
    }
    
    ####################################################
    # Set the breaks and break labels
    ####################################################
    breaks <- seq(minNum, maxNum, length=numBreaks)
    breakLabels <- breaks
    breakIndex <- floor(seq(1, numBreaks, length=5))
    breakLabels <- signif(breakLabels, digits=3)
    labelPos <- breaks
    
    # Replace any values above/below the max/min with the max/min and
    # ...change the break labels were appropreate
    if(any(val<minNum, na.rm=TRUE)) {
        if(showRange){
            breakLabels[1] <- paste('[', signif( trueMin, digits=2), 'to', breakLabels[2], ']')
        } else {
            breakLabels[1] <- paste(breakLabels[2], '-')
        }
        val[val<minNum] <- minNum
    } else if(all(val > minNum, na.rm=TRUE)) { # all values are less then min
        breakIndex[1] <- which(breaks > min(val,na.rm=TRUE))[1]
    }
    if(any(val>maxNum, na.rm=TRUE)) {
        if(showRange) {
            breakLabels[numBreaks] <- paste('[', breakLabels[numBreaks],
                                            'to', signif(trueMax, digits=2) ,']')
        } else {
            breakLabels[numBreaks] <- paste(breakLabels[numBreaks],'+')
        }
        val[val>maxNum] <- maxNum
    } else if(all(val < maxNum, na.rm=TRUE)) { # all values are less then min
        breakIndex[numBreaks] <- rev(which(breaks < max(val,na.rm=TRUE)))[1]
    }
    
    if(verbose) {
        cat('trueMin: [', trueMin, '], minNum: [', minNum,']\n')
        cat('breakIndex: [', breakIndex, ']\n')
        cat('breakLabels: [', breakLabels, ']\n')
        cat('lables: [', breakLabels[breakIndex],']\n')
        cat('labelPos: [', labelPos[breakIndex],']\n')
    }
    
    ################################################################
    # Set the parameter list
    ################################################################
    par(las=1)
    if(!is.null(parList)) par(parList)
    
    #################################################################
    # Plot the figures
    #################################################################
    numlon <- length(lon)
    half.numlon <- ceiling(length(lon) * 0.527) # yields a division at 190 for a 360 grid
    
    # Split at the Pacific ocean
    shiftIndex <- c(half.numlon:numlon, 1:(half.numlon-1))
    
    # legend.mar set to 12 to give lots of room to the break labels
    if(!simple) {
        image.plot(lon-180, lat[lat > -60], val[shiftIndex,lat > -60],
                   main = main,
                   col = col, breaks = breaks,
                   axis.args=list(at=labelPos[breakIndex],
                                  labels=breakLabels[breakIndex]),
                   legend.mar=12,
                   xlab = '', ylab='', yaxt='n', xaxt='n')
        
    } else {
        image(lon-180, lat[lat > -60], val[shiftIndex, lat > -60],
              main = main,
              col = col, breaks = breaks,
              xlab = '', ylab='', yaxt='n', xaxt='n')
    }
    # points(c(-180, 180), c(0,0), type='l')
    if(axisFlag) {
        if(lonAxis) axis(1, at=c(-120, -60, 0, 60, 120),
                         labels=c('-120', '-60', '0','60', '120'))
        if(latAxis) axis(2, at=c(-60, -30, 0, 30, 60),
                         labels=c('-60', '-30', '0','30', '60'))
    }
    box()
    if(verbose) cat('******world.plot ending************\n')
} # world.plot
