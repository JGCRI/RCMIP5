library(fields)

world.plot <- function(lon, lat, val, title='world plot', parList=NULL,
                       centerZero=FALSE,  absNum=NULL, axisFlag=TRUE,
                       showRange=TRUE, debugging=FALSE, simple=FALSE,
                       col=NULL, latAxis=TRUE, lonAxis=TRUE){
    ##Purpose: create a world map with reasonable coloring
    ##Inputs: lon - a vector with the logitudes from 0 to 360 (Note that this needs to START at 0 and END at 360-step)
    ##        lat - a vector with the latitudes from -90 to 90 (Note this does not have to span the full latitudes but does need to match the dimentions of the val)
    ##        val - a matrix of values that is (lon,lat)
    ##        title - string that is the title of the plot
    ##        parList - a list of par values
    ##        centerZero - coloring of the map is centered around 0
    ##        absNum - absolute c(min, max) for the coloring
    ##        showRange - If true then show the max value in the color labels otherwise just show +/-
    ##        debugging - true, then this code is broken and you are trying to fix it.
    ##        simple - don't show the color key and just print the maps
    ##Date: April 2012
    ##Update: September 2013, 1) correct centerZero to handle absNum 2) show
    ##                        true min/max if not on color scale
    ##Programmer: K Todd-Brown ktoddbrown@gmail.com

    if(debugging){cat('******world.plot starting************\n')}

    val[!is.finite(val)] <- NA           #clean up the values to plot

    ##Pull the real max/min before we change anything, just in case
    trueMax <-max(val, na.rm=TRUE)
    trueMin <-min(val, na.rm=TRUE)

#    numBreaks <- 13 #make sure that the breakIndex plays nicely with numBreaks
    numBreaks <- 65
    ###################################################################
    ###Figure out the color scheme and what the min/max we are plotting
    ###################################################################
    if(!is.null(absNum)){ ##are there min/max cut off's provided?
        if(debugging){cat('setting abs min/max \n')}
        ##set the min/max to some absolute number
        minNum <- absNum[1]
        maxNum <- absNum[2]
        if(centerZero){
            maxNum <- max(abs(absNum))
            minNum <- -maxNum
        }
    }else if(centerZero){ ##set the min/max to be symmetric about 0
        if(debugging){cat('setting 0-centered min/max')}
        quant <- quantile(abs(val), na.rm=TRUE)
        maxDiv <- quant[[3]]+10*(quant[[4]]-quant[[3]])*1.05
        trueDiv <- max(abs(c(trueMax, trueMin)))
        if(trueDiv < maxDiv){maxDiv <- trueDiv}
        minNum <- -maxDiv
        maxNum <- maxDiv
    }else{ #set the min/max relitive to the quantiles
       if(debugging){cat('setting quant derived min/max')}
        quant <- quantile(val, na.rm=TRUE)
        minNum <- quant[[3]]-10*(quant[[3]]-quant[[2]])
        maxNum <- quant[[3]]+10*(quant[[4]]-quant[[3]])
        minNum <- max(minNum, quant[[1]])
        maxNum <- min(maxNum, quant[[5]])
    }

    ##############################################################
    ##Figure out what colors we are using
    ##############################################################
    if(is.null(col)){
        if(centerZero){
            ##be smart about the color map if we want it zero centered
            if(debugging){cat('centering colors \n')}
            ##col.gen <- colorRampPalette(c('violet', 'dark blue', 'beige',
            ##                                'dark red', 'yellow'), space='rgb')
            col.gen <- colorRampPalette(c('dark red', 'beige', 'dark green'),
                                        space='rgb')
            col <- col.gen(numBreaks-1)
        }else{
            ##Use default tim.colors otherwise
            col <- tim.colors(numBreaks-1)
        }
    }else{
        col <- col(numBreaks-1)
    }

    ####################################################
    ###Set the breaks and break labels
    ####################################################
    breaks <- seq(minNum, maxNum, length=numBreaks)
    breakLabels <- breaks
    breakIndex <- floor(seq(1, numBreaks, length=5))
    breakLabels <- signif(breakLabels, digits=3)
    labelPos <- breaks

    ##Replace any values above/below the max/min with the max/min and
    ##...change the break labels were appropreate
    if(any(val<minNum, na.rm=TRUE)){
        if(showRange){
            breakLabels[1] <- paste('[', signif( trueMin, digits=2)
                                    , 'to',  breakLabels[2], ']')
        }else{
            breakLabels[1] <- paste(breakLabels[2], '-')
        }
        val[val<minNum] <- minNum
    }else if(all(val > minNum, na.rm=TRUE)){ #all values are less then min
        breakIndex[1] <- which(breaks > min(val,na.rm=TRUE))[1]
    }
    if(any(val>maxNum, na.rm=TRUE)){
        if(showRange){
            breakLabels[numBreaks] <- paste('[', breakLabels[numBreaks],
                                          'to', signif(trueMax, digits=2) ,']')
        }else{
            breakLabels[numBreaks] <- paste(breakLabels[numBreaks],'+')
        }
        val[val>maxNum] <- maxNum
    }else if(all(val < maxNum, na.rm=TRUE)){ #all values are less then min
        breakIndex[numBreaks] <- rev(which(breaks < max(val,na.rm=TRUE)))[1]
    }

    if(debugging){
        cat('trueMin: [', trueMin, '], minNum: [', minNum,']\n')
        cat('breakIndex: [', breakIndex, ']\n')
        cat('breakLabels: [', breakLabels, ']\n')
        cat('lables: [', breakLabels[breakIndex],']\n')
        cat('labelPos: [', labelPos[breakIndex],']\n')
    }


    ################################################################
    ##Set the parameter list
    ################################################################
    par(las=1)
    if(!is.null(parList)){par(parList)}

    #################################################################
    ##Plot the figures
    #################################################################
    numlon <- length(lon)
    half.numlon <- ceiling(length(lon)*0.527) #calculated to yeild a division at 190 for a 360 grid

    ##split at the Pacific ocean
    shiftIndex <- c(half.numlon:numlon, 1:(half.numlon-1))


    ##legend.mar set to 12 to give lots of room to the break labels
    if(!simple){
        image.plot(lon-180, lat[lat > -60], val[shiftIndex,lat > -60],
                   main = title,
                   col=col, breaks=breaks,
                   axis.args=list(at=labelPos[breakIndex],
                                  labels=breakLabels[breakIndex]),
                   legend.mar=12,
                   xlab = '', ylab='', yaxt='n', xaxt='n')

    }else{
        image(lon-180, lat[lat > -60], val[shiftIndex, lat > -60],
              main=title,
              col=col, breaks=breaks,
              xlab = '', ylab='', yaxt='n', xaxt='n')

    }
#        points(c(-180, 180), c(0,0), type='l')
    if(axisFlag){
        if(lonAxis) axis(1, at=c(-120, -60, 0, 60, 120),
             labels=c('-120', '-60', '0','60', '120'))
        if(latAxis) axis(2, at=c(-60, -30, 0, 30, 60),
             labels=c('-60', '-30', '0','30', '60'))
    }
    box()
    if(debugging){cat('******world.plot ending************\n')}

}
