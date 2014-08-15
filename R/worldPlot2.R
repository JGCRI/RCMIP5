library(ggplot2)
library(abind)

#' Plot global data.
#' 
#' Plot a quick world map with reasonable coloring.
#'
#' @param x cmip5data A structure returned from loadEnsemble() or loadModel()
#' @param time numeric. Which time slice(s) should we plot?
#' @param splitPacific logical. Try to split image in the Pacific?
#' @param capMinMax logical. Cap data min and max by quantile? This may produce better coloring.
#' @param verbose logical. Print info as we go?
#' @details Uses ggplot's \code{\link{geom_raster}}.
#' @export
worldPlot2 <- function(x, time=1, splitPacific=TRUE, capMinMax=TRUE, verbose=TRUE) {
    
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.numeric(time)) 
    stopifnot(is.logical(capMinMax) & length(capMinMax)==1)
    stopifnot(is.logical(verbose) & length(verbose)==1)
    if(length(time) > 16) time <- time[1:16]  # can't see anything smaller...
    
    # Preliminaries
    lon <- x$lon
    lat <- x$lat
    val <- x$val
    ndims <- length(dim(val))
    if(ndims > 2) {
        stopifnot(time > 0 & time <= length(x$time))
        if(verbose) cat("Stripping extra dimensions and isolating time\n")
        idxlist <- ifelse(ndims > 3, 
                          list(rep(1, ndims-3), time=c(time)), # only 1st lev/depth if present
                          list(time=c(time))) # time values as specified by caller
        val <- asub(val, idx=idxlist, dims=3:ndims)
        if(verbose) print(dim(val))
    }
    val[!is.finite(val)] <- NA           # clean up
    
    # Split at the Pacific ocean
    if(splitPacific) {
        if(verbose) cat("Splitting Pacific\n")
        half.numlon <- ceiling(length(lon) * 0.527) # yields a division at 190 for a 360 grid    
        shiftIndex <- c(half.numlon:length(lon), 1:(half.numlon-1))
        if(length(time) == 1) {
            val <- val[shiftIndex, lat > -60] 
        } else {
            ans <- list()
            for(i in time) {
                ans[[i]] <- asub(val, idx=i == time, dims=3)[shiftIndex, lat > -60]
            }
            val <- abind(ans, along=3)            
        }
    }
    
    if(capMinMax) {
        if(verbose) cat('Setting quantile-derived min/max\n')
        quant <- quantile(val, na.rm=TRUE)
        minNum <- quant[[3]]-10*(quant[[3]]-quant[[2]])
        maxNum <- quant[[3]]+10*(quant[[4]]-quant[[3]])
        minNum <- max(minNum, quant[[1]])
        maxNum <- min(maxNum, quant[[5]])
        val[val<minNum] <- minNum
        val[val>maxNum] <- maxNum        
    }
    
    # Transform data, as ggplot2 uses data frames
    if(verbose) "Melting to data frame...\n"
    val_df <- melt(val)
    val_df$lon <- x$lon[val_df$Var1]
    val_df$lat <- x$lat[val_df$Var2]
    if(length(time) > 1) {
        val_df$time <- x$time[time[val_df$Var3]]
    } else {
        val_df$time <- x$time[time]
    }
    
    # Plot
    p <- ggplot(val_df, aes(lon, lat))
    p <- p + geom_raster(aes(fill=value))
    p <- p + scale_x_continuous(expand=c(0,0))
    p <- p + scale_y_continuous(expand=c(0,0))
    p <- p + scale_fill_gradientn(colours=rainbow(4))
    p <- p + facet_wrap(~time, nrow=floor(sqrt(length(time))), ncol=ceiling(sqrt(length(time))))
    p + ggtitle(paste0(x$model, " ", x$experiment, " ", x$variable, " (", x$valUnit, ")"))
} # worldPlot2
