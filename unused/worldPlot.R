#' Plot global data
#' 
#' Plot a quick world map with reasonable coloring.
#'
#' @param x A \code{\link{cmip5data}} object
#' @param time numeric. Which time slice(s) should we plot?
#' @param splitPacific logical. Try to split image in the Pacific?
#' @param capMinMax logical. Cap data min and max by quantile? This may produce better coloring.
#' @param verbose logical. Print info as we go?
#' @return A ggplot object.
#' @details Uses \code{ggplot2::geom_raster}.
#' @examples
#' d <- cmip5data(1970:1975)   # sample data
#' worldPlot(d, time=1:12)
#' @keywords internal
worldPlot <- function(x, time=1, splitPacific=TRUE, capMinMax=TRUE, verbose=FALSE) {
 
    # Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.numeric(time)) 
    stopifnot(is.logical(capMinMax) & length(capMinMax)==1)
    stopifnot(is.logical(verbose) & length(verbose)==1)
    length(time) <- min(length(time), 16)   # can't see anything smaller...
    stopifnot(require(ggplot2))
    
    # Preliminaries
    lon <- x$lon
    lat <- x$lat
    val <- filter(x$val, Z == min(x$val$Z))  # only use the first lev/depth
    
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
    if(verbose) cat("Melting to data frame...\n")
    val_df <- melt(val)
    val_df$lon <- x$lon[val_df$Var1]
    val_df$lat <- x$lat[val_df$Var2]
    if(length(time) > 1) {
        val_df$time <- x$time[time[val_df$Var3]]
    } else {
        val_df$time <- x$time[time]
    }
    
    # Plot
    value <- 1  # this is here only to avoid a CRAN warning (no visible binding inside geom_raster)
    p <- ggplot2::ggplot(val_df, ggplot2::aes(lon, lat))
    p <- p + ggplot2::geom_raster(ggplot2::aes(fill=value))
    p <- p + ggplot2::scale_x_continuous(expand=c(0,0))
    p <- p + ggplot2::scale_y_continuous(expand=c(0,0))
    p <- p + ggplot2::scale_fill_gradientn(colours=rainbow(4))
    p <- p + ggplot2::facet_wrap(~time, nrow=floor(sqrt(length(time))), ncol=ceiling(sqrt(length(time))))
    p + ggplot2::ggtitle(paste0(x$model, " ", x$experiment, " ", x$variable, " (", x$valUnit, ")"))
} # worldPlot2
