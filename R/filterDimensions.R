#' Filter dimensions, limiting to arbitrary lon/lat/Z/time ranges
#'
#' We frequently want to filter CMIP5 data according to some predetermined
#' criteria: only high-latitude cells, for example, or certain years, months,
#' Zs, etc. This function provides convenient one-stop service
#' for such tasks.
#' 
#' @param x A \code{\link{cmip5data}} object
#' @param lonRange Longitude values (min, max) to filter data against
#' @param latRange Latitude values (min, max) to filter data against
#' @param ZRange Z values (min, max) to filter data against
#' @param yearRange Years (min, max) to filter data against
#' @param monthRange Months (min, max) to filter data against
#' @param verbose logical. Print info as we go?
#' @return The filtered \code{\link{cmip5data}} object.
#' @note If a filter is requested but no relevant data are present,
#' a \code{\link{warning}} will be produced.
#' @examples
#' d <- cmip5data(1970:2014)   # sample data
#' filterDimensions(d, yearRange=c(1980, 1985))
#' filterDimensions(d, monthRange=c(6, 8))  # summer
#' filterDimensions(d, latRange=c(-20, 20))  # the tropics
#' filterDimensions(d, latRange=c(-20, 20), monthRange=c(6, 8))  # tropical summer
#' @export
filterDimensions <- function(x, lonRange=NULL, latRange=NULL, ZRange=NULL,
                             yearRange=NULL, monthRange=NULL, verbose=FALSE) {
    
    # Sanity checks
    assert_that(class(x)=="cmip5data")
    assert_that(is.flag(verbose))
    # Other parameters checked in their respective functions
    
    x <- filterDimensionLon(x, lonRange, verbose)
    x <- filterDimensionLat(x, latRange, verbose)
    x <- filterDimensionZ(x, ZRange, verbose)
    x <- filterDimensionTimeYears(x, yearRange, verbose)
    x <- filterDimensionTimeMonths(x, monthRange, verbose)
    
    x
} # filterDimensions

#' Filter longitude dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param lonRange Longitude values (min, max) to filter data against
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
#' @keywords internal
filterDimensionLon <- function(x, lonRange=NULL, verbose=FALSE) {
    
    if(!is.null(lonRange)) {
        
        # Sanity checks
        assert_that(is.numeric(lonRange) & length(lonRange) == 2)
        
        # Filter longitude dimension
        if(is.null(x[["lon"]])) {
            warning("No lon data found")
        } else {
            lonRowsInRange <- apply(x$lon, 1, function(x) any(x >= min(lonRange) & x <= max(lonRange)))
            if(is.array(x$val)) { # array code
                # 'punch' NA holes (in case of irregular grid)
                x$val[x$lon < min(lonRange) | x$lon > max(lonRange)] <- NA
                x$val <- x$val[lonRowsInRange,,,, drop=FALSE] # trim grid
            } else if(is.data.frame(x$val)) { # data frame code
                lon <- NULL  # Suppress stupid NOTEs from R CMD CHECK
                x$val <- filter(x$val, lon >= min(lonRange) & lon <= max(lonRange))
            } else 
                stop("Unknown data type")
            
            # Trim any rows (longitude) completely outside of filter range
            x$lon <- x$lon[lonRowsInRange,, drop=FALSE]
            x$lat <- x$lat[lonRowsInRange,, drop=FALSE]
            x <- addProvenance(x, paste("Filtered for lons in range [",
                                        paste(lonRange, collapse=', '), "]"))
            x$filtered <- TRUE
            if(verbose) cat("Filtered by lon\n")
        }
    }
    x
} # filterDimensionLon

#' Filter latitude dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param latRange Latitude values (min, max) to filter data against
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
#' @keywords internal
filterDimensionLat <- function(x, latRange=NULL, verbose=FALSE) {
    
    if(!is.null(latRange)) {
        
        # Sanity checks
        assert_that(is.numeric(latRange) & length(latRange) == 2)
        
        # Filter latitude dimension
        if(is.null(x[["lat"]])) {
            warning("No lat data found")
        } else {
            latColsInRange <- apply(x$lat, 2, function(x) any(x >= min(latRange) & x <= max(latRange)))
            if(is.array(x$val)) { # array code
                # 'punch' NA holes (in case of irregular grid)
                x$val[x$lat < min(latRange) | x$lat > max(latRange)] <- NA
                x$val <- x$val[,latColsInRange,,, drop=FALSE] # trim grid
            } else if(is.data.frame(x$val)) { # data frame code
                lat <- NULL  # Suppress stupid NOTEs from R CMD CHECK
                x$val <- filter(x$val, lat >= min(latRange) & lat <= max(latRange))
            } else 
                stop("Unknown data type")
            
            # Trim any columns (latitude) completely outside of filter range
            x$lon <- x$lon[,latColsInRange, drop=FALSE]
            x$lat <- x$lat[,latColsInRange, drop=FALSE]
            x <- addProvenance(x, paste("Filtered for lats in range [",
                                        paste(latRange, collapse=', '), "]"))
            x$filtered <- TRUE
            if(verbose) cat("Filtered by lat\n")
        }
    }
    x
} # filterDimensionLat

#' Filter Z dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param ZRange Z values (min, max) to filter data against
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
#' @keywords internal
filterDimensionZ <- function(x, ZRange=NULL, verbose=FALSE) {
    
    if(!is.null(ZRange)) {
        
        # Sanity checks
        assert_that(is.numeric(ZRange) & length(ZRange) == 2)
        
        if(is.null(x[["Z"]])) {
            warning("No Z data found")
        } else {
            ZsInRange <- which(x$Z >= min(ZRange) & x$Z <= max(ZRange))
            if(is.array(x$val)) { # array code
                x$val <- x$val[,,ZsInRange,]
            } else if(is.data.frame(x$val)) { # data frame code
                Z <- NULL  # Suppress stupid NOTEs from R CMD CHECK
                x$val <- filter(x$val, Z >= min(ZRange) & Z <= max(ZRange))
            } else 
                stop("Unknown data type")
            
            x$Z <- x$Z[ZsInRange]
            x <- addProvenance(x, paste("Filtered for Zs in range [",
                                        paste(ZRange, collapse=', '), "]"))
            x$filtered <- TRUE
            if(verbose) cat("Filtered by Z\n")
        }
    }
    x
} # filterDimensionZ

#' Filter time (years) dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param yearRange Years (min, max) to filter data against
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
#' @keywords internal
filterDimensionTimeYears <- function(x, yearRange=NULL, verbose=FALSE) {
    
    if(!is.null(yearRange)) {
        
        # Sanity checks
        assert_that(is.numeric(yearRange) & length(yearRange) == 2)
        yearRange <- floor(yearRange)
        
        if(is.null(x[["time"]])) {
            warning("No time data found")
        } else {
            yearsInRange <- floor(x$time) >= min(yearRange) & floor(x$time) <= max(yearRange)
            if(is.array(x$val)) { # array code
                x$val <- x$val[,,,yearsInRange, drop = FALSE]
            } else if(is.data.frame(x$val)) { # data frame code
                time <- NULL  # Suppress stupid NOTEs from R CMD CHECK
                x$val <- filter(x$val, floor(time) >= min(yearRange) & floor(time) <= max(yearRange))
            } else                 
                stop("Unknown data type")
            
            x$time <- x$time[yearsInRange]
            x <- addProvenance(x, paste("Filtered for years in range [",
                                        paste(yearRange, collapse=', '), "]"))
            x$filtered <- TRUE
            if(verbose) cat("Filtered by year\n")
        }
    }
    x
} # filterDimensionTimeYears

#' Filter time (months) dimension.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param monthRange Months (min, max) to filter data against
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @note This is an internal RCMIP5 function and not exported.
#' @keywords internal
filterDimensionTimeMonths <- function(x, monthRange=NULL, verbose=FALSE) {
    
    if(!is.null(monthRange)) {
        
        # Sanity checks
        assert_that(is.numeric(monthRange) & length(monthRange) == 2)
        assert_that(all(monthRange %in% 1:12))
        
        if(is.null(x[["time"]])) {
            warning("No time data found")
        } else if(x$debug$timeFreqStr != "mon") {
            warning("A monthly filter can only be applied to monthly data")            
        }
        else {
            fracmonths <- round((monthRange-0.5) / 12, 2) # From Jan=1, Feb=2 to Jan 15=0.042, Feb15=0.123, etc.
            monthfilter <- round(x$time %% 1, 2) >= min(fracmonths) & round(x$time %% 1, 2) <= max(fracmonths)
            
            if(is.array(x$val)) { # array code
                x$val <- x$val[,,,monthfilter, drop = FALSE]
            } else if(is.data.frame(x$val)) { # data frame code
                time <- NULL  # Suppress stupid NOTEs from R CMD CHECK
                x$val <- filter(x$val, round(time %% 1, 2) %in% fracmonths)
            } else                 
                stop("Unknown data type")
            
            x$time <- x$time[monthfilter]
            x <- addProvenance(x, paste("Filtered for months in range [",
                                        paste(monthRange, collapse=', '), "]"))
            x$filtered <- TRUE
            if(verbose) cat("Filtered by month\n")
        }
    }
    x
} # filterDimensionTimeMonths
