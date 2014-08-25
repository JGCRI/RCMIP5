#' Filter dimensions, limiting to arbitrary lon/lat/time/lev/depth.
#'
#' @param x cmip5data A \code{\link{cmip5data}} object.
#' @param lons numeric vector. Longitudes to filter (in).
#' @param lats numeric vector. Latitudes to filter (in).
#' @param depths numeric vector. Depths to filter.
#' @param levs numeric vector
#' @param years numeric vector
#' @param months numeric vector
#' @param verbose logical. Print info as we go?
#' @return A \code{\link{cmip5data}} object.
#' @export
filterDimensions <- function(x, lons=NULL, lats=NULL, depths=NULL, levs=NULL,
                             years=NULL, months=NULL, verbose=FALSE) {

    ## Sanity checks
    stopifnot(class(x)=="cmip5data")
    stopifnot(is.null(lons) | class(lons)=="numeric")
    stopifnot(is.null(lats) | class(lats)=="numeric")
    stopifnot(is.null(depths) | class(depths)=="numeric")
    stopifnot(is.null(levs) | class(levs)=="numeric")
    stopifnot(is.null(years) | class(years)=="numeric" | class(years) == 'integer')
    stopifnot(is.null(months) | class(months)=="numeric")
    stopifnot(length(verbose)==1 & is.logical(verbose))

    # KTB do we need to copy this?
    ans <- x
    # Check to see if there is a constraint on the years we want to pull
    if(!is.null(years)){
        if(length(dim(x$val)) == 3){
            ans$val <- ans$val[,,floor(ans$time) %in% years]
        }else if(length(dim(x$val)) == 4){
            ans$val <- ans$val[,,,floor(ans$time) %in% years]
        }else{
            # do nothing since, ordering of ans$val dimentions is
            # ...[lon, lat, ?depth/lev, time] so anything other then 3 or 4 is
            # ...not valid
            warning("Years specified for time invarient value.")
            break #skip out on the rest of the if statement
        }
        ans$time <- ans$time[floor(ans$time) %in% years]
        ans$provenance <- addProvenance(ans$provenance,
                                        paste("Filtering for years in range [",
                                              paste(range(years),
                                                    collapse=', '), "]"))
    }

    # TODO: Do we want a function that does this? It's going to be very common
    # for users to only care about surface CO2, for example, and they don't want
    # makeAnnualMean() to run computations on all levels.

    # TODO: ask 'mask' (spatial lon/lat mask) above?

    # TODO: Should variables above be bounds, e.g. to filter to western hemisphere
    #   lons=c(-180, 0)
    # or straight numbers
    #   lons=c(-180, -175, ..., 0)
    # ? The first is simpler, while the second requires more work from the user but
    # gives more flexibility. I'm inclined to go with simple.



    return(ans)
} # filterDimensions
