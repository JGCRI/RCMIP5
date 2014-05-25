# Script to process ESG files into CSV annual summary format
# Ben B.-L. and Corinne H.
# August 2013
# combines both land and ocean data into one script with latitude and longitude bands

# To use:
#	1. Set CELL_AREA_VAR and CELL_AREA_DIR variables below
#	2. Call process_directory or process_file as appropriate

# TODO: if there's an error and we die halfway through, or if a new file is added
# to a directory, don't want to have to re-run entire directory; instead combine with old data

DATA_DIR            <- "sampledata/"
OUTPUT_DIR			<- "outputs/"
LOG_DIR				<- "logs/"

DATAFREQ_MONTHLY    <- "monthly"
DATAFREQ_ANNUAL     <- "annual"
LEVEL_DEPTH     	<- 100 #20000 # from 10000000 to 20000 the upper limits of the troposphere.
LEVEL_NAME      	<- "lev" # change to lev for ocean data or plev for atmo data
LAT_NAME      		<- "lat"
LAT_NAME2       	<- "j"
LAT_NAME3        	<- "rlat"
LAT_NAME4        	<- "x"
LON_NAME      		<- "lon"
LON_NAME2       	<- "i"
LON_NAME3        	<- "rlon"
LON_NAME4        	<- "y"
TIME_NAME       	<- "time"
CELL_AREA_VAR    	<- "areacella"  # change this for land variable or ocean variable
CELL_AREA_DIR    	<- "gridareas/"  #change this for land variable or ocean variable
#CELL_AREA_DIR    	<- "~/Desktop/"  # change this for land variable or ocean variable
MONTHS_PER_YEAR   	<- 12
LATITUDE_SUMMARY  	<- 5    # Set to >90 for N/S hemispheres; 0 for global avg
LONGITUDE_SUMMARY 	<- 0  # zero for no longitude bands
SKIPFILE			<- paste0( OUTPUT_DIR, "skipfile.csv" )
RESULTS_FILE        <- "results.csv"

# Support functions and common definitions

SCRIPTNAME		<- "esg_data.R"
SEPARATOR		<- "-------------------"


# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function( msg, ..., ts=TRUE, cr=TRUE ) {
	if( ts ) cat( date(), " " )
	cat( msg, ... )
	if( cr ) cat( "\n")
}

# -----------------------------------------------------------------------------
# Write error to file that logs skips
skiplog <- function( fname="", reason="", newfile=FALSE, skipped=T ) {
	if( newfile | !file.exists(SKIPFILE)) 
		cat( "Date,File,Skipped,Reason\n", file=SKIPFILE )
	if(fname!= "" | reason != "")
		cat( date(), fname, skipped, reason, "\n", file=SKIPFILE, sep=",", append=T )
}

# -----------------------------------------------------------------------------
# Get names of dimensions for a particular variable
read_dimension_names <- function( ncid, variable ) {
	dnames <- c()
	for( i in 1:ncid$nvars )
		if( ncid$var[[ i ]]$name == variable ) {
			for( j in 1:ncid$var[[ i ]]$ndims ) {
				dnames[ j ] <- ncid$var[[ i ]]$dim[[ j ]]$name
			}
		return( dnames )  
		}
	stop( "*** Variable", variable, "not found!!" )
}

# -----------------------------------------------------------------------------
# Figure out whether there are levels in the file, and if so, how many we need to use
# to approximately reach a particular depth
determine_levels <- function( ncid, dnames, depth ) {
	levels_to_read <- 0
	if( LEVEL_NAME %in% dnames )
		levels_to_read <- which.min( abs( get.var.ncdf( ncid, LEVEL_NAME )-depth ) )
	printlog( "Levels to read for", LEVEL_DEPTH, "=", levels_to_read )
	return( levels_to_read )
}

# -----------------------------------------------------------------------------
# Read a single time point, averaging over levels if necessary
read_timepoint <- function( ncid, variable, levels_to_avg, startdata, countdata, levindex ) {
  # printlog( "read_timepoint, startdata=", startdata, ", countdata=", countdata, ", variable=", variable, "levindex=", levindex )
  if( levels_to_avg >0 ) {
    dsum <- data.frame()
    for( i in 1:levels_to_avg ) {
      # printlog( "--     reading level", i )
      startdata[ levindex ] <- i
      dtemp <- get.var.ncdf( ncid, variable, start=startdata, count=countdata )
      if( i==1 )
        dsum <- dtemp
      else
        dsum <- dsum + dtemp 
    }
    d <- dsum / levels_to_avg
  } else {
    d <- get.var.ncdf( ncid, variable, start=startdata, count=countdata ) 
  }
  
  return( d )
}

# -----------------------------------------------------------------------------
# Summarise data by lat/lon bands
latlon_summary <- function( d, latlon, m_area ) {
    # At this point we have the global grid (perhaps averaged over levels,
    # perhaps averaged over months). Area-weight it and stick in results
    d_m <- melt( d )
    d_m$lat <- latlon$Var2
    d_m$lon <- latlon$Var1
    d_m$area <- m_area$value
    
    # Summarize by latitude bands (arbitrary increments)
    latsign <- sign( d_m$lat )
    latsign[ latsign==0 ] <- 1
    if( LATITUDE_SUMMARY <= 0 )
        d_m$latband = 0   # i.e., none, produce global avg
    else
        d_m$latband <- ( floor( abs( d_m$lat ) / LATITUDE_SUMMARY ) * LATITUDE_SUMMARY +
                             LATITUDE_SUMMARY/2 ) * latsign
    
    # Summarize by longitude bands (arbitrary increments)
    lonsign <- sign( d_m$lon )
    lonsign[ lonsign==0 ] <- 1
    if( LONGITUDE_SUMMARY <= 0 )
        d_m$lonband = 0   # i.e., none, produce global avg
    else
        d_m$lonband <- ( floor( abs( d_m$lon ) / LONGITUDE_SUMMARY ) * LONGITUDE_SUMMARY + 
                             LONGITUDE_SUMMARY/2 ) * lonsign
    
#    res <- ddply( d_m, .( latband, lonband ), summarise, 
#                  value=weighted.mean( value, area, na.rm=T ), area=sum( area, na.rm=T ), 
#                  value_u=mean( value, na.rm=T ), ncell=length( na.omit( value ) ) ) # TODO: ncell not correct

    # Not sure why this is necessary, but make sure lat and long are normal numerics
    d_m$lat <- as.numeric( d_m$lat )
    d_m$lon <- as.numeric( d_m$lon )
    d_m$latband <- as.numeric( d_m$latband )
    d_m$lonband <- as.numeric( d_m$lonband )
    results_grouped <- group_by( d_m, latband, lonband )
    results_agg <- dplyr::summarise( results_grouped, 
                                     value=weighted.mean( value, area, na.rm=T ),
                                     area=sum( area, na.rm=T ), 
                                     value_u=mean( value, na.rm=T ),
                                     ncell=n() )
    
    
    return( results_agg )
}

# -----------------------------------------------------------------------------
# The workhorse: read a netCDF file, aggregate by lat/lon bands, return data
process_data <- function( fn, variable, model, ensemble, scenario,
						  beginyear, beginmonth, endyear, endmonth, datafreq ) {

	# Get cell area data for this particular model
	printlog ('Opening cell area file for model', model, cellareas[[model]] )
	if( is.null( cellareas[[ model ]] ) ) {
		printlog( "*** Uh oh! Couldn't find cell area data for this model. Skipping ***" )
		#    warning( paste( "Couldn't find cell area file entry", cellareas[[ model ]] ) )
		skiplog( fn, "Couldn't find entry for cell area file" )
		return( NULL )
	}
	if( !file.exists( cellareas[[ model ]] ) ) {
		printlog( "*** Uh oh! Couldn't find cell area data file. Skipping ***" )
		#    warning( paste( "Couldn't find cell area file", cellareas[[ model ]] ) )
		skiplog( fn, "Couldn't find cell area file on disk" )
		return( NULL )
	}
	ncid_a <- open.ncdf( cellareas[[ model ]] )
	area <- get.var.ncdf( ncid_a, CELL_AREA_VAR )  # units m2
	m_area <- melt (area)

	# Set up results data frame
	results <- data.frame()

	printlog ('Opening file', fn )
    if( !file.exists( fn ) ) {
        skiplog( fn, "File doesn't seem to exist anymore" )
        return( NULL )
    }
	ncid <- open.ncdf( fn )
	print( ncid )

	printlog( "Getting lat/lon data" )  
	lat <- get.var.ncdf( ncid, LAT_NAME )
	lon <- get.var.ncdf( ncid, LON_NAME )
	# lat and lon can be returned in two different ways
	if( is.matrix( lat ) ) {
		latlon <- data.frame( Var1=melt( lon )$value, Var2=melt( lat )$value )
	} else {
		latlon <- expand.grid( lon, lat )    
	}  
	if( nrow( latlon ) != nrow( m_area ) ) {
		skiplog( fn, "Area and lat/lon grids aren't the same" )
		return( NULL )
	}

	dnames <- read_dimension_names( ncid, variable )
	printlog( "Dimensions names for", variable, "=", dnames )

	levels_to_avg <- determine_levels( ncid, dnames, LEVEL_DEPTH )

	latindex <- which( dnames==LAT_NAME | dnames==LAT_NAME2 | dnames==LAT_NAME3 | dnames==LAT_NAME4 )
	lonindex <- which( dnames==LON_NAME | dnames==LON_NAME2| dnames==LON_NAME3 | dnames==LON_NAME4)
	levindex <- which( dnames==LEVEL_NAME )
	timeindex <- which( dnames==TIME_NAME )
	printlog( "latindex lonindex levindex timeindex" )
	printlog( latindex, lonindex, levindex, timeindex )
	startdata <- rep( 1, length( dnames ) )
	countdata <- rep( -1, length( dnames ) )
	countdata[ timeindex ] <- 1 # always reading only 1 time slice at a time
	countdata[ levindex ] <- 1  # always reading only 1 level at a time

	year <- beginyear
    month <- beginmonth
    endyearmonth <- endyear + endmonth/12
    timespot <- 1
    results <- data.frame()
    while( ( year + month/12 ) <= endyearmonth ) {
        
        startdata[ timeindex ] <- timespot
        d <- read_timepoint( ncid, variable, levels_to_avg, startdata, countdata, levindex )    
        timespot <- timespot + 1
        
        dsum <- latlon_summary( d, latlon, m_area )
        
        # Add other (unchanging) information to results data frame
        dsum$variable <- variable
        dsum$ensemble <- ensemble
        dsum$scenario <- scenario
        dsum$source <- model
        dsum$units <- att.get.ncdf (ncid, variable,  "units" )$value
        dsum$month <- month
        dsum$year <- year
        
        results <- rbind( results, dsum )
        
        month <- month + 1
        if( month > MONTHS_PER_YEAR ) {
            month <- 1
            year <- year + 1
        }
    }
    	
	close.ncdf(ncid)

	return( results ) 
}

# -----------------------------------------------------------------------------
# Process a single file, parsing information from its name and calling process_data
process_file <- function( fn, tf, skip_existing=TRUE) {
  filedata <- strsplit( basename( fn ), "_" )[[ 1 ]]
  printlog( "------------------------" )
  printlog( "File:", fn )
  variable <- filedata[ 1 ]
  model <- filedata[ 3 ]
  scenario <- filedata[ 4 ]
  ensemble <- filedata[ 5 ]
  #outfn <- paste( fn, ".csv", sep="" )
  
  #if (file.exists( outfn ) & skip_existing ) {
  #  printlog ("Skipping file", fn )
  #  skiplog( fn, "Output filename already exists" )
  #  return()
  #}
  
  printlog( variable, model, scenario, ensemble )
  filename <- strsplit( filedata[ 6 ], ".", fixed=T )[[ 1 ]][ 1 ]
  timedata <- strsplit( filename, "-" )[[ 1 ]]
  begintime <- timedata[ 1 ]
  endtime <- timedata[ 2 ]
  
  if( nchar( begintime ) != nchar( endtime ) | ( nchar( begintime ) != 4 & nchar( begintime ) != 6 ) ) {
    printlog( "*** Uh oh! Something's wrong--date not 4 or 6 digits. Skipping ***" )
    skiplog( fn, "Date not 4 or 6 digits" )
    warning( paste( "Couldn't parse filename", i ) )
    skiplog( fn, "Couldn't parse filename" )
    return( 0 )
  }
  
  beginyear <- as.numeric( substr( begintime, 1, 4 ) )
  endyear <- as.numeric( substr( endtime, 1, 4 ) )
  beginmonth <- 0
  endmonth <- 0
  
  if( nchar( begintime )==6 ) {
    datafreq <- DATAFREQ_MONTHLY
    beginmonth <- as.numeric( substr( begintime, 5, 6 ) )
    endmonth <- as.numeric( substr( endtime, 5, 6 ) )
  } else {
    datafreq <- DATAFREQ_ANNUAL
  }
  
  printlog( "This appears to be", datafreq, "data" )
  printlog( beginyear, beginmonth, endyear, endmonth )
  
  results <- process_data( fn, variable, model, ensemble, scenario, 
  							beginyear, beginmonth, endyear, endmonth, datafreq )
  
  # At this point we write the data out to a tempfile, which grows as we process the directory
  if( !is.null( results ) ) {
#    outfn <- paste( fn, ".csv", sep="" )
    printlog( "Writing data to tempfile..." )
    first <- !file.exists( tf )
    write.table( results, file=tf, sep=",", row.names=F, col.names=first, append=!first )
    skiplog( fn, "OK", skipped=F )
    return( nrow( results ) )
  } else {
    printlog( "NULL results! Not writing any output" )
    skiplog( fn, "process_data returned NULL" )
    return( 0 )
  }
}

# -----------------------------------------------------------------------------
# Process a whole directory of files
process_directory <- function( dir=DATA_DIR, pattern="*.nc$" ) {
  
  
  printlog( "------------------------" )
  printlog( "Welcome to process_directory" )
  files <- list.files( dir, pattern=pattern )
  printlog( length( files ), "files to process" )
  
  resultsfile <-  paste( dir, RESULTS_FILE, sep="/" )
  already_processed <- ""
  if( file.exists( SKIPFILE ) & file.exists( resultsfile ) ) {
    if( readline( "Directory has already been processed. Use existing data and process only skips? " ) %in% c( 'y', "Y" ) ) {
        
        already_processed <- subset( read.csv( SKIPFILE ), Skipped=FALSE )$File
        
    } else {
        skiplog( newfile=T )    # erase the skip log and start a new one
        
    }
      
      
  }
  
  tf <- tempfile()
  printlog( "Using tempfile", tf )
  total_rows <- 1
    
  print( system.time( {
    for( i in files ) {
        if( i %in% already_processed ) {
            printlog( "Looks like", i, "has already neen processed, skipping it" )
            next
        }
        tryCatch( {   # file-reading is tricky; catch any error thrown
            total_rows <- total_rows + process_file( paste( dir, i, sep="/" ), tf ) - 1
        }, error=function( err ) {
            printlog( "ERROR from process_file:", as.character( err ) )
            skiplog( i, "process_file crashed" )
        } 
        )
  }
  } ) )

	if( !file.exists( tf ) ) {
		printlog( "No data processed!" )
		return()
	}
	
  # Now read back in all the data from the tempfile and process it
  printlog( "All done; reading tempfile (", total_rows, ") back in..." )
  alldata <- read.csv( tf, nrows=total_rows )
    printlog( "Rows =", nrow( alldata ) )
  
  results_grouped <- group_by( alldata, year, latband, lonband, scenario, source, ensemble )
  results_agg <- dplyr::summarise( results_grouped, 
                                   value=mean( value, na.rm=T ), 
                                   sd=sd( value, na.rm=T ) )

  printlog( "Data summarised, writing..." )
  write.csv( results_agg, resultsfile, row.names=F )
}


# =============================================================================
# 										MAIN
# =============================================================================

if( !file.exists( OUTPUT_DIR ) ) {
	printlog( "Creating", OUTPUT_DIR )
	dir.create( OUTPUT_DIR )
}
if( !file.exists( LOG_DIR ) ) {
	printlog( "Creating", LOG_DIR )
	dir.create( LOG_DIR )
}

sink( paste0( LOG_DIR, SCRIPTNAME, ".txt" ), split=T )

printlog( "Welcome to", SCRIPTNAME )

library( ncdf )
library( reshape )
library( ggplot2 )
theme_set( theme_bw() )
library( dplyr )

# build a list of all the cell area files
cellareas <- list()
printlog( "Building cell area list in", CELL_AREA_DIR )
files <- list.files( CELL_AREA_DIR, pattern=paste0( CELL_AREA_VAR, "*.*" ) )
for( i in files ) {
	printlog( "Processing", i )
	modelname <- strsplit( i, "_" )[[ 1 ]][ 3 ]
	printlog( "Adding for model", modelname )
	cellareas[ modelname ] <- paste( CELL_AREA_DIR, i, sep="/" )
}

process_directory()
#process_directory( "~/Desktop/files/" )

print( sessionInfo() )
printlog( "All done with", SCRIPTNAME )
sink()
