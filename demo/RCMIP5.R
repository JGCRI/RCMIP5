# Sample script that uses dummy data to exercise many of the RCMIP5 functions

## This is a sample script that generates dummy data and demonstrates
## some of the functionality of the RCMIP5 package.
## 
## This script does not demonstrate the functionality of getFileInfo(),
## checkTimePeriod(), or loadCMIP5(), all of which require
## existing CMIP5 netcdf files.
readline("<return>")

# Create random data
## First we create two sample data sets to work with. One is 'historical',
## 2001-2005, and the other 'future', 2006-2010.
## Creating historical data
historical <- cmip5data(2001:2005, randomize=T)
print(historical)
## Creating future data
future <- cmip5data(2006:2010, randomize=T)
print(future)
## 
## Printing a data set displays a one-line summary, showing the variable,
## model, experiment, years, data matrix dimensions  (in this case 10
## longitude cells by 10 latitude cells by 60 months) and number of ensembles.
readline("<return>")

## Merging datasets
combined <- mergeExperiments(historical, future, verbose=T)
combined$experiment <- "combined"
print(combined)
## 
## Now we have a single data set 'combined', 120 months long.
readline("<return>")

## Limiting to the tropics
combined <- filterDimensions(combined, lats=-30:30, verbose=T)
print(combined)
## 
## We've filtered these data to just the tropics, -30 to 30 degrees latitude.
readline("<return>")

## Computing annual mean. For faster processing, run with 'parallel=TRUE'
print(makeAnnualStat(combined, verbose=T))
## 
## After this operation that data have been reduced 120 months to 10 annual means.
## Note the 'annual summary' note when printing.
## The makeAnnualStat function can apply any summary function, not just mean.
readline("<return>")

## Computing global area-weighted mean:
globalmean <- makeGlobalStat(combined, verbose=T)
print(globalmean)
## 
## This computed a 'global' (but here tropical, since we previously filtered the
## data to just those latitudes) mean. Note printing the resulting data set confirms
## that the data dimensions are 1 x 1 x 120 months.
readline("<return>")

## More detailed look at the resulting dataset:
print(summary(globalmean))
readline("<return>")

## Each step is tracked in the data's provenance, which includes a timestamp,
## operation performed, parameters, message, and data dimensions and MD5 hash.
## For example:" )
print(globalmean$provenance[c("timestamp","message")])
readline("<return>")

## Visualization of the first 12 months of data:
print(worldPlot2(combined, time=1:12))
readline("<return>")

## It's easy to convert 'cmip5data' structures to data frames:
print(head(as.data.frame(combined)))
readline("<return>")

## ...or save them as netcdf files. (Not run.)

## All done!