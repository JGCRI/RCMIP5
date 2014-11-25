## Sample script that uses dummy data to exercise many of the RCMIP5 functions

## This is a sample script that generates dummy data and demonstrates
## some of the functionality of the RCMIP5 package.
## 
## This script does not demonstrate the functionality of getFileInfo(),
## checkTimePeriod(), or loadCMIP5(), all of which require
## existing CMIP5 NetCDF files.
readline("<return>")

## Create random (dummy) data
historical <- cmip5data(2001:2005, randomize=T)
print(historical)

## Printing a monthly data set displays a one-line summary, showing 
## the variable, model, experiment, years, data matrix dimensions 
## (here 10 longitude cells by 10 latitude cells by 0 Z levels,
## over 5 years or 60 months) and number of ensembles.
readline("<return>")

## Limiting to the tropics
historical <- filterDimensions(historical, lats=-30:30, verbose=T)
print(historical)

## We've filtered these data to just the tropics, -30 to 30 latitude.
readline("<return>")

## Compute annual mean
print(makeAnnualStat(historical, verbose=T))

## After this operation the data have been reduced from 60 months to 
## 5 annual means. Note the makeAnnualStat (and all the make...Stat 
## functions) can apply any summary function, not just mean.
readline("<return>")

## Computing global area-weighted mean:
globalmean <- makeGlobalStat(historical, verbose=T)
print(globalmean)

## This computed a 'global' (but here tropical, since we 
## previously filtered the data to just those latitudes) mean.
## Printing the resulting data set confirms that there's no 
## spatial dimensions left, only time.
##
## Note that because we didn't supply area data to makeGlobalStat,
## it made its own estimation of grid cell areas.
readline("<return>")

## More detailed look at the resulting dataset:
print(summary(globalmean))
readline("<return>")

## Each step is tracked in the data's provenance, which includes
## a timestamp, operation performed, parameters, message, and 
## data dimensions and MD5 hash. For example:
print(globalmean$provenance[c("timestamp","message")])
readline("<return>")

## It's easy to convert 'cmip5data' structures to arrays or data frames:
print(head(as.data.frame(historical)))
readline("<return>")

## ...or save them as NetCDF files. (Not run.)

## All done!
