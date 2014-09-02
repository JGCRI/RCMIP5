# Sample script that uses dummy data to exercise many of the RCMIP5 functions

print("This is a sample script that generates dummy data and demonstrates")
print("some of the functionality of the RCMIP5 package.")
print("")
print("This script does not demonstrate the functionality of getFileInfo(),")
print("checkTimePeriod(), or loadCMIP5(), all of which require")
print("existing CMIP5 netcdf files.")
readline("<return>")

# Create random data
print("First we create two sample data sets to work with. One is 'historical',")
print("2001-2005, and the other 'future', 2006-2010.")
print("Creating historical data")
historical <- cmip5data(2001:2005, randomize=T)
print(historical)
print("Creating future data")
future <- cmip5data(2006:2010, randomize=T)
print(future)
print("")
print("Printing a data set displays a one-line summary, showing the variable,")
print("model, experiment, years, data matrix dimensions  (in this case 10")
print("longitude cells by 10 latitude cells by 60 months) and number of ensembles.")
readline("<return>")

print("Merging datasets")
combined <- mergeExperiments(historical, future, verbose=T)
combined$experiment <- "combined"
print(combined)
print("")
print("Now we have a single data set 'combined', 120 months long.")
readline("<return>")

print("Limiting to the tropics")
combined <- filterDimensions(combined, lats=-30:30, verbose=T)
print(combined)
print("")
print("We've filtered these data to just the tropics, -30 to 30 degrees latitude.")
readline("<return>")

print("Computing annual mean. For faster processing, run with 'parallel=TRUE'")
print(makeAnnualStat(combined, verbose=T))
print("")
print("After this operation that data have been reduced 120 months to 10 annual means.")
print("Note the 'annual summary' note when printing.")
print("The makeAnnualStat function can apply any summary function, not just mean.")
readline("<return>")

print("Computing global area-weighted mean:")
globalmean <- makeGlobalStat(combined, verbose=T)
print(globalmean)
print("")
print("This computed a 'global' (but here tropical, since we previously filtered the")
print("data to just those latitudes) mean. Note printing the resulting data set confirms")
print("that the data dimensions are 1 x 1 x 120 months.")
readline("<return>")

print("More detailed look at the resulting dataset:")
print(summary(globalmean))
readline("<return>")

print("Each step is tracked in the data's provenance, which includes a timestamp,")
print("operation performed, parameters, message, and data dimensions and MD5 hash.")
print("For example:" )
print(globalmean$provenance[c("timestamp","message")])
readline("<return>")

print("Visualization of the first 12 months of data:")
print(worldPlot2(combined, time=1:12))
readline("<return>")

print("It's easy to convert 'cmip5data' structures to data frames:")
print(head(as.data.frame(combined)))
readline("<return>")

print("...or save them as netcdf files. (Not run.)")

print("All done!")