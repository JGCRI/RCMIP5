# Sample script that uses dummy data to exercise many of the RCMIP5 functions

print("This is a sample script that generates dummy data and demonstrates")
print("some of the functionality of the RCMIP5 package.")
print("")
print("This script does not demonstrate the functionality of getFileInfo(),")
print("checkTimePeriod(), or loadCMIP5(), all of which require")
print("existing CMIP5 netcdf files.")
readline("<return>")

# Create random data
print("Creating historical data")
historical <- cmip5data(2000:2005, randomize=T)
print(historical)

print("Creating future data")
future <- cmip5data(2006:2010, randomize=T)
print(future)
readline("<return>")

print("Merging datasets")
combined <- mergeExperiments(historical, future, verbose=T)
print(combined)
readline("<return>")

print("Limiting to certain latitudes and longitudes")
combined <- filterDimensions(combined, lons=50:200, lats=-30:30, verbose=T)
print(combined)
readline("<return>")

print("Computing annual mean. For faster processing, run with 'parallel=TRUE'")
print(makeAnnualStat(combined, verbose=T))
readline("<return>")

print("Computing annual sd")
print(makeAnnualStat(combined, FUN=sd, verbose=T))
readline("<return>")

print("Computing global area-weighted mean")
globalmean <- makeGlobalStat(combined, verbose=T)
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