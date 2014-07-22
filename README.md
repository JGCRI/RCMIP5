CMIP5_R
=======

This package provides general R scripts for processing CMIP5 outputs downloaded from the Earth System Grid Federation http://pcmdi9.llnl.gov/esgf-web-fe/.

TODO: Insert summary and citations for CMIP5

TODO: Register this package/manuscript at http://cmip.llnl.gov/cmip5/publications/allpublications

Use-case example can be found in *devMain.R*

##Basic read/write
1. *loadEnsemble.R*
	-Loads a single specified ensemble run.
	-example: ```R
	output <- loadEnsemble(experiment='historical', variable='cSoil', model='GISS-E2-R', ensemble='r1i1p1')
	```

2. *loadModel.R*
	-Averages the ensembles for a specified model-varaible-experiment.
	-example: ```R
	output <- loadModel(experiment='historical', variable='cSoil', model='CanESM2')
	```
	
##Basic data processing
1. *makeAnnualMean.R*
	-Construct the annual means.
2. *makeSeasonalMean.R*
	-Merge specified years to create a multi-year mean.

##File management
1. *getFileInfo.R*
	-Pull all .nc files and extract information based on CMIP5 file naming conventions.
2. *checkTimePeriod.R*
	-Check that all the start-end dates match up for multi-file ensembles. Example:
    
    ```R
	checkTimePeriod(getFileInfo())
	```

#Use case example taken from devMain.R