RCMIP5: Tools for Manipulating and Summarizing CMIP5 Data
=======

This package provides R functions for exploring, manipulating, and summarizing model outputs from the Coupled Model Intercomparison Project Phase 5 (CMIP5).

Working with CMIP5 data can be tricky, forcing scientists to write custom scripts and programs. The `RCMIP5` package aims to ease this process, providing a reproducible, robust, and high-performance set of functions to (i) explore what data have been downloaded, (ii) identify missing data, (iii) average (or apply other mathematical operations) across experimental ensembles, (iv) produce both temporal and spatial statistical summaries, and (v) produce easy-to-work-with graphical and data summaries. 

More information about CMIP5 can be found on the project home page http://cmip-pcmdi.llnl.gov/cmip5/, as well as in Taylor et al. 2012, "An overview of CMIP5 and the experiment design" in *Bulletin of the American Meteorological Society* 93:485-498, http://dx.doi.org/10.1175/BAMS-D-11-00094.1.

**Installing this package**

* The RCMIP5 package can be installed from [CRAN](http://cran.r-project.org/package=RCMIP5) or directly from this repository, using the devtools `install_github` command.

**Bug reports**

* CMIP5 data are highly variable in their structure and assumptions, and there are undoubtedly cases we haven't encountered or anticipated. If you find a bug (something unexpected happens or the code crashes) we want to know about it!
* Please either [open an issue](https://github.com/JGCRI/RCMIP5/issues/new), or email one of the maintainers.
* In either case, give us a *reproducible example*: tell us (i) what file(s) you were trying to process, (ii) what sequence of operations led to the problem, and (iii) any other pertinent information.

**Other important notes**

* This package does *not* handle downloading (i.e. from nodes in the Earth System Grid Federation, http://esgf.org) the NetCDF data themselves. Sorry.
* See http://cmip.llnl.gov/cmip5/publications/allpublications about registering CMIP5 manuscripts.
* If you use this package/code in your work, please cite it! See `citation("RCMIP5")`.
* Behind the scenes, RCMIP5 uses the [dplyr](http://cran.r-project.org/package=dplyr) package for heavy data lifting.
* Want to get started? An extensive vignette and demo are included with the package.

