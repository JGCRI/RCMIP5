RCMIP5
=======

This package provides R functions for exploring, manipulating, and summarizing model outputs from the Coupled Model Intercomparison Project Phase 5 (CMIP5).

Working with CMIP5 data can be tricky, forcing scientists to write custom scripts and programs. The `RCMIP5` package aims to ease this process, providing a standard, robust, and high-performance set of functions to (i) explore what data have been downloaded, (ii) identify missing data, (iii) average (or apply other mathematical operations) across experimental ensembles, (iv) produce both temporal and spatial statistical summaries, (v) regrid data, and (vi) produce easy-to-work-with graphical and data summaries. 

More information about the CMIP5 experiment can be found on the project home page http://cmip-pcmdi.llnl.gov/cmip5/, as well as in: Taylor, K. E., Stouffer, R. J., and Meehl, G. A., 2012: An overview of CMIP5 and the experiment design, *Bulletin of the American Meteorological Society*, 93, 485-498. http://dx.doi.org/10.1175/BAMS-D-11-00094.1.

**A few important notes:**

* This package does *not* handle downloading (i.e. from the Earth System Grid Federation, http://pcmdi9.llnl.gov/esgf-web-fe/) the data themselves.

* See http://cmip.llnl.gov/cmip5/publications/allpublications about registering CMIP5 manuscripts.

* If you use this package/code in your work, please cite it! See `citation("RCMIP5")`.

* Want to get started? See the package vignettes or demo, `demo(RCMIP5)`.


