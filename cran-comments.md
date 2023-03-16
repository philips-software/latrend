# v1.5.1
* Resolved CRAN warnings and notes
* Alternative implementation of ASW and Dunn index due to removal of clusterCrit package from CRAN (#131, #132)
* New PAP.adh dataset (#130)
* Added support to lcmm methods for (parallel) gridsearch (#126)
* More robust implementation of meta methods, allowing argument pass-through (#128)
* Improved package documentation (#127)

# v1.5.0
* Implemented virtual class for meta methods (#61)
* Implemented a meta method for repeatedly fitting a method and selecting the best fit (see `lcFitRep`, `lcFitRepMin`, `lcFitRepMax`) (#61)
* Implemented a meta method for repeatedly fitting a method until convergence (see `lcFitConverged`) (#61)
* Made `getLcMethod()` generic
* Added check for formula argument to LMKM method
* Added "converged" slot to lcModelPartition
* Generic validate() checks for correct output length
* Added workaround for erroneous R CMD check rmarkdown import note

# v1.4.3
* Removed usage of soon-to-be deprecated ggplot functions
* Reduced runtime of example

# v1.4.2
* Fixed minor bug in `evaluate.lcMethod()`
* Resolved lcmm test error
* Shortened runtime of vignettes
* Shortened runtime of tests

# v1.4.1
* Fixed bug in `predict.lcModel()` introduced by #116
* Added check to the `postprob()` method of the `lcModelKML` class.
* Fixed Rd ampersand NOTE
* Ensured compatibility with lcmm 2.0.0

# v1.4.0
* Resolved CRAN error
* Resolved warnings from `latrendBoot()` and `latrendCV()`
* Removed redundant `lcModelCustom` in favor of `lcModelPartition`
* Renamed `lcMethodCustom` to `lcMethodFunction` for clarity
* Added RMSE and WRMSE metrics
* Improved kml package compatibility; fixed errors on datasets with variable-length short trajectories
* Improved some error messages
* `plotMetric()` now shows legend when grouping

## Test environments
* Windows (release, devel, 3.6)
* Ubuntu (release and devel)
* macOS (release)

# v1.3.0
* Removed archived longclust package dependency
* Added testing framework for longitudinal cluster method implementations
* Changed examples, vignettes and tests to pass with `_R_CHECK_DEPENDS_ONLY_ = true`

## Test environments
* Windows (release, devel, 3.6)
* Ubuntu (release and devel)
* macOS (release)

# v1.2.1
* Fewer required imports
* Reduced test and example time
* Added documentation of cluster metrics and external metrics

## Test environments
* Windows (release, devel, 3.6)
* Ubuntu (release and devel)
* macOS (release)

# Resubmission v1.2.0
This is a resubmission. This version is a major update over v1.1. See NEWS.md for a list of changes.

## Test environments
* Windows (release, devel, 3.6)
* Ubuntu (release and devel)
* macOS (release)
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The 1 NOTE is related to a parallel computation example which needs sufficient time to run in order to be noticeable on Windows.

The CRAN pre-check finds an additional NOTE:
```
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/met0000048
    From: man/lcMethodMixTVEM.Rd
    Status: 400
    Message: Bad Request
```
The issue may originate from a http redirect. The DOI link works fine on my pc and Rhub.

# Resubmission v1.1.4
# Submission v1.0
