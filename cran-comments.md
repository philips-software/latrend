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
