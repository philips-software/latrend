#' @import data.table
#' @import assertthat
#' @import methods
#' @import magrittr
#' @import stats
#' @importFrom foreach foreach %do%
#' @import ggplot2
#' @importFrom utils hasName capture.output combn getS3method modifyList head tail data
#' @section Getting started:
#' * See `vignette("demo", package="latrend")` for an introduction to conducting a longitudinal cluster analysis on a example case study.
#' * See `vignette("customModels", package="latrend")` for examples on constructing your own cluster models.
#' * See `vignette("modelSelection", package="latrend")`
#' * See `vignette("modelValidation", package="latrend")`
#' * See `vignette("simulationStudy", package="latrend")`
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  opts = list(
    latrend.verbose = R.utils::Verbose(threshold = -1),
    latrend.time = 'Time',
    latrend.id = 'Id',
    latrend.clusterNames = LETTERS,
    latrend.printOptions = FALSE,
    latrend.printSharedModelArgs = FALSE
  )

  optMask = !(names(opts) %in% names(options()))
  if (any(optMask)) {
    options(opts[optMask])
  }
}

.loadPackage = function(name) {
  if(!isNamespaceLoaded(name)) {
    if(requireNamespace(name, quiet = TRUE)) {
      ns = loadNamespace(name)
      attachNamespace(ns)
    } else {
      stop('unable to load package "', name , '"')
    }
  }
}
