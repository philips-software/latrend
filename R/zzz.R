#' @import data.table
#' @import assertthat
#' @import methods
#' @import magrittr
#' @importFrom foreach foreach %do%
#' @import ggplot2
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
    latrend.response = 'Value',
    latrend.time = 'Time',
    latrend.id = 'Id',
    latrend.clusterNames = LETTERS,
    latrend.printOptions = FALSE,
    latrend.printSharedModelArgs = FALSE
  )

  optMask = !(names(opts) %in% names(options()))
  if (any(optMask)) {
    options(opts[optMask])

    if (getOption('latrend.printOptions', default = FALSE)) {
      packageStartupMessage('Default options:')
      packageStartupMessage(paste0('\t', names(opts[optMask]), ' = ', opts[optMask], collapse =
                                     '\n'))
    }
  }
}
