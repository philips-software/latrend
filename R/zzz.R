#' @import data.table
#' @import assertthat
#' @import methods
#' @import magrittr
#' @importFrom foreach foreach %do%
#' @import ggplot2
#' @section Getting started:
#' * See `vignette("demo", package="cluslong")` for an introduction to conducting a longitudinal cluster analysis on a example case study.
#' * See `vignette("customModels", package="cluslong")` for examples on constructing your own cluster models.
#' * See `vignette("modelSelection", package="cluslong")`
#' * See `vignette("modelValidation", package="cluslong")`
#' * See `vignette("simulationStudy", package="cluslong")`
"_PACKAGE"


.onLoad = function(libname, pkgname) {
  opts = list(
    cluslong.verbose = R.utils::Verbose(threshold = -1),
    cluslong.response = 'Value',
    cluslong.time = 'Time',
    cluslong.id = 'Id',
    cluslong.clusterNames = LETTERS,
    cluslong.printOptions = FALSE,
    cluslong.printSharedModelArgs = FALSE
  )

  optMask = !(names(opts) %in% names(options()))
  if (any(optMask)) {
    options(opts[optMask])

    if (getOption('cluslong.printOptions', default = FALSE)) {
      packageStartupMessage('Default options:')
      packageStartupMessage(paste0('\t', names(opts[optMask]), ' = ', opts[optMask], collapse =
                                     '\n'))
    }
  }
}
