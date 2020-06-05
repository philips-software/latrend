#' @import data.table
#' @import assertthat
#' @import methods
#' @import magrittr
#' @import foreach
#' @import ggplot2

.onLoad = function(libname, pkgname) {
  opts = list(
    cluslong.verbose = R.utils::Verbose(threshold=-1),
    cluslong.response = 'Value',
    cluslong.time = 'Time',
    cluslong.id = 'Id',
    cluslong.clusterNames = LETTERS,
    cluslong.printOptions = FALSE,
    cluslong.printSharedModelArgs = FALSE)

  optMask = !(names(opts) %in% names(options()))
  if(any(optMask)) {
    options(opts[optMask])

    if(getOption('cluslong.printOptions', default=FALSE)) {
      packageStartupMessage('Default options:')
      packageStartupMessage(paste0('\t', names(opts[optMask]), ' = ', opts[optMask], collapse='\n'))
    }
  }
}