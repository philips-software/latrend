#' #' @include method.R
#' setClass('lcMethodDiss', contains = 'lcMethod')
#' setClass('lcMethodDissAHC', contains = 'lcMethodDiss')
#' setClass('lcMethodDissPAM', contains = 'lcMethodDiss')
#' setClass('lcMethodDissTadpole', contains = 'lcMethodDiss')
#'
#' #' @export
#' #' @title Specify a dissimilarity-based method
#' #' @param time Time variable.
#' #' @param id Strata variable.
#' #' @param nClusters Number of clusters.
#' #' @examples
#' #' method = lcMethodDissimilarity(diss = "EUCL", nClusters=2)
#' #' model = latrend(method, testLongData)
#' #' @family lcMethod implementations
#' lcMethodDiss = function(response,
#'                         type = 'AHC',
#'                          time = getOption('latrend.time'),
#'                          id = getOption('latrend.id'),
#'                          nClusters = 2,
#'                          ...) {
#'   ns = getNamespace('TSclust')
#'   dissName = paste0('diss.', METHOD)
#'
#'   if(dissName %in% names(ns)) {
#'     dissFun = ns[[dissName]]
#'     assert_that(is.function(dissFun))
#'   } else {
#'     dissFun = function() {}
#'   }
#'
#'   lcMethod.call(
#'     'lcMethodTSclust',
#'     call = match.call.defaults(),
#'     defaults = c(TSclust::diss, dissFun),
#'     excludeArgs = 'SERIES'
#'   )
#' }
