#' @include method.R

#' @name interface-dtwclust
#' @rdname interface-dtwclust
#' @title dtwclust interface
#' @seealso [lcMethodDtwclust] \link[dtwclust]{dtwclust-package}
NULL

setClass('lcMethodDtwclust', contains = 'lcMethod')

#' @export
#' @title Specify time series clustering via dtwclust
#' @param response The name of the response variable.
#' @param time The name of the time variable.
#' @param id The name of the trajectory identifier variable.
#' @param nClusters Number of clusters.
#' @param ... Arguments passed to [dtwclust::tsclust].
#' The following arguments are ignored: series, k, trace.
#' @examples
#' library(dtwclust)
#' data(latrendData)
#' method <- lcMethodDtwclust("Y", nClusters = 3)
#' model <- latrend(method, latrendData)
#' @family lcMethod implementations
lcMethodDtwclust = function(response,
                       time = getOption('latrend.time'),
                       id = getOption('latrend.id'),
                       nClusters = 2,
                       ...) {
  .loadOptionalPackage('dtwclust')

  m = lcMethod.call(
    'lcMethodDtwclust',
    call = stackoverflow::match.call.defaults(),
    defaults = dtwclust::tsclust,
    excludeArgs = c('series', 'k', 'trace')
  )
  return(m)
}

#' @rdname interface-dtwclust
#' @inheritParams getName
setMethod('getName', signature('lcMethodDtwclust'), function(object) paste0('time series clustering with ', object$distance, '-dissimilarity'))

#' @rdname interface-dtwclust
setMethod('getShortName', signature('lcMethodDtwclust'), function(object) paste0('diss-', object$distance))

#' @rdname interface-dtwclust
setMethod('preFit', signature('lcMethodDtwclust'), function(method, data, envir, verbose, ...) {
  e = new.env()
  # convert data to list format
  e$seriesList = split(data, data[[idVariable(method)]]) %>%
    lapply('[[', responseVariable(method))
  return(e)
})

#' @rdname interface-dtwclust
#' @inheritParams fit
setMethod('fit', signature('lcMethodDtwclust'), function(method, data, envir, verbose, ...) {
  args = as.list(method)
  args$series = envir$seriesList
  args$k = method$nClusters
  args$trace = as.logical(verbose)

  cat(verbose, 'Running tsclust()...', level = verboseLevels$finest)
  tsClusters = do.call(dtwclust::tsclust, args)

  new(
    'lcModelDtwclust',
    method = method,
    data = data,
    model = tsClusters,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
