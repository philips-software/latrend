#' @include method.R
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
#' data(testLongData)
#' method <- lcMethodDtwclust("Value", nClusters = 3)
#' model <- latrend(method, testLongData)
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

setMethod('getName', signature('lcMethodDtwclust'), function(object) paste0('time series clustering with ', object$distance, '-dissimilarity'))

setMethod('getShortName', signature('lcMethodDtwclust'), function(object) paste0('diss-', object$distance))

setMethod('preFit', signature('lcMethodDtwclust'), function(method, data, envir, verbose, ...) {
  e = new.env()
  # convert data to list format
  e$seriesList = split(data, data[[idVariable(method)]]) %>%
    lapply('[[', responseVariable(method))
  return(e)
})

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
