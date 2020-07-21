#' @include method.R
setClass('lcMethodDtwclust', contains = 'lcMethod')

#' @export
#' @inheritDotParams dtwclust::tsclust
#' @title Specify time series clustering via dtwclust
#' @param response Response variable.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @family lcMethod implementations
lcMethodDtwclust = function(response,
                       time = getOption('latrend.time'),
                       id = getOption('latrend.id'),
                       nClusters = 2,
                       ...) {
  library(dtwclust)
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
