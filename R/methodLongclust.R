#' @include method.R
setClass('lcMethodLongclust', contains = 'lcMatrixMethod')

#' @export
#' @title Specify Longclust method
#' @inheritParams lcMatrixMethod
#' @param ... Arguments passed to [longclust::longclustEM].
#' The following external arguments are ignored: data, x, Gmin, Gmax, userseed.
#' @examples
#' library(longclust)
#' data(testLongData)
#' method = lcMethodLongclust(Value ~ 1,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' latrend(method, testLongData)
#' @family lcMethod implementations
lcMethodLongclust = function(response,
                             time = getOption('latrend.time'),
                             id = getOption('latrend.id'),
                             nClusters = 2,
                             ...) {
  lcMethod.call(
    'lcMethodLongclust',
    call = match.call.defaults(),
    defaults = longclust::longclustEM,
    excludeArgs = c('data', 'x', 'Gmin', 'Gmax', 'userseed')
  )
}

setMethod('getName', signature('lcMethodLongclust'), function(object) 'longclust')

setMethod('getShortName', signature('lcMethodLongclust'), function(object) 'longclust')

setMethod('fit', signature('lcMethodLongclust'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = longclust::longclustEM)
  args$x = envir$dataMat
  args$Gmin = method$nClusters
  args$Gmax = method$nClusters
  if (hasName(method, 'seed')) {
    args$userseed = method$seed
  }

  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(longclust::longclustEM, args)
  })

  new(
    'lcModelLongclust',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
