#' @include clMethod.R
setClass('clMethodLongclust', contains = 'clMatrixMethod')

#' @export
#' @title Specify Longclust method
#' @param basis The basis function.
#' @inheritParams clMatrixMethod
#' @inheritDotParams longclust::longclustEM
#' @examples
#' method = clMethodLongclust(Value ~ .,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' cluslong(method, testLongData)
#' @family clMethod implementations
clMethodLongclust = function(formula = Value ~ 1,
                             time = getOption('cluslong.time'),
                             id = getOption('cluslong.id'),
                             nClusters = 2,
                             ...) {
  clMethod.call(
    'clMethodLongclust',
    call = match.call.defaults(),
    defaults = longclust::longclustEM,
    excludeArgs = c('data', 'x', 'Gmin', 'Gmax', 'userseed')
  )
}

setMethod('getName', signature('clMethodLongclust'), function(object) 'longclust')

setMethod('getShortName', signature('clMethodLongclust'), function(object) 'longclust')

setMethod('fit', signature('clMethodLongclust'), function(method, data, envir, verbose, ...) {
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
    'clModelLongclust',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
