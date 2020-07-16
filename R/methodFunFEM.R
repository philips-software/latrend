#' @include method.R
setClass('lcMethodFunFEM', contains = 'lcMatrixMethod')

#' @export
#' @title Specify a FunFEM method
#' @param basis The basis function.
#' @inheritParams lcMatrixMethod
#' @inheritParams funFEM::funFEM
#' @examples
#' method = lcMethodFunFEM(Value ~ 1,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' latrend(method, testLongData)
#' @family lcMethod implementations
lcMethodFunFEM = function(response,
                          time = getOption('latrend.time'),
                          id = getOption('latrend.id'),
                          nClusters = 2,
                          basis = function(time)
                            create.bspline.basis(time, nbasis = 10, norder = 3),
                          ...) {
  lcMethod.call(
    'lcMethodFunFEM',
    call = match.call.defaults(),
    defaults = funFEM::funFEM,
    excludeArgs = c('fd', 'K', 'disp', 'graph')
  )
}

setMethod('getName', signature('lcMethodFunFEM'), function(object) 'functional subspace clustering with FunFEM')

setMethod('getShortName', signature('lcMethodFunFEM'), function(object) 'funfem')

setMethod('preFit', signature('lcMethodFunFEM'), function(method, data, envir, verbose, ...) {
  e = callNextMethod()
  e$basis = method$basis(range(e$times))
  e$fd = smooth.basis(e$times, t(e$dataMat), e$basis)$fd

  return(e)
})


setMethod('fit', signature('lcMethodFunFEM'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = funFEM::funFEM)
  args$fd = envir$fd
  args$K = method$nClusters
  args$disp = FALSE
  args$graph = FALSE

  # Helper variables
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(funFEM::funFEM, args)
  })
  model$basis = envir$basis
  model$fd = envir$fd

  new(
    'lcModelFunFEM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
