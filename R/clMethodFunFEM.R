#' @include clMethod.R
setClass('clMethodFunFEM', contains = 'clMatrixMethod')

#' @export
#' @title Specify a FunFEM method
#' @param basis The basis function.
#' @inheritParams clMatrixMethod
#' @inheritParams funFEM::funFEM
#' @examples
#' method = clMethodFunFEM(Value ~ 1,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' cluslong(method, testLongData)
#' @family clMethod implementations
clMethodFunFEM = function(formula = Value ~ 1,
                          time = getOption('cluslong.time'),
                          id = getOption('cluslong.id'),
                          nClusters = 2,
                          basis = function(time)
                            create.bspline.basis(time, nbasis = 10, norder = 3),
                          ...) {
  clMethod.call(
    'clMethodFunFEM',
    call = match.call.defaults(),
    defaults = funFEM::funFEM,
    excludeArgs = c('fd', 'K', 'disp', 'graph')
  )
}

setMethod('getName', signature('clMethodFunFEM'), function(object) 'functional subspace clustering with FunFEM')

setMethod('getShortName', signature('clMethodFunFEM'), function(object) 'funfem')

setMethod('preFit', signature('clMethodFunFEM'), function(method, data, envir, verbose, ...) {
  e = callNextMethod()
  valueColumn = formula(method) %>% getResponse

  e$basis = method$basis(range(e$times))
  e$fd = smooth.basis(e$times, t(e$dataMat), e$basis)$fd

  return(e)
})


setMethod('fit', signature('clMethodFunFEM'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = funFEM::funFEM)
  args$fd = envir$fd
  args$K = method$nClusters
  args$disp = FALSE
  args$graph = FALSE

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(funFEM::funFEM, args)
  })
  model$basis = envir$basis
  model$fd = envir$fd

  new(
    'clModelFunFEM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
