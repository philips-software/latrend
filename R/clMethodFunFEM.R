#' @include clMethod.R
setClass('clMethodFunFEM', contains='clMatrixMethod')

#' @export
#' @import funFEM
#' @title Specify FunFEM method
#' @param basis The basis function.
#' @inheritParams clMatrixMethod
#' @inheritParams funFEM::funFEM
#' @examples
#' method = clMethodFunFEM(Value ~ 1,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' cluslong(method, testLongData)
#' @family clMethod implementations
clMethodFunFEM = function(formula=Value ~ 1,
                          time=getOption('cluslong.time'),
                          id=getOption('cluslong.id'),
                          nClusters=2,
                          basis=function(time) create.bspline.basis(time, nbasis=10, norder=3),
                          ...
) {
  clMethod('clMethodFunFEM', call=match.call.defaults(),
           defaults=funFEM::funFEM,
           excludeArgs=c('fd', 'K', 'disp', 'graph'))
}

setMethod('getName', signature('clMethodFunFEM'), function(object) 'functional subspace clustering with FunFEM')

setMethod('getName0', signature('clMethodFunFEM'), function(object) 'funfem')

setMethod('prepare', signature('clMethodFunFEM'), function(method, data, verbose, ...) {
  e = callNextMethod()
  valueColumn = formula(method) %>% getResponse

  e$basis = method$basis(range(e$times))
  e$fd = smooth.basis(e$times, t(e$dataMat), e$basis)$fd

  return(e)
})


setMethod('fit', signature('clMethodFunFEM'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  args = as.list(method)
  args$fd = envir$fd
  args$K = method$nClusters
  args$disp = FALSE
  args$graph = FALSE
  args[setdiff(names(args), formalArgs(funFEM))] = NULL #remove undefined arguments

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    e$model = do.call(funFEM, args)
  })
  e$model$basis = envir$basis
  e$model$fd = envir$fd
  return(e)
})

setMethod('finalize', signature('clMethodFunFEM'), function(method, data, envir, verbose, ...) {
  model = new('clModelFunFEM',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
