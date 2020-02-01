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
#' @family clMethod classes
clMethodFunFEM = function(formula=Value ~ 1,
                          time=getOption('cluslong.time'),
                          id=getOption('cluslong.id'),
                          nClusters=2,
                          basis=function(time) create.bspline.basis(time, nbasis=10, norder=3),
                          model='AkjBk',
                          init='hclust',
                          maxit=50,
                          eps=1e-06,
                          lambda=0) {
  new('clMethodFunFEM', call=match.call.defaults())
}

setMethod('getName', signature('clMethodFunFEM'), function(object) 'functional subspace clustering with FunFEM')

setMethod('getName0', signature('clMethodFunFEM'), function(object) 'funfem')

setMethod('prepare', signature('clMethodFunFEM'), function(method, data) {
  e = callNextMethod()
  valueColumn = formula(method) %>% getResponse

  e$basis = method$basis(range(e$times))
  e$fd = smooth.basis(e$times, t(e$dataMat), e$basis)$fd

  return(e)
})


setMethod('fit', signature('clMethodFunFEM'), function(method, data, prepEnv) {
  e = new.env(parent=prepEnv)

  args = as.list(method)
  args$fd = prepEnv$fd
  args$K = method$nClusters
  args$disp = FALSE
  args$graph = FALSE
  args[setdiff(names(args), formalArgs(funFEM))] = NULL #remove undefined arguments

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = if(canShowModelOutput()) force else capture.output

  startTime = Sys.time()
  suppressFun({
    e$model = do.call(funFEM, args)
  })
  e$model$basis = prepEnv$basis
  e$model$fd = prepEnv$fd
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})

setMethod('finalize', signature('clMethodFunFEM'), function(method, data, fitEnv) {
  model = new('clModelFunFEM',
              method=method,
              data=data,
              model=fitEnv$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
