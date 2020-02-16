#' @include clMethod.R
setClass('clMethodLongclust', contains='clMatrixMethod')

#' @export
#' @import longclust
#' @title Specify Longclust method
#' @param basis The basis function.
#' @inheritParams clMatrixMethod
#' @inheritParams longclust::longclustEM
#' @examples
#' method = clMethodLongclust(Value ~ .,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' cluslong(method, testLongData)
#' @family clMethod classes
clMethodLongclust = function(formula=Value ~ 1,
                          time=getOption('cluslong.time'),
                          id=getOption('cluslong.id'),
                          nClusters=2,
                          linearMeans=FALSE,
                          initWithKMeans=FALSE,
                          equalDF=FALSE,
                          gaussian=FALSE, ...) {
  new('clMethodLongclust', call=match.call.defaults())
}

setMethod('getName', signature('clMethodLongclust'), function(object) 'longclust')

setMethod('getName0', signature('clMethodLongclust'), function(object) 'longclust')

setMethod('fit', signature('clMethodLongclust'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  args = as.list(method)
  args$x = envir$dataMat
  args$Gmin = method$nClusters
  args$Gmax = method$nClusters
  if(hasName(method, 'seed')) {
    args$userseed = method$seed
  }
  args[setdiff(names(args), formalArgs(longclustEM))] = NULL #remove undefined arguments

  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  startTime = Sys.time()
  suppressFun({
    e$model = do.call(longclustEM, args)
  })
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})

setMethod('finalize', signature('clMethodLongclust'), function(method, data, envir, verbose, ...) {
  model = new('clModelLongclust',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
