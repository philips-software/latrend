setClass('clMethodLMKM', contains='clMethod')

setValidity('clMethodLMKM', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodLMKM)))

  if(isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
  }
})


#' @export
#' @title Two-step clustering through linear modeling and k-means
#' @param formula Trajectory-specific formula
#' @param time Time variable.
#' @param nClusters Number of clusters.
#' @inheritParams clMethodTwoStep
#' @inheritDotParams stats::lm
#' @examples
#' method = clMethodLMKM(Measurement ~ Assessment + (Assessment | Subject),
#'                      time='Assessment',
#'                      id='Subject', nClusters=3)
#' @family clMethod implementations
clMethodLMKM = function(formula = Value ~ Time,
                        time = getOption('cluslong.time'),
                        id = getOption('cluslong.id'),
                        nClusters = 2,
                        standardize = scale,
                        ...
) {
  .clMethod('clMethodLMKM', call=match.call.defaults(),
           defaults=c(lm, kmeans),
           excludeArgs=c('x', 'data', 'control', 'centers', 'trace'))
}


setMethod('getName', signature('clMethodLMKM'), function(object) 'glm-kmeans')

setMethod('getShortName', signature('clMethodLMKM'), function(object) 'glmkm')


setMethod('prepare', signature('clMethodLMKM'), function(method, data, verbose) {
  cat(verbose, 'Representation step...')
  lmArgs = as.list(method, args=lm)

  coefdata = data[, do.call(lm, c(lmArgs, data=list(.SD))) %>% coef() %>% as.list(), keyby = c(method$id)]
  # construct the coefficient matrix
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  assert_that(nrow(coefmat) == uniqueN(data[[method$id]]))

  e = new.env()
  e$x = standardizeTrajectoryCoefMatrix(coefmat, method$standardize)
  return(e)
})


setMethod('fit', signature('clMethodLMKM'), function(method, data, envir, verbose, ...) {
  cat(verbose, 'Cluster step...')
  km = kmeans(envir$x, centers=method$nClusters, trace=canShow(verbose, 'fine'))

  new('clModelLMKM',
      method=method,
      data=data,
      model=km,
      coefNames=colnames(envir$x),
      clusterNames=make.clusterNames(method$nClusters))
})
