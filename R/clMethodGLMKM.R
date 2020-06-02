setClass('clMethodGLMKM', contains='clMethod')

setValidity('clMethodGLMKM', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodGLMKM)))

  if(isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
  }
})


#' @export
#' @title Two-step clustering through generalized linear modeling and k-means
#' @param formula Trajectory-specific formula
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @inheritParams glm
#' @inheritParams clMethodTwoStep
#' @examples
#' method = clMethodGLMKM(Measurement ~ Assessment + (Assessment | Subject),
#'                      time='Assessment',
#'                      id='Subject', nClusters=3)
#' @family clMethod implementations
clMethodGLMKM = function(formula = Value ~ Time,
                        time = getOption('cluslong.time'),
                        id = getOption('cluslong.id'),
                        nClusters = 2,
                        standardize = scale,
                        ...
) {
  m = clMethod('clMethodGLMKM', call=match.call.defaults(),
           defaults=c(glm, glm.control, kmeans),
           excludeArgs=c('x', 'data', 'control', 'centers', 'trace'))
  return(m)
}


setMethod('getName', signature('clMethodGLMKM'), function(object) 'glm-kmeans')

setMethod('getShortName', signature('clMethodGLMKM'), function(object) 'glmkm')


setMethod('prepare', signature('clMethodGLMKM'), function(method, data, verbose) {
  cat(verbose, 'Representation step...')
  glmArgs = method[c(glm, glm.control), expand=FALSE]

  coefdata = data[, do.call(glm, c(glmArgs, data=list(.SD))) %>% coef() %>% as.list(), keyby = c(method$id)]
  # construct the coefficient matrix
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  assert_that(nrow(coefmat) == uniqueN(data[[method$id]]))

  e = new.env()
  e$x = standardizeTrajectoryCoefMatrix(coefmat, method$standardize)
  return(e)
})


setMethod('fit', signature('clMethodGLMKM'), function(method, data, envir, verbose, ...) {
  cat(verbose, 'Cluster step...')
  km = kmeans(envir$x, centers=method$nClusters, trace=canShow(verbose, 'fine'))

  new('clModelGLMKM',
      method=method,
      data=data,
      model=km,
      clusterNames=make.clusterNames(method$nClusters))
})
