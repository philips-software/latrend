setClass('lcMethodLMKM', contains = 'lcMethod')

setValidity('lcMethodLMKM', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodLMKM)))

  if (isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
  }
})


#' @export
#' @title Two-step clustering through linear modeling and k-means
#' @param formula Trajectory-specific formula
#' @param time Time variable.
#' @param nClusters Number of clusters.
#' @inheritParams lcMethodTwoStep
#' @inheritDotParams stats::lm
#' @examples
#' method = lcMethodLMKM(Measurement ~ Assessment + (Assessment | Subject),
#'                      time='Assessment',
#'                      id='Subject', nClusters=3)
#' @family lcMethod implementations
lcMethodLMKM = function(formula,
                        time = getOption('latrend.time'),
                        id = getOption('latrend.id'),
                        nClusters = 2,
                        standardize = scale,
                        ...) {
  lcMethod.call(
    'lcMethodLMKM',
    call = match.call.defaults(),
    defaults = c(lm, kmeans),
    excludeArgs = c('x', 'data', 'control', 'centers', 'trace')
  )
}


setMethod('getName', signature('lcMethodLMKM'), function(object) 'glm-kmeans')

setMethod('getShortName', signature('lcMethodLMKM'), function(object) 'glmkm')


setMethod('prepareData', signature('lcMethodLMKM'), function(method, data, verbose) {
  cat(verbose, 'Representation step...')
  lmArgs = as.list(method, args = lm)

  id = idVariable(method)
  coefdata = data[, do.call(lm, c(lmArgs, data = list(.SD))) %>% coef() %>% as.list(), keyby = c(id)]
  # construct the coefficient matrix
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  assert_that(nrow(coefmat) == uniqueN(data[[id]]))

  e = new.env()
  e$x = standardizeTrajectoryCoefMatrix(coefmat, method$standardize)
  return(e)
})


setMethod('fit', signature('lcMethodLMKM'), function(method, data, envir, verbose, ...) {
  cat(verbose, 'Cluster step...')
  km = kmeans(envir$x,
              centers = method$nClusters,
              trace = canShow(verbose, 'fine'))

  new(
    'lcModelLMKM',
    method = method,
    data = data,
    model = km,
    coefNames = colnames(envir$x),
    clusterNames = make.clusterNames(method$nClusters)
  )
})
