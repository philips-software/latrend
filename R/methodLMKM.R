setClass('lcMethodLMKM', contains = 'lcMethod')

setValidity('lcMethodLMKM', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodLMKM)))

  if (isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
  }
})


#' @export
#' @title Two-step clustering through linear regression modeling and k-means
#' @inheritParams lcMethodFeature
#' @inheritParams lcMethodKML
#' @param formula A `formula` specifying the linear trajectory model.
#' @param ... Arguments passed to [stats::lm].
#' The following external arguments are ignored: x, data, control, centers, trace.
#' @examples
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method, latrendData)
#' @family lcMethod implementations
lcMethodLMKM = function(
  formula,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  standardize = scale,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodLMKM'
  do.call(new, as.list(mc))
}

#' @rdname interface-featureBased
setMethod('getArgumentDefaults', signature('lcMethodLMKM'), function(object) {
  c(
    formals(lcMethodLMKM),
    formals(lm),
    formals(kmeans),
    callNextMethod()
  )
})

#' @rdname interface-featureBased
setMethod('getArgumentExclusions', signature('lcMethodLMKM'), function(object) {
  union(
    callNextMethod(),
    c('x', 'data', 'control', 'centers', 'trace')
  )
})

#' @rdname interface-featureBased
setMethod('getName', signature('lcMethodLMKM'), function(object) 'glm-kmeans')

#' @rdname interface-featureBased
setMethod('getShortName', signature('lcMethodLMKM'), function(object) 'glmkm')

#' @rdname interface-featureBased
setMethod('prepareData', signature('lcMethodLMKM'), function(method, data, verbose) {
  cat(verbose, 'Representation step...')
  lmArgs = as.list(method, args = lm)

  id = idVariable(method)
  coefdata = as.data.table(data) %>%
    .[, do.call(lm, c(lmArgs, data = list(.SD))) %>% coef() %>% as.list(), keyby = c(id)]
  # construct the coefficient matrix
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  assert_that(nrow(coefmat) == uniqueN(data[[id]]))

  e = new.env()
  e$x = standardizeTrajectoryCoefMatrix(coefmat, method$standardize)
  return(e)
})

#' @rdname interface-featureBased
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
