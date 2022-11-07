#' @include modelPartition.R

#' @name interface-featureBased
#' @rdname interface-featureBased
#' @title featureBased interface
#' @seealso [lcMethodFeature] [lcMethodGCKM] [lcMethodLMKM]
#' @keywords internal
NULL

setClass('lcMethodFeature', contains = 'lcMethod')

setValidity('lcMethodFeature', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodFeature)))

  assert_that(
    !isArgDefined(object, 'representationStep') ||
      is.function(object$representationStep) ||
      is.matrix(object$representationStep)
  )

  assert_that(
    !isArgDefined(object, 'clusterStep') || is.function(object$clusterStep)
  )
})

#' @export
#' @title Feature-based clustering
#' @description Feature-based clustering.
#' @inheritParams lcModelPartition
#' @param representationStep A `function` with signature `function(method, data)` that computes the representation per strata, returned as a `matrix`.
#' Alternatively, `representationStep` is a pre-computed representation `matrix`.
#' @param clusterStep A `function` with signature `function(repdata)` that outputs a `lcModel`.
#' @param standardize A `function` to standardize the output `matrix` of the representation step. By default, the output is shifted and rescaled to ensure zero mean and unit variance.
#' @param ... Additional arguments.
#' @section Linear regresion & k-means example:
#' In this example we define a feature-based approach where each trajectory is represented using a linear regression model.
#' The coefficients of the trajectories are then clustered using k-means.
#'
#' Note that this method is already implemented as [lcMethodLMKM()].
#'
#' Representation step:
#' \preformatted{
#' repStep <- function(method, data, verbose) {
#'   library(data.table)
#'   library(magrittr)
#'   xdata = as.data.table(data)
#'   coefdata <- xdata[,
#'     lm(method$formula, .SD) %>% coef() %>% as.list(),
#'     keyby = c(method$id)
#'   ]
#'   # exclude the id column
#'   coefmat <- subset(coefdata, select = -1) %>% as.matrix()
#'   rownames(coefmat) <- coefdata[[method$id]]
#'   return(coefmat)
#' }
#' }
#'
#' Cluster step:
#' \preformatted{
#' clusStep <- function(method, data, repMat, envir, verbose) {
#'   km <- kmeans(repMat, centers = method$nClusters)
#'
#'   lcModelPartition(
#'     response = method$response,
#'     data = data,
#'     trajectoryAssignments = km$cluster
#'   )
#' }
#' }
#'
#' Now specify the method and fit the model:
#' \preformatted{
#' data(latrendData)
#' method <- lcMethodFeature(
#'   formula = Y ~ Time,
#'   response = "Y",
#'   id = "Id",
#'   time = "Time",
#'   representationStep = repStep,
#'   clusterStep = clusStep
#'
#' model <- latrend(method, data = latrendData)
#' )
#' }
#'
#' @family lcMethod implementations
lcMethodFeature = function(
  response,
  representationStep,
  clusterStep,
  standardize = scale,
  center = meanNA,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodFeature'
  do.call(new, as.list(mc))
}

#' @rdname interface-featureBased
setMethod('getArgumentDefaults', 'lcMethodFeature', function(object) {
  c(
    formals(lcMethodFeature),
    callNextMethod()
  )
})

#' @rdname interface-featureBased
#' @inheritParams getName
setMethod('getName', 'lcMethodFeature', function(object) 'two-step clustering')

#' @rdname interface-featureBased
setMethod('getShortName', 'lcMethodFeature', function(object) 'twostep')

#' @rdname interface-featureBased
setMethod('prepareData', 'lcMethodFeature', function(method, data, verbose, ...) {
  assert_that(has_name(data, responseVariable(method)))
  callNextMethod()
})

#' @rdname interface-featureBased
#' @inheritParams fit
setMethod('fit', 'lcMethodFeature', function(method, data, envir, verbose, ...) {
  nIds = uniqueN(data[[idVariable(method)]])

  ## Representation step #
  rstep = method$representationStep
  if (is.function(rstep)) {
    repOut = rstep(method, data, verbose)
  } else {
    repOut = rstep
  }

  if (is.environment(repOut)) {
    repEnv = repOut
  } else if (is.list(repOut)) {
    repEnv = list2env(repOut)
  } else if (is.matrix(repOut)) {
    repEnv = new.env()
    repEnv$repMat = repOut
  } else {
    stop('unexpected output from the representationStep function. See the documentation ?lcMethodFeature for help.')
  }

  assert_that(exists('repMat', envir = repEnv))
  assert_that(is.matrix(repEnv$repMat), msg = 'invalid class output from the representation step. Expected "repMat" to be a matrix.')
  assert_that(nrow(repEnv$repMat) == nIds, msg = 'invalid output from the representation step; expected "repMat" to be a matrix with one row per id.')
  assert_that(ncol(repEnv$repMat) >= 1)

  repEnv$repMat = standardizeTrajectoryCoefMatrix(repEnv$repMat, method$standardize)

  ## Cluster step #
  model = method$clusterStep(
    method = method,
    data = data,
    repMat = repEnv$repMat,
    envir = repEnv,
    verbose = verbose
  )

  assert_that(
    is.lcModel(model),
    msg = 'invalid output from the clusterStep function; expected object of class lcModel. See the documentation of ?lcMethodFeature for help.'
  )

  model@data = data
  model@call = getCall(method) # will be updated by latrend
  model@method = method

  model
})


standardizeTrajectoryCoefMatrix = function(x, fun) {
  assert_that(is.matrix(x))

  if (is.function(fun)) {
    newx = fun(x)
    assert_that(
      is.matrix(newx),
      nrow(newx) == nrow(x),
      ncol(newx) == ncol(x),
      msg = 'standardize function changed dimensions of the input matrix'
    )
    return(newx)
  } else if (isTRUE(fun)) {
    scale(x)
  } else if (isFALSE(fun)) {
    x
  } else {
    stop('unsupported value for standardize')
  }
}
