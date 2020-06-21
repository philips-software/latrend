#' @include clModelCustom.R
setClass('clMethodTwoStep', contains = 'clMethod')

setValidity('clMethodTwoStep', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodTwoStep)))

  assert_that(
    !isArgDefined(object, 'representationStep') ||
      is.function(object$representationStep) ||
      is.matrix(object$representationStep)
  )

  assert_that(!isArgDefined(object, 'clusterStep') ||
                is.function(object$clusterStep))
})

#' @export
#' @title Two-step clustering
#' @description Two-step clustering.
#' @param representationStep A `function` with signature `function(method, data)` that computes the representation per strata, returned as a `matrix`.
#' Alternatively, `representationStep` is a pre-computed representation `matrix`.
#' @param clusterStep A `function` with signature `function(repdata)` that outputs a `clModel`.
#' @param standardize A `function` to standardize the output `matrix` of the representation step. By default, the output is shifted and rescaled to ensure zero mean and unit variance.
#' @inheritParams clMethodCustom
#' @family clMethod implementations
clMethodTwoStep = function(representationStep,
                           clusterStep,
                           standardize = scale,
                           center = meanNA,
                           response = getOption('cluslong.response'),
                           time = getOption('cluslong.time'),
                           id = getOption('cluslong.id'),
                           ...) {
  clMethod.call('clMethodTwoStep',
                 call = match.call.defaults(),
                 excludeArgs = c('verbose'))
}


setMethod('getName', signature('clMethodTwoStep'), function(object) 'two-step clustering')


setMethod('getShortName', signature('clMethodTwoStep'), function(object) 'twostep')


setMethod('prepareData', signature('clMethodTwoStep'), function(method, data, verbose, ...) {
  assert_that(has_name(data, responseVariable(method)))
  return(NULL)
})


setMethod('fit', signature('clMethodTwoStep'), function(method, data, envir, verbose, ...) {
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
    stop(
      'unexpected output from the representationStep function. See the documentation ?clMethodTwoStep for help.'
    )
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

  assert_that(is.clModelCustom(model), msg = 'invalid output from the clusterStep function; expected object of class clModelCustom. See the documentation of ?clMethodTwoStep for help.')


  # convert clModelCustom to a clModelTwoStep with the appropriate call
  slots = slotNames(model) %>%
    lapply(slot, object = model) %>%
    setNames(slotNames(model))

  slots$Class = 'clModelTwoStep'
  slots$data = data
  slots$call = getCall(method) # will be updated by cluslong
  slots$method = method

  newmodel = do.call(new, slots, quote = TRUE)
  return(newmodel)
})

#' @export
standardizeTrajectoryCoefMatrix = function(x, fun) {
  assert_that(is.matrix(x))

  if (is.function(fun)) {
    newx = fun(x)
    assert_that(is.matrix(newx),
                nrow(newx) == nrow(x),
                ncol(newx) == ncol(x),
                msg = 'standardize function changed dimensions of the input matrix')
    return(newx)
  } else if (isTRUE(fun)) {
    scale(x)
  } else if (isFALSE(fun)) {
    x
  } else {
    stop('unsupported value for standardize')
  }
}
