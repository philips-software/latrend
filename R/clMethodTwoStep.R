#' @include clModelCustom.R
setClass('clMethodTwoStep', contains='clMethod')

#' @export
#' @title Two-step clustering
#' @description Two-step clustering.
#' @param representationStep A function with signature function(method, data) that computes the representation per strata, returned as a matrix.
#' Alternatively, representationStep is a pre-computed representation matrix.
#' @param clusterStep A function with signature function(repdata) that computes the cluster model.
#' @param standardize A function to standardize the output of the representation step. By default, the output is translated and rescaled to ensure zero mean and unit variance.
#' @inheritParams clMethodCustom
#' @family clMethod classes
clMethodTwoStep = function(representationStep,
                           clusterStep,
                           standardize=scale,
                           center=meanNA,
                           response=getOption('cluslong.response'),
                           time=getOption('cluslong.time'),
                           id=getOption('cluslong.id')) {
  object = new('clMethodTwoStep', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object)
  }
  return(object)
}


setMethod('checkArgs', signature('clMethodTwoStep'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodTwoStep) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  assert_that(!isArgDefined(object, 'representationStep') || is.function(object$representationStep) || is.matrix(object$representationStep))

  assert_that(!isArgDefined(object, 'clusterStep') || is.function(object$clusterStep))
})


setMethod('getName', signature('clMethodTwoStep'), function(object) 'two-step clustering')


setMethod('getName0', signature('clMethodTwoStep'), function(object) 'twostep')


setMethod('prepare', signature('clMethodTwoStep'), function(method, data, verbose, ...) {
  assert_that(has_name(data, method$response))
  assert_that(has_name(data, method$id))
  assert_that(has_name(data, method$time))
  return(NULL)
})


setMethod('fit', signature('clMethodTwoStep'), function(method, data, envir, verbose, ...) {
  nIds = uniqueN(data[[method$id]])

  ## Representation step #
  rstep = method$representationStep
  if(is.function(rstep)) {
    repOut = rstep(method, data, verbose)
  } else {
    repOut = rstep
  }

  if(is.environment(repOut)) {
    repEnv = repOut
  } else if(is.list(repOut)) {
    repEnv = list2env(repOut)
  } else if(is.matrix(repOut)) {
    repEnv = new.env()
    repEnv$repMat = repOut
  } else {
    stop('unexpected output from the representationStep function. See the documentation ?clMethodTwoStep for help.')
  }

  assert_that(exists('repMat', envir=repEnv))
  assert_that(is.matrix(repEnv$repMat), msg='invalid class output from the representation step. Expected "repMat" to be a matrix.')
  assert_that(nrow(repEnv$repMat) == nIds, msg='invalid output from the representation step; expected "repMat" to be a matrix with one row per id.')
  assert_that(ncol(repEnv$repMat) >= 1)

  ## Cluster step #
  clusEnv = new.env(parent=repEnv)
  model = method$clusterStep(method=method, data=data, repMat=repEnv$repMat, envir=repEnv, verbose=verbose)

  assert_that(is.clModelCustom(model), msg='invalid output from the clusterStep function; expected object of class clModelCustom. See the documentation of ?clMethodTwoStep for help.')
  clusEnv$model = model
  return(clusEnv)
})


setMethod('finalize', signature('clMethodTwoStep'), function(method, data, envir, verbose, ...) {
  # convert clModelCustom to a clModelTwoStep with the appropriate call
  model = envir$model

  slots = slotNames(model) %>%
    lapply(slot, object=model) %>%
    setNames(slotNames(model))

  slots$Class = 'clModelTwoStep'
  slots$data = data
  slots$call = getCall(method) # will be updated by cluslong
  slots$method = method

  newmodel = do.call(new, slots, quote=TRUE)
  return(newmodel)
})