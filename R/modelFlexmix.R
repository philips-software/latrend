#' @include model.R
setClass('lcModelFlexmix', contains = 'lcModel')


#' @export
#' @rdname interface-flexmix
#' @inheritParams predict.lcModel
predict.lcModelFlexmix = function(object, ...,
                                  newdata = NULL,
                                  what = 'mu') {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')

  if (is.null(newdata)) {
    predOut = flexmix::predict(object@model)
  } else {
    predOut = flexmix::predict(object@model, newdata = newdata)
  }
  predMat = do.call(cbind, predOut) %>%
    set_colnames(clusterNames(object))

  transformPredict(pred = predMat,
                   model = object,
                   newdata = newdata)
}


#' @export
#' @rdname interface-flexmix
#' @inheritParams fitted.lcModel
fitted.lcModelFlexmix = function(object, ..., clusters = trajectoryAssignments(object)) {
  predNames = paste0('pred_m', 1:nClusters(object))
  predMat = flexmix::fitted(object@model) %>%
    set_colnames(clusterNames(object))
  transformFitted(pred = predMat,
                  model = object,
                  clusters = clusters)
}

#' @rdname interface-flexmix
setMethod('postprob', signature('lcModelFlexmix'), function(object, ...) {
  pp = postProbFromObs(object@model@posterior$scaled, genIdRowIndices(object))
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
#' @rdname interface-flexmix
logLik.lcModelFlexmix = function(object, ...) {
  logLik(object@model)
}

#' @export
#' @rdname interface-flexmix
coef.lcModelFlexmix = function(object, ...) {
  flexmix::parameters(object@model)
}

#' @rdname interface-flexmix
setMethod('converged', signature('lcModelFlexmix'), function(object, ...) {
  object@model@converged
})
