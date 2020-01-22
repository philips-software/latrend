#' @include clModel.R
setClass('clFlexmixModel', contains='clModel')

#' @export
#' @importFrom flexmix fitted
fitted.clFlexmixModel = function(object, clusters=clusterAssignments(object)) {
  predNames = paste0('pred_m', 1:nClusters(object))
  predMat = fitted(object@model) %>%
    set_colnames(clusterNames(object))
  transformFitted(object, predMat, clusters=clusters)
}

#' @export
#' @importFrom flexmix predict
predict.clFlexmixModel = function(object, newdata=NULL, what='mu') {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg='only what="mu" is supported')

  predMat = predict(object@model, newdata=newdata) %>%
    do.call(cbind, .) %>%
    set_colnames(clusterNames(object))

  transformPredict(object, predMat, newdata=newdata)
}

setMethod('postprob', signature('clFlexmixModel'), function(object) {
  pp = postProbFromObs(object@model@posterior$scaled, genIdRowIndices(object))
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
logLik.clFlexmixModel = function(object) {
  logLik(object@model)
}

#' @export
#' @importFrom flexmix parameters
coef.clFlexmixModel = function(object) {
  parameters(object@model)
}

setMethod('converged', signature('clFlexmixModel'), function(object) {
  object@model@converged
})