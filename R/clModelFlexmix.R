#' @include clModel.R
setClass('clModelFlexmix', contains='clModel')

#' @export
#' @importFrom flexmix fitted
fitted.clModelFlexmix = function(object, clusters=clusterAssignments(object)) {
  predNames = paste0('pred_m', 1:nClusters(object))
  predMat = fitted(object@model) %>%
    set_colnames(clusterNames(object))
  transformFitted(object, predMat, clusters=clusters)
}

#' @export
#' @importFrom flexmix predict
predict.clModelFlexmix = function(object, newdata=NULL, what='mu') {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg='only what="mu" is supported')

  predMat = predict(object@model, newdata=newdata) %>%
    do.call(cbind, .) %>%
    set_colnames(clusterNames(object))

  transformPredict(object, predMat, newdata=newdata)
}

setMethod('postprob', signature('clModelFlexmix'), function(object) {
  pp = postProbFromObs(object@model@posterior$scaled, genIdRowIndices(object))
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
logLik.clModelFlexmix = function(object) {
  logLik(object@model)
}

#' @export
#' @importFrom flexmix parameters
coef.clModelFlexmix = function(object) {
  parameters(object@model)
}

setMethod('converged', signature('clModelFlexmix'), function(object) {
  object@model@converged
})