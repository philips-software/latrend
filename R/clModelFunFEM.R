#' @include clModel.R
setClass('clModelFunFEM', contains='clModel')

#' @export
fitted.clModelFunFEM = function(object, clusters=clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id=rep(ids(object), each=length(times)),
                       Time=times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata=newdata) %>%
    transformFitted(model = object, clusters)
}


#' @export
#' @importFrom fda eval.fd
#' @rdname predict.clModel
#' @inheritParams predict.clModel
#' @param approxFun The interpolation function to use for time points not in the feature set.
predict.clModelFunFEM = function(object, newdata=NULL, what='mu', approxFun=approx) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg='only what="mu" is supported')
  assert_that(is.function(approxFun))

  if(is.null(newdata)) {
    predMat = fitted(object, clusters=NULL)
  } else {
    assert_that(has_name(newdata, timeVariable(object)))
    fdmeans = object@model$fd
    fdmeans$coefs = t(object@model$prms$my)
    predMat = eval.fd(evalarg=newdata[[timeVariable(object)]], fdobj=fdmeans)
  }

  transformPredict(pred = predMat, model = object, newdata = newdata)
}


setMethod('postprob', signature('clModelFunFEM'), function(object) {
  pp = object@model$P
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
coef.clModelFunFEM = function(object) {
  coefMat = t(object@model$prms$my)
  colnames(coefMat) = clusterNames(object)
  return(coefMat)
}


#' @export
logLik.clModelFunFEM = function(object) {
  ll = object@model$ll
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = object@model$nbprm / 2
  class(ll) = 'logLik'
  return(ll)
}

setMethod('converged', signature('clModelFunFEM'), function(object) {
  TRUE
})