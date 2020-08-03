#' @include model.R
setClass('lcModelFunFEM', contains = 'lcModel')

#' @export
fitted.lcModelFunFEM = function(object, ..., clusters = clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id = rep(ids(object), each = length(times)),
                       Time = times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata = newdata) %>%
    transformFitted(model = object, clusters)
}


#' @export
predict.lcModelFunFEM = function(object, ...,
                                 newdata = NULL,
                                 what = 'mu',
                                 approxFun = approx) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')
  assert_that(is.function(approxFun))

  if (is.null(newdata)) {
    predMat = fitted(object, clusters = NULL)
  } else {
    assert_that(has_name(newdata, timeVariable(object)))
    fdmeans = object@model$fd
    fdmeans$coefs = t(object@model$prms$my)
    predMat = fda::eval.fd(evalarg = newdata[[timeVariable(object)]], fdobj =
                        fdmeans)
  }

  transformPredict(pred = predMat,
                   model = object,
                   newdata = newdata)
}


setMethod('postprob', signature('lcModelFunFEM'), function(object) {
  pp = object@model$P
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
coef.lcModelFunFEM = function(object, ...) {
  coefMat = t(object@model$prms$my)
  colnames(coefMat) = clusterNames(object)
  return(coefMat)
}


#' @export
logLik.lcModelFunFEM = function(object, ...) {
  ll = object@model$ll
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = object@model$nbprm / 2
  class(ll) = 'logLik'
  return(ll)
}

setMethod('converged', signature('lcModelFunFEM'), function(object) {
  TRUE
})
