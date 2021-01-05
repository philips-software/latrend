#' @include model.R
setClass('lcModelFunFEM', contains = 'lcModel')

#' @export
#' @rdname interface-funFEM
#' @inheritParams fitted.lcApproxModel
fitted.lcModelFunFEM = function(object, ..., clusters = trajectoryAssignments(object)) {
  times = time(object)
  newdata = data.table(Id = rep(ids(object), each = length(times)),
                       Time = times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))

  predict(object, newdata = newdata) %>%
    transformFitted(model = object, clusters)
}


#. predictForCluster ####
#' @rdname interface-funFEM
#' @inheritParams predictForCluster
setMethod('predictForCluster', signature('lcModelFunFEM'),
  function(object, newdata, cluster, what = 'mu', approxFun = approx, ...) {
  clusIdx = match(cluster, clusterNames(object))
  fdmeans = object@model$fd
  fdmeans$coefs = t(object@model$prms$my)

  predMat = fda::eval.fd(evalarg = newdata[[timeVariable(object)]], fdobj = fdmeans)
  predMat[, clusIdx]
})


#' @rdname interface-funFEM
setMethod('postprob', signature('lcModelFunFEM'), function(object, ...) {
  pp = object@model$P
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
#' @rdname interface-funFEM
coef.lcModelFunFEM = function(object, ...) {
  coefMat = t(object@model$prms$my)
  colnames(coefMat) = clusterNames(object)
  return(coefMat)
}


#' @export
#' @rdname interface-funFEM
logLik.lcModelFunFEM = function(object, ...) {
  ll = object@model$ll
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = object@model$nbprm / 2
  class(ll) = 'logLik'
  return(ll)
}

#' @rdname interface-funFEM
setMethod('converged', signature('lcModelFunFEM'), function(object, ...) {
  TRUE
})
