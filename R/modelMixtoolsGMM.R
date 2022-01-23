#' @include model.R
setClass('lcModelMixtoolsGMM', contains = 'lcModel')


#. clusterTrajectories ####
#' @rdname interface-mixtools
#' @inheritParams predictForCluster
setMethod('predictForCluster', signature('lcModelMixtoolsGMM'), function(
  object, newdata, cluster, what = 'mu', ...)
{
  assert_that(what == 'mu')

  idVar = idVariable(object)
  fixed = dropResponse(object@model$fixed)
  random = object@model$random
  betaMat = object@model$mu

  Xfix = model.matrix(fixed, data = newdata)
  predFix = as.vector(Xfix %*% object@model$alpha)

  # compute fitted per cluster
  Xran = model.matrix(random, data = newdata)
  if (hasName(newdata, idVar)) {
    # patient-specific prediction
    XidList = split(Xran, newdata[[idVar]]) %>%
      lapply(matrix, ncol = ncol(Xran))
    ranefList = ranef.lcModelMixtoolsGMM(object) %>%
      asplit(2)
    assert_that(
      all(names(XidList) %in% names(ranefList)),
      msg = 'unknown Ids specified in newdata. prediction for new Ids is not supported')

    predMat = Map('%*%', XidList, ranefList[names(XidList)]) %>%
      do.call(rbind, .) + predFix
    assert_that(nrow(predMat) == nrow(newdata))
  } else {
    fitRan = apply(betaMat, 2, function(beta) Xran %*% beta) %>%
      set_colnames(clusterNames(object))
    predMat = fitRan + predFix
  }

  clusIdx = match(cluster, clusterNames(object))
  predMat[, clusIdx]
})


#' @rdname interface-mixtools
setMethod('postprob', signature('lcModelMixtoolsGMM'), function(object, ...) {
  pp = object@model$posterior.z
  colnames(pp) = clusterNames(object)
  return(pp)
})


#' @export
#' @rdname interface-mixtools
logLik.lcModelMixtoolsGMM = function(object, ...) {
  ll = object@model$loglik
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = coef(object) %>% lengths() %>% sum()
  class(ll) = 'logLik'
  return(ll)
}


#' @export
#' @rdname interface-mixtools
coef.lcModelMixtoolsGMM = function(object, ...) {
  return(
    list(
      alpha = object@model$alpha,
      beta = object@model$mu,
      cov = object@model$R,
      rho = object@model$rho,
      sigma = object@model$sigma
    )
  )
}


#' @export
#' @rdname interface-mixtools
sigma.lcModelMixtoolsGMM = function(object, ...) {
  object@model$sigma
}


ranef.lcModelMixtoolsGMM = function(object, ...) {
  betaNames = colnames(object@model$x[[1]])
  nBeta = length(betaNames)

  ranefMat = do.call(rbind, object@model$posterior.beta)
  assert_that(nrow(ranefMat) == nBeta * nIds(object))
  assert_that(ncol(ranefMat) == nClusters(object))

  array(
    ranefMat,
    dim = c(nBeta, nIds(object), nClusters(object)),
    dimnames = list(betaNames, ids(object), clusterNames(object))
  )
}
