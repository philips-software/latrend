#' @include model.R
setClass('lcModelCrimCV', contains = 'lcModel')


#. predictForCluster ####
#' @rdname interface-crimCV
#' @inheritParams predictForCluster
setMethod('predictForCluster', signature('lcModelCrimCV'), function(
  object, newdata, cluster, what = 'mu', ...)
{
  assert_that(what %in% c('mu', 'nu', 'mean'))

  if (nrow(newdata) == 0) {
    return(numeric())
  }

  newtime = (newdata[[timeVariable(object)]] - object@model$minTime) / object@model$durTime

  X = splines::bs(
    x = newtime,
    degree = getLcMethod(object)$dpolyp,
    intercept = TRUE,
    Boundary.knots = c(0, 1)
  )
  Xmat = X %*% object@model$beta
  lambdaMat = exp(Xmat)

  if (hasName(object@model, 'tau')) {
    nuMat = exp(-object@model$tau * t(Xmat)) %>% t()
  } else {
    Zmat = splines::bs(
      x = newtime,
      degree = getLcMethod(object)$dpolyl,
      intercept = TRUE,
      Boundary.knots = c(0, 1)
    )
    nuMat = exp(Zmat %*% object@model$gamma)
  }
  nuMat = nuMat / (1 + nuMat)

  predMat = switch(what,
    mu = lambdaMat,
    nu = nuMat,
    mean = (1 - nuMat) * lambdaMat)

  clusIdx = match(cluster, clusterNames(object))
  predMat[, clusIdx]
})


#' @rdname interface-crimCV
#' @keywords internal
setMethod('postprob', signature('lcModelCrimCV'), function(object) {
  pp = object@model$gwt
  colnames(pp) = clusterNames(object)
  return(pp)
})


#' @export
#' @rdname interface-crimCV
logLik.lcModelCrimCV = function(object, ...) {
  ll = object@model$llike
  attr(ll, 'nobs') = nIds(object) #crimCV uses nIds*nTime
  attr(ll, 'df') = length(coef(object)) + 1
  class(ll) = 'logLik'
  return(ll)
}


#' @export
#' @rdname interface-crimCV
coef.lcModelCrimCV = function(object, ...) {
  betaMat = object@model$beta
  colnames(betaMat) = clusterNames(object)
  rownames(betaMat) = paste0('beta', seq_len(nrow(betaMat)) - 1)

  if (hasName(object@model, 'tau')) {
    tau = object@model$tau
    tauMat = matrix(tau, nrow = length(tau), ncol = nClusters(object))
    rownames(tauMat) = paste0('tau', seq_along(tau))
    coefMat = rbind(betaMat, tauMat)
  } else {
    gammaMat = object@model$gamma
    rownames(gammaMat) = paste0('gamma', seq_len(nrow(gammaMat)) - 1)
    coefMat = rbind(betaMat, gammaMat)
  }
  return(coefMat)
}


#' @rdname interface-crimCV
setMethod('converged', signature('lcModelCrimCV'), function(object) {
  TRUE
})
