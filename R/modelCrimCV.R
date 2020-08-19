#' @include model.R
setClass('lcModelCrimCV', contains = 'lcModel')


setMethod('postprob', signature('lcModelCrimCV'), function(object) {
  pp = object@model$gwt
  colnames(pp) = clusterNames(object)
  return(pp)
})


#' @export
predict.lcModelCrimCV = function(object, ...,
                                 newdata = NULL,
                                 what = 'mean') {
  assert_that(is.newdata(newdata))
  assert_that(what %in% c('mu', 'nu', 'mean'))

  # compute cluster trajectories
  if (is.null(newdata)) {
    newdata = model.data(object)
  }
  assert_that(has_name(newdata, timeVariable(object)))
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
    nuMat = exp(-object@model$tau * t(Xmat)) %>% t
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

  transformPredict(pred = predMat,
                   model = object,
                   newdata = newdata)
}


fitted.lcModelCrimCV = function(object, ...,
                                clusters = clusterAssignments(object),
                                what = 'mean') {
  predict(object, newdata = NULL, what = what) %>%
    transformFitted(model = object, clusters = clusters)
}


#' @export
logLik.lcModelCrimCV = function(object, ...) {
  ll = object@model$llike
  attr(ll, 'nobs') = nIds(object) #crimCV uses nIds*nTime
  attr(ll, 'df') = length(coef(object)) + 1
  class(ll) = 'logLik'
  return(ll)
}


#' @export
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


setMethod('converged', signature('lcModelCrimCV'), function(object) {
  TRUE
})
