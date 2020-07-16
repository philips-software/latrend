#' @include model.R
setClass('lcModelMixTVEM', contains = 'lcModel')


#' @export
predict.lcModelMixTVEM = function(object,
                                  newdata = NULL,
                                  what = 'mu') {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')

  if (is.null(newdata)) {
    pred = object@model$bestFit$fittedY %>%
      set_colnames(clusterNames(object))
  } else {
    times = object@model$timeGrid
    newtimes = unique(newdata[[timeVariable(object)]])
    assert_that(all(newdata$Time %between% range(times)), msg = 'extrapolation is not supported')
    # TODO computation of beta at custom times
    trajMat = object@model$betaByGrid[[1]]
    predMat = apply(trajMat, 2, function(x)
      approx(times, x, newtimes)$y) %>%
      set_colnames(clusterNames(object))
    pred = data.table(Time = newtimes, G = predMat) %>%
      setnames(c(timeVariable(object), clusterNames(object))) %>%
      melt(
        id.vars = 'Time',
        value.name = 'Fit',
        variable.name = 'Cluster'
      )
  }

  transformPredict(pred = pred,
                   model = object,
                   newdata = newdata)
}


setMethod('postprob', signature('lcModelMixTVEM'), function(object) {
  pp = object@model$bestFit$postProbsBySub
  colnames(pp) = clusterNames(object)
  return(pp)
})


setMethod('converged', signature('lcModelMixTVEM'), function(object) {
  object@model$bestFit$converged
})


#' @export
logLik.lcModelMixTVEM = function(object) {
  ll = object@model$bestFit$logLik
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = object@model$bestFit$enp
  class(ll) = 'logLik'
  return(ll)
}


#' @export
sigma.lcModelMixTVEM = function(object) {
  sqrt(object@model$bestFit$sigsq.total) %>%
    weighted.mean(w = clusterProportions(object))
}


#' @export
coef.lcModelMixTVEM = function(object) {
  thetas = object@model$bestFit$theta %>%
    setNames(paste0('theta',
                    rep(1:ncol(.), each = nrow(.)),
                    '_',
                    rep(1:nrow(.), ncol(.))))
  sigmas = sqrt(object@model$bestFit$sigsq.total) %>%
    setNames(paste0('sigma', seq_along(.)))
  gammas = object@model$bestFit$gamma[1, ] %>%
    setNames(paste0('gamma', seq_along(.)))

  coefs = c(
    thetas,
    gammas,
    lambda = object@model$bestFit$lambda,
    rho = object@model$bestFit$rho,
    proportionNugget = object@model$bestFit$proportionNugget
  )
  return(coefs)
}
