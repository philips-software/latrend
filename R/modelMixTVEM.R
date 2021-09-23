#' @include model.R
setClass('lcModelMixTVEM', contains = 'lcModel')


#' @export
#' @importFrom stats approx
#' @rdname interface-mixtvem
#' @inheritParams predict.lcModel
predict.lcModelMixTVEM = function(object, ...,
                                  newdata = NULL,
                                  what = 'mu') {
  assert_that(is_newdata(newdata))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')

  if (is.null(newdata)) {
    newdata = model.data(object)
    newdata$Cluster = NULL
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
        id.vars = timeVariable(object),
        value.name = 'Fit',
        variable.name = 'Cluster'
      )
  }

  transformPredict(
    pred = pred,
    model = object,
    newdata = newdata
  )
}

#' @rdname interface-mixtvem
setMethod('postprob', signature('lcModelMixTVEM'), function(object, ...) {
  pp = object@model$bestFit$postProbsBySub
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @rdname interface-mixtvem
setMethod('converged', signature('lcModelMixTVEM'), function(object, ...) {
  object@model$bestFit$converged
})


#' @export
#' @rdname interface-mixtvem
logLik.lcModelMixTVEM = function(object, ...) {
  ll = object@model$bestFit$logLik
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = object@model$bestFit$enp
  class(ll) = 'logLik'
  return(ll)
}


#' @export
#' @rdname interface-mixtvem
sigma.lcModelMixTVEM = function(object, ...) {
  sqrt(object@model$bestFit$sigsq.total) %>%
    weighted.mean(w = clusterProportions(object))
}


#' @export
#' @rdname interface-mixtvem
coef.lcModelMixTVEM = function(object, ...) {
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
