#' @include clModel.R
setClass('clModelMixTVEM', contains='clModel')

#' @export
logLik.clModelMixTVEM = function(object) {
  ll = object@model$bestFit$logLik
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = object@model$bestFit$enp
  class(ll) = 'logLik'
  return(ll)
}

#' @export
coef.clModelMixTVEM = function(object) {
  thetas = object@model$bestFit$theta %>%
    setNames(paste0('theta',
                    rep(1:ncol(.), each=nrow(.)),
                    '_',
                    rep(1:nrow(.), ncol(.))))
  sigmas = sqrt(object@model$bestFit$sigsq.total) %>%
    setNames(paste0('sigma', seq_along(.)))
  gammas = object@model$bestFit$gamma[1,] %>%
    setNames(paste0('gamma', seq_along(.)))

  coefs = c(thetas,
            gammas,
            lambda=object@model$bestFit$lambda,
            rho=object@model$bestFit$rho,
            proportionNugget=object@model$bestFit$proportionNugget)
  return(coefs)
}

#' @export
fitted.clModelMixTVEM = function(object, clusters=clusterAssignments(object)) {
  predMat = object@model$bestFit$fittedY
  colnames(predMat) = clusterNames(object)
  transformFitted(object, predMat, clusters=clusters)
}


#' @export
sigma.clModelMixTVEM = function(object) {
  sqrt(object@model$bestFit$sigsq.total) %>%
    weighted.mean(w=clusterProportions(object))
}


#' @export
predict.clModelMixTVEM = function(object, newdata) {
  # construct design matrix
  stop('not implemented')
}


setMethod('postprob', signature('clModelMixTVEM'), function(object) {
  pp = object@model$bestFit$postProbsBySub
  colnames(pp) = clusterNames(object)
  return(pp)
})


setMethod('converged', signature('clModelMixTVEM'), function(object) {
  object@model$bestFit$converged
})

setMethod('modelData', signature('clModelMixTVEM'), function(object) {
  data.table(Id=object@model$id, Value=object@model$dep, object@model$covarMats)
})

#' @export
#' @rdname clusterTrajectories
setMethod('clusterTrajectories', signature('clModelMixTVEM'), function(object, what, at) {
  stop('not implemented')
})

#' @export
#' @rdname trajectories
setMethod('trajectories', signature('clModelMixTVEM'), function(object, what, at, clusters) {
  id = getIdName(object)

  if(is.null(at)) {
    fitmat = fitted(object)
    rowClusters = rleid(modelIds(object))
    dt_pred = as.data.table(fitmat[cbind(1:nrow(fitmat), rowClusters)]) %>%
      .[, Id := modelIds(object)[get(id)]] %>%
      setcolorder('Id') %>%
      setnames(c(id, clusterNames(object)))
    return(dt_pred[])
  } else {
    stop('not supported')
  }
})