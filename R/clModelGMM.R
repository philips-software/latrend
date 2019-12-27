#' @include clModel.R
setClass('clModelGMM', contains='clModel')

#' @export
logLik.clModelGMM = function(object) {
  ll = object@model$loglik
  N = nIds(object)
  df = length(coef(object))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}

#' @export
sigma.clModelGMM = function(object) {
  coef(object)['stderr'] %>% unname
}

setMethod('postprob', signature('clModelGMM'), function(object, newdata) {
  if(is.null(newdata)) {
    pp = object@model$pprob %>%
      as.matrix %>%
      .[, c(-1, -2)]
  } else {
    stop('not supported')
  }
  colnames(pp) = clusterNames(object)
  return(pp)
})

setMethod('converged', signature('clModelGMM'), function(object) {
  object@model$conv
})

setMethod('modelData', signature('clModelGMM'), function(object) {
  object@model$data
})

#' @export
#' @rdname clusterTrajectories
#' @param at The time points at which to compute the cluster trajectories.
setMethod('clusterTrajectories', signature('clModelGMM'), function(object, what, at) {
  time = getTimeName(object)
  vars = union(getCovariates(object@model$mixture), getCovariates(object@model$fixed))

  if(is.null(at)) {
    clusdata = modelData(object) %>%
      as.data.table %>%
      .[, lapply(.SD, mean), .SDcols=vars, keyby=c(time)]
  } else {
    clusdata = at
  }

  predmat = predictY(object@model, newdata=clusdata)$pred
  dt_ctraj = data.table(Cluster=rep(clusterNames(object, factor=TRUE), each=nrow(clusdata)),
                        Time=clusdata[[time]],
                        Value=as.numeric(predmat)) %>%
    setnames(c('Time', 'Value'), c(time, getResponseName(object)))
  return(dt_ctraj)
})

#' @export
#' @rdname trajectories
setMethod('trajectories', signature('clModelGMM'), function(object, what, at, clusters) {
  id = getIdName(object)

  if(is.null(at)) {
    predNames = paste0('pred_m', 1:nClus(object))
    dt_pred = object@model$pred[, c(id, predNames)] %>%
      as.data.table %>%
      .[, c(id) := modelIds(object)[get(id)]] %>%
      setnames(predNames, clusterNames(object))
    return(dt_pred[])
  } else {
    stop('not supported')
  }
})