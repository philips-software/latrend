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

setMethod('pp', signature('clModelGMM'), function(object, newdata) {
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
setMethod('trajectories', signature('clModelGMM'), function(object, what, at) {
  dt_ctraj = clusterTrajectories(object, what=what, at=at, approxFun=approxFun)
  clusters = clusterAssignments(object)

  if(is.null(at)) {
    newdata = modelData(object)
  } else {
    newdata = at
  }

  subIdx = cbind(seq_along(clusters), as.integer(clusters))
  predmat = predictY(object@model, newdata=newdata)$pred[, subIdx]
})