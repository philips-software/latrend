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

})

#' @export
#' @rdname trajectories
setMethod('trajectories', signature('clModelGMM'), function(object, what, at) {
  dt_ctraj = clusterTrajectories(object, what=what, at=at, approxFun=approxFun)
  clusters = clusterAssignments(object)


})