#' @include clModel.R
setClass('clModelKML', contains='clModel')

#' @export
fitted.clModelKML = function(object) {
  dt_ctraj = clusterTrajectories(object)
  clusters = getClusters(object@model, nClus(object))

  dt_ctraj[[getResponseName(object)]] %>%
    matrix(ncol=nClus(object)) %>%
    .[, as.integer(clusters)] %>%
    as.numeric
}


#' @export
logLik.clModelKML = function(object) {
  # A negated version of BIC is precomputed by kml package so let's use that
  bic = -getKMLPartition(object)@criterionValues['BIC'] %>% unname
  N = nIds(object)
  df = nClus(object) * length(modelTimes(object)) + 1
  ll = -.5 * (bic - df * log(N))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}


setMethod('pp', signature('clModelKML'), function(object, newdata) {
  if(is.null(newdata)) {
    pp = getKMLPartition(object)@postProba
  } else {
    stop('not supported')
  }
  colnames(pp) = clusterNames(object)
  return(pp)
})


setMethod('modelData', signature('clModelKML'), function(object) {
  resp = getResponseName(object)
  id = getIdName(object)
  time = getTimeName(object)
  times = modelTimes(object)

  data = data.table(Id=rep(object@model@idAll, each=length(times)),
                    Time=times,
                    Value=as.numeric(object@model@traj)) %>%
    setnames(c(id, time, resp))
  return(data)
})


setMethod('modelTimes', signature('clModelKML'), function(object) {
  object@model@time
})

#' @export
#' @rdname clusterTrajectories
#' @param at The time points at which to compute the cluster trajectories.
#' @param approxFun The interpolation function to use for time points not in the feature set.
setMethod('clusterTrajectories', signature('clModelKML'), function(object, what, at, approxFun=approx) {
  trajmat = calculTrajMean(traj=object@model@traj,
                 clust=getClusters(object@model, nbCluster=nClus(object)),
                 centerMethod=getMethod(object)$center)

  times = modelTimes(object)

  if(!is.null(at)) {
    assert_that(all(is.numeric(at)))
    assert_that(all(is.finite(at)))
    trajmat = apply(trajmat, 1, function(y) approxFun(x=times, y=y, xout=at)$y) %>% t
    times = at
  }

  dt_traj = data.table(Cluster=rep(clusterNames(object, factor=TRUE), each=length(times)),
                       Time=rep(times, nClus(object)),
                       Value=as.numeric(t(trajmat))) %>%
    setnames(c('Time', 'Value'), c(getTimeName(object), getResponseName(object)))
  return(dt_traj)
})

#' @export
#' @rdname trajectories
#' @param approxFun The interpolation function to use for time points not in the feature set.
setMethod('trajectories', signature('clModelKML'), function(object, what, at, approxFun=approx) {
  dt_ctraj = clusterTrajectories(object, what=what, at=at, approxFun=approxFun)
  clusters = clusterAssignments(object)
  ntime = nrow(dt_ctraj) / nClus(object)
  id = getIdName(object)

  idx = dt_ctraj[, .I] %>%
    matrix(ncol=nClus(object)) %>%
    .[, as.integer(clusters)]

  dt_traj = dt_ctraj[as.vector(idx)] %>%
    .[, c(id) := rep(object@model@idAll, each=ntime)] %>%
    setkeyv(id) %>%
    setcolorder

  return(dt_traj[]) #[] to ensure the return table is printed
})


getKMLPartition = function(object) {
  object@model[paste0('c', nClus(object))][[1]]
}
