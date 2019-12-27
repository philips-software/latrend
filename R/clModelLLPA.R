#' @include clModel.R
setClass('clModelLLPA', contains='clModel')

setMethod('pp', signature('clModelLLPA'), function(object, newdata) {
  if(is.null(newdata)) {
    pp = object@model$z
  } else {
    pp = predict(object@model, newdata=newdata)$z
  }
  colnames(pp) = clusterNames(object)
  return(pp)
})

setMethod('modelIds', signature('clModelLLPA'), function(object) {
  object@model$data %>% rownames
})

setMethod('modelTimes', signature('clModelLLPA'), function(object) {
  object@model$time
})

setMethod('modelData', signature('clModelLLPA'), function(object) {
  resp = getResponseName(object)
  id = getIdName(object)
  time = getTimeName(object)
  times = modelTimes(object)

  data = data.table(Id=rep(modelIds(object), each=length(times)),
                    Time=times,
                    Value=as.numeric(object@model$data)) %>%
    setnames(c(id, time, resp))
  return(data)
})


#' @export
#' @rdname clusterTrajectories
#' @param at The time points at which to compute the cluster trajectories.
#' @param approxFun The interpolation function to use for time points not in the feature set.
setMethod('clusterTrajectories', signature('clModelLLPA'), function(object, what, at, approxFun=approx) {
  trajmat = object@model$parameters$mean
  times = modelTimes(object)

  if(!is.null(at)) {
    assert_that(all(is.numeric(at)))
    assert_that(all(is.finite(at)))
    trajmat = apply(trajmat, 2, function(y) approxFun(x=times, y=y, xout=at)$y) %>% t
    times = at
  }

  dt_traj = data.table(Cluster=rep(clusterNames(object, factor=TRUE), each=length(times)),
                       Time=rep(times, nClus(object)),
                       Value=as.numeric(trajmat)) %>%
    setnames(c('Time', 'Value'), c(getTimeName(object), getResponseName(object)))
  return(dt_traj)
})

#' @export
#' @rdname trajectories
#' @param approxFun The interpolation function to use for time points not in the feature set.
setMethod('trajectories', signature('clModelLLPA'), function(object, what, at, clusters, approxFun=approx) {
  dt_ctraj = clusterTrajectories(object, what=what, at=at, approxFun=approxFun)
  ntime = nrow(dt_ctraj) / nClus(object)
  id = getIdName(object)

  idx = dt_ctraj[, .I] %>%
    matrix(ncol=nClus(object)) %>%
    .[, as.integer(clusters)]

  dt_traj = dt_ctraj[as.vector(idx)] %>%
    .[, c(id) := rep(modelIds(object), each=ntime)] %>%
    setkeyv(id) %>%
    setcolorder

  return(dt_traj[])
})