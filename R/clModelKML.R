#' @include clModel.R
setClass('clModelKML', contains='clModel')

fitted.clModelKML = function(object, what='mu') {

}


nobs.clModelKML = function(object) {
  length(object@model@traj)
}


setMethod('pp', signature('clModelKML'), function(object, newdata) {
  if(is.null(newdata)) {
    pp = slot(object@model, 'c2')[[1]]@postProba
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
  times = modelTime(object)

  data = data.table(Id=rep(object@model@idAll, each=length(times)),
                    Time=times,
                    Value=as.numeric(object@model@traj)) %>%
    setnames(c(id, time, resp))
  return(data)
})


setMethod('modelTime', signature('clModelKML'), function(object) {
  object@model@time
})


setMethod('groupTrajectories', signature('clModelKML'), function(object) {
  trajmat = calculTrajMean(traj=object@model@traj,
                 clust=getClusters(object@model, nbCluster=nClus(object)),
                 centerMethod=getMethod(object)$center)

  times = modelTime(object)
  dt_traj = data.table(Cluster=rep(clusterNames(object), each=length(times)),
                       Time=rep(times, nClus(object)),
                       Value=as.numeric(t(trajmat))) %>%
    setnames(c('Time', 'Value'), c(getTimeName(object), getResponseName(object)))
  return(dt_traj)
})

setMethod('trajectories', signature('clModelKML'), fitted)