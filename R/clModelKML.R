#' @include clModel.R
setClass('clModelKML', contains='clModel')

fitted.clModelKML = function(object, ...) {

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

  data = data.table(Id=rep(object@model@idAll, each=length(cld@time)),
                    Time=modelTime(object),
                    Value=as.numeric(object@model@traj)) %>%
    setnames(c(id, time, resp))
  return(data)
})


setMethod('modelTime', signature('clModelKML'), function(object) {
  object@model@time
})


setMethod('groupTrajectories', signature('clModelKML'), function(object, what, at) {
  print('testy')
  print(what)
})


