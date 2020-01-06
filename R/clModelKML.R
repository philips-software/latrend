#' @include clModel.R
setClass('clModelKML', contains='clModel')


#' @export
#' @rdname predict.clModel
#' @inheritParams predict.clModel
#' @param approxFun The interpolation function to use for time points not in the feature set.
predict.clModelKML = function(object, newdata=NULL, what='mu', approxFun=approx) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg='only what="mu" is supported')
  assert_that(is.function(approxFun))

  # compute cluster trajectories
  trajMat = calculTrajMean(traj=object@model@traj,
                           clust=getClusters(object@model, nbCluster=nClusters(object)),
                           centerMethod=getMethod(object)$center)

  if(is.null(newdata)) {
    return(fitted(object))
  } else {
    assert_that(has_name(newdata, timeVariable(object)))
    newtimes = newdata[[timeVariable(object)]]
    predMat = apply(trajMat, 1, function(y) approxFun(x=time(object), y=y, xout=newtimes)$y)
  }

  transformPredict(object, predMat, newdata=newdata)
}


#' @export
fitted.clModelKML = function(object, clusters=clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id=rep(ids(object), each=length(times)),
                       Cluster=rep(clusters, each=length(times)),
                       Time=times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata=newdata)
}


#' @export
logLik.clModelKML = function(object) {
  # A negated version of BIC is precomputed by kml package so let's use that
  bic = -getKMLPartition(object)@criterionValues['BIC'] %>% unname
  N = nIds(object)
  df = nClusters(object) * length(time(object)) + 1
  ll = -.5 * (bic - df * log(N))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}


setMethod('converged', signature('clModelKML'), function(object) {
  TRUE
})


setMethod('postprob', signature('clModelKML'), function(object) {
  pp = getKMLPartition(object)@postProba
  colnames(pp) = clusterNames(object)
  return(pp)
})

getKMLPartition = function(object) {
  object@model[paste0('c', nClusters(object))][[1]]
}
