#' @include clApproxModel.R
setClass('clModelKML', contains='clApproxModel')

#. clusterTrajectories ####
setMethod('clusterTrajectories', signature('clModelKML'), function(object, what, at, ...) {
  if(is.null(at)) {
    trajMat = calculTrajMean(traj=object@model@traj,
                             clust=getClusters(object@model, nbCluster=nClusters(object)),
                             centerMethod=getMethod(object)$centerMethod)

    if(!is.matrix(trajMat)) {
      trajMat = matrix(trajMat, nrow=1)
      rownames(trajMat) = clusterNames(object)
    }

    meltRepeatedMeasures(trajMat,
                         times=time(object),
                         id='Cluster',
                         time=timeVariable(object),
                         response=responseVariable(object))
  } else {
    callNextMethod()
  }
})


#. converged ####
setMethod('converged', signature('clModelKML'), function(object) {
  TRUE
})


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


#. postprob ####
setMethod('postprob', signature('clModelKML'), function(object) {
  if(nClusters(object) == 1) {
    pp = matrix(1, nrow=nIds(object), ncol=1)
  } else {
    pp = getKMLPartition(object)@postProba
  }
  colnames(pp) = clusterNames(object)
  return(pp)
})



getKMLPartition = function(object) {
  object@model[paste0('c', nClusters(object))][[1]]
}
