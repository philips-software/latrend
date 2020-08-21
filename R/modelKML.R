#' @include modelApprox.R
setClass('lcModelKML', contains = 'lcApproxModel')

#. clusterTrajectories ####
setMethod('clusterTrajectories', signature('lcModelKML'), function(object, at = time(object), ...) {
  if (is.null(at)) {
    trajMat = computeKMLCenters(object)

    meltRepeatedMeasures(
      trajMat,
      times = time(object),
      id = 'Cluster',
      time = timeVariable(object),
      response = responseVariable(object)
    )
  } else {
    callNextMethod()
  }
})


#. converged ####
#' @noRd
setMethod('converged', signature('lcModelKML'), function(object) {
  TRUE
})


#' @export
logLik.lcModelKML = function(object, ...) {
  # A negated version of BIC is precomputed by kml package so let's use that
  bic = -getKMLPartition(object)@criterionValues['BIC'] %>% unname()
  N = nIds(object)
  df = nClusters(object) * length(time(object)) + 1
  ll = .5 * (bic - df * log(N))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}


#. postprob ####
setMethod('postprob', signature('lcModelKML'), function(object) {
  if (nClusters(object) == 1) {
    pp = matrix(1, nrow = nIds(object), ncol = 1)
  } else {
    pp = getKMLPartition(object)@postProba
  }
  colnames(pp) = clusterNames(object)
  return(pp)
})


#. predictPostprob
setMethod('predictPostprob', signature('lcModelKML'), function(object, newdata, ...) {
  assert_that(has_name(newdata, idVariable(object)),
              has_name(newdata, timeVariable(object)),
              all(newdata[[timeVariable(object)]] %in% time(object)))

  valueColumn = responseVariable(object)
  centerMat = computeKMLCenters(object)
  distFun = getLcMethod(object)$distance
  times = time(object)

  if (is.null(formalArgs(distFun))) {
    affectFun = function(traj, centers) {
      kml::affectIndivC(traj = traj, clustersCenter = centers)
    }
  } else {
    affectFun = function(traj, centers) {
      kml::affectIndiv(traj = traj, clustersCenter = centers, distance = distFun)
    }
  }

  trajFun = function(trajData) {
    traj = trajData[[valueColumn]] %>% matrix(nrow=1)
    trajTimes = trajData[[timeVariable(object)]]

    affectFun(traj, centerMat[, match(trajTimes, times)]) %>%
      rep(nrow(trajData))
  }

  dtAffect = as.data.table(newdata)[, .(Cluster = trajFun(.SD)), by=c(idVariable(object))]

  pp = postprobFromAssignments(dtAffect$Cluster, nClusters(object))
  colnames(pp) = clusterNames(object)
  pp
})


getKMLPartition = function(object) {
  object@model[paste0('c', nClusters(object))][[1]]
}

computeKMLCenters = function(object) {
  centerMat = kml::calculTrajMean(
    traj = object@model@traj,
    clust = kml::getClusters(object@model, nbCluster = nClusters(object)),
    centerMethod = getLcMethod(object)$centerMethod
  )

  if (!is.matrix(centerMat)) {
    centerMat = matrix(centerMat, nrow = 1)
    rownames(centerMat) = clusterNames(object)
  }
  return(centerMat)
}
