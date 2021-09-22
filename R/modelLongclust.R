#' @include model.R
setClass('lcModelLongclust', contains='lcApproxModel')


#. clusterTrajectories ####
#' @rdname interface-longclust
#' @inheritParams clusterTrajectories
#' @inheritParams predictForCluster
setMethod('clusterTrajectories', signature('lcModelLongclust'), function(object, at = time(object), ...) {
  if (length(at) == 0) {
    trajMat = object@model$mubest
    assert_that(
      is.matrix(trajMat),
      nrow(trajMat) > 0,
      msg='empty estimate for mu. model probably did not converge'
    )

    dtclus = meltRepeatedMeasures(
      trajMat,
      times = time(object),
      id = 'Cluster',
      time = timeVariable(object),
      response = responseVariable(object),
      as.data.table = TRUE)

    dtclus[, Cluster := factor(Cluster, levels = seq_len(nClusters(object)), labels = clusterNames(object))]
    dtclus
  } else {
    callNextMethod()
  }
})


#' @rdname interface-longclust
setMethod('postprob', signature('lcModelLongclust'), function(object, ...) {
  pp = object@model$zbest
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @rdname interface-longclust
setMethod('converged', signature('lcModelLongclust'), function(object, ...) {
  object@model$Gbest > 0
})


#' @export
#' @rdname interface-longclust
logLik.lcModelLongclust = function(object, ...) {
  logLiks = -object@model$llres
  if(length(logLiks) == 1) {
    ll = logLiks
  } else {
    # determine best model based on criteria
    bestIdx = switch(tolower(getLcMethod(object)$criteria),
           bic={which.max(object@model$bicres)},
           icl={which.max(object@model$iclres)})
    ll = logLiks[bestIdx]
  }
  N = nIds(object)
  attr(ll, 'nobs') = N
  class(ll) = 'logLik'
  return(ll)
}


#' @export
#' @rdname interface-longclust
BIC.lcModelLongclust = function(object, ...) {
  bics = -object@model$bicres
  if(length(bics) == 1) {
    bics
  } else {
    # determine best model based on criteria
    bestIdx = switch(tolower(getLcMethod(object)$criteria),
                     bic={which.max(object@model$bicres)},
                     icl={which.max(object@model$iclres)})
    bics[bestIdx]
  }
}
