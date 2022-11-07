#' @include model.R modelPartition.R
setClass('lcModelLMKM',
  representation(
    coefNames = 'character',
    trajectoryCoefs = 'matrix'
  ),
  contains = 'lcModelPartition'
)


#' @export
#' @rdname interface-featureBased
#' @param cluster The cluster name.
coef.lcModelLMKM = function(object, ..., cluster = NULL) {
  coefmat = t(object@model$centers)
  colnames(coefmat) = clusterNames(object)
  rownames(coefmat) = object@coefNames

  if (is.null(cluster)) {
    return(coefmat)
  } else {
    assert_that(
      is.count(cluster) || is.character(cluster) && cluster %in% clusterNames(object)
    )
    coefmat[, cluster, drop = FALSE]
  }
}


#' @export
#' @rdname interface-featureBased
logLik.lcModelLMKM = function(object, ...) {
  assert_that(ncol(coef(object)) == nClusters(object))
  assert_that(noNA(object@trajectoryCoefs))

  muMat = t(coef(object))

  # subtract respective cluster center from the trajectory coefficients
  coefDiffMat = object@trajectoryCoefs - muMat[trajectoryAssignments(object), ]

  # compute density across trajectory, per coefficient
  ll_coef = apply(coefDiffMat, 2, function(x) sum(dnorm(x, sd = sd(x), log = TRUE)))
  ll = sum(ll_coef)

  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = nClusters(object) * ncol(object@model$centers)
  class(ll) = 'logLik'
  return(ll)
}


#. converged ####
#' @rdname interface-featureBased
setMethod('converged', signature('lcModelLMKM'), function(object, ...) {
  if (length(object@converged) > 0) {
    object@converged
  }
  else {
    not(object@model$ifault)
  }
})


#. postprob ####
#' @rdname interface-featureBased
setMethod('postprob', signature('lcModelLMKM'), function(object, ...) {
  k = nrow(object@model$centers)
  postprobFromAssignments(object@model$cluster, k)
})
