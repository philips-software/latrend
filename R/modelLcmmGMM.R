#' @include model.R
setClass('lcModelLcmmGMM', contains = 'lcModel')

#' @export
#' @rdname interface-lcmm
#' @inheritParams fitted.lcModel
fitted.lcModelLcmmGMM = function(object, ..., clusters = trajectoryAssignments(object)) {
  predNames = paste0('pred_ss', 1:nClusters(object))
  predMat = object@model$pred[predNames] %>%
    as.matrix() %>%
    set_colnames(clusterNames(object))
  transformFitted(predMat, model = object, clusters = clusters)
}


#. predictForCluster ####
#' @rdname interface-lcmm
#' @inheritParams predictForCluster
setMethod('predictForCluster', signature('lcModelLcmmGMM'), function(
    object, newdata, cluster, what = 'mu', ...)
{
  assert_that(what == 'mu', msg = 'only what="mu" is supported')
  vars = union(getCovariates(object@model$fixed), getCovariates(object@model$mixture))
  missingVars = setdiff(vars, names(newdata))

  if (nrow(newdata) == 0) {
    return(numeric())
  }

  if (length(missingVars) > 0) {
    # compute marginal means for unspecified covariates
    missingVarMeans = model.data(object) %>%
      .[missingVars] %>%
      vapply(mean, na.rm = TRUE, FUN.VALUE = 0)
    newdata = cbind(newdata, missingVarMeans)
    newdata$Cluster = make.clusterIndices(object, newdata$Cluster)
  }

  predMat = lcmm::predictY(object@model, newdata = newdata)$pred %>%
    set_colnames(clusterNames(object))

  clusIdx = match(cluster, clusterNames(object))

  predMat[, clusIdx]
})


#' @export
#' @rdname interface-lcmm
model.matrix.lcModelLcmmGMM = function(object, ..., what = 'mu') {
  if (what == 'mu') {
    f = merge.formula(object@model$fixed, object@model$mixture)
    model.matrix(f, data = model.data(object))
  } else {
    model.matrix(object@model$mb, data = model.data(object))
  }
}


#' @export
#' @rdname interface-lcmm
logLik.lcModelLcmmGMM = function(object, ...) {
  ll = object@model$loglik
  N = nIds(object)
  df = length(coef(object))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}


#' @export
#' @rdname interface-lcmm
sigma.lcModelLcmmGMM = function(object, ...) {
  coef(object)['stderr'] %>% unname()
}


#' @rdname interface-lcmm
setMethod('postprob', signature('lcModelLcmmGMM'), function(object, ...) {
  pp = object@model$pprob %>%
    as.matrix() %>%
    .[, c(-1, -2), drop = FALSE]
  colnames(pp) = clusterNames(object)
  return(pp)
})


#' @rdname interface-lcmm
setMethod('converged', signature('lcModelLcmmGMM'), function(object, ...) {
  object@model$conv
})
