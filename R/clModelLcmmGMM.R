#' @include clModel.R
setClass('clModelLcmmGMM', contains = 'clModel')

#' @export
fitted.clModelLcmmGMM = function(object, clusters = clusterAssignments(object)) {
  predNames = paste0('pred_m', 1:nClusters(object))
  predMat = object@model$pred[predNames] %>%
    as.matrix %>%
    set_colnames(clusterNames(object))
  transformFitted(predMat, model = object, clusters = clusters)
}

#' @export
predict.clModelLcmmGMM = function(object,
                                  newdata = NULL,
                                  what = 'mu') {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')

  if (is.null(newdata)) {
    predMat = fitted(object, clusters = NULL)
  } else {
    vars = union(getCovariates(object@model$fixed),
                 getCovariates(object@model$mixture))
    missingVars = setdiff(vars, names(newdata))

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
  }

  transformPredict(pred = predMat,
                   model = object,
                   newdata = newdata)
}

#' @export
model.matrix.clModelLcmmGMM = function(object, what = 'mu') {
  if (what == 'mu') {
    f = merge.formula(object@model$fixed, object@model$mixture)
    model.matrix(f, data = model.data(object))
  } else {
    model.matrix(object@model$mb, data = model.data(object))
  }
}

#' @export
logLik.clModelLcmmGMM = function(object) {
  ll = object@model$loglik
  N = nIds(object)
  df = length(coef(object))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}

#' @export
sigma.clModelLcmmGMM = function(object) {
  coef(object)[grepl('std err', names(coef(object@model)))] %>% unname
}

setMethod('postprob', signature('clModelLcmmGMM'), function(object) {
  pp = object@model$pprob %>%
    as.matrix %>%
    .[, c(-1, -2), drop = FALSE]
  colnames(pp) = clusterNames(object)
  return(pp)
})

setMethod('converged', signature('clModelLcmmGMM'), function(object) {
  object@model$conv
})
