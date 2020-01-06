#' @include clModel.R
setClass('clModelGMM', contains='clModel')

#' @export
fitted.clModelGMM = function(object, clusters=clusterAssignments(object)) {
  predNames = paste0('pred_m', 1:nClusters(object))
  predMat = object@model$pred[predNames] %>%
    as.matrix %>%
    set_colnames(clusterNames(object))
  transformFitted(object, predMat, clusters=clusters)
}

#' @export
#' @importFrom lcmm predictY
predict.clModelGMM = function(object, newdata=NULL, what='mu') {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg='only what="mu" is supported')

  if(is.null(newdata)) {
    predMat = fitted(object)
  } else {
    vars = union(getCovariates(object@model$fixed),
                 getCovariates(object@model$mixture))
    missingVars = setdiff(vars, names(newdata))

    if(length(missingVars) > 0) {
      # compute marginal means for unspecified covariates
      missingVarMeans = model.data(object) %>%
        .[missingVars] %>%
        sapply(mean, na.rm=TRUE)
      newdata = cbind(newdata, missingVarMeans)
      newdata$Cluster = make.clusterIndices(object, newdata$Cluster)
    }

    predMat = predictY(object@model, newdata=newdata)$pred %>%
      set_colnames(clusterNames(object))
  }

  transformPredict(object, predMat, newdata=newdata)
}

#' @export
model.matrix.clModelGMM = function(object, what='mu') {
  if(what == 'mu') {
    f = merge.formula(object@model$fixed, object@model$mixture)
    model.matrix(f, data=model.data(object))
  } else {
    model.matrix(object@model$mb, data=model.data(object))
  }
}

#' @export
logLik.clModelGMM = function(object) {
  ll = object@model$loglik
  N = nIds(object)
  df = length(coef(object))
  attr(ll, 'nobs') = N
  attr(ll, 'df') = df
  class(ll) = 'logLik'
  return(ll)
}

#' @export
sigma.clModelGMM = function(object) {
  coef(object)['stderr'] %>% unname
}

setMethod('postprob', signature('clModelGMM'), function(object) {
  pp = object@model$pprob %>%
      as.matrix %>%
      .[, c(-1, -2)]
  colnames(pp) = clusterNames(object)
  return(pp)
})

setMethod('converged', signature('clModelGMM'), function(object) {
  object@model$conv
})

