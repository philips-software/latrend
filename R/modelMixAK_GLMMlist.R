#' @include model.R
setClass('lcModelMixAK_GLMMlist', contains = 'lcModel')

#' @rdname interface-mixAK
setMethod('postprob', 'lcModelMixAK_GLMMlist', function(object, ...) {
  models = getGLMM_MCMCs(object)
  pp = Reduce('+', lapply(models, .postprob_GLMM_MCMC)) / length(models)
  colnames(pp) = clusterNames(object)
  pp
})

#' @rdname interface-mixAK
setMethod('predictForCluster', 'lcModelMixAK_GLMMlist', function(
  object, newdata, cluster, what = 'mu', ...
) {
  models = getGLMM_MCMCs(object)
  k = match(cluster, clusterNames(object))

  Reduce(
    '+',
    lapply(
      models,
      .predictForCluster_GLMM_MCMC,
      k = k,
      method = getLcMethod(object),
      newdata = newdata,
      ...
    )
  ) / length(models)
})


#' @exportS3Method stats::coef
coef.lcModelMixAK_GLMMlist = function(object, ...) {
  models = getGLMM_MCMCs(object)
  Reduce('+', lapply(models, coef, ...)) / length(models)
}


#' @exportS3Method stats::deviance
deviance.lcModelMixAK_GLMMlist = function(object) {
  models = getGLMM_MCMCs(object)
  Reduce('+', lapply(models, deviance)) / length(models)
}


getGLMM_MCMCs = function(object) {
  object@model[which(vapply(names(object@model), nchar, FUN.VALUE = 0) == 0)]
}
