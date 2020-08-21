#' @include model.R
setClass('lcModelMixAK_GLMM', contains = 'lcModel')


#. postprob ####
setMethod('postprob', signature('lcModelMixAK_GLMM'), function(object) {
  pp = .postprob_GLMM_MCMC(object@model)
  colnames(pp) = clusterNames(object)
  pp
})

.postprob_GLMM_MCMC = function(model) {
  model$poster.comp.prob_b
}


#. predictForCluster ####
setMethod('predictForCluster', signature('lcModelMixAK_GLMM'), function(object, newdata, cluster, what = 'mu', ...) {
  predictForCluster(object@model, cluster = cluster, newdata = newdata, what = what, ...)
})

.predictForCluster_GLMM_MCMC = function(model, method, k, newdata, ...) {
  assert_that(is.finite(k),
              is.newdata(newdata),
              !is.null(newdata))

  # fixed design
  x = dropResponse(method$fixed) %>%
    dropIntercept() %>%
    model.matrix(newdata)

  # random design
  z = dropIntercept(method$random) %>%
    model.matrix(newdata)

  pred = fitted(model, x, z, ...)[[1]]
  pred[, k]
}

setMethod('predictForCluster', signature('lcModelMixAK_GLMM'), function(object, newdata, cluster, what = 'mu', ...) {
  .predictForCluster_GLMM_MCMC(object@model,
                               method = getLcMethod(object),
                               k = match(cluster, clusterNames(object)),
                               newdata = newdata,
                               ...)
})


coef.lcModelMixAK_GLMM = function(object, ..., stat = 'Mean') {
  coef(object@model, stat = stat)
}

coef.GLMM_MCMC = function(object, ..., stat = 'Mean') {
  c(object$summ.b.Mean[stat, ],
    object$summ.b.SDCorr[stat, ],
    sigma_eps = unname(object$summ.sigma_eps[stat]))
}

deviance.lcModelMixAK_GLMM = function(object, ...) {
  deviance(object@model)
}

deviance.GLMM_MCMC = function(object, ...) {
  mean(object$Deviance)
}
