#' @include model.R
setClass('lcModelMixAK_GLMM', contains = 'lcModel')


#. postprob ####
#' @rdname interface-mixAK
setMethod('postprob', 'lcModelMixAK_GLMM', function(object, ...) {
  pp = .postprob_GLMM_MCMC(object@model)
  colnames(pp) = clusterNames(object)
  pp
})

.postprob_GLMM_MCMC = function(model) {
  model$poster.comp.prob_b
}


#. predictForCluster ####
#' @rdname interface-mixAK
#' @inheritParams predictForCluster
setMethod('predictForCluster', 'lcModelMixAK_GLMM', function(object, newdata, cluster, what = 'mu', ...) {
  predictForCluster(object@model, cluster = cluster, newdata = newdata, what = what, ...)
})

.predictForCluster_GLMM_MCMC = function(model, method, k, newdata, ...) {
  assert_that(
    is.finite(k),
    is_newdata(newdata),
    !is.null(newdata)
  )

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

#' @rdname interface-mixAK
setMethod('predictForCluster', 'lcModelMixAK_GLMM', function(object, newdata, cluster, what = 'mu', ...) {
  .predictForCluster_GLMM_MCMC(object@model,
                               method = getLcMethod(object),
                               k = match(cluster, clusterNames(object)),
                               newdata = newdata,
                               ...)
})

#' @export
#' @rdname interface-mixAK
#' @param stat The aggregate statistic to extract. The mean is used by default.
coef.lcModelMixAK_GLMM = function(object, ..., stat = 'Mean') {
  coef(object@model, stat = stat)
}

coef.GLMM_MCMC = function(object, ..., stat = 'Mean') {
  c(object$summ.b.Mean[stat, ],
    object$summ.b.SDCorr[stat, ],
    sigma_eps = unname(object$summ.sigma_eps[stat]))
}

#' @export
#' @rdname interface-mixAK
deviance.lcModelMixAK_GLMM = function(object, ...) {
  deviance(object@model)
}

deviance.GLMM_MCMC = function(object, ...) {
  mean(object$Deviance)
}
