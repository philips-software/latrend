#' @include model.R
setClass('lcModelMixtoolsRM', contains = 'lcApproxModel')


#. clusterTrajectories ####
#' @rdname interface-mixtools
#' @inheritParams clusterTrajectories
#' @param se Whether to compute the standard error of the prediction.
#' @param ci The confidence interval to compute.
setMethod('clusterTrajectories', signature('lcModelMixtoolsRM'), function(
    object, at = time(object), what = 'mu', se = TRUE, ci = c(.025, .975), ...)
  {
  if (length(at) == 0) {
    assert_that(
      what %in% c('mu', 'sigma'),
      is.logical(se),
      is.null(ci) || is.numeric(ci)
    )

    respFun = switch(what, mu = mean, sigma = sd)
    if (isTRUE(se)) {
      seFun = function(x) sd(x, na.rm = TRUE) / sum(is.finite(x))
    } else {
      seFun = function(x) numeric(0)
    }

    # compute cluster trajectory
    times = time(object)
    blocks = object@model$blockid
    assert_that(length(blocks) == length(time(object)))

    comboMat = cbind(
      .Component = seq_len(nClusters(object)) %>% rep(each = length(blocks)),
      .Block = rep(blocks, nClusters(object))
    )

    statFun = function(com, block) {
      dd = density(
        object@model,
        component = com,
        block = block,
        scale = FALSE
      )

      c(
        Fit = respFun(dd$x, na.rm = TRUE),
        Se.fit = seFun(dd$x),
        Q = quantile(dd$x, ci)
      )
    }

    statMat = Map(statFun, comboMat[, 1], comboMat[, 2]) %>%
      do.call(rbind, .)

    dtStats = cbind(comboMat, statMat) %>%
      as.data.table() %>%
      .[, Time := times[.Block]] %>%
      .[, Cluster := factor(.Component, levels = seq_len(nClusters(object)), labels = clusterNames(object))] %>%
      .[, .Component := NULL] %>%
      .[, .Block := NULL] %>%
      setcolorder(c('Cluster', 'Time')) %>%
      setnames('Time', timeVariable(object)) %>%
      setnames('Fit', responseVariable(object))

    dtStats[]
  } else {
    callNextMethod()
  }
})


#' @rdname interface-mixtools
setMethod('postprob', signature('lcModelMixtoolsRM'), function(object, ...) {
  pp = object@model$posteriors
  colnames(pp) = clusterNames(object)
  return(pp)
})


#' @export
#' @rdname interface-mixtools
logLik.lcModelMixtoolsRM = function(object, ...) {
  ll = object@model$loglik
  attr(ll, 'nobs') = nIds(object)
  attr(ll, 'df') = length(coef(object)) + 1
  class(ll) = 'logLik'
  return(ll)
}

#' @rdname interface-mixtools
setMethod('converged', signature('lcModelMixtoolsRM'), function(object, ...) {
  TRUE
})
