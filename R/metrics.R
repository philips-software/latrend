#' @include clModel.R
metricsEnv = new.env()

#' @export
#' @title Get the names of the available metrics
getModelMetricNames = function() {
  names(metricsEnv) %>% sort
}

#' @export
#' @title Define a metric for clModels
#' @param name The name of the metric.
#' @param fun The function to compute the metric, accepting a clModel object as input.
#' @param warnIfExists Whether to output a warning when the new metric is already defined.
defineModelMetric = function(name, fun, warnIfExists=TRUE) {
  assert_that(is.function(fun))
  assert_that(!is.null(formalArgs(fun)), msg='function must accept one argument (a clModel)')

  if(warnIfExists && exists(name, envir=metricsEnv)) {
    warning(sprintf('ovewriting existing metric definition for %s', name))
  }

  assign(name, value=fun, envir=metricsEnv)
}

#' @export
#' @title Get the metric definition
#' @inheritParams defineModelMetric
#' @return The metric function, or NULL if not defined.
getModelMetricDefinition = function(name) {
  if(exists(name, envir=metricsEnv)) {
    get(name, envir=metricsEnv)
  } else {
    NULL
  }
}

metricsEnv$AIC = AIC
metricsEnv$BIC = BIC
metricsEnv$logLik = logLik
metricsEnv$deviance = deviance
metricsEnv$resSd = sigma
