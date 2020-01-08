#' @include clModel.R
#' @importFrom clusterCrit intCriteria extCriteria
intMetricsEnv = new.env()
extMetricsEnv = new.env()

#' @export
#' @title Get the names of the available internal metrics
#' @family metric functions
getInternalMetricNames = function() {
  names(intMetricsEnv) %>% sort
}

#' @export
#' @title Get the names of the available external metrics
#' @family metric functions
getExternalMetricNames = function() {
  names(extMetricsEnv) %>% sort
}

#' @export
#' @title Define an internal metric for clModels
#' @param name The name of the metric.
#' @param fun The function to compute the metric, accepting a clModel object as input.
#' @param warnIfExists Whether to output a warning when the new metric is already defined.
#' @family metric functions
defineInternalMetric = function(name, fun, warnIfExists=TRUE) {
  assert_that(is.function(fun))
  assert_that(!is.null(formalArgs(fun)), msg='function must accept one argument (a clModel)')
  defineMetric(name, fun, warnIfExists, intMetricsEnv)
}

#' @export
#' @title Define an external metric for clModels
#' @param name The name of the metric.
#' @param fun The function to compute the metric, accepting a clModel object as input.
#' @param warnIfExists Whether to output a warning when the new metric is already defined.
#' @family metric functions
defineExternalMetric = function(name, fun, warnIfExists=TRUE) {
  assert_that(is.function(fun))
  assert_that(length(formalArgs(fun)) == 2, msg='function must accept two arguments (two clModels)')
  defineMetric(name, fun, warnIfExists, extMetricsEnv)
}

defineMetric = function(name, fun, warnIfExists, envir) {
  if(warnIfExists && exists(name, envir=intMetricsEnv)) {
    warning(sprintf('ovewriting existing metric definition for %s', name))
  }
  assign(name, value=fun, envir=envir)
}


#' @export
#' @title Get the internal metric definition
#' @inheritParams defineInternalMetric
#' @return The metric function, or NULL if not defined.
#' @family metric functions
getInternalMetricDefinition = function(name) {
  getMetricDef(name, intMetricsEnv)
}

#' @export
#' @title Get the external metric definition
#' @inheritParams defineInternalMetric
#' @return The metric function, or NULL if not defined.
#' @family metric functions
getExternalMetricDefinition = function(name) {
  getMetricDef(name, extMetricsEnv)
}

getMetricDef = function(name, envir) {
  if(exists(name, envir=envir)) {
    get(name, envir=envir)
  } else {
    NULL
  }
}

# Internal metric definitions ####
intMetricsEnv$AIC = AIC
intMetricsEnv$BIC = BIC
intMetricsEnv$logLik = logLik
intMetricsEnv$deviance = deviance
intMetricsEnv$resSd = sigma

# External metric definitions ####
extMetricsEnv$Jaccard = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Jaccard')[[1]]
}