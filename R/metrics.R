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
intMetricsEnv$deviance = deviance
intMetricsEnv$logLik = logLik

intMetricsEnv$MAE = function(m) {
  residuals(m) %>% abs %>% mean
}

intMetricsEnv$RSS = function(m) {
  sum(residuals(m)^2)
}

intMetricsEnv$sigma = sigma

intMetricsEnv$WMAE = function(m) {
  wMat = postprob(m)[genIdRowIndices(m),]
  resMat = residuals(m, clusters=NULL)
  mean(wMat * abs(resMat))
}

intMetricsEnv$WRSS = function(m) {
  wMat = postprob(m)[genIdRowIndices(m),]
  resMat = residuals(m, clusters=NULL)
  sum(wMat * resMat^2)
}



# External metric definitions ####
extMetricsEnv$AdjustedRand = function(m1, m2) {
  mclust::adjustedRandIndex(
    clusterAssignments(m1) %>% as.integer,
    clusterAssignments(m2) %>% as.integer)
}

extMetricsEnv$CohensKappa = function(m1, m2) {
  psych::cohen.kappa(
    cbind(clusterAssignments(m1) %>% as.integer,
          clusterAssignments(m2) %>% as.integer), alpha=1)$kappa
}

extMetricsEnv$CzekanowskiDice = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Czekanowski_Dice')[[1]]
}

extMetricsEnv$FolkesMallows = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Folkes_Mallows')[[1]]
}

extMetricsEnv$Hubert = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Hubert')[[1]]
}

extMetricsEnv$Jaccard = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Jaccard')[[1]]
}

extMetricsEnv$Kulczynski = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Kulczynski')[[1]]
}

extMetricsEnv$McNemar = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'McNemar')[[1]]
}

extMetricsEnv$NMI = function(m1, m2) {
  igraph::compare(
    clusterAssignments(m1) %>% as.integer,
    clusterAssignments(m2) %>% as.integer,
    method='nmi')
}

extMetricsEnv$NSJ = function(m1, m2) {
  extMetricsEnv$SplitJoin(m1, m2) / (2 * nIds(m1))
}

extMetricsEnv$Phi = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Phi')[[1]]
}

extMetricsEnv$Precision = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Precision')[[1]]
}

extMetricsEnv$Rand = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Rand')[[1]]
}

extMetricsEnv$Recall = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Recall')[[1]]
}

extMetricsEnv$RogersTanimoto = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Rogers_Tanimoto')[[1]]
}

extMetricsEnv$RusselRao = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Russel_Rao')[[1]]
}

extMetricsEnv$SplitJoin = function(m1, m2) {
  igraph::split_join_distance(
    clusterAssignments(m1) %>% as.integer,
    clusterAssignments(m2) %>% as.integer) %>% sum
}

extMetricsEnv$SplitJoin1 = function(m1, m2) {
  igraph::split_join_distance(
    clusterAssignments(m1) %>% as.integer,
    clusterAssignments(m2) %>% as.integer)[1]
}

extMetricsEnv$SokalSneath1 = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Sokal_Sneath1')[[1]]
}

extMetricsEnv$SokalSneath2 = function(m1, m2) {
  extCriteria(clusterAssignments(m1) %>% as.integer,
              clusterAssignments(m2) %>% as.integer,
              'Sokal_Sneath2')[[1]]
}

extMetricsEnv$VI = function(m1, m2) {
  igraph::compare(
    clusterAssignments(m1) %>% as.integer,
    clusterAssignments(m2) %>% as.integer,
    method='vi')
}