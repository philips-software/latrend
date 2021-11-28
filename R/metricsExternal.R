#' @include model.R metricsInternal.R

#' @name externalMetric
#' @rdname externalMetric
#' @title Compute external model metric(s)
#' @description Compute one or more external metrics for two or more `lcModel` objects.
#'
#' Call [getExternalMetricNames()] to retrieve the names of the defined internal metrics.
#' @inheritParams metric
#' @param object2 The other `lcModel` to compare with.
#' @param name The name(s) of the external metric(s) to compute. If no names are given, the names specified in the `latrend.externalMetric` option (none by default) are used.
#' @return A named `numeric` vector containing the computed model metrics.
#' @references
#' \insertRef{desgraupes2018clustercrit}{latrend}
#'
#' \insertRef{you2018mclustcomp}{latrend}
#'
#' \insertRef{csardi2006igraph}{latrend}
#'
#' \insertRef{hubert1985comparing}{latrend}
#'
#' \insertRef{revelle2019psych}{latrend}
#'
#' \insertRef{scrucca2016mclust}{latrend}
#' @seealso [metric]
NULL

extMetricsEnv = new.env()

#' @export
#' @title Get the names of the available external metrics
#' @family metric functions
getExternalMetricNames = function() {
  sort(names(extMetricsEnv))
}

#' @export
#' @title Define an external metric for lcModels
#' @param name The name of the metric.
#' @param fun The function to compute the metric, accepting a lcModel object as input.
#' @param warnIfExists Whether to output a warning when the metric is already defined.
#' @family metric functions
defineExternalMetric = function(name, fun, warnIfExists = getOption('latrend.warnMetricOverride', TRUE)) {
  assert_that(is.function(fun))
  assert_that(length(formalArgs(fun)) == 2, msg = 'function must accept two arguments (two lcModels)')
  .defineMetric(name, fun = fun, warnIfExists = warnIfExists, envir = extMetricsEnv)
}

#' @export
#' @title Get the external metric definition
#' @inheritParams defineInternalMetric
#' @return The metric function, or NULL if not defined.
#' @family metric functions
getExternalMetricDefinition = function(name) {
  .getMetricDef(name, envir = extMetricsEnv)
}

# External metric definitions ####
extMetricsEnv$adjustedRand = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclust::adjustedRandIndex(trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer)
}

extMetricsEnv$CohensKappa = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  psych::cohen.kappa(
    cbind(
      trajectoryAssignments(m1) %>% as.integer,
      trajectoryAssignments(m2) %>% as.integer
    ),
    alpha = 1
  )$kappa
}

extMetricsEnv$`F` = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'f'
  )$scores
}

extMetricsEnv$F1 = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'sdc'
  )$scores
}

extMetricsEnv$FolkesMallows = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Folkes_Mallows'
  )[[1]]
}

extMetricsEnv$Hubert = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Hubert'
  )[[1]]
}

extMetricsEnv$Jaccard = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Jaccard'
  )[[1]]
}

extMetricsEnv$jointEntropy = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'jent'
  )$scores
}

extMetricsEnv$Kulczynski = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Kulczynski'
  )[[1]]
}

extMetricsEnv$MaximumMatch = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'mmm'
  )$scores
}

extMetricsEnv$McNemar = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'McNemar'
  )[[1]]
}

extMetricsEnv$MeilaHeckerman = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'mhm'
  )$scores
}

extMetricsEnv$Mirkin = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'mirkin'
  )$scores
}

extMetricsEnv$MI = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'mi'
  )$scores
}

extMetricsEnv$NMI = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  igraph::compare(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    method = 'nmi'
  )
}

extMetricsEnv$NSJ = function(m1, m2) {
  extMetricsEnv$splitJoin(m1, m2) / (2 * nIds(m1))
}

extMetricsEnv$NVI = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'nvi'
  )$scores
}

extMetricsEnv$Overlap = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'overlap'
  )$scores
}

extMetricsEnv$PD = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'pd'
  )$scores
}


extMetricsEnv$Phi = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Phi'
  )[[1]]
}

extMetricsEnv$precision = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Precision'
  )[[1]]
}

extMetricsEnv$Rand = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Rand'
  )[[1]]
}

extMetricsEnv$recall = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Recall'
  )[[1]]
}

extMetricsEnv$RogersTanimoto = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Rogers_Tanimoto'
  )[[1]]
}

extMetricsEnv$RusselRao = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Russel_Rao'
  )[[1]]
}

extMetricsEnv$SMC = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'smc'
  )$scores
}

extMetricsEnv$splitJoin = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  igraph::split_join_distance(trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer) %>% sum
}

extMetricsEnv$splitJoin_ref = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  igraph::split_join_distance(trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer)[1]
}

extMetricsEnv$SokalSneath1 = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Sokal_Sneath1'
  )[[1]]
}

extMetricsEnv$SokalSneath2 = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  clusterCrit::extCriteria(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    'Sokal_Sneath2'
  )[[1]]
}

extMetricsEnv$VI = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  igraph::compare(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    method = 'vi'
  )
}

extMetricsEnv$Wallace1 = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'wallace1'
  )$scores
}

extMetricsEnv$Wallace2 = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'wallace2'
  )$scores
}

.wmsse = function(m1, m2, newdata = union(time(m1), time(m2))) {
  resp1 = responseVariable(m1)
  resp2 = responseVariable(m2)

  trajmat1 = clusterTrajectories(m1, at = newdata)[[resp1]] %>%
    matrix(ncol = nClusters(m1))
  trajmat2 = clusterTrajectories(m2, at = newdata)[[resp2]] %>%
    matrix(ncol = nClusters(m2))

  groupMetric1 = foreach(g = seq_len(nClusters(m1)), .combine = c) %do% {
    min(colSums(sweep(trajmat2, 1, trajmat1[, g]) ^ 2))
  }

  groupMetric2 = foreach(g = seq_len(nClusters(m2)), .combine = c) %do% {
    min(colSums(sweep(trajmat1, 1, trajmat2[, g]) ^ 2))
  }

  wmsse1 = clusterProportions(m1) * groupMetric1
  wmsse2 = clusterProportions(m2) * groupMetric2

  c(wmsse1, wmsse2)
}

extMetricsEnv$WMSSE = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmsse(m1, m2, newdata)
  sum(out)
}

extMetricsEnv$WMSSE_ref = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmsse(m1, m2, newdata)
  out[2]
}

extMetricsEnv$WMMSE = function(m1, m2, newdata = union(time(m1), time(m2))) {
  if (is.data.frame(newdata) || is.matrix(newdata)) {
    nob = nrow(newdata)
  } else {
    nob = length(newdata)
  }

  extMetricsEnv$WMSSE(m1, m2, newdata) / (2 * nob)
}

extMetricsEnv$WMMSE_ref = function(m1, m2, newdata = union(time(m1), time(m2))) {
  if (is.data.frame(newdata) || is.matrix(newdata)) {
    nob = nrow(newdata)
  } else {
    nob = length(newdata)
  }

  extMetricsEnv$WMSSE_ref(m1, m2, newdata) / nob
}

.wmmae = function(m1, m2, newdata = union(time(m1), time(m2))) {
  resp1 = responseVariable(m1)
  resp2 = responseVariable(m2)

  trajmat1 = clusterTrajectories(m1, at = newdata)[[resp1]] %>%
    matrix(ncol = nClusters(m1))
  trajmat2 = clusterTrajectories(m2, at = newdata)[[resp2]] %>%
    matrix(ncol = nClusters(m2))

  groupMetric1 = foreach(g = seq_len(nClusters(m1)), .combine = c) %do% {
    min(colMeans(abs(sweep(
      trajmat2, 1, trajmat1[, g]
    ))))
  }

  groupMetric2 = foreach(g = seq_len(nClusters(m2)), .combine = c) %do% {
    min(colMeans(abs(sweep(
      trajmat1, 1, trajmat2[, g]
    ))))
  }

  wmmae1 = clusterProportions(m1) * groupMetric1
  wmmae2 = clusterProportions(m2) * groupMetric2

  c(wmmae1, wmmae2)
}

extMetricsEnv$WMMAE = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmmae(m1, m2, newdata)
  mean(out)
}

extMetricsEnv$WMMAE_ref = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmmae(m1, m2, newdata)
  out[2]
}
