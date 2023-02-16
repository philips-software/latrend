#' @include model.R metricsInternal.R

#' @name externalMetric
#' @rdname externalMetric
#' @title Compute external model metric(s)
#' @description Compute one or more external metrics for two or more `lcModel` objects.
#'
#' Note that there are many external metrics available, and there exists no external metric that works best in all scenarios.
#' It is recommended to carefully consider which metric is most appropriate for your use case.
#'
#' Many of the external metrics depend on implementations in other packages:
#' \itemize{
#'   \item clusterCrit  \insertCite{desgraupes2018clustercrit}{latrend}
#'   \item mclustcomp  \insertCite{you2018mclustcomp}{latrend}
#'   \item igraph  \insertCite{csardi2006igraph}{latrend}
#'   \item psych  \insertCite{revelle2019psych}{latrend}
#' }
#'
#' See [mclustcomp::mclustcomp()] for a grouped overview of similarity metrics.
#'
#' Call [getInternalMetricNames()] to retrieve the names of the defined internal metrics.
#' Call [getExternalMetricNames()] to retrieve the names of the defined internal metrics.
#' @inheritParams metric
#' @param object2 The other `lcModel` to compare with.
#' @param name The name(s) of the external metric(s) to compute. If no names are given, the names specified in the `latrend.externalMetric` option (none by default) are used.
#' @return A named `numeric` vector containing the computed model metrics.
#' @section Supported external metrics:
#' | **Metric name** | **Description** | **Function / Reference** |
#' | --- | :-------- | :--- |
#' | `adjustedRand` | [Adjusted Rand index](https://en.wikipedia.org/wiki/Rand_index). Based on the Rand index, but adjusted for agreements occurring by chance. A score of 1 indicates a perfect agreement, whereas a score of 0 indicates an agreement no better than chance. | [mclustcomp::mclustcomp()], \insertCite{hubert1985comparing}{latrend} |
#' | `CohensKappa` | [Cohen's kappa](https://en.wikipedia.org/wiki/Cohen%27s_kappa). A partitioning agreement metric correcting for random chance. A score of 1 indicates a perfect agreement, whereas a score of 0 indicates an agreement no better than chance. | [psych::cohen.kappa()], \insertCite{cohen1960coefficient}{latrend} |
#' | `F` | [F-score](https://en.wikipedia.org/wiki/F-score) | [mclustcomp::mclustcomp()] |
#' | `F1` | [F1-score](https://en.wikipedia.org/wiki/F-score), also referred to as the [Sørensen–Dice Coefficient](https://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient), or Dice similarity coefficient | [mclustcomp::mclustcomp()] |
#' | `FolkesMallows` | [Fowlkes-Mallows index](https://en.wikipedia.org/wiki/Fowlkes%E2%80%93Mallows_index) | [mclustcomp::mclustcomp()] |
#' | `Hubert` | Hubert index | [clusterCrit::extCriteria()] |
#' | `Jaccard` | [Jaccard index](https://en.wikipedia.org/wiki/Jaccard_index) | [mclustcomp::mclustcomp()] |
#' | `jointEntropy` | [Joint entropy](https://en.wikipedia.org/wiki/Joint_entropy) between model assignments | [mclustcomp::mclustcomp()] |
#' | `Kulczynski` | Kulczynski index | [clusterCrit::extCriteria()] |
#' | `MaximumMatch` | Maximum match measure | [mclustcomp::mclustcomp()] |
#' | `McNemar` | McNemar statistic | [clusterCrit::extCriteria()] |
#' | `MeilaHeckerman` | Meila-Heckerman measure | [mclustcomp::mclustcomp()] |
#' | `Mirkin` | Mirkin metric | [mclustcomp::mclustcomp()] |
#' | `MI` | [Mutual information](https://en.wikipedia.org/wiki/Mutual_information) | [mclustcomp::mclustcomp()] |
#' | `NMI` | Normalized mutual information | [igraph::compare()] |
#' | `NSJ` | Normalized version of `splitJoin`. The proportion of edits relative to the maximum changes (twice the number of ids) | |
#' | `NVI` | Normalized variation of information | [mclustcomp::mclustcomp()] |
#' | `Overlap` | [Overlap coefficient](https://en.wikipedia.org/wiki/Overlap_coefficient), also referred to as the Szymkiewicz–Simpson coefficient | [mclustcomp::mclustcomp()] \insertCite{vijaymeena2016survey}{latrend} |
#' | `PD` | Partition difference | [mclustcomp::mclustcomp()] |
#' | `Phi` | [Phi coefficient](https://en.wikipedia.org/wiki/Phi_coefficient). | [clusterCrit::extCriteria()] |
#' | `precision` | [precision](https://en.wikipedia.org/wiki/Precision_and_recall) | [clusterCrit::extCriteria()] |
#' | `Rand` | [Rand index](https://en.wikipedia.org/wiki/Rand_index) | [mclustcomp::mclustcomp()] |
#' | `recall` | [recall](https://en.wikipedia.org/wiki/Precision_and_recall) | [clusterCrit::extCriteria()] |
#' | `RogersTanimoto` | Rogers-Tanimoto dissimilarity | [clusterCrit::extCriteria()] |
#' | `RusselRao` | Russell-Rao dissimilarity | [clusterCrit::extCriteria()] |
#' | `SMC` | [Simple matching coefficient](https://en.wikipedia.org/wiki/Simple_matching_coefficient) | [mclustcomp::mclustcomp()] |
#' | `splitJoin` | total split-join index | [igraph::split_join_distance()] |
#' | `splitJoin.ref` | Split-join index of the first model to the second model. In other words, it is the edit-distance between the two partitionings. | |
#' | `SokalSneath1` | Type-1 Sokal-Sneath dissimilarity | [clusterCrit::extCriteria()] |
#' | `SokalSneath2` | Type-2 Sokal-Sneath dissimilarity | [clusterCrit::extCriteria()] |
#' | `VI` | [Variation of information](https://en.wikipedia.org/wiki/Variation_of_information) | [mclustcomp::mclustcomp()] |
#' | `Wallace1` | Type-1 Wallace criterion | [mclustcomp::mclustcomp()] |
#' | `Wallace2` | Type-2 Wallace criterion | [mclustcomp::mclustcomp()] |
#' | `WMSSE` | Weighted minimum sum of squared errors between cluster trajectories | |
#' | `WMMSE` | Weighted minimum mean of squared errors between cluster trajectories | |
#' | `WMMAE` | Weighted minimum mean of absolute errors between cluster trajectories | |
#' @section Implementation:
#' See the documentation of the [defineExternalMetric()] function for details on how to define your own external metrics.
#' @references \insertAllCited{}
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
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'adjrand'
  )$scores
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
  igraph::split_join_distance(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer
  ) %>%
    sum()
}

extMetricsEnv$splitJoin.ref = function(m1, m2) {
  assert_that(has_same_ids(m1, m2))
  igraph::split_join_distance(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer
  )[1]
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
  mclustcomp::mclustcomp(
    trajectoryAssignments(m1) %>% as.integer,
    trajectoryAssignments(m2) %>% as.integer,
    types = 'vi'
  )$scores
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

.wmrss = function(m1, m2, newdata = union(time(m1), time(m2))) {
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

extMetricsEnv$WMRSS = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmrss(m1, m2, newdata)
  sum(out)
}

extMetricsEnv$WMRSS.ref = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmrss(m1, m2, newdata)
  out[2]
}

extMetricsEnv$WMMSE = function(m1, m2, newdata = union(time(m1), time(m2))) {
  if (is.data.frame(newdata) || is.matrix(newdata)) {
    nob = nrow(newdata)
  } else {
    nob = length(newdata)
  }

  extMetricsEnv$WMRSS(m1, m2, newdata) / (2 * nob)
}

extMetricsEnv$WMMSE.ref = function(m1, m2, newdata = union(time(m1), time(m2))) {
  if (is.data.frame(newdata) || is.matrix(newdata)) {
    nob = nrow(newdata)
  } else {
    nob = length(newdata)
  }

  extMetricsEnv$WMRSS.ref(m1, m2, newdata) / nob
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

extMetricsEnv$WMMAE.ref = function(m1, m2, newdata = union(time(m1), time(m2))) {
  out = .wmmae(m1, m2, newdata)
  out[2]
}
