#' @include model.R

#' @name metric
#' @rdname metric
#' @aliases internalMetric
#' @title Compute internal model metric(s)
#' @description Compute one or more internal metrics for the given `lcModel` object.
#'
#' Note that there are many metrics available, and there exists no metric that works best in all scenarios.
#' It is recommended to carefully consider which metric is most appropriate for your use case.
#'
#' Recommended overview papers:
#' \itemize{
#'   \item \insertCite{arbelaitz2013extensive;textual}{latrend} provide an extensive overview validity indices for cluster algorithms.
#'   \item \insertCite{vandernest2020overview;textual}{latrend} provide an overview of metrics for mixture models (GBTM, GMM); primarily likelihood-based or posterior probability-based metrics.
#'   \item \insertCite{henson2007detecting;textual}{latrend} provide an overview of likelihood-based metrics for mixture models.
#' }
#'
#' Call [getInternalMetricNames()] to retrieve the names of the defined internal metrics.
#'
#' See the _Details_ section below for a list of supported metrics.
#' @section Supported internal metrics:
#' | **Metric name** | **Description** | **Function / Reference** |
#' | --- | :-------- | :--- |
#' | `AIC` | [Akaike information criterion](https://en.wikipedia.org/wiki/Akaike_information_criterion). A goodness-of-fit estimator that adjusts for model complexity (i.e., the number of parameters). Only available for models that support the computation of the model log-likelihood through [logLik]. | [stats::AIC()], \insertCite{akaike1974new}{latrend} |
#' | `APPA.mean` | Mean of the average posterior probability of assignment (APPA) across clusters. A measure of the precision of the trajectory classifications. A score of 1 indicates perfect classification. | [APPA()], \insertCite{nagin2005group}{latrend} |
#' | `APPA.min` | Lowest APPA among the clusters | [APPA()], \insertCite{nagin2005group}{latrend} |
#' | `BIC` | [Bayesian information criterion](https://en.wikipedia.org/wiki/Bayesian_information_criterion). A goodness-of-fit estimator that corrects for the degrees of freedom (i.e., the number of parameters) and sample size. Only available for models that support the computation of the model log-likelihood through [logLik]. | [stats::BIC()], \insertCite{schwarz1978estimating}{latrend} |
#' | `CAIC` | Consistent Akaike information criterion | \insertCite{bozdogan1987model}{latrend} |
#' | `CLC` | Classification likelihood criterion | \insertCite{mclachlan2000finite}{latrend} |
#' | `converged` | Whether the model converged during estimation | [converged()] |
#' | `deviance` | The model [deviance](https://en.wikipedia.org/wiki/Deviance_(statistics)) | [stats::deviance()] |
#' | `Dunn` | The [Dunn index](https://en.wikipedia.org/wiki/Dunn_index) | |
#' | `entropy` | Entropy of the posterior probabilities | |
#' | `estimationTime` | The time needed for fitting the model | [estimationTime()] |
#' | `ED` | [Euclidean distance](https://en.wikipedia.org/wiki/Euclidean_distance) between the cluster trajectories and the assigned observed trajectories | |
#' | `ED.fit` | Euclidean distance between the cluster trajectories and the assigned fitted trajectories | |
#' | `ICL.BIC` | Integrated classification likelihood (ICL) approximated using the BIC | \insertCite{biernacki2000assessing}{latrend} |
#' | `logLik` | Model log-[likelihood](https://en.wikipedia.org/wiki/Likelihood_function) | [stats::logLik()] |
#' | `MAE` | [Mean absolute error](https://en.wikipedia.org/wiki/Mean_absolute_error) of the fitted trajectories (assigned to the most likely respective cluster) to the observed trajectories | |
#' | `Mahalanobis` | [Mahalanobis distance](https://en.wikipedia.org/wiki/Mahalanobis_distance) between the cluster trajectories and the assigned observed trajectories | \insertCite{mahalanobis1936generalized}{latrend} |
#' | `MSE` | [Mean squared error](https://en.wikipedia.org/wiki/Mean_squared_error) of the fitted trajectories (assigned to the most likely respective cluster) to the observed trajectories | |
#' | `relativeEntropy`, `RE` | A measure of the precision of the trajectory classification. A value of 1 indicates perfect classification, whereas a value of 0 indicates a non-informative uniform classification. It is the normalized version of `entropy`, scaled between \[0, 1\]. | \insertCite{ramaswamy1993empirical}{latrend}, \insertCite{muthen2004latent}{latrend} |
#' | `RMSE` | [Root mean squared error](https://en.wikipedia.org/wiki/Root-mean-square_deviation) of the fitted trajectories (assigned to the most likely respective cluster) to the observed trajectories | |
#' | `RSS` | [Residual sum of squares](https://en.wikipedia.org/wiki/Residual_sum_of_squares) under most likely cluster allocation | |
#' | `scaledEntropy` | See `relativeEntropy` | |
#' | `sigma` | The residual standard deviation | [stats::sigma()] |
#' | `ssBIC` | Sample-size adjusted BIC | \insertCite{sclove1987application}{latrend} |
#' | `SED` | Standardized Euclidean distance between the cluster trajectories and the assigned observed trajectories | |
#' | `SED.fit` | The cluster-weighted standardized Euclidean distance between the cluster trajectories and the assigned fitted trajectories | |
#' | `WMAE` | `MAE` weighted by cluster-assignment probability | |
#' | `WMSE` | `MSE` weighted by cluster-assignment probability | |
#' | `WRMSE` | `RMSE` weighted by cluster-assignment probability | |
#' | `WRSS` | `RSS` weighted by cluster-assignment probability | |
#'
#' @section Implementation:
#' See the documentation of the [defineInternalMetric()] function for details on how to define your own metrics.
#' @param object The `lcModel`, `lcModels`, or `list` of `lcModel` objects to compute the metrics for.
#' @param name The name(s) of the metric(s) to compute. If no names are given, the names specified in the `latrend.metric` option (WRSS, APPA, AIC, BIC) are used.
#' @param ... Additional arguments.
#' @return For `metric(lcModel)`: A named `numeric` vector with the computed model metrics.
#' @references \insertAllCited{}
#' @seealso [externalMetric] [min.lcModels] [max.lcModels]
NULL

intMetricsEnv = new.env()

#' @export
#' @title Get the names of the available internal metrics
#' @family metric functions
getInternalMetricNames = function() {
  sort(names(intMetricsEnv))
}

#' @export
#' @title Define an internal metric for lcModels
#' @param name The name of the metric.
#' @param fun The function to compute the metric, accepting a lcModel object as input.
#' @param warnIfExists Whether to output a warning when the metric is already defined.
#' @family metric functions
#' @examples
#' defineInternalMetric("BIC", fun = BIC)
#'
#' mae <- function(object) {
#'   mean(abs(residuals(object)))
#' }
#' defineInternalMetric("MAE", fun = mae)
defineInternalMetric = function(name, fun, warnIfExists = getOption('latrend.warnMetricOverride', TRUE)) {
  assert_that(is.function(fun))
  assert_that(!is.null(formalArgs(fun)), msg = 'function must accept one argument (a lcModel)')
  .defineMetric(name, fun = fun, warnIfExists = warnIfExists, envir = intMetricsEnv)
}

.defineMetric = function(name, fun, warnIfExists, envir) {
  if (warnIfExists && exists(name, envir = envir, inherits = FALSE)) {
    warning(sprintf('ovewriting existing metric definition for %s', name))
  }
  assign(name, value = fun, envir = envir)
}


#' @export
#' @title Get the internal metric definition
#' @inheritParams defineInternalMetric
#' @return The metric function, or NULL if not defined.
#' @family metric functions
getInternalMetricDefinition = function(name) {
  .getMetricDef(name, envir = intMetricsEnv)
}

.getMetricDef = function(name, envir) {
  if (exists(name, envir = envir, inherits = FALSE)) {
    get(name, envir = envir)
  } else {
    stop(sprintf('No metric definition for "%s"', name))
  }
}

#' @importFrom stats weighted.mean
.defineInternalDistanceMetric = function(
  name,
  type = c('traj', 'fit'),
  distanceFun,
  clusterAggregationFun = weighted.mean,
  assertNonEmpty = TRUE,
  assertNonSolitary = FALSE,
  assertNonIdentical = FALSE,
  ...
) {
  type = match.arg(type[1], c('traj', 'fit'))
  if (type != 'traj') {
    fullName = paste(name, type, sep = '.')
  } else {
    # traj is the default
    fullName = name
  }

  assert_that(
    is.function(distanceFun),
    is.function(clusterAggregationFun)
  )

  trajFun = switch(type,
    traj = trajectories,
    fit = fittedTrajectories,
  )

  fun = function(m) {
    nTimes = length(time(m))
    dfTraj = trajFun(m)
    trajMat = tsmatrix(
      dfTraj,
      id = idVariable(m),
      time = timeVariable(m),
      response = responseVariable(m)
    )
    assert_that(ncol(trajMat) == nTimes)
    trajMatList = lapply(split(trajMat, trajectoryAssignments(m)), matrix, ncol = nTimes)

    dtClus = clusterTrajectories(m, at = time(m))
    clusMat = tsmatrix(
      dtClus,
      id = 'Cluster',
      time = timeVariable(m),
      response = responseVariable(m)
    )
    clusVecList = split(clusMat, row(clusMat))
    assert_that(all(lengths(clusVecList) == nTimes))

    emptyMask = vapply(trajMatList, nrow, FUN.VALUE = 0) == 0 & assertNonEmpty
    solitaryMask = vapply(trajMatList, nrow, FUN.VALUE = 0) == 1 & assertNonSolitary
    identicalMask = vapply(
      trajMatList,
      function(x) all(x[1,] == t(x)),
      FUN.VALUE = FALSE
    ) & assertNonIdentical

    if (any(emptyMask)) {
      warning(
        sprintf(
          'Cannot compute distance metric "%s" for cluster(s) "%s": No trajectories assigned to the cluster.',
          fullName,
          paste0(clusterNames(m)[emptyMask], collapse = '", "')
        )
      )
    }

    if (any(solitaryMask)) {
      warning(
        sprintf(
          'Cannot compute distance metric "%s" for cluster(s) "%s": Only 1 trajectory assigned to the cluster.',
          fullName,
          paste0(clusterNames(m)[solitaryMask], collapse = '", "')
        )
      )
    }

    if (any(identicalMask)) {
      warning(
        sprintf(
          'Cannot compute distance metric "%s" for cluster(s) "%s": All trajectories are identical (i.e., zero covariance).',
          fullName,
          paste0(clusterNames(m)[identicalMask], collapse = '", "')
        )
      )
    }

    validMask = !emptyMask & !solitaryMask & !identicalMask

    if (!any(validMask)) {
      return(as.numeric(NA))
    }

    clusDistances = Map(
      distanceFun,
      trajMatList[validMask],
      clusVecList[validMask],
      clusterNames(m)[validMask]
    )
    clusterAggregationFun(unlist(clusDistances), w = clusterProportions(m)[validMask])
  }

  defineInternalMetric(fullName, fun = fun, ...)
}

#' @title Define the distance metrics for multiple types at once
#' @keywords internal
.defineInternalDistanceMetrics = Vectorize(
  FUN = .defineInternalDistanceMetric,
  vectorize.args = 'type',
  SIMPLIFY = FALSE
)


# Internal metric definitions ####
#' @importFrom stats AIC
intMetricsEnv$AIC = AIC

intMetricsEnv$CAIC = function(m) {
  ll = logLik(m)
  df = attr(ll, 'df')
  - 2 * ll + (log(nIds(m)) + 1) * df
}

intMetricsEnv$ssBIC = function(m) {
  ll = logLik(m)
  df = attr(ll, 'df')
  - 2 * ll + log((nIds(m) + 2) / 24) * df
}

intMetricsEnv$APPA.mean = function(m) {
  mean(APPA(m))
}

intMetricsEnv$APPA.min = function(m) {
  min(APPA(m))
}

intMetricsEnv$ASW = function(m) {
  part = as.integer(trajectoryAssignments(m))
  tsmat = tsmatrix(
    data = model.data(m),
    response = responseVariable(m),
    id = idVariable(m),
    time = timeVariable(m),
    fill = NA_real_
  )

  clusterCrit::intCriteria(tsmat, part, crit = 'Silhouette')$silhouette
}

#' @importFrom stats BIC
intMetricsEnv$BIC = BIC

intMetricsEnv$CalinskiHarabasz = function(m) {
  part = as.integer(trajectoryAssignments(m))
  tsmat = tsmatrix(
    data = model.data(m),
    response = responseVariable(m),
    id = idVariable(m),
    time = timeVariable(m),
    fill = NA_real_
  )

  clusterCrit::intCriteria(tsmat, part, crit = 'Calinski_Harabasz')$calinski_harabasz
}

intMetricsEnv$CLC = function(m) {
  ll = logLik(m)
  df = attr(ll, 'df')
  E = intMetricsEnv$entropy(m)
  - 2 * ll + 2 * E
}

intMetricsEnv$converged = function(m) {
  converged(m) > 0
}

intMetricsEnv$DaviesBouldin = function(m) {
  part = as.integer(trajectoryAssignments(m))
  tsmat = tsmatrix(
    data = model.data(m),
    response = responseVariable(m),
    id = idVariable(m),
    time = timeVariable(m),
    fill = NA_real_
  )

  clusterCrit::intCriteria(tsmat, part, crit = 'Davies_Bouldin')$davies_bouldin
}

#' @importFrom stats deviance
intMetricsEnv$deviance = deviance

.defineInternalDistanceMetrics(
  name = 'ED',
  type = c('traj', 'fit'),
  distanceFun = function(trajClusMat, clusVec, clusName) {
    mean(sqrt((t(trajClusMat) - clusVec) ^ 2))
  },
  clusterAggregationFun = weighted.mean
)

intMetricsEnv$Dunn = function(m) {
  if (nClusters(m) == 1L) {
    # not defined for K=1
    return (NA_real_)
  }

  part = as.integer(trajectoryAssignments(m))
  tsmat = tsmatrix(
    data = model.data(m),
    response = responseVariable(m),
    id = idVariable(m),
    time = timeVariable(m),
    fill = NA_real_
  )

  clusterCrit::intCriteria(tsmat, part, crit = 'Dunn')$dunn
}

intMetricsEnv$entropy = function(m) {
  pp = postprob(m) %>% pmax(.Machine$double.xmin)
  - sum(rowSums(pp * log(pp)))
}

intMetricsEnv$estimationTime = estimationTime

#' @importFrom stats logLik
intMetricsEnv$logLik = logLik

intMetricsEnv$ICL.BIC = function(m) {
  ll = logLik(m)
  df = attr(ll, 'df')
  - 2 * ll + log(nIds(m)) * df + 2 * intMetricsEnv$entropy(m)
}

intMetricsEnv$MAE = function(m) {
  mean(abs(residuals(m)))
}

# . Mahalanobis distance ####
#' @importFrom stats mahalanobis
.defineInternalDistanceMetric(
  name = 'Mahalanobis',
  type = 'traj',
  distanceFun = function(trajClusMat, clusVec, clusName) {
    vcovMat = cov(trajClusMat)
    if (det(vcovMat) == 0) {
      warning(
        sprintf(
          'Cannot compute Mahalanobis distance for cluster "%s": covariance matrix is singular',
          clusName
        )
      )
      as.numeric(NA)
    } else {
      mean(mahalanobis(trajClusMat, center = clusVec, cov = vcovMat))
    }
  },
  clusterAggregationFun = weighted.mean,
  assertNonSolitary = TRUE,
  assertNonIdentical = TRUE
)

intMetricsEnv$MSE = function(m) {
  mean(residuals(m) ^ 2)
}

intMetricsEnv$RMSE = function(m) {
  sqrt(mean(residuals(m) ^ 2))
}

# also referred to as the scaled entropy
intMetricsEnv$relativeEntropy = function(m) {
  N = nIds(m)
  K = nClusters(m)
  E = metric(m, 'entropy')
  1 - E / (N * log(K))
}

intMetricsEnv$RE = intMetricsEnv$relativeEntropy

intMetricsEnv$RSS = function(m) {
  sum(residuals(m) ^ 2)
}

intMetricsEnv$scaledEntropy = intMetricsEnv$relativeEntropy

intMetricsEnv$sigma = sigma

# . Standardized Euclidean distance ####
#' @importFrom stats mahalanobis
.defineInternalDistanceMetrics(
  name = 'SED',
  type = c('traj', 'fit'),
  distanceFun = function(trajClusMat, clusVec, clusName) {
    varMat = diag(diag(var(trajClusMat)))
    if (det(varMat) == 0) {
      warning(
        sprintf(
          'Cannot compute standard Euclidean distance for cluster "%s": variance matrix is singular',
          clusName
        )
      )
      as.numeric(NA)
    } else {
      mean(mahalanobis(trajClusMat, center = clusVec, cov = varMat))
    }
  },
  clusterAggregationFun = weighted.mean,
  assertNonSolitary = TRUE,
  assertNonIdentical = TRUE
)

intMetricsEnv$WMAE = function(m) {
  wMat = postprob(m)[make.idRowIndices(m), ]
  resMat = residuals(m, clusters = NULL)
  mean(rowSums(wMat * abs(resMat)))
}

intMetricsEnv$WMSE = function(m) {
  wMat = postprob(m)[make.idRowIndices(m), ]
  resMat = residuals(m, clusters = NULL)
  mean(rowSums(wMat * resMat ^ 2))
}

intMetricsEnv$WRMSE = function(m) {
  sqrt(metric(m, 'WMSE'))
}

intMetricsEnv$WRSS = function(m) {
  wMat = postprob(m)[make.idRowIndices(m), ]
  resMat = residuals(m, clusters = NULL)
  sum(wMat * resMat ^ 2)
}
