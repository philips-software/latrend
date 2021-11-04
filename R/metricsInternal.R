#' @include model.R
#' @importFrom clusterCrit intCriteria
#' @importFrom mclustcomp mclustcomp

#' @name metric
#' @rdname metric
#' @title Compute internal model metric(s)
#' @description Compute one or more internal metrics for the given `lcModel` object.
#'
#' Call [getInternalMetricNames()] to retrieve the names of the defined internal metrics.
#' @param object The `lcModel`, `lcModels`, or `list` of `lcModel` objects to compute the metrics for.
#' @param name The name(s) of the metric(s) to compute. If no names are given, the names specified in the `latrend.metric` option (WRSS, APPA, AIC, BIC) are used.
#' @param ... Additional arguments.
#' @return For `metric(lcModel)`: A named `numeric` vector with the computed model metrics.
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

.defineInternalDistanceMetric = function(
  name,
  type = c('traj', 'fitted'),
  distanceFun,
  clusterAggregationFun = weighted.mean,
  assertNonEmpty = TRUE,
  assertNonSolitary = FALSE,
  assertNonIdentical = FALSE,
  ...
) {
  type = match.arg(type[1], c('traj', 'fitted'))
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
    fitted = fittedTrajectories,
  )

  fun = function(m) {
    nTimes = length(time(m))
    dfTraj = trajFun(m)
    trajMat = dcastRepeatedMeasures(
      dfTraj,
      id = idVariable(m),
      time = timeVariable(m),
      response = responseVariable(m)
    )
    assert_that(ncol(trajMat) == nTimes)
    trajMatList = lapply(split(trajMat, trajectoryAssignments(m)), matrix, ncol = nTimes)

    dtClus = clusterTrajectories(m, at = time(m))
    clusMat = dcastRepeatedMeasures(
      dtClus,
      id = 'Cluster',
      time = timeVariable(m),
      response = responseVariable(m)
    )
    clusVecList = split(clusMat, row(clusMat))
    assert_that(all(lengths(clusVecList) == nTimes))

    emptyMask = vapply(trajMatList, nrow, FUN.VALUE = 0) == 0 & !assertNonEmpty
    solitaryMask = vapply(trajMatList, nrow, FUN.VALUE = 0) == 1 & !assertNonSolitary
    identicalMask = vapply(
      trajMatList,
      function(x) all(x[1,] == t(x)),
      FUN.VALUE = FALSE
    ) & !assertNonIdentical

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

#' @importFrom matrixStats rowMaxs
intMetricsEnv$APPA = function(m) {
  postprob(m) %>%
    rowMaxs() %>%
    mean()
}

#' @importFrom stats BIC
intMetricsEnv$BIC = BIC

intMetricsEnv$converged = function(m) {
  converged(m) > 0
}

#' @importFrom stats deviance
intMetricsEnv$deviance = deviance

intMetricsEnv$entropy = function(m) {
  pp = postprob(m) %>% pmax(.Machine$double.xmin)
  - sum(rowSums(pp * log(pp)))
}

#' @importFrom stats logLik
intMetricsEnv$logLik = logLik

intMetricsEnv$MAE = function(m) {
  residuals(m) %>% abs %>% mean
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

intMetricsEnv$relativeEntropy = function(m) {
  N = nIds(m)
  K = nClusters(m)
  1 - intMetricsEnv$entropy(m) / (N * log(K))
}

intMetricsEnv$estimationTime = estimationTime

intMetricsEnv$RSS = function(m) {
  sum(residuals(m) ^ 2)
}

intMetricsEnv$sigma = sigma

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

intMetricsEnv$WRSS = function(m) {
  wMat = postprob(m)[make.idRowIndices(m), ]
  resMat = residuals(m, clusters = NULL)
  sum(wMat * resMat ^ 2)
}
