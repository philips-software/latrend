#' @include model.R


#' @export
#' @importFrom matrixStats rowMaxs
#' @title Average posterior probability of assignment (APPA)
#' @description Computes the average posterior probability of assignment (APPA) for each cluster.
#' @param object The model, of type `lcModel`.
#' @return The APPA per cluster, as a `numeric vector` of length `nClusters(object)`.
#' Empty clusters will output `NA`.
#' @seealso [confusionMatrix] [OCC]
#' @references
#' \insertRef{nagin2005group}{latrend}
#'
#' \insertRef{vandernest2020overview}{latrend}
APPA = function(object) {
  assert_that(is.lcModel(object))

  trajMaxPp = rowMaxs(postprob(object))
  trajCluster = trajectoryAssignments(object, strategy = which.max)

  clusAppa = rep(as.numeric(NA), nClusters(object))
  names(clusAppa) = clusterNames(object)

  validAppa = tapply(trajMaxPp, INDEX = trajCluster, FUN = mean, simplify = TRUE)
  clusAppa[match(names(validAppa), names(clusAppa))] = validAppa

  clusAppa
}


#' @export
#' @title Compute the posterior confusion matrix
#' @description Compute the posterior confusion matrix (PCM).
#' The entry \eqn{(i,j)} represents the probability (or number, in case of  `scale = TRUE`) of a trajectory
#' belonging to cluster \eqn{i} is assigned to cluster \eqn{j} under the specified trajectory cluster assignment strategy.
#' @inheritParams APPA
#' @param strategy The strategy for assigning trajectories to a specific cluster, see [trajectoryAssignments()].
#' If `strategy = NULL`, the posterior probabilities are used as weights (analogous to a repeated evaluation of `strategy = which.weight`).
#' @param scale Whether to express the confusion in probabilities (`scale = TRUE`), or in terms of the number of trajectories.
#' @inheritDotParams trajectoryAssignments
#' @return A K-by-K confusion `matrix` with `K = nClusters(object)`.
#' @seealso [postprob] [clusterProportions] [trajectoryAssignments] [APPA] [OCC]
#' @examples
#' data(latrendData)
#' model = latrend(lcMethodLcmmGMM(
#'   fixed = Y ~ Time, mixture = ~ Time, random = ~ 1,
#'   id = "Id", time = "Time"),
#'   data=latrendData)
#' confusionMatrix(model)
confusionMatrix = function(object, strategy = which.max, scale = TRUE, ...) {
  assert_that(is.lcModel(object))

  I = nIds(object)
  K = nClusters(object)
  pp_it = postprob(object)

  if (is.null(strategy)) {
    w_is = pp_it
  } else {
    trajLabels = trajectoryAssignments(object, strategy = strategy, ...)
    idxMat = cbind(seq_len(I), as.integer(trajLabels))
    w_is = matrix(0, nrow = I, ncol = K)
    w_is[idxMat] = 1
  }

  cfMat = matrix(nrow = K, ncol = K)
  for (s in 1:K) {
    for (t in 1:K) {
      cfMat[s,t] = sum(pp_it[, t] * w_is[, s])
    }
  }

  if (isTRUE(scale)) {
    cfMat = cfMat / rowSums(cfMat)
  }

  clusNames = clusterNames(object)
  rownames(cfMat) = clusNames
  colnames(cfMat) = clusNames

  cfMat
}


#' @export
#' @importFrom stats logLik
#' @title Extract the log-likelihood of a lcModel
#' @details The default implementation checks for the existence of the `logLik()` function for the internal model, and returns the output, if available.
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `numeric` with the computed log-likelihood. If unavailable, `NA` is returned.
#' @seealso [stats::logLik] [metric]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ 1,
#'   id = "Id", time = "Time", nClusters = 3)
#' gbtm <- latrend(method, data = latrendData)
#' logLik(gbtm)
logLik.lcModel = function(object, ...) {
  if (is.null(object@model) ||
      is.null(getS3method('logLik', class = class(object@model)[1], optional = TRUE))) {
    N = nIds(object)
    df = length(coef(object))
    ll = as.numeric(NA)
    attr(ll, 'nobs') = N
    attr(ll, 'df') = df
    class(ll) = 'logLik'
    ll
  } else {
    logLik(object@model, ...)
  }
}


#' @export
#' @title Odds of correct classification (OCC)
#' @description Computes the odds of correct classification (OCC) for each cluster.
#' @details An OCC of 1 indicates that the cluster assignment is no better than by random chance.
#' @inheritParams APPA
#' @seealso [confusionMatrix] [APPA]
#' @return The OCC per cluster, as a `numeric vector` of length `nClusters(object)`.
#' Empty clusters will output `NA`.
#' @references
#' \insertRef{nagin2005group}{latrend}
#'
#' \insertRef{vandernest2020overview}{latrend}
OCC = function(object) {
  assert_that(is.lcModel(object))

  appas = APPA(object)
  props = clusterProportions(object)
  (appas / (1 - appas)) / (props / (1 - props))
}
