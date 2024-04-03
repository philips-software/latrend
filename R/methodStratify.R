#' @include method.R
setClass('lcMethodStratify', contains = 'lcMethod')

setValidity('lcMethodStratify', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodStratify)))

  if (isArgDefined(object, 'nClusters')) {
    assert_that(is.na(object$nClusters) || is.count(object$nClusters))
  }

  if (isArgDefined(object, 'clusterNames')) {
    assert_that(is.null(object$clusterNames) || is.character(object$clusterNames))
  }

  if (isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})

#' @export
#' @title Specify a stratification method
#' @inheritParams lcMethodFunction
#' @param stratify An `expression` returning a `number` or `factor` value per trajectory, representing the cluster assignment. Alternatively, a `function` can be provided that takes separate trajectory `data.frame` as input.
#' @param center The `function` for computing the longitudinal cluster centers, used for representing the cluster trajectories.
#' @param nClusters The number of clusters. This is optional, as this can be derived from the largest assignment number by default, or the number of `factor` levels.
#' @param clusterNames The names of the clusters. If a `factor` assignment is returned, the levels are used as the cluster names.
#' @examples
#' data(latrendData)
#' # Stratification based on the mean response level
#' method <- lcMethodStratify(
#'   "Y",
#'   mean(Y) > 0,
#'   clusterNames = c("Low", "High"),
#'   id = "Id",
#'   time = "Time"
#' )
#' model <- latrend(method, latrendData)
#' summary(model)
#'
#' # Stratification function
#' stratfun <- function(trajdata) {
#'    trajmean <- mean(trajdata$Y)
#'    factor(
#'      trajmean > 1.7,
#'      levels = c(FALSE, TRUE),
#'      labels = c("Low", "High")
#'    )
#' }
#' method <- lcMethodStratify("Y", stratfun, id = "Id", time = "Time")
#'
#' # Multiple clusters
#' stratfun3 <- function(trajdata) {
#'    trajmean <- mean(trajdata$Y)
#'    cut(
#'      trajmean,
#'      c(-Inf, .5, 2, Inf),
#'      labels = c("Low", "Medium", "High")
#'    )
#' }
#' method <- lcMethodStratify("Y", stratfun3, id = "Id", time = "Time")
#' @family lcMethod implementations
lcMethodStratify = function(
  response,
  stratify,
  center = meanNA,
  nClusters = NaN,
  clusterNames = NULL,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  name = 'stratify'
) {
  mc = match.call.all()
  mc$Class = 'lcMethodStratify'
  do.call(new, as.list(mc))
}

#' @rdname interface-featureBased
setMethod('getArgumentDefaults', 'lcMethodStratify', function(object) {
  c(
    formals(lcMethodStratify),
    callNextMethod()
  )
})

#' @rdname interface-featureBased
setMethod('getName', 'lcMethodStratify', function(object) {
  if (isArgDefined(object, 'name') && !is.null(object$name)) {
    return(object$name)
  }

  if (isArgDefined(object, 'stratify')) {
    expr = object[['stratify', eval = FALSE]]
    if (is.name(fun)) {
      return(paste('stratification ', expr))
    }
  }

  return('stratify')
})

#' @rdname interface-featureBased
setMethod('getShortName', 'lcMethodStratify', function(object) 'strat')

#' @rdname interface-featureBased
setMethod('compose', 'lcMethodStratify', function(method, envir = NULL, ...) {
  evaluate.lcMethod(
    method,
    try = FALSE,
    exclude = 'stratify',
    envir = envir
  )
})


#' @rdname interface-featureBased
setMethod('fit', 'lcMethodStratify', function(method, data, envir, verbose, ...) {
  data = as.data.table(data)
  id = idVariable(method)

  # Stratify
  strat = method[['stratify', eval = FALSE]]
  assignments = .stratifyTrajectories(
    strat,
    data = data,
    id = id,
    envir = environment(method)
  )

  if (is.factor(assignments)) {
    assert_that(is.na(method$nClusters) || nlevels(assignments) == method$nClusters)
  }
  intAssignments = as.integer(assignments)
  assert_that(is.na(method$nClusters) || max(intAssignments) <= method$nClusters)

  # Determine number of clusters
  if (is.na(method$nClusters)) {
    if (is.factor(assignments)) {
      numClus = nlevels(assignments)
    } else {
      numClus = max(intAssignments)
    }
  } else {
    numClus = method$nClusters
  }
  assert_that(min(intAssignments) >= 1, max(intAssignments) <= numClus)

  # Derive cluster names
  if (is.null(method$clusterNames)) {
    if (is.factor(assignments)) {
      clusNames = levels(assignments)
    } else {
      clusNames = make.clusterNames(numClus)
    }
  } else {
    assert_that(is.character(method$clusterNames))
    clusNames = method$clusterNames
  }
  assert_that(length(clusNames) == numClus)

  if (numClus > 1) {
    clusterSizes = table(intAssignments)
    if (numClus > 1 && uniqueN(intAssignments) == 1) {
      warning(
        sprintf(
          'Stratification assigned all trajectories to the same cluster ("%s").',
          clusNames[clusterSizes > 0][1]
        )
      )
    } else if (uniqueN(intAssignments) < numClus) {
      warning(
        sprintf(
          'Cluster(s) "%s" were not assigned any trajectories during stratification.',
          paste0(clusNames[clusterSizes == 0], collapse = '", "')
        )
      )
    }
  }

  .lcModelStratify(
    method = method,
    data = data,
    id = id,
    time = timeVariable(method),
    center = method$center,
    response = responseVariable(method),
    clusterNames = clusNames,
    trajectoryClusterIndices = intAssignments,
    name = method$name
  )
})


.stratifyTrajectories = function(strat, data, id, envir = parent.frame()) {
  assert_that(
    is.data.table(data),
    is.call(strat) || is.name(strat)
  )

  numIds = uniqueN(data[[id]])

  if (is.name(strat) || is.call(strat) && strat[[1]] == 'function') {
    # function evaluation
    stratfun = eval(strat, envir = envir)
    out = data[, .(Cluster = stratfun(.SD)), by = c(id)]
  } else {
    # expression evaluation
    out = data[, .(Cluster = eval(strat)), by = c(id)]
  }

  assert_that(length(out) == 2, msg = 'expected scalar output from the stratification function. got multiple columns as output')
  assert_that(is.logical(out[['Cluster']]) || is.numeric(out[['Cluster']]) || is.factor(out[['Cluster']]))
  assert_that(nrow(out) == numIds, msg = 'expected scalar output from the stratification function. got multiple values per id')
  assert_that(all(is.finite(out[['Cluster']])), msg = 'some ids are assigned to NA')

  if (is.logical(out[['Cluster']])) {
    out[, Cluster := as.integer(Cluster) + 1L]
  }

  # determine output order
  ids = make.ids(data[[id]])

  out[match(ids, .id), Cluster, env = list(.id = id)]
}


#' @export
#' @title Create a posterior probability matrix from a vector of cluster assignments.
#' @description For each trajectory, the probability of the assigned cluster is 1.
#' @param assignments Integer vector indicating cluster assignment per trajectory
#' @param k The number of clusters.
postprobFromAssignments = function(assignments, k) {
  assert_that(
    is.count(k),
    is.integer(assignments),
    all(is.finite(assignments)),
    min(assignments) >= 1,
    max(assignments) <= k
  )

  postprob = matrix(0, nrow = length(assignments), ncol = k)
  idxMat = cbind(seq_along(assignments), assignments)
  postprob[idxMat] = 1
  return(postprob)
}

#' @export
#' @title Mean ignoring NAs
#' @inheritParams base::mean
#' @keywords internal
meanNA = function(x, ...) {
  mean(x, ..., na.rm = TRUE)
}

#' @export
#' @title Weighted arithmetic mean ignoring NAs
#' @inheritParams stats::weighted.mean
#' @keywords internal
weighted.meanNA = function(x, w, ...) {
  weighted.mean(x, w = w, ..., na.rm = TRUE)
}
