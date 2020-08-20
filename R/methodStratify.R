#' @include method.R
#' @importFrom longitudinalData meanNA
setClass('lcMethodStratify', contains = 'lcMethod')

setValidity('lcMethodStratify', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodStratify)))

  if (isArgDefined(object, 'nClusters')) {
    assert_that(is.na(object$nClusters) || is.count(object$nClusters))
  }

  if (isArgDefined(object, 'clusterNames')) {
    assert_that(is.null(object$clusterNames) ||
                  is.character(object$clusterNames))
  }

  if (isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})

#' @export
#' @title Specify a stratification method
#' @inheritParams lcMethodCustom
#' @param stratify An `expression` returning a `number` or `factor` value per trajectory, representing the cluster assignment. Alternatively, a `function` can be provided that takes separate trajectory `data.frame` as input.
#' @param center The `function` for computing the longitudinal cluster centers, used for representing the cluster trajectories.
#' @param nClusters The number of clusters. This is optional, as this can be derived from the largest assignment number by default, or the number of `factor` levels.
#' @param clusterNames The names of the clusters. If a `factor` assignment is returned, the levels are used as the cluster names.
#' @examples
#' data(testLongData)
#' # Stratification based on the mean response level
#' method <- lcMethodStratify(
#'    mean(Value) > 1.7,
#'    clusterNames = c("Low", "High"))
#' model <- latrend(method, testLongData)
#' summary(model)
#'
#' # Stratification function
#' stratfun <- function(trajdata) {
#'    trajmean <- mean(trajdata$Y)
#'    factor(trajmean > 1.7,
#'       levels = c(FALSE, TRUE),
#'       labels = c("Low", "High"))
#' }
#' method <- lcMethodStratify(stratfun)
#'
#' # Multiple clusters
#' stratfun3 <- function(trajdata) {
#'    trajmean <- mean(trajdata$Y)
#'    cut(trajmean,
#'       c(-Inf, .5, 2, Inf),
#'       labels = c("Low", "Medium", "High"))
#' }
#' method <- lcMethodStratify(stratfun3)
#' @family lcMethod implementations
lcMethodStratify = function(response,
                            stratify,
                            center = meanNA,
                            nClusters = NaN,
                            clusterNames = NULL,
                            time = getOption('latrend.time'),
                            id = getOption('latrend.id'),
                            name = 'stratify') {
  lcMethod.call('lcMethodStratify', call = match.call.all())
}

setMethod('getName', signature('lcMethodStratify'), function(object) {
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

setMethod('getShortName', signature('lcMethodStratify'), function(object) 'strat')


setMethod('compose', signature('lcMethodStratify'), function(method, envir = NULL) {
  evaluate.lcMethod(method,
                      try = FALSE,
                      exclude = 'stratify',
                      envir = envir)
})


setMethod('fit', signature('lcMethodStratify'), function(method, data, envir, verbose) {
  data = as.data.table(data)
  id = idVariable(method)

  # Stratify
  strat = method[['stratify', eval = FALSE]]
  assignments = stratifyTrajectories(strat,
                                     data = data,
                                     id = id,
                                     envir = environment(method))

  if (is.factor(assignments)) {
    assert_that(is.na(method$nClusters) ||
                  nlevels(assignments) == method$nClusters)
  }
  intAssignments = as.integer(assignments)
  assert_that(is.na(method$nClusters) ||
                max(intAssignments) <= method$nClusters)

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

  # Generate postprob
  postprob = postprobFromAssignments(intAssignments, numClus)

  # Compute cluster trajectories
  clusTrajs = computeCenterClusterTrajectories(
    data,
    intAssignments,
    nClusters = numClus,
    fun = method$center,
    id = id,
    time = timeVariable(method),
    response = responseVariable(method)
  )

  setkey(clusTrajs, Cluster, Time)
  setnames(clusTrajs, c('Cluster', timeVariable(method), responseVariable(method)))

  assert_that(uniqueN(clusTrajs$Cluster) == numClus)

  .lcModelStratify(
    method = method,
    data = data,
    id = id,
    time = timeVariable(method),
    response = responseVariable(method),
    clusterNames = clusNames,
    clusterTrajectories = clusTrajs,
    postprob = postprob,
    name = method$name
  )
})


stratifyTrajectories = function(strat, data, id, envir = parent.frame()) {
  assert_that(is.data.table(data), is.call(strat) || is.name(strat))
  numIds = uniqueN(data[[id]])

  if (is.name(strat) ||
      is.call(strat) && strat[[1]] == 'function') {
    # function evaluation
    stratfun = eval(strat, envir = envir)
    out = data[, stratfun(.SD), by = c(id)]
  } else {
    # expression evaluation
    out = data[, eval(strat), by = c(id)]
  }

  assert_that(length(out) == 2, msg = 'expected scalar output from the stratification function. got multiple columns as output')

  assignments = out[[names(out)[2]]]
  assert_that(is.logical(assignments) ||
                is.numeric(assignments) || is.factor(assignments))
  assert_that(length(assignments) == numIds, msg = 'expected scalar output from the stratification function. got multiple values per id')
  assert_that(all(is.finite(assignments)), msg = 'some ids are assigned to NA')

  if (is.logical(assignments)) {
    return(as.integer(assignments) + 1L)
  } else {
    return(assignments)
  }
}


#' @export
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
weighted.meanNA = function(x, w) {
  weighted.mean(x, w = w, na.rm = TRUE)
}
