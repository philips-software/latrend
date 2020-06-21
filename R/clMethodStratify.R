#' @include clMethod.R
setClass('clMethodStratify', contains = 'clMethod')

setValidity('clMethodStratify', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodStratify)))

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
#' @inheritParams clMethodCustom
#' @param stratify An expression returning a `number` or `factor` value per trajectory, representing the cluster assignment. Alternatively, a `function` that takes separate trajectory `data.frame` as input.
#' @param center Method for computing the longitudinal cluster centers, used for representing the cluster trajectories.
#' @param nClusters Number of clusters. Optional, as this is derived from the largest assignment number by default, or the number of `factor` levels.
#' @param clusterNames Names of the clusters. If a `factor` assignment is returned, the levels are used as the cluster names.
#' @examples
#' # Stratification based on the mean response level
#' method = clMethodStratify(mean(Value) > 1.7, clusterNames=c('Low', 'High'))
#' model = cluslong(method, testLongData)
#'
#' summary(model)
#'
#' # Stratification function
#' stratfun = function(trajdata) {
#'    trajmean = mean(trajdata$Y)
#'    factor(trajmean > 1.7, levels=c(FALSE, TRUE), labels=c('Low', 'High'))
#' }
#' method = clMethodStratify(stratfun)
#'
#' # Multiple clusters
#' stratfun3 = function(trajdata) {
#'    trajmean = mean(trajdata$Y)
#'    cut(trajmean, c(-Inf, .5, 2, Inf), labels=c('Low', 'Medium', 'High'))
#' }
#' method = clMethodStratify(stratfun3)
#'
#' @family clMethod implementations
clMethodStratify = function(stratify,
                            center = meanNA,
                            nClusters = NaN,
                            clusterNames = NULL,
                            response = getOption('cluslong.response'),
                            time = getOption('cluslong.time'),
                            id = getOption('cluslong.id'),
                            name = 'stratify') {
  .clMethod.call('clMethodStratify', call = match.call.defaults())
}

setMethod('getName', signature('clMethodStratify'), function(object) {
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

setMethod('getShortName', signature('clMethodStratify'), function(object) 'strat')


setMethod('compose', signature('clMethodStratify'), function(method, envir = NULL) {
  substitute.clMethod(method,
                      try = FALSE,
                      exclude = 'stratify',
                      envir = envir)
})


setMethod('fit', signature('clMethodStratify'), function(method, data, envir, verbose) {
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

  .clModelStratify(
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
meanNA = function(x) {
  mean(x, na.rm = TRUE)
}

#' @export
weighted.meanNA = function(x, w) {
  weighted.mean(x, w = w, na.rm = TRUE)
}
