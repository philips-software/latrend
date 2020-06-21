#' @include clModelPartition.R
setClass('clModelWeightedPartition', contains = 'clModelPartition')

#' @export
#' @title Create a clModel with pre-defined weighted partitioning
#' @param weights A `numIds` x `numClusters` matrix of partition probabilities.
#' @param nClusters The number of clusters
#' @param clusterNames The names of the clusters, or a function with input `n` outputting a `character vector` of names.
clModelWeightedPartition = function(data,
                                    weights,
                                    center = weighted.meanNA,
                                    clusterNames = colnames(weights),
                                    response = getOption('cluslong.response'),
                                    time = getOption('cluslong.time'),
                                    id = getOption('cluslong.id'),
                                    name = 'wpart') {
  assert_that(
    is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, id)
  )
  assert_that(
    is.matrix(weights),
    all(is.finite(weights)),
    all(weights >= 0),
    nrow(weights) == uniqueN(data[[id]])
  )
  if (is.null(clusterNames)) {
    clusterNames = make.clusterNames(ncol(weights))
  } else if (is.function(clusterNames)) {
    clusterNames = clusterNames(ncol(weights))
  }
  assert_that(is.character(clusterNames),
              length(clusterNames) == ncol(weights))
  assert_that(is.function(center))

  # normalize weights
  pp = apply(weights, 2, '/', rowSums(weights))

  clusTrajs = computeWeightedCenterClusterTrajectories(
    data,
    postprob = pp,
    fun = center,
    id = id,
    time = time,
    response = response
  )

  mc = match.call()
  new(
    'clModelWeightedPartition',
    call = mc,
    center = center,
    clusterTrajectories = clusTrajs,
    postprob = pp,
    name = name,
    clusterNames = clusterNames,
    id = id,
    time = time,
    response = response
  )
}

#' @export
computeWeightedCenterClusterTrajectories = function(data, postprob, fun =
                                                      weighted.mean, id, time, response) {
  assert_that(
    is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, id)
  )
  assert_that(
    is.matrix(postprob),
    nrow(postprob) == uniqueN(data[[id]]),
    ncol(postprob) > 0,
    all(is.finite(postprob))
  )
  assert_that(is.function(fun))
  rowProbsMat = postprob[rleidv(data[[id]]),] %>%
    set_colnames(NULL)

  clusIdData = data.table(Value = data[[response]],
                          Time = data[[time]],
                          ClusProb = rowProbsMat) %>%
    melt(
      id.vars = c('Value', 'Time'),
      measure.vars = names(.)[c(-1, -2)],
      variable.name = 'Cluster',
      value.name = 'Prob'
    )

  clusTrajs = clusIdData[, .(Value = fun(Value, w = Prob)), by = .(Cluster, Time)]
  clusTrajs[, Cluster := as.integer(Cluster)]
  setnames(clusTrajs, 'Value', response)
  setnames(clusTrajs, 'Time', time)
  return(clusTrajs[])
}
