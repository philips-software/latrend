#' @include modelPartition.R
setClass('lcModelWeightedPartition',
  representation(
    name = 'character',
    postprob = 'matrix'
  ),
  contains = 'lcApproxModel'
)

#' @export
#' @title Create a lcModel with pre-defined weighted partitioning
#' @inheritParams lcMethodStratify
#' @inheritParams lcModelCustom
#' @param weights A `numIds` x `numClusters` matrix of partition probabilities.
#' @param clusterNames The names of the clusters, or a function with input `n` outputting a `character vector` of names.
lcModelWeightedPartition = function(
  data,
  response,
  weights,
  clusterNames = colnames(weights),
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  name = 'wpart'
) {
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
  }
  else if (is.function(clusterNames)) {
    clusterNames = clusterNames(ncol(weights))
  }

  assert_that(
    is.character(clusterNames),
    length(clusterNames) == ncol(weights)
  )

  # normalize weights
  pp = apply(weights, 2, '/', rowSums(weights))
  assert_that(is_valid_postprob(pp))

  mc = match.call()
  new(
    'lcModelWeightedPartition',
    call = mc,
    data = data,
    postprob = pp,
    name = name,
    clusterNames = clusterNames,
    id = id,
    time = time,
    response = response,
    estimationTime = 0
  )
}

#. clusterTrajectories
#' @rdname interface-custom
#' @param center The function to use to compute the cluster trajectory center at the respective moment in time.
setMethod('clusterTrajectories', 'lcModelWeightedPartition',
  function(
    object,
    at = time(object),
    center = weighted.meanNA,
    approxFun = approx,
    ...
  ) {

    if (length(at) > 0) {
      return(callNextMethod())
    }

    data = as.data.table(model.data(object))
    response = responseVariable(object)
    time = timeVariable(object)

    pp = postprob(object)
    safeClusterNames = paste0('C', seq_len(nClusters(object)))
    rowProbsMat = pp[make.idRowIndices(object), ] %>%
      set_colnames(safeClusterNames)

    clusIdData = data.table(
      Value = data[[response]],
      Time = data[[time]]
    ) %>%
      cbind(rowProbsMat) %>%
      melt(
        id.vars = c('Value', 'Time'),
        measure.vars = names(.)[c(-1, -2)],
        variable.name = 'Cluster',
        value.name = 'Prob'
      )

    clusIdData[, Cluster := factor(Cluster, levels = safeClusterNames, labels = clusterNames(object))]

    emptyMask = clusIdData[, !any(Prob > 0), keyby = Cluster]$V1
    if (any(emptyMask)) {
      warning(
        sprintf(
          'Cannot compute cluster trajectory for cluster(s) "%s": no associated trajectories because all trajectories have zero weight.',
          paste0(clusterNames(object)[emptyMask], collapse = '", "')
        )
      )
    }

    clusTrajs = clusIdData[, .(Value = center(Value, w = Prob)), by = .(Cluster, Time)]

    setnames(clusTrajs, 'Value', response)
    setnames(clusTrajs, 'Time', time)

    as.data.frame(clusTrajs)
})

#. converged ####
#' @rdname interface-custom
setMethod('converged', 'lcModelWeightedPartition', function(object, ...) {
  TRUE
})

# . getName ####
#' @rdname interface-custom
setMethod('getName', 'lcModelWeightedPartition', function(object, ...) object@name)

# . getShortName ####
#' @rdname interface-custom
setMethod('getShortName', 'lcModelWeightedPartition', function(object, ...) object@name)


#. postprob ####
#' @rdname interface-custom
setMethod('postprob', 'lcModelWeightedPartition', function(object, ...) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})
