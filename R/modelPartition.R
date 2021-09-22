#' @include modelApprox.R
setClass(
  'lcModelPartition',
  representation(
    center = 'function',
    clusterTrajectories = 'data.frame',
    postprob = 'matrix',
    name = 'character'
  ),
  contains = 'lcApproxModel'
)

#' @export
#' @title Create a lcModel with pre-defined partitioning
#' @description Represents an arbitrary partitioning of a set of trajectories.
#' As such, this model has no predictive capabilities. The cluster trajectories are represented by the specified center function (mean by default).
#' @inheritParams lcMethodStratify
#' @param data A `data.frame` representing the trajectory data.
#' @param trajectoryAssignments A `vector` of cluster membership per trajectory, a `data.frame` with an id column and `"Cluster"` column, or the name of the cluster membership column in the `data` argument..
#' For `vector` input, the type must be `factor`, `character`, or `integer` (`1` to `nClusters`).
#' The order of the trajectory, and thus the respective assignments, is determined by the id column of the data.
#' Provide a `factor` id column for the input data to ensure that the ordering is as you aspect.
#' @param nClusters The number of clusters. Should be `NA` for trajectory assignments of type `factor`.
#' @param clusterNames The names of the clusters, or a function with input `n` outputting a `character vector` of names.
#' If unspecified, the names are determined from the `trajectoryAssignments` argument.
#' @param envir The `environment` associated with the model. Used for evaluating the assigned `data` object by [model.data.lcModel].
#' @examples
#' # comparing a model to the ground truth using the adjusted Rand index
#' data(latrendData)
#' model <- latrend(lcMethodKML(), data = latrendData, response = "Y")
#' # extract the reference class from the Class column
#' trajLabels <- aggregate(Class ~ Id, head, 1, data = latrendData)
#' trajLabels$Cluster <- trajLabels$Class
#' refModel <- lcModelPartition(latrendData, response = "Y", trajectoryAssignments = trajLabels)
#' externalMetric(model, refModel, 'adjustedRand') # 0.76
lcModelPartition = function(data,
                            response,
                            trajectoryAssignments,
                            nClusters = NA,
                            center = meanNA,
                            clusterNames = NULL,
                            time = getOption('latrend.time'),
                            id = getOption('latrend.id'),
                            name = 'part',
                            envir = parent.frame()) {
  assert_that(
    is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, id),
    is.character(clusterNames) || is.null(clusterNames),
    is.na(nClusters) || length(clusterNames) %in% c(0, nClusters),
    noNA(trajectoryAssignments),
    is.function(center)
  )


  if (is.na(nClusters) && length(clusterNames) > 0) {
    nClusters = length(clusterNames)
  }

  if (length(trajectoryAssignments) == 1 &&
      is.character(trajectoryAssignments) &&
      uniqueN(data[[id]]) > 1) {
    # cluster column name
    assert_that(has_name(data, trajectoryAssignments), msg = 'cluster column specified in "trajectoryAssignments" argument is not present in data')
    trajectoryAssignments = aggregate(data[[trajectoryAssignments]], by = list(Id = data[[id]]), FUN = head, 1) %>%
      set_names(c(id, 'Cluster'))
  }

  if (is.data.frame(trajectoryAssignments)) {
    # data.frame input
    assert_that(
      has_name(trajectoryAssignments, id),
      has_name(trajectoryAssignments, 'Cluster'),
      all(trajectoryAssignments[[id]] %in% unique(data[[id]]))
    )

    newOrder = match(trajectoryAssignments[[id]], levels(factor(data[[id]])))
    assert_that(
      noNA(newOrder),
      uniqueN(newOrder) == length(newOrder)
    )

    trajectoryAssignments = trajectoryAssignments$Cluster[newOrder]
  }

  # vector input
  assert_that(
    length(trajectoryAssignments) == uniqueN(data[[id]]),
    msg = 'Number of trajectory assignments does not match the number of trajectories (ids) in the data'
  )

  if (is.numeric(trajectoryAssignments)) {
    # integer
    assert_that(all(vapply(trajectoryAssignments, is.count, FUN.VALUE = TRUE)), msg = 'numeric input must be integer from 1,2,3,...')

    if (is.na(nClusters)) {
      numClus = max(trajectoryAssignments)
    } else {
      assert_that(max(trajectoryAssignments) <= nClusters)
      numClus = nClusters
    }
  } else if (is.character(trajectoryAssignments)) {
    # character
    if (is.na(nClusters)) {
      numClus = uniqueN(trajectoryAssignments)
    } else {
      assert_that(uniqueN(trajectoryAssignments) <= nClusters)
      numClus = nClusters
    }

    if (is.null(clusterNames)) {
      trajectoryAssignments = factor(trajectoryAssignments)
      clusterNames = levels(trajectoryAssignments)
    } else {
      assert_that(all(trajectoryAssignments %in% clusterNames))
      trajectoryAssignments = factor(trajectoryAssignments, levels = clusterNames)
    }
  } else if (is.factor(trajectoryAssignments)) {
    # factor
    assert_that(is.na(nClusters), msg = 'nClusters cannot be specified for trajectoryAssignments of type factor')
    if (is.null(clusterNames)) {
      clusterNames = levels(trajectoryAssignments)
    } else {
      assert_that(nlevels(trajectoryAssignments) == length(clusterNames))
      trajectoryAssignments = factor(trajectoryAssignments, levels = clusterNames, labels = clusterNames)
    }
    numClus = nlevels(trajectoryAssignments)
  } else {
    stop('unsupported input type for trajectoryAssignments argument')
  }



  intAssignments = as.integer(trajectoryAssignments)
  assert_that(min(intAssignments) >= 1, max(intAssignments) <= numClus)

  pp = postprobFromAssignments(intAssignments, k = numClus)

  if (is.null(clusterNames)) {
    clusterNames = make.clusterNames(numClus)
  }
  assert_that(length(clusterNames) == numClus)

  clusTrajs = computeCenterClusterTrajectories(
    data,
    assignments = intAssignments,
    nClusters = numClus,
    fun = center,
    id = id,
    time = time,
    response = response
  )

  mc = match.call()
  model = new(
    'lcModelPartition',
    call = mc,
    data = data,
    center = center,
    clusterTrajectories = clusTrajs,
    postprob = pp,
    name = name,
    clusterNames = clusterNames,
    id = id,
    time = time,
    response = response,
    estimationTime = 0
  )
  environment(model) = envir
  return(model)
}

#' @rdname interface-custom
setMethod('clusterTrajectories', signature('lcModelPartition'), function(object, at = time(object), ...) {
  if (length(at) == 0) {
    clusTrajs = as.data.table(object@clusterTrajectories)
    clusTrajs[, Cluster := factor(Cluster,
                                  levels = seq_len(nClusters(object)),
                                  labels = clusterNames(object))]
    return(clusTrajs[])
  } else {
    callNextMethod()
  }
})


#. converged ####
#' @rdname interface-custom
setMethod('converged', signature('lcModelPartition'), function(object, ...) {
  TRUE
})


# . getName ####
#' @rdname interface-custom
setMethod('getName', signature('lcModelPartition'), function(object, ...) object@name)

# . getShortName ####
#' @rdname interface-custom
setMethod('getShortName', signature('lcModelPartition'), function(object, ...)
  object@name)


#. postprob ####
#' @rdname interface-custom
setMethod('postprob', signature('lcModelPartition'), function(object, ...) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})



computeCenterClusterTrajectories = function(data,
                                            assignments,
                                            nClusters,
                                            fun = mean,
                                            id,
                                            time,
                                            response) {
  assert_that(
    is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, id)
  )
  assert_that(nClusters >= 1)
  assert_that(
    is.integer(assignments),
    all(is.finite(assignments)),
    all(vapply(assignments, is.count, FUN.VALUE = TRUE)),
    length(assignments) == uniqueN(data[[id]]),
    min(assignments) >= 1,
    max(assignments) <= nClusters
  )
  assert_that(is.function(fun))

  rowClusters = assignments[rleidv(data[[id]])]
  clusTrajs = as.data.table(data) %>%
    .[, .(Value = fun(get(response))), by = .(Cluster = rowClusters, Time = get(time))]

  if (uniqueN(assignments) < nClusters) {
    warning(
      'empty clusters present. cluster trajectory for empty clusters will be set constant at 0'
    )
    # add missing clusters
    emptyClusTraj = clusTrajs[, .(Time = unique(Time), Value = 0)]
    clusTrajs = rbind(clusTrajs,
                      data.table(Cluster = rep(
                        setdiff(seq_len(nClusters), unique(rowClusters)), each = nrow(emptyClusTraj)
                      ),
                      emptyClusTraj))
  }

  setnames(clusTrajs, 'Value', response)
  setnames(clusTrajs, 'Time', time)
  return(clusTrajs[])
}
