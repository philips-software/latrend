#' @include modelApprox.R
setClass(
  'lcModelPartition',
  representation(
    name = 'character',
    center = 'function',
    trajectoryClusterIndices = 'integer'
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
#' @param method Optional `lcMethod` object that was used for fitting this model to the data.
#' @param envir The `environment` associated with the model. Used for evaluating the assigned `data` object by [model.data.lcModel].
#' @examples
#' # comparing a model to the ground truth using the adjusted Rand index
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
#' model <- latrend(method, latrendData, nClusters = 3)
#'
#' # extract the reference class from the Class column
#' trajLabels <- aggregate(Class ~ Id, head, 1, data = latrendData)
#' trajLabels$Cluster <- trajLabels$Class
#' refModel <- lcModelPartition(latrendData, response = "Y", trajectoryAssignments = trajLabels)
#'
#' if (require("mclustcomp")) {
#'   externalMetric(model, refModel, "adjustedRand")
#' }
lcModelPartition = function(
  data,
  response,
  trajectoryAssignments,
  nClusters = NA,
  clusterNames = NULL,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  name = 'part',
  center = meanNA,
  method = NULL,
  envir = parent.frame()
) {
  assert_that(
    is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, id),
    is.character(clusterNames) || is.null(clusterNames),
    is.na(nClusters) || length(clusterNames) %in% c(0, nClusters),
    noNA(trajectoryAssignments)
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

  if (is.null(clusterNames)) {
    clusterNames = make.clusterNames(numClus)
  }
  assert_that(length(clusterNames) == numClus)

  mc = match.call()
  model = new(
    'lcModelPartition',
    call = mc,
    data = data,
    name = name,
    clusterNames = clusterNames,
    center = center,
    trajectoryClusterIndices = intAssignments,
    id = id,
    time = time,
    response = response,
    estimationTime = 0
  )

  if (!is.null(method)) {
    model@method = method
  }

  environment(model) = envir

  model
}

#. clusterTrajectories
#' @rdname interface-custom
#' @param center The function to use to compute the cluster trajectory center at the respective moment in time.
setMethod('clusterTrajectories', 'lcModelPartition',
  function(
    object,
    at = time(object),
    center = object@center,
    approxFun = approx,
    ...
  ) {
  if (length(at) > 0) {
    return(callNextMethod())
  }

  if (is.null(center)) {
    center = meanNA
  }

  assert_that(
    is.function(center),
    length(formalArgs(center)) > 0,
    msg = 'center argument must be a function accepting an input vector'
  )

  data = as.data.table(model.data(object))
  response = responseVariable(object)
  time = timeVariable(object)
  trajClusters = trajectoryAssignments(object)
  rowClusters = trajClusters[make.idRowIndices(object)]
  assert_that(length(rowClusters) == nrow(data))

  # compute cluster trajectories at all moments in time
  clusTrajs = data[,
    .(Value = center(get(response))),
    keyby = .(Cluster = rowClusters, Time = get(time))
  ]

  if (uniqueN(trajClusters) < nClusters(object)) {
    emptyMask = clusterSizes(object) == 0
    warning(
      sprintf(
        'Cannot compute cluster trajectory for cluster(s) "%s": no trajectories were assigned to the cluster(s).',
        paste0(clusterNames(object)[emptyMask], collapse = '", "')
      )
    )
    # add missing clusters
    emptyClusTraj = clusTrajs[, .(Time = unique(Time), Value = NaN)]
    clusTrajs = rbind(
      clusTrajs,
      data.table(
        Cluster = rep(
          setdiff(seq_len(nClusters(object)), unique(rowClusters)),
          each = nrow(emptyClusTraj)
        ),
        emptyClusTraj
      )
    )
  }

  setnames(clusTrajs, 'Value', response)
  setnames(clusTrajs, 'Time', time)

  as.data.frame(clusTrajs)
})


#. converged ####
#' @rdname interface-custom
setMethod('converged', 'lcModelPartition', function(object, ...) {
  TRUE
})


# . getName ####
#' @rdname interface-custom
setMethod('getName', 'lcModelPartition', function(object, ...) object@name)

# . getShortName ####
#' @rdname interface-custom
setMethod('getShortName', 'lcModelPartition', function(object, ...) object@name)


#. postprob ####
#' @rdname interface-custom
setMethod('postprob', 'lcModelPartition', function(object, ...) {
  pp = postprobFromAssignments(object@trajectoryClusterIndices, k = nClusters(object))
  colnames(pp) = clusterNames(object)
  return(pp)
})
