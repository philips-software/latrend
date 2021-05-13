#' @include model.R
setClassUnion('functionOrNULL', members = c('function', 'NULL'))
setClass(
  'lcModelCustom',
  representation(
    trajectoryAssignments = 'integer',
    clusterTrajectories = 'data.frame',
    trajectories = 'data.frame',
    converged = 'numeric',
    postprob = 'matrix',
    name = 'character',
    predict = 'functionOrNULL',
    predictPostprob = 'functionOrNULL'
  ),
  contains = 'lcModel'
)

.lcModelCustom = function(...) new('lcModelCustom', ...)

#' @export
#' @title Specify a model based on a pre-computed result.
#' @param data The data on which the cluster result is based, a data.frame.
#' @param trajectoryAssignments A vector indicating cluster membership per strata. Either a `numeric` vector with range `1:numClus`, or a `factor`.
#' @param clusterTrajectories The cluster trajectories as a data.frame, or a function computing the center trajectory based on the strata of the respective cluster.
#' @param clusterNames The names of the clusters. Optional.
#' @param model An optional object representing the internal model.
#' @param trajectories The fitted trajectories.
#' @param response The response variable.
#' @param time The time variable.
#' @param id The id variable.
#' @param name The name of the model.
#' @param converged Convergence state of the model. TRUE by default.
#' @param postprob Optional posterior probability matrix.
#' @param predict Predict function for the response.
#' @param predictPostprob Predict function for the posterior probability.
#' @param method The method used to create this lcModelCustom instance. Optional.
lcModelCustom = function(data,
                         response,
                         trajectoryAssignments = NULL,
                         clusterTrajectories = mean,
                         trajectories = data,
                         time = getOption('latrend.time'),
                         id = getOption('latrend.id'),
                         clusterNames = NULL,
                         converged = TRUE,
                         postprob = NULL,
                         model = NULL,
                         name = 'custom',
                         predict = NULL,
                         predictPostprob = NULL,
                         method = new('lcMethod')) {
  call = match.call()

  # Data
  assert_that(
    is.data.frame(data),
    is.scalar(time),
    is.scalar(id),
    has_name(data, response),
    has_name(data, id),
    has_name(data, time)
  )
  nIds = uniqueN(data[[id]])
  times = unique(data[[time]]) %>% sort

  # postprob
  if (!is.null(postprob)) {
    assert_that(
      is.matrix(postprob),
      nrow(postprob) == nIds,
      noNA(postprob),
      min(postprob) >= 0,
      max(postprob) <= 1,
      all(rowSums(postprob) == 1)
    )
  }

  # Cluster assignments
  if (is.null(trajectoryAssignments)) {
    assert_that(
      !is.null(postprob),
      msg = 'postprob must be specified when trajectoryAssignments is null'
    )
    trajectoryAssignments = apply(postprob, 1, which.max)
    nClusters = ncol(postprob)
    if (is.null(clusterNames)) {
      clusterNames = colnames(postprob)
    }
  } else {
    assert_that(
      is.factor(trajectoryAssignments) ||
        all(vapply(trajectoryAssignments, is.count, FUN.VALUE = FALSE))
    )
    assert_that(
      noNA(trajectoryAssignments),
      length(trajectoryAssignments) == nIds
    )
    if (is.null(clusterNames) && is.factor(trajectoryAssignments)) {
      clusterNames = levels(trajectoryAssignments)
    }
    trajectoryAssignments = as.integer(trajectoryAssignments)
    nClusters = max(trajectoryAssignments)
  }
  assert_that(nClusters >= 1)

  # postprob generation
  if (is.null(postprob)) {
    postprob = matrix(0, nrow = nIds, ncol = nClusters)
    idxMat = cbind(1:nIds, trajectoryAssignments)
    postprob[idxMat] = 1
    colnames(postprob) = clusterNames
  }

  # Cluster names
  if (is.null(clusterNames)) {
    clusterNames = make.clusterNames(nClusters)
  }
  assert_that(is.character(clusterNames))
  assert_that(length(clusterNames) == nClusters)

  # Trajectories
  assert_that(is.null(trajectories) || is.data.frame(trajectories))

  # Cluster trajectories
  assert_that(is.data.frame(clusterTrajectories) || is.function(clusterTrajectories))
  if (is.function(clusterTrajectories)) {
    # compute cluster trajectories
    center = clusterTrajectories
    rowClusters = trajectoryAssignments[rleidv(data[[id]])]
    clusterTrajectories = as.data.table(data) %>%
      .[, center(get(response)), by = .(rowClusters, get(time))] %>%
      setnames(c('Cluster', time, response))
  }
  assert_that(has_name(clusterTrajectories, 'Cluster'))
  assert_that(has_name(clusterTrajectories, response))
  assert_that(has_name(clusterTrajectories, time))

  # Converged
  assert_that(is.scalar(converged))
  assert_that(is.logical(converged) ||
                is.numeric(converged) || is.integer(converged))
  assert_that(is.finite(converged))

  # Predict
  assert_that(is.null(predict) || is.function(predict))
  assert_that(is.null(predictPostprob) ||
                is.function(predictPostprob))

  # Create object
  object = .lcModelCustom(
    call = call,
    method = method,
    data = data,
    response = response,
    time = time,
    id = id,
    clusterNames = clusterNames,
    trajectoryAssignments = trajectoryAssignments,
    clusterTrajectories = clusterTrajectories,
    converged = as.numeric(converged),
    name = name,
    postprob = postprob,
    predict = predict,
    predictPostprob = predictPostprob
  )
  return(object)
}

#' @export
#' @rdname is
is.lcModelCustom = function(x) {
  is.lcModel(x) && is(x, 'lcModelCustom')
}

#' @rdname interface-custom
setMethod('getName', signature('lcModelCustom'), function(object, ...) object@name)

#' @rdname interface-custom
setMethod('getShortName', signature('lcModelCustom'), function(object, ...) 'custom')

#' @rdname interface-custom
setMethod('converged', signature('lcModelCustom'), function(object, ...) object@converged)

#' @rdname interface-custom
setMethod('postprob', signature('lcModelCustom'), function(object, ...) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @export
#' @rdname interface-custom
predict.lcModelCustom = function(object, ..., newdata = NULL, what = 'mu') {
  if (is.null(object@predict)) {
    NULL
  } else {
    object@predict(object, newdata, what, ...)
  }
}


#. predictPostprob ####
#' @rdname interface-custom
setMethod('predictPostprob', signature('lcModelCustom'), function(object, newdata = NULL, ...) {
  pp = object@predictPostprob(object, newdata, ...)

  assert_that(
    is.matrix(pp),
    nrow(pp) == nIds(object),
    noNA(pp),
    min(pp) >= 0,
    max(pp) <= 1,
    all(rowSums(pp) == 1)
  )

  colnames(pp) = clusterNames(object)
  return(pp)
})

#' @rdname interface-custom
#' @inheritParams clusterTrajectories
setMethod('clusterTrajectories', signature('lcModelCustom'),
  function(object, at = time(object), ...) {

  if (all(at %in% time(object))) {
    dt_traj = object@clusterTrajectories %>%
      as.data.table() %>%
      .[, Cluster := factor(Cluster,
                            levels = 1:nClusters(object),
                            labels = clusterNames(object))]
  } else if (is.null(object@predict)) {
    stop('predict() not specified for this model')
  } else {
    dt_traj = object@predict(object, at, ...)
  }

  dt_traj[]
})

#' @rdname interface-custom
#' @inheritParams trajectories
setMethod('fittedTrajectories', signature('lcModelCustom'), function(object, at, ...) {
  if (all(at %in% time(object))) {
    object@trajectories
  } else {
    stop('not supported')
  }
})
