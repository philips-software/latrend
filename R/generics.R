#' @name latrend-generics
#' @title Method- and model-specific generics defined by the latrend package
#' @description List of S4 generic methods which have no general use other than supporting
#' functions with signatures of `lcMethod` or `lcModel`.
#' @param object The object.
#' @param method The method.
#' @param data `data.frame`.
#' @param newdata `data.frame` of newdata.
#' @param name Metric name.
#' @param envir `environment`.
#' @param what Parameter.
#' @param cluster Cluster name.
#' @param verbose [R.utils::Verbose].
#' @param ... Arguments.
#' @keywords internal
NULL


# clusterProportions ####
#' @export
#' @name latrend-generics
setGeneric('clusterProportions', function(object, ...) {
  props <- standardGeneric('clusterProportions')

  valid = validate_that(
    is.numeric(props),
    length(props) == nClusters(object),
    noNA(props),
    all(is.finite(props)),
    min(props) >= 0,
    max(props) <= 1
  )

  if (!isTRUE(valid)) {
    stop(
      sprintf(
        'Invalid output returned by clusterProportions(%s, ...): %s',
        class(object)[1],
        valid
      )
    )
  }

  names(props) = clusterNames(object)
  props
})


# clusterTrajectories ####
#' @export
#' @name latrend-generics
#' @param at A vector of times at which to compute the cluster trajectories.
setGeneric('clusterTrajectories', function(object, at = time(object), ...) {
  assert_that(is_at(at))

  dfclus <- standardGeneric('clusterTrajectories')

  valid = validate_that(
    is.data.frame(dfclus),
    names(dfclus)[1] == 'Cluster',
    names(dfclus)[2] == timeVariable(object),
    names(dfclus)[3] == responseVariable(object)
  )

  if (!isTRUE(valid)) {
    stop(
      sprintf(
        '%1$s implemention error: output of clusterTrajectories(%1$s, ...) is not valid: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  return (dfclus)
})


# compose ####
#' @export
#' @name latrend-generics
setGeneric('compose', def = function(method, envir, ...) {
  newmethod <- standardGeneric('compose')

  assert_that(
    is.lcMethod(newmethod),
    msg = sprintf(
      '%1$s implemention error: output of compose(%1$s, ...) must be of type lcMethod, not %2$s',
      class(method)[1],
      class(newmethod)[2]
    )
  )

  assert_that(
    is.character(idVariable(newmethod)),
    is.character(timeVariable(newmethod)),
    is.character(responseVariable(newmethod)),
    msg = sprintf(
      '%1$s implemention error: output of compose(%1$s, ...) must contain id, time, and response variables',
      class(method)[1]
    )
  )

  return (newmethod)
})

# converged ####
#' @export
#' @name latrend-generics
setGeneric('converged', function(object, ...) {
  state <- standardGeneric('converged')

  assert_that(
    is.logical(state) || is.numeric(state),
    length(state) == 1,
    msg = sprintf(
      '%1$s implemention error: output of converged(%1$s, ...) must be either logical or numeric, and of length 1',
      class(object)[1]
    )
  )

  return (state)
})

# externalMetric ####
#' @export
#' @name latrend-generics
#' @param object2 The model to compare with.
setGeneric('externalMetric', function(
  object,
  object2,
  name = getOption('latrend.externalMetric'),
  ...)

  standardGeneric('externalMetric')
)


# estimationTime ####
#' @export
#' @name latrend-generics
#' @param unit Time unit.
setGeneric('estimationTime', function(object, unit = 'secs', ...) {
  duration <- standardGeneric('estimationTime')

  assert_that(
    is.scalar(duration),
    is.finite(duration)
  )

  return (duration)
})


# fit ####
#' @export
#' @name latrend-generics
setGeneric('fit', function(method, data, envir, verbose, ...) {
  dateStart = Sys.time()
  start = .tic()
  model <- standardGeneric('fit')
  estimationTime = .toc(start)

  assert_that(
    is.lcModel(model),
    msg = sprintf('%1$s implementation error: fit(%1$s, ...) should output an lcModel object, not %2$s',
      class(method)[1],
      class(model)[1]
    )
  )

  model@method = method
  model@id = idVariable(method)
  model@time = timeVariable(method)
  model@response = responseVariable(method)
  model@label = getLabel(method)
  model@date = dateStart
  model@estimationTime = as.numeric(estimationTime, 'secs')

  return (model)
})


# fittedTrajectories ####
#' @export
#' @name latrend-generics
setGeneric('fittedTrajectories', function(
  object,
  at = time(object),
  what = 'mu',
  clusters = trajectoryAssignments(object),
  ...) {

  assert_that(is_at(at))

  standardGeneric('fittedTrajectories')
})


# getLabel ####
#' @export
#' @name latrend-generics
setGeneric('getLabel', function(object, ...) {
  label <- standardGeneric('getLabel')

  assert_that(
    is.character(label),
    length(label) == 1
  )

  return (label)
})


# getName ####
#' @export
#' @name latrend-generics
setGeneric('getName', function(object, ...) {
  name <- standardGeneric('getName')

  assert_that(
    is.character(name),
    length(name) == 1,
    nchar(name) > 0
  )

  return (name)
})


# getShortName ####
#' @export
#' @name latrend-generics
setGeneric('getShortName', function(object, ...) {
  name <- standardGeneric('getShortName')

  assert_that(
    is.character(name),
    length(name) == 1,
    nchar(name) > 0
  )

  return (name)
})


# idVariable ####
#' @export
#' @name latrend-generics
setGeneric('idVariable', function(object, ...) {
  id <- standardGeneric('idVariable')

  assert_that(
    is.character(id),
    length(id) == 1,
    nchar(id) > 0
  )

  return (id)
})


# metric ####
#' @export
#' @name latrend-generics
setGeneric('metric', function(
  object,
  name = getOption('latrend.metric', c('WRSS', 'APPA', 'AIC', 'BIC')),
  ...) standardGeneric('metric'))


# plotFittedTrajectories ####
#' @export
#' @name latrend-generics
setGeneric('plotFittedTrajectories',
  function(object, ...) standardGeneric('plotFittedTrajectories'))


# plotClusterTrajectories ####
#' @export
#' @name latrend-generics
setGeneric('plotClusterTrajectories',
  function(object, ...) standardGeneric('plotClusterTrajectories'))


# plotTrajectories ####
#' @export
#' @rdname plotTrajectories
#' @title Plot the data trajectories
#' @description Plots the output of [trajectories] for the given object.
setGeneric('plotTrajectories', function(object, ...) standardGeneric('plotTrajectories'))


# postFit ####
#' @export
#' @name latrend-generics
setGeneric('postFit', function(method, data, model, envir, verbose, ...) {
  model <- standardGeneric('postFit')

  assert_that(
    inherits(model, 'lcModel'),
    msg = sprintf(
      '%1$s implementation error: postFit should return an object of type lcModel, not %2$s',
      class(method)[1],
      class(model)[1]
    )
  )

  return (model)
})


# postprob ####
#' @export
#' @name latrend-generics
setGeneric('postprob', function(object, ...) {
  pp <- standardGeneric('postprob')

  assert_that(
    is.numeric(pp),
    ncol(pp) == nClusters(object),
    nrow(pp) == nIds(object)
  )

  colnames(pp) = clusterNames(object)

  valid = validate_that(is_valid_postprob(pp, object))
  if(!isTRUE(valid)) {
    warning(
      sprintf(
        '%1$s implementation error: Output returned by postprob(%1$s, ...) was not valid: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  pp
})


# predictAssignments ####
#' @export
#' @name latrend-generics
setGeneric('predictAssignments', function(object, newdata = NULL, ...) {
  assert_that(is_newdata(newdata))

  assignments <- standardGeneric('predictAssignments')

  assert_that(
    is.null(newdata) || length(assignments) == nrow(newdata)
  )

  make.trajectoryAssignments(object, assignments)
})


# predictForCluster ####
#' @export
#' @name latrend-generics
setGeneric('predictForCluster', function(object, newdata = NULL, cluster, ...) {
  assert_that(
    is_newdata(newdata),
    is.scalar(cluster),
    cluster %in% clusterNames(object)
  )

  # special case for when no newdata is provided
  if (is.null(newdata)) {
    newdata = model.data(object)
    if (has_name(newdata, 'Cluster')) {
      warning('model data used in predictForCluster() contains a "Cluster" column. This column will be ignored.')
    }
  }
  else {
    if (nrow(newdata) == 0) {
      warning('called predictForCluster() with empty newdata data.frame (nrow = 0)')
    }
    if (has_name(newdata, 'Cluster')) {
      warning('newdata for predictForCluster() contains a "Cluster" column. This column will be ignored.')
    }
  }

  if (!has_name(newdata, timeVariable(object))) {
    stop(
      sprintf(
        'newdata argument of predictForCluster() requires the time index column "%s"',
        timeVariable(object)
      )
    )
  }

  if (has_name(newdata, 'Cluster')) {
    newdata[['Cluster']] = NULL
  }

  out <- standardGeneric('predictForCluster')

  assert_that(
    is.numeric(out) || is.data.frame(out),
    msg = sprintf(
      '%1$s implementation error: predictForCluster(%1$s, ...) implementation returned output of type %$2s. Only numeric and data.frame are supported outputs.',
      class(object)[1],
      class(out)[1]
    )
  )

  if (!is.null(newdata)) {
    if (is.numeric(out)) {
      assert_that(
        length(out) == nrow(newdata),
        msg = sprintf(
          '%1$s implementation error: predictForCluster(%1$s, ...) returned numeric vector with %2$d predictions, but should have returned %3$d predictions.',
          class(object)[1],
          length(out),
          nrow(newdata)
        )
      )
    } else if (is.data.frame(out)) {
      assert_that(
        nrow(out) == nrow(newdata),
        msg = sprintf(
          '%1$s implementation error: predictForCluster(%1$s, ...) returned data.frame with %2$d predictions (rows), but should have returned %3$d predictions.',
          class(object)[1],
          nrow(out),
          nrow(newdata)
        )
      )
    }
  }

  return (out)
})


# predictPostprob ####
#' @export
#' @name latrend-generics
setGeneric('predictPostprob', function(object, newdata = NULL, ...) {
  assert_that(is_newdata(newdata))

  pp <- standardGeneric('predictPostprob')

  valid = validate_that(
    is.null(newdata) || nrow(pp) == nrow(newdata),
    is_valid_postprob(pp, object)
  )

  if (!isTRUE(valid)) {
    stop(
      sprintf(
        '%1$s implementation error: predictForCluster(%1$s, ...) has invalid output: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  colnames(pp) = clusterNames(object)
  return (pp)
})


# prepareData ####
#' @export
#' @name latrend-generics
setGeneric('prepareData', function(method, data, verbose, ...) {
  envir <- standardGeneric('prepareData')
  assert_that(
    is.environment(envir),
    msg = sprintf(
      'prepareData(%s, ...) should return an environment',
      class(method)[1]
    )
  )

  return (envir)
})


# preFit ####
#' @export
#' @name latrend-generics
setGeneric('preFit', function(method, data, envir, verbose, ...) {
  modelEnv <- standardGeneric('preFit')
  assert_that(
    is.environment(modelEnv),
    msg = sprintf(
      'preFit(%s, ...) should return an environment',
      class(method)[1]
    )
  )

  return (modelEnv)
})


# qqPlot ####
#' @export
#' @name latrend-generics
#' @title Quantile-quantile plot
setGeneric('qqPlot', function(object, ...) standardGeneric('qqPlot'))


# responseVariable ####
#' @export
#' @name latrend-generics
setGeneric('responseVariable', function(object, ...) {
  response <- standardGeneric('responseVariable')

  assert_that(
    is.character(response),
    length(response) == 1,
    nchar(response) > 0
  )

  return (response)
})


# strip ####
#' @export
#' @name latrend-generics
#' @description Reduce the (serialized) memory footprint of an object.
#' @details Serializing references to environments results in the serialization of the object
#' together with any associated environments and references. This method removes those environments
#' and references, greatly reducing the serialized object size.
#' @return The stripped (i.e., updated) object.
setGeneric('strip', function(object, ...) standardGeneric('strip'))


# timeVariable ####
#' @export
#' @name latrend-generics
setGeneric('timeVariable', function(object, ...) {
  time <- standardGeneric('timeVariable')

  assert_that(
    is.character(time),
    length(time) == 1,
    nchar(time) > 0
  )

  return (time)
})


# trajectories ####
#' @export
#' @name trajectories
#' @rdname trajectories
#' @title Extract the trajectories
#' @description Transform or extract the trajectories from the given object to a standardized format.
#'
#' The standardized data format is for method estimation by [latrend], and for plotting functions.
#' @param object The data or model or extract the trajectories from.
#' @param id The identifier variable name.
#' @param time The time variable name.
#' @param response The response variable name.
#' @param ... Additional arguments.
#' @return A `data.frame` with columns matching the `id`, `time`, and `response` name arguments.
#' @seealso [plotTrajectories] [latrend]
setGeneric('trajectories', function(object, ...) {
  data <- standardGeneric('trajectories')

  assert_that(
    is.data.frame(data),
    ncol(data) > 2
  )

  return (data)
})


# trajectoryAssignments ####
#' @export
#' @name latrend-generics
setGeneric('trajectoryAssignments', function(object, ...) {
  clusters <- standardGeneric('trajectoryAssignments')
  assert_that(
    is.factor(clusters),
    !is.lcModel(object) || all(levels(clusters) == clusterNames(object))
  )

  return (clusters)
})


# validate ####
#' @export
#' @name latrend-generics
setGeneric('validate', function(method, data, envir, ...) {
  validationResult <- standardGeneric('validate')

  if (!isTRUE(validationResult)) {
    stop(sprintf('%s validation failed: %s', class(method)[1], validationResult))
  }
})
