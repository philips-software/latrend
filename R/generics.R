# nClusters ####
#' @export
#' @name nClusters
#' @title Number of clusters
#' @description Get the number of clusters estimated by the given object.
#' @param object The object
#' @param ... Not used.
#' @return The number of clusters: a scalar `numeric` non-zero count.
setGeneric('nClusters', function(object, ...) {
  nClus <- standardGeneric('nClusters')

  assert_that(
    is.count(nClus),
    nClus > 0,
    msg = 'invalid output for nClusters(): expecting a scalar non-zero count'
  )

  nClus
})


# clusterProportions ####
#' @export
#' @name clusterProportions
#' @title Proportional size of each cluster
#' @description Obtain the proportional size per cluster, between 0 and 1.
#' @param object The model.
#' @param ... Not used.
#' @return A `named numeric vector` of length `nClusters(object)` with the proportional size of each cluster.
#' @seealso [nClusters] [clusterNames]
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
#' @name clusterTrajectories
#' @title Extract cluster trajectories
#' @description Extracts a `data.frame` of the cluster trajectories associated with the given object.
#' @param object The model.
#' @param at A `numeric vector` of the times at which to compute the cluster trajectories.
#' @param ... Not used.
#' @return A `data.frame` of the estimated values at the specified times.
#' The first column should be named "Cluster".
#' The second column should be time, with the name matching the `timeVariable(object)`.
#' The third column should be the expected value of the observations, named after the `responseVariable(object)`.
#' @seealso [plotClusterTrajectories]
setGeneric('clusterTrajectories', function(object, ...) {
  # this mechanism is needed because defining "at" as an argument in the
  # generic function overrides any default value set in a subclass
  mc = match.call()
  if (hasName(mc, 'at')) {
    assert_that(is_at(list(...)[['at']]))
  }
  #

  clusTrajs <- standardGeneric('clusterTrajectories')

  if (inherits(object, 'lcModel')) {
    valid = validate_that(
      is.data.frame(clusTrajs),
      names(clusTrajs)[1] == 'Cluster',
      names(clusTrajs)[2] == timeVariable(object),
      names(clusTrajs)[3] == responseVariable(object)
    )
  } else {
    valid = validate_that(
      is.data.frame(clusTrajs),
      names(clusTrajs)[1] == 'Cluster'
    )
  }

  if (!isTRUE(valid)) {
    stop(
      sprintf(
        '%1$s implemention error: output of clusterTrajectories(%1$s, ...) is not valid: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  as.data.frame(clusTrajs)
})


# compose ####
#' @export
#' @name compose
#' @param ... Not used.
setGeneric('compose', function(method, envir, ...) {
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
    msg = sprintf(
      'Invalid %1$s specification or implementation:\nidVariable(%1$s) is not character.\nDid you forget to specify the "id" argument?',
      class(method)[1]
    )
  )

  assert_that(
    is.character(timeVariable(newmethod)),
    msg = sprintf(
      'Invalid %1$s specification or implementation:\ntimeVariable(%1$s) is not character.\nDid you forget to specify the "time" argument?',
      class(method)[1]
    )
  )

  assert_that(
    is.character(responseVariable(newmethod)),
    msg = sprintf(
      'Invalid %1$s specification or implementation:\nresponseVariable(%1$s) is not character.\nDid you forget to specify the "response" argument?',
      class(method)[1]
    )
  )

  newmethod
})


# converged ####
#' @export
#' @name converged
#' @title Check model convergence
#' @description Check whether the fitted object converged.
#' @param object The model.
#' @param ... Not used.
#' @return Either `logical` indicating convergence, or a `numeric` status code.
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

  state
})


# externalMetric ####
#' @export
#' @name externalMetric
#' @title Compute external model metric(s)
#' @description Compute one or more external metrics for two or more objects.
#'
#' Note that there are many external metrics available, and there exists no external metric that works best in all scenarios.
#' It is recommended to carefully consider which metric is most appropriate for your use case.
#' @param object The object to compare to the second object
#' @param object2 The second object
#' @param name The name(s) of the external metric(s) to compute. If no names are given, the names specified in the `latrend.externalMetric` option (none by default) are used.
#' @usage NULL
setGeneric('externalMetric', function(
  object,
  object2,
  name = getOption('latrend.externalMetric'),
  ...)

  standardGeneric('externalMetric')
)


# estimationTime ####
#' @export
#' @name estimationTime
#' @title Estimation time
#' @description
#' Get the elapsed time for estimating the given model.
#' @param object The model.
#' @param unit The time unit in which the estimation time should be outputted.
#' By default, estimation time is in seconds.
#' For accepted units, see [base::difftime].
#' @param ... Not used.
#' @return A non-negative `scalar numeric` representing the estimation time in the specified unit..
setGeneric('estimationTime', function(object, unit = 'secs', ...) {
  dtime <- standardGeneric('estimationTime')

  if (is(dtime, 'difftime')) {
    duration = as.numeric(dtime, units = unit)
  } else {
    duration = dtime
  }

  assert_that(
    is.scalar(duration),
    is.finite(duration)
  )

  duration
})


# fit ####
#' @export
#' @name fit
#' @param ... Not used.
#' @return The fitted object, inheriting from `lcModel`.
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

  newMethod = getLcMethod(model)
  if (length(names(newMethod)) == 0L) {
    newMethod = method
  }
  assert_that(
    is.lcMethod(newMethod),
    msg = sprintf(
      '%1$s implementation error: fit(%1$s, ...) output model does not have an lcMethod object associated with it',
      class(model)[1]
    )
  )

  model@method = newMethod
  model@id = idVariable(newMethod)
  model@time = timeVariable(newMethod)
  model@response = responseVariable(newMethod)
  model@label = getLabel(newMethod)
  model@date = dateStart
  model@estimationTime = as.numeric(estimationTime, 'secs')

  model
})


# fittedTrajectories ####
#' @export
#' @name fittedTrajectories
#' @title Extract the fitted trajectories
#' @param ... Not used.
#' @return A `data.frame` representing the fitted response per trajectory per moment in time for the respective cluster.
#' @seealso [plotFittedTrajectories]
setGeneric('fittedTrajectories', function(object, ...) {
  fitTrajs <- standardGeneric('fittedTrajectories')

  if (inherits(object, 'lcModel')) {
    valid = validate_that(
      is.data.frame(fitTrajs),
      names(fitTrajs)[1] == idVariable(object),
      names(fitTrajs)[2] == timeVariable(object),
      names(fitTrajs)[3] == responseVariable(object),
      names(fitTrajs)[4] == 'Cluster'
    )
  } else {
    valid = validate_that(
      is.data.frame(fitTrajs)
    )
  }

  if (!isTRUE(valid)) {
    stop(
      sprintf(
        '%1$s implemention error: output of fittedTrajectories(%1$s, ...) is not valid: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  as.data.frame(fitTrajs)
})


# getArgumentDefaults ####
#' @export
#' @name getArgumentDefaults
#' @title Default argument values for the given method specification
#' @param object The method specification object.
#' @param ... Not used.
#' @return A `named list` of argument values.
#' @seealso [getArgumentExclusions]
setGeneric('getArgumentDefaults', function(object, ...) {
  out <- standardGeneric('getArgumentDefaults')

  assert_that(
    is.list(out),
    is_named(out),
    msg = sprintf(
      'Implementation error for %1$s: getArgumentDefaults(%1$s) must return named list',
      class(object)[1]
    )
  )

  out
})

# getArgumentExclusions ####
#' @export
#' @name getArgumentExclusions
#' @title Arguments to be excluded from the specification
#' @description Returns the names of arguments that should be excluded during instantiation of the specification.
#' @param object The object.
#' @param ... Not used.
#' @return A `character vector` of argument names.
#' @seealso [getArgumentDefaults]
setGeneric('getArgumentExclusions', function(object, ...) {
  out <- standardGeneric('getArgumentExclusions')

  assert_that(
    is.character(out),
    noNA(out),
    all(nchar(out) > 0),
    msg = sprintf(
      'Implementation error for %1$s: getArgumentExclusions(%1$s) must return character vector with non-empty elements',
      class(object)[1]
    )
  )

  out
})


# getCitation ####
#' @export
#' @importFrom utils citation
#' @name getCitation
#' @title Get citation info
#' @description Get a citation object indicating how to cite the underlying R packages used for estimating or representing the given method or model.
#' @param object The object
#' @param ... Not used.
#' @return A [utils::citation] object.
#' @seealso [utils::citation]
setGeneric('getCitation', function(object, ...) {
  out <- standardGeneric('getCitation')

  assert_that(
    inherits(out, 'citation'),
    msg = sprintf(
      'Implementation error for %1$s: getCitation(%1$s) must return an object of class "citation"',
      class(object)[1]
    )
  )

  out
})


# getLabel ####
#' @export
#' @name getLabel
#' @title Object label
#' @description Get the object label, if any.
#' @param object The object.
#' @param ... Not used.
#' @return A `scalar character`. The empty string is returned if there is no label.
#' @seealso [getName]
setGeneric('getLabel', function(object, ...) {
  label <- standardGeneric('getLabel')

  assert_that(
    is.character(label),
    length(label) == 1
  )

  label
})


# getLcMethod ####
#' @export
#' @name getLcMethod
#' @title Get the method specification
#' @description Get the `lcMethod` specification that was used for fitting the given object.
#' @param object The model.
#' @param ... Not used.
#' @return An `lcMethod` object.
setGeneric('getLcMethod', function(object, ...) {
  method <- standardGeneric('getLcMethod')

  assert_that(
    is.lcMethod(method)
  )

  method
})


# getName ####
#' @export
#' @name getName
#' @title Object name
#' @description Get the name associated with the given object.
#' @param object The object.
#' @param ... Not used.
#' @return A nonempty string, as `character`.
#' @seealso [getShortName] [getLabel]
setGeneric('getName', function(object, ...) {
  name <- standardGeneric('getName')

  assert_that(
    is.character(name),
    length(name) == 1,
    nchar(name) > 0
  )

  name
})


# getShortName ####
#' @export
#' @rdname getName
#' @description `getShortName()`: Extracts the short object name
setGeneric('getShortName', function(object, ...) {
  name <- standardGeneric('getShortName')

  assert_that(
    is.character(name),
    length(name) == 1,
    nchar(name) > 0
  )

  name
})


# idVariable ####
#' @export
#' @name idVariable
#' @title Extract the trajectory identifier variable
#' @description Extracts the trajectory identifier variable (i.e., column name) from the given `object`.
#' @param object The object.
#' @param ... Not used.
#' @return A nonempty string, as `character`.
#' @family variables
setGeneric('idVariable', function(object, ...) {
  id <- standardGeneric('idVariable')

  assert_that(
    is.character(id),
    length(id) == 1,
    nchar(id) > 0
  )

  id
})


# metric ####
#' @export
#' @name metric
setGeneric('metric', function(
  object,
  name = getOption('latrend.metric', c('WRSS', 'APPA.mean')),
  ...) standardGeneric('metric')
)


# plotFittedTrajectories ####
#' @export
#' @name plotFittedTrajectories
#' @title Plot the fitted trajectories
#' @description Plot the fitted trajectories as represented by the given model
#' @inheritParams fittedTrajectories
#' @inheritDotParams fittedTrajectories
#' @seealso [fittedTrajectories]
setGeneric('plotFittedTrajectories',
  function(object, ...) standardGeneric('plotFittedTrajectories')
)


# plotClusterTrajectories ####
#' @export
#' @name plotClusterTrajectories
#' @title Plot cluster trajectories
#' @description Plot the cluster trajectories associated with the given model.
#' @inheritParams clusterTrajectories
#' @seealso [clusterTrajectories]
setGeneric('plotClusterTrajectories',
  function(object, ...) standardGeneric('plotClusterTrajectories')
)


# plotTrajectories ####
#' @export
#' @name plotTrajectories
#' @title Plot the data trajectories
#' @description Plots the output of [trajectories] for the given object.
#' @seealso [trajectories]
setGeneric('plotTrajectories', function(object, ...) standardGeneric('plotTrajectories'))


# postFit ####
#' @export
#' @name postFit
#' @param ... Not used.
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

  model
})


# postprob ####
#' @export
#' @name postprob
#' @title Posterior probability per fitted trajectory
#' @description Get the posterior probability matrix with element \eqn{(i,j)} indicating the probability of trajectory \eqn{i} belonging to cluster \eqn{j}.
#' @param object The model.
#' @param ... Not used.
#' @return An I-by-K `numeric matrix` with `I = nIds(object)` and `K = nClusters(object)`.
setGeneric('postprob', function(object, ...) {
  pp <- standardGeneric('postprob')

  valid = validate_that(
    is.numeric(pp),
    ncol(pp) == nClusters(object),
    nrow(pp) == nIds(object)
  )

  if(!isTRUE(valid)) {
    stop(
      sprintf(
        '%1$s implementation error: Output format returned by postprob(%1$s, ...) was not valid: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  colnames(pp) = clusterNames(object)

  valid = validate_that(is_valid_postprob(pp))
  if(!isTRUE(valid)) {
    warning(
      sprintf(
        '%1$s implementation error: matrix content returned by postprob(%1$s, ...) was not valid: %2$s',
        class(object)[1],
        valid
      )
    )
  }

  if (anyNA(pp)) {
    naMsk = is.na(pp[, 1L])
    warning(
      sprintf(
        '%s implementation error: postprob(%s) output contains %d trajectories with NA probabilities. Setting uniform postprob for trajectories: \n%s',
        class(object)[1],
        class(object)[1],
        sum(naMsk),
        paste0('  ', ids(object)[naMsk], collapse = '\n')
      )
    )

    pp[naMsk, ] = 1 / nClusters(object)
  }

  pp
})


# predictAssignments ####
#' @export
#' @name predictAssignments
#' @title Predict the cluster assignments for new trajectories
#' @description Predict the most likely cluster membership for each trajectory in the given data.
#' @inheritParams predictForCluster
#' @return A `factor` of length `nrow(newdata)` that indicates the assigned cluster per trajectory per observation.
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
#' @name predictForCluster
#' @title Predict trajectories conditional on cluster membership
#' @description Predicts the expected trajectory observations at the given time under the assumption that the trajectory belongs to the specified cluster.
#' @param object The model.
#' @param newdata A `data.frame` of trajectory data for which to compute trajectory assignments.
#' @param cluster The cluster name (as `character`) to predict for.
#' @param ... Not used.
#' @return A `vector` with the predictions per `newdata` observation, or a `data.frame` with the predictions and newdata alongside.
setGeneric('predictForCluster', function(object, newdata = NULL, cluster, ...) {
  assert_that(
    is_newdata(newdata),
    is.scalar(cluster),
    cluster %in% clusterNames(object)
  )

  # special case for when no newdata is provided
  if (is.null(newdata)) {
    newdata = model.data(object)
    if (has_name(newdata, 'Cluster') && getOption('latrend.warnModelDataClusterColumn', TRUE)) {
      warning('model data used in predictForCluster() contains a "Cluster" column. This column will be ignored.')
    }
  }
  else {
    if (nrow(newdata) == 0) {
      warning('called predictForCluster() with empty newdata data.frame (nrow = 0)')
    }
    if (has_name(newdata, 'Cluster') && getOption('latrend.warnNewDataClusterColumn', TRUE)) {
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
      '%1$s implementation error: predictForCluster(%1$s, ...) implementation returned output of type %2$s. Only numeric and data.frame are supported outputs.',
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

  out
})


# predictPostprob ####
#' @export
#' @name predictPostprob
#' @title Posterior probability for new data
#' @description Returns the observation-specific posterior probabilities for the given data.
#' @param newdata Optional `data.frame` for which to compute the posterior probability. If omitted, the model training data is used.
#' @inheritParams predictForCluster
#' @return A N-by-K `matrix` indicating the posterior probability per trajectory per measurement on each row, for each cluster (the columns).
#' Here, `N = nrow(newdata)` and `K = nClusters(object)`.
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
  pp
})


# prepareData ####
#' @export
#' @name prepareData
#' @param ... Not used.
#' @return An `environment`.
setGeneric('prepareData', function(method, data, verbose, ...) {
  envir <- standardGeneric('prepareData')
  assert_that(
    is.environment(envir),
    msg = sprintf(
      'prepareData(%s, ...) should return an environment',
      class(method)[1]
    )
  )

  envir
})


# preFit ####
#' @export
#' @name preFit
#' @param ... Not used.
setGeneric('preFit', function(method, data, envir, verbose, ...) {
  modelEnv <- standardGeneric('preFit')
  assert_that(
    is.environment(modelEnv),
    msg = sprintf(
      'preFit(%s, ...) should return an environment',
      class(method)[1]
    )
  )

  modelEnv
})


# qqPlot ####
#' @export
#' @name qqPlot
#' @title Quantile-quantile plot
#' @description Quantile-quantile (Q-Q) plot for the residuals of the given model.
#' @param object The model.
#' @param ... Not used.
setGeneric('qqPlot', function(object, ...) standardGeneric('qqPlot'))


# responseVariable ####
#' @export
#' @name responseVariable
#' @title Extract response variable
#' @description Extracts the response variable from the given `object`.
#' @inheritParams idVariable
#' @description Get the response variable, i.e., the dependent variable.
#' @inherit idVariable return
#' @family variables
setGeneric('responseVariable', function(object, ...) {
  response <- standardGeneric('responseVariable')

  assert_that(
    is.character(response),
    length(response) == 1,
    nchar(response) > 0
  )

  response
})


# strip ####
#' @export
#' @name strip
#' @title Reduce the memory footprint of an object for serialization
#' @description Reduce the (serialized) memory footprint of an object.
#' @param object The model.
#' @param ... Not used.
#' @details Serializing references to environments results in the serialization of the object
#' together with any associated environments and references. This method removes those environments
#' and references, greatly reducing the serialized object size.
#' @return The stripped (i.e., updated) object.
setGeneric('strip', function(object, ...) standardGeneric('strip'))


# timeVariable ####
#' @export
#' @name timeVariable
#' @title Extract the time variable
#' @description Extracts the time variable (i.e., column name) from the given `object`.
#' @inheritParams idVariable
#' @inherit idVariable return
#' @family variables
setGeneric('timeVariable', function(object, ...) {
  time <- standardGeneric('timeVariable')

  assert_that(
    is.character(time),
    length(time) == 1,
    nchar(time) > 0
  )

  time
})


# trajectories ####
#' @export
#' @name trajectories
#' @title Extract the trajectories
#' @description Transform or extract the trajectories from the given object to a standardized format.
#'
#' The standardized data format is for method estimation by [latrend], and for plotting functions.
#' @param object The data or model or extract the trajectories from.
#' @param id The identifier variable name, see [idVariable].
#' @param time The time variable name, see [timeVariable].
#' @param response The response variable name, see [responseVariable].
#' @param ... Not used.
#' @return A `data.frame` with columns matching the `id`, `time`, and `response` name arguments.
#' @details The generic function removes unused factor levels in the Id column, and any trajectories which are only comprised of NAs in the response.
#' @seealso [plotTrajectories] [latrend]
setGeneric('trajectories', function(
    object,
    id = idVariable(object),
    time = timeVariable(object),
    response = responseVariable(object),
    ...
) {
  data <- standardGeneric('trajectories')

  assert_that(
    is_data(data)
  )

  if (!no_empty_trajectories(data, id = id)) {
    # remove empty trajectories
    warn_that(
      no_empty_trajectories(data, id = id),
      prepend = 'NOTE: Empty trajectories will be removed.\n',
      append = '\nThis warning can be disabled using options(latrend.warnEmptyTrajectories = FALSE)',
      show = getOption('latrend.warnEmptyTrajectories', default = TRUE)
    )

    data[[id]] = droplevels(data[[id]], exclude = NULL)
  }

  if (!no_trajectories_allNA(data, id = id, response = response)) {
    warn_that(
      no_trajectories_allNA(data, id = id, response = response),
      prepend = 'NOTE: Trajectories comprising only NA observations will be removed.\n',
      append = '\nThis warning can be disabled using options(latrend.warnNaTrajectories = FALSE)',
      show = getOption('latrend.warnNaTrajectories', default = TRUE)
    )

    keepIds = as.data.table(data)[, .(AllNA = all(is.na(get(..response)))), by = c(id)] %>%
      .[AllNA == FALSE, get(..id)]

    data = as.data.table(data)[get(id) %in% keepIds]

    if (is.factor(data)) {
      data[[id]] = droplevels(data[[id]], exclude = NULL)
    }
  }

  as.data.frame(data)
})


# trajectoryAssignments ####
#' @export
#' @name trajectoryAssignments
#' @title Get the cluster membership of each trajectory
#' @description Get the cluster membership of each trajectory associated with the given model.
#' @param object The model.
#' @param ... Not used.
#' @return A `factor vector` indicating the cluster membership for each trajectory.
setGeneric('trajectoryAssignments', function(object, ...) {
  clusters <- standardGeneric('trajectoryAssignments')

  assert_that(
    is.factor(clusters),
    !is.lcModel(object) || all(levels(clusters) == clusterNames(object))
  )

  clusters
})


# validate ####
#' @export
#' @name validate
#' @param ... Not used.
#' @return Either `TRUE` if all validation checks passed,
#' or a `scalar character` containing a description of the failed validation checks.
setGeneric('validate', function(method, data, envir, ...) {
  validationResult <- standardGeneric('validate')

  assert_that(
    length(validationResult) == 1L,
    msg = sprintf(
      'implementation error in validate(%s): output should be length 1',
      class(method)[1]
    )
  )

  if (!isTRUE(validationResult)) {
    stop(sprintf('%s validation failed: %s', class(method)[1], validationResult))
  }

  validationResult
})
