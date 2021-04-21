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

#' @export
#' @name latrend-generics
setGeneric('trajectoryAssignments', function(object, ...) {
  assignments <- standardGeneric('trajectoryAssignments')

  assert_that(
    length(assignments) == nIds(object)
  )

  make.trajectoryAssignments(object, assignments)
})

#' @export
#' @name latrend-generics
setGeneric('clusterProportions', function(object, ...) {
  props <- standardGeneric('clusterProportions')

  assert_that(
    is.numeric(props),
    length(props) == nClusters(object),
    noNA(props),
    all(is.finite(props)),
    min(props) >= 0,
    max(props) <= 1
  )

  names(props) = clusterNames(object)
  props
})

#' @export
#' @name latrend-generics
setGeneric('clusterTrajectories', function(object, ...) {
  dfclus <- standardGeneric('clusterTrajectories')
  assert_that(
    is.data.frame(dfclus),
    names(dfclus)[1] == 'Cluster',
    names(dfclus)[2] == timeVariable(object),
    names(dfclus)[3] == responseVariable(object),
    msg = paste0('Invalid output format for data.frame from clusterTrajectories() implementation of lcModel ', class(object)))

  dfclus
})

#' @export
#' @name latrend-generics
setGeneric('compose', def = function(method, envir, ...) {
  newmethod <- standardGeneric('compose')
  assert_that(is.lcMethod(newmethod),
    msg = paste0('invalid method output from compose(', class(newmethod), ')')
  )

  assert_that(
    is.character(idVariable(newmethod)),
    is.character(timeVariable(newmethod)),
    is.character(responseVariable(newmethod))
  )
  return(newmethod)
})

#' @export
#' @name latrend-generics
setGeneric('converged', function(object, ...) {
  state <- standardGeneric('converged')
  assert_that(
    is.logical(state) || is.numeric(state),
    length(state) == 1,
    msg = 'output of converged() must be either logical or numeric, and of length 1'
  )
  state
})

#' @export
#' @name latrend-generics
#' @param object2 The model to compare with.
setGeneric('externalMetric', function(
  object,
  object2,
  name = getOption('latrend.externalMetric'),
  ...) standardGeneric('externalMetric'))

#' @export
#' @name latrend-generics
setGeneric('fit', function(method, data, envir, verbose, ...) {
  dateStart = Sys.time()
  start = proc.time()
  model <- standardGeneric('fit')
  estimationTime = proc.time()['elapsed'] - start['elapsed']

  assert_that(is.lcModel(model),
    msg = 'fit(lcMethod, ...) returned an unexpected object. Should be of type lcModel.')

  model@method = method
  model@id = idVariable(method)
  model@time = timeVariable(method)
  model@response = responseVariable(method)
  model@label = getLabel(method)
  model@date = dateStart
  model@estimationTime = as.numeric(estimationTime, 'secs')

  return(model)
})

#' @export
#' @name latrend-generics
setGeneric('getLabel', function(object, ...) {
  label <- standardGeneric('getLabel')
  assert_that(
    is.character(label),
    length(label) == 1
  )
  label
})

#' @export
#' @name latrend-generics
setGeneric('getName', function(object, ...) {
  name <- standardGeneric('getName')
  assert_that(
    is.character(name),
    length(name) == 1,
    nchar(name) > 0
  )
  name
})

#' @export
#' @name latrend-generics
setGeneric('getShortName', function(object, ...) {
  name <- standardGeneric('getShortName')
  assert_that(
    is.character(name),
    length(name) == 1,
    nchar(name) > 0
  )
  name
})

#' @export
#' @name latrend-generics
setGeneric('idVariable', function(object, ...) {
  id <- standardGeneric('idVariable')
  assert_that(
    is.character(id),
    length(id) == 1,
    nchar(id) > 0
  )
  id
})

#' @export
#' @name latrend-generics
setGeneric('metric', function(
  object,
  name = getOption('latrend.metric', c('WRSS', 'APPA', 'AIC', 'BIC')),
  ...) standardGeneric('metric'))

#' @export
#' @name latrend-generics
setGeneric('plotClusterTrajectories',
  function(object, ...) standardGeneric('plotClusterTrajectories'))

#' @export
#' @name latrend-generics
#' @title Quantile-quantile plot
setGeneric('qqPlot', function(object, ...) standardGeneric('qqPlot'))

#' @export
#' @name latrend-generics
setGeneric('plotTrajectories', function(object, ...) standardGeneric('plotTrajectories'))

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

  msg = validate_that(is_valid_postprob(pp, object))
  if(!isTRUE(msg)) {
    warning('Output returned by postprob() of ', class(object), ' was not valid: ', msg)
  }

  pp
})

#' @export
#' @name latrend-generics
setGeneric('predictAssignments', function(object, newdata = NULL, ...) {
  assert_that(is.newdata(newdata))

  assignments <- standardGeneric('predictAssignments')

  assert_that(
    is.null(newdata) || length(assignments) == nrow(newdata)
  )

  make.trajectoryAssignments(object, assignments)
})

#' @export
#' @name latrend-generics
setGeneric('predictForCluster', function(object, newdata = NULL, cluster, ...) {
  assert_that(
    is.newdata(newdata),
    is.scalar(cluster),
    cluster %in% clusterNames(object)
  )

  # special case for when no newdata is provided
  if (is.null(newdata)) {
    newdata = model.data(object)
    if (hasName(newdata, 'Cluster')) {
      newdata[['Cluster']] = NULL
    }
  }
  else {
    if (nrow(newdata) == 0) {
      warning('called predictForCluster() with empty newdata data.frame (nrow = 0)')
    }
  }

  out <- standardGeneric('predictForCluster')

  assert_that(is.numeric(out) || is.data.frame(out))

  if (!is.null(newdata)) {
    if (is.numeric(out)) {
      assert_that(length(out) == nrow(newdata))
    } else if (is.data.frame(out)) {
      assert_that(nrow(out) == nrow(newdata))
    }
  }

  out
})

#' @export
#' @name latrend-generics
setGeneric('predictPostprob', function(object, newdata = NULL, ...) {
  assert_that(is.newdata(newdata))

  pp <- standardGeneric('predictPostprob')

  assert_that(
    is.null(newdata) || nrow(pp) == nrow(newdata),
    is_valid_postprob(pp, object)
  )

  colnames(pp) = clusterNames(object)
  pp
})

#' @export
#' @name latrend-generics
setGeneric('postFit', function(method, data, model, envir, verbose, ...) {
  model <- standardGeneric('postFit')
  assert_that(
    inherits(model, 'lcModel'),
    msg = 'postFit(lcMethod, ...) returned an unexpected object. Should be of type lcModel.')

  return(model)
})

#' @export
#' @name latrend-generics
setGeneric('preFit', function(method, data, envir, verbose, ...) {
  modelEnv <- standardGeneric('preFit')
  assert_that(
    is.null(modelEnv) || is.environment(modelEnv),
    msg = 'preFit(method, ...) returned an unexpected object. Should be environment or NULL')

  return(modelEnv)
})

#' @export
#' @name latrend-generics
setGeneric('prepareData', function(method, data, verbose, ...) {
  envir <- standardGeneric('prepareData')
  assert_that(is.null(envir) ||
      is.environment(envir),
    msg = 'prepareData(method, ...) returned an unexpected object. Should be environment or NULL')
  return(envir)
})

#' @export
#' @name latrend-generics
setGeneric('responseVariable', function(object, ...) {
  response <- standardGeneric('responseVariable')

  assert_that(
    is.character(response),
    length(response) == 1,
    nchar(response) > 0
  )

  response
})

#' @export
#' @name latrend-generics
#' @description Reduce the (serialized) memory footprint of an object.
#' @details Serializing references to environments results in the serialization of the object
#' together with any associated environments and references. This method removes those environments
#' and references, greatly reducing the serialized object size.
#' @return The stripped (i.e., updated) object.
setGeneric('strip', function(object, ...) standardGeneric('strip'))

#' @export
#' @name latrend-generics
setGeneric('timeVariable', function(object, ...) {
  time <- standardGeneric('timeVariable')

  assert_that(
    is.character(time),
    length(time) == 1,
    nchar(time) > 0
  )

  time
})

#' @export
#' @name latrend-generics
setGeneric('validate', function(method, data, envir, ...) {
  validationResult <- standardGeneric('validate')

  if (!isTRUE(validationResult)) {
    stop('method validation failed: ', validationResult)
  }
})
