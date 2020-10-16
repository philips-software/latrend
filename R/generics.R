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
    !anyNA(props),
    all(is.finite(props)),
    min(props) >= 0
  )

  names(props) = clusterNames(object)
  props
})

#' @export
#' @name latrend-generics
setGeneric('clusterTrajectories', function(object, ...) standardGeneric('clusterTrajectories'))

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
setGeneric('converged', function(object, ...) standardGeneric('converged'))

#' @export
#' @name latrend-generics
#' @param object2 The model to compare with.
setGeneric('externalMetric',
  function(object, object2, name, ...) standardGeneric('externalMetric'))

#' @export
#' @name latrend-generics
setGeneric('fit', function(method, data, envir, verbose, ...) {
  start = Sys.time()
  model <- standardGeneric('fit')
  estimationTime = Sys.time() - start

  assert_that(is.lcModel(model),
    msg = 'fit(lcMethod, ...) returned an unexpected object. Should be of type lcModel.')

  model@method = method
  model@id = idVariable(method)
  model@time = timeVariable(method)
  model@response = responseVariable(method)
  model@label = getLabel(method)
  model@estimationTime = as.numeric(estimationTime, 'secs')

  return(model)
})

#' @export
#' @name latrend-generics
setGeneric('getLabel', function(object, ...) standardGeneric('getLabel'))

#' @export
#' @name latrend-generics
setGeneric('getName', function(object, ...) standardGeneric('getName'))

#' @export
#' @name latrend-generics
setGeneric('getShortName', function(object, ...) standardGeneric('getShortName'))

#' @export
#' @name latrend-generics
setGeneric('idVariable', function(object, ...) standardGeneric('idVariable'))

#' @export
#' @name latrend-generics
setGeneric('metric', function(object, name, ...) standardGeneric('metric'))

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
    ncol(pp) == nClusters(object),
    nrow(pp) == nIds(object),
    is_valid_postprob(pp, object)
  )

  colnames(pp) = clusterNames(object)
  pp
})

#' @export
#' @name latrend-generics
setGeneric('predictAssignments', function(object, newdata = NULL, ...) {
    assignments <- standardGeneric('predictAssignments')

    assert_that(
      is.null(newdata) || length(assignments) == nrow(newdata)
    )

    make.trajectoryAssignments(object, assignments)
})

#' @export
#' @name latrend-generics
setGeneric('predictForCluster',
  function(object, newdata = NULL, cluster, ...) standardGeneric('predictForCluster'))

#' @export
#' @name latrend-generics
setGeneric('predictPostprob', function(object, newdata = NULL, ...) {
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
setGeneric('responseVariable', function(object, ...) standardGeneric('responseVariable'))

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
setGeneric('timeVariable', function(object, ...) standardGeneric('timeVariable'))

#' @export
#' @name latrend-generics
setGeneric('validate', function(method, data, envir, ...) {
  validationResult <- standardGeneric('validate')
  if (!isTRUE(validationResult)) {
    stop('method validation failed: ', validationResult)
  }
})
