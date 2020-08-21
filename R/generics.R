#' @name latrend-generics
#' @title Method- and model-specific generics defined by the latrend package
#' @description List of S4 generic methods which have no general use other than supporting functions with signatures of `lcMethod` or `lcModel`.
#' @param object The object.
#' @param method The method.
#' @param data `data.frame`.
#' @param newdata `data.frame` of newdata.
#' @param name Metric name.
#' @param envir `environment`.
#' @param verbose [R.utils::Verbose].
#' @param ... Arguments.
NULL

#' @export
#' @rdname latrend-generics
setGeneric('clusterAssignments', function(object, ...) standardGeneric('clusterAssignments'))

#' @export
#' @rdname latrend-generics
setGeneric('clusterProportions', function(object, ...) standardGeneric('clusterProportions'))

#' @export
#' @rdname latrend-generics
setGeneric('clusterTrajectories', function(object, ...) standardGeneric('clusterTrajectories'))

#' @export
#' @rdname latrend-generics
setGeneric('compose', function(method, envir, ...) standardGeneric('compose'))

#' @export
#' @rdname latrend-generics
setGeneric('converged', function(object, ...) standardGeneric('converged'))

#' @export
#' @rdname latrend-generics
#' @param object2 The model to compare with.
setGeneric('externalMetric', function(object, object2, name, ...) standardGeneric('externalMetric'))

#' @export
#' @rdname latrend-generics
setGeneric('fit', function(method, data, envir, verbose, ...) standardGeneric('fit'))

#' @export
#' @rdname latrend-generics
setGeneric('getLabel', function(object, ...) standardGeneric('getLabel'))

#' @export
#' @rdname latrend-generics
setGeneric('getName', function(object, ...) standardGeneric('getName'))

#' @export
#' @rdname latrend-generics
setGeneric('getShortName', function(object, ...) standardGeneric('getShortName'))

#' @export
#' @rdname latrend-generics
setGeneric('idVariable', function(object, ...) standardGeneric('idVariable'))

#' @export
#' @rdname latrend-generics
setGeneric('metric', function(object, name, ...) standardGeneric('metric'))

#' @export
#' @rdname latrend-generics
setGeneric('plotClusterTrajectories', function(object, ...) standardGeneric('plotClusterTrajectories'))

#' @export
#' @rdname latrend-generics
#' @title Quantile-quantile plot
setGeneric('plotQQ', function(object, ...) standardGeneric('plotQQ'))

#' @export
#' @rdname latrend-generics
setGeneric('plotTrajectories', function(object, ...) standardGeneric('plotTrajectories'))

#' @export
#' @rdname latrend-generics
setGeneric('postprob', function(object, ...) standardGeneric('postprob'))

#' @export
#' @rdname latrend-generics
setGeneric('predictAssignments', function(object, newdata = NULL, ...) standardGeneric('predictAssignments'))

#' @export
#' @rdname latrend-generics
setGeneric('predictForCluster', function(object, newdata = NULL, cluster, ...) standardGeneric('predictForCluster'))

#' @export
#' @rdname latrend-generics
setGeneric('predictPostprob', function(object, newdata = NULL, ...) standardGeneric('predictPostprob'))

#' @export
#' @rdname latrend-generics
setGeneric('postFit', function(method, data, model, envir, verbose, ...) standardGeneric('postFit'))

#' @export
#' @rdname latrend-generics
setGeneric('preFit', function(method, data, envir, verbose, ...) standardGeneric('preFit'))

#' @export
#' @rdname latrend-generics
setGeneric('prepareData', function(method, data, verbose, ...) standardGeneric('prepareData'))

#' @export
#' @rdname latrend-generics
setGeneric('responseVariable', function(object, ...) standardGeneric('responseVariable'))

#' @export
#' @rdname latrend-generics
#' @description Reduce the (serialized) memory footprint of an object.
#' @details Serializing references to environments results in the serialization of the object together with any associated environments and references. This method removes those environments and references, greatly reducing the serialized object size.
#' @return The stripped (i.e., updated) object.
setGeneric('strip', function(object, ...) standardGeneric('strip'))

#' @export
#' @rdname latrend-generics
setGeneric('timeVariable', function(object, ...) standardGeneric('timeVariable'))

#' @export
#' @rdname latrend-generics
setGeneric('validate', function(method, data, envir, ...) standardGeneric('validate'))
