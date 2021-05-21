#' @include method.R trajectories.R latrend.R
#' @importFrom stats coef deviance df.residual getCall logLik model.frame model.matrix predict residuals sigma time update


# Model ####
#' @export
#' @name lcModel-class
#' @title lcModel class
#' @description Abstract class for defining estimated longitudinal cluster models.
#' @details An extending class must implement the following methods to ensure basic functionality:
#' * `predict.lcModelExt`: Used to obtain the fitted cluster trajectories and trajectories.
#' * `postprob(lcModelExt)`: The posterior probability matrix is used to determine the cluster assignments of the trajectories.
#'
#' For predicting the posterior probability for unseen data, the `predictPostprob()` should be implemented.
#'
#' @param object The `lcModel` object.
#' @param ... Any additional arguments.
#' @slot method The \link{lcMethod-class} object specifying the arguments under which the model was fitted.
#' @slot call The `call` that was used to create this `lcModel` object. Typically, this is the call to `latrend()` or any of the other fitting functions.
#' @slot model An arbitrary underlying model representation.
#' @slot data A `data.frame` object, or an expression to resolves to the `data.frame` object.
#' @slot date The date-time when the model estimation was initiated.
#' @slot id The name of the trajectory identifier column.
#' @slot time The name of the time variable.
#' @slot response The name of the response variable.
#' @slot label The label assigned to this model.
#' @slot ids The trajectory identifier values the model was fitted on.
#' @slot times The exact times on which the model has been trained
#' @slot clusterNames The names of the clusters.
#' @slot estimationTime The time, in seconds, that it took to fit the model.
#' @slot tag An arbitrary user-specified data structure. This slot may be accessed and updated directly.
#' @family model-specific methods
setClass(
  'lcModel',
  representation(
    model = 'ANY',
    method = 'lcMethod',
    call = 'call',
    data = 'ANY',
    id = 'character',
    time = 'character',
    response = 'character',
    label = 'character',
    ids = 'vector',
    times = 'vector',
    clusterNames = 'character',
    date = 'POSIXct',
    estimationTime = 'numeric',
    tag = 'ANY'
  )
)

# . initialize ####
setMethod('initialize', 'lcModel', function(.Object, ...) {
  .Object@date = Sys.time()
  .Object = callNextMethod(.Object, ...)
  method = .Object@method

  assert_that(length(.Object@id) > 0 ||
                has_name(method, 'id'), msg = '@id not specified, nor defined in lcMethod')
  if (length(.Object@id) == 0) {
    .Object@id = idVariable(method)
  }
  assert_that(length(.Object@time) > 0 ||
                has_name(method, 'time'), msg = '@time not specified, nor defined in lcMethod')
  if (length(.Object@time) == 0) {
    .Object@time = timeVariable(method)
  }
  if (length(.Object@response) == 0) {
    .Object@response = responseVariable(method)
    assert_that(!is.null(.Object@response))
  }
  .Object
})


setValidity('lcModel', function(object) {
  return(TRUE)

  if (as.character(object@call[[1]]) == "<undef>") {
    # nothing to validate as lcModel is incomplete
    return(TRUE)
  }

  assert_that(
    nchar(object@id) > 0,
    nchar(object@time) > 0,
    nchar(object@response) > 0
  )

  data = model.data(object)
  assert_that(!is.null(data),
    msg = 'invalid data object for new lcModel. Either specify the data slot or ensure that the model call contains a data argument which correctly evaluates.')
  assert_that(has_name(data, c(object@id, object@time, object@response)))
  return(TRUE)
})


# . clusterTrajectories ####
#' @export
#' @name clusterTrajectories
#' @rdname clusterTrajectories
#' @aliases clusterTrajectories,lcModel-method
#' @title Extract the cluster trajectories
#' @description Extracts a data frame of all cluster trajectories.
#' @inheritParams predict.lcModel
#' @inheritParams predictForCluster
#' @param at An optional vector, list or data frame of covariates at which to compute the cluster trajectory predictions.
#' If a vector is specified, this is assumed to be the time covariate. Otherwise, a named list or data frame must be provided.
#' @return A data.frame of the estimated values at the given times. The first column should be named "Cluster". The second column should be time, with the name matching the `timeVariable(object)`. The third column should be the expected value of the observations, named after the `responseVariable(object)`.
#' @examples
#' model <- latrend(method = lcMethodLcmmGMM(fixed = Y ~ Time, mixture = fixed),
#'   id = "Id", time = "Time", data = latrendData)
#' clusterTrajectories(model)
#'
#' clusterTrajectories(model, at = c(0, .5, 1))
#' @family model-specific methods
setMethod('clusterTrajectories', signature('lcModel'), function(object, at = time(object), what = 'mu', ...) {
  if (is.numeric(at)) {
    newdata = data.table(
      Cluster = rep(clusterNames(object, factor = TRUE), each = length(at)),
      Time = at) %>%
      setnames('Time', timeVariable(object))
  } else if (is.list(at)) {
    at = as.data.table(at)
    idx = seq_len(nrow(at)) %>% rep(nClusters(object))
    newdata = data.table(
      Cluster = rep(clusterNames(object, factor = TRUE), each = nrow(at)), at[idx,])
  } else {
    stop('unsupported input')
  }

  dfPred = predict(object, newdata = newdata, what = what, ...)
  assert_that(is.data.frame(dfPred), msg = 'invalid output from predict()')
  assert_that(nrow(dfPred) == nrow(newdata), msg = 'invalid output from predict function of lcModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what = what)) := dfPred$Fit]
  return(newdata[])
})


#' @export
#' @title Get the cluster names
#' @param object The `lcModel` object.
#' @param factor Whether to return the cluster names as a factor.
#' @return A `character` of the cluster names.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' clusterNames(model) # A, B
clusterNames = function(object, factor = FALSE) {
  assert_that(is.lcModel(object))
  if (factor[1]) {
    object@clusterNames %>% factor(levels = object@clusterNames)
  } else {
    object@clusterNames
  }
}

#' @export
#' @title Update the cluster names
#' @param object The `lcModel` object to update.
#' @param value The `character` with the new names.
#' @return The updated `lcModel` object.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' clusterNames(model) <- c("Group 1", "Group 2")
`clusterNames<-` = function(object, value) {
  assert_that(
    is.lcModel(object),
    is.character(value),
    length(value) == nClusters(object)
  )

  object@clusterNames = value
  return(object)
}

#' @export
#' @title Number of trajectories per cluster
#' @description Obtain the size of each cluster, where the size is determined by the number of assigned trajectories to each cluster.
#' @details The cluster sizes are computed from the trajectory cluster membership as decided by the [trajectoryAssignments()] function.
#' @param object The `lcModel` object.
#' @inheritDotParams trajectoryAssignments
#' @seealso [clusterProportions] [trajectoryAssignments]
#' @return A named `integer vector` of length `nClusters(object)` with the number of assigned trajectories per cluster.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' clusterSizes(model)
clusterSizes = function(object, ...) {
  assert_that(is.lcModel(object))

  trajectoryAssignments(object, ...) %>%
    table() %>%
    as.integer() %>%
    setNames(clusterNames(object))
}

#. clusterProportions ####
#' @export
#' @name clusterProportions
#' @aliases clusterProportions,lcModel-method
#' @title Proportional size of each cluster
#' @description Obtain the proportional size per cluster, with sizes between 0 and 1.
#' By default, the cluster proportions are determined from the cluster-averaged posterior probabilities of the fitted data (as computed by the [postprob()] function).
#' @section Implementation:
#' Classes extending `lcModel` can override this method to return, for example, the exact estimated mixture proportions based on the model coefficients.
#' \preformatted{
#' setMethod("clusterProportions", "lcModelExt", function(object, ...) {
#'   # return cluster proportion vector
#' })
#' }
#' @param object The `lcModel` to obtain the proportions from.
#' @inheritDotParams postprob
#' @return A named `numeric vector` of length `nClusters(object)` with the proportional size of each cluster.
#' @seealso [clusterSizes] [postprob]
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' clusterProportions(model)
setMethod('clusterProportions', signature('lcModel'), function(object, ...) {
  pp = postprob(object, ...)
  assert_that(!is.null(pp), msg = 'cannot determine cluster assignments because postprob() returned NULL')
  colMeans(pp)
})


#' @export
#' @importFrom stats coef
#' @title Extract lcModel coefficients
#' @description Extract the coefficients of the `lcModel` object, if defined.
#' The returned set of coefficients depends on the underlying type of `lcModel`.
#' The default implementation checks for the existence of a `coef()` function for the internal model as defined in the `@model` slot, returning the output if available.
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @section Implementation:
#' Classes extending `lcModel` can override this method to return model-specific coefficients.
#' \preformatted{
#' coef.lcModelExt <- function(object, ...) {
#'   # return model coefficients
#' }
#' }
#' @return A named `numeric vector` with all coefficients, or a `matrix` with each column containing the cluster-specific coefficients. If `coef()` is not defined for the given model, an empty `numeric vector` is returned.
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ 1,
#'   id = "Id", time = "Time", nClusters = 3)
#' gbtm <- latrend(method, data = latrendData)
#' coef(gbtm)
coef.lcModel = function(object, ...) {
  if (is.null(object@model) ||
      is.null(getS3method('coef', class = class(object@model), optional = TRUE))) {
    numeric()
  } else {
    coef(object@model)
  }
}


#' @export
#' @title Compute the posterior confusion matrix
#' @description Compute the posterior confusion matrix (PCM).
#' The entry \eqn{(i,j)} represents the probability (or number, in case of  `scale = TRUE`) of a trajectory
#' belonging to cluster \eqn{i} is assigned to cluster \eqn{j} under the specified trajectory cluster assignment strategy.
#' @param object The object.
#' @param strategy The strategy for assigning trajectories to a specific cluster, see [trajectoryAssignments()].
#' If `strategy = NULL`, the posterior probabilities are used as weights (analogous to a repeated evaluation of `strategy = which.weight`).
#' @param scale Whether to express the confusion in probabilities (`scale = TRUE`), or in terms of the number of trajectories.
#' @inheritDotParams trajectoryAssignments
#' @return A K-by-K confusion `matrix` with `K = nClusters(object)`.
#' @seealso [postprob] [clusterProportions] [trajectoryAssignments]
#' @examples
#' data(latrendData)
#' model = latrend(lcMethodLcmmGMM(
#'   fixed = Y ~ Time, mixture = ~ Time, random = ~ 1,
#'   id = "Id", time = "Time"),
#'   data=latrendData)
#' confusionMatrix(model)
confusionMatrix = function(object, strategy = which.max, scale = TRUE, ...) {
  assert_that(is.lcModel(object))

  I = nIds(object)
  K = nClusters(object)
  pp_it = postprob(object)
  props = clusterProportions(object)

  if (is.null(strategy)) {
    w_is = pp_it
  } else {
    trajLabels = trajectoryAssignments(object, strategy = strategy, ...)
    idxMat = cbind(seq_len(I), as.integer(trajLabels))
    w_is = matrix(0, nrow = I, ncol = K)
    w_is[idxMat] = 1
  }

  cfMat = matrix(nrow = K, ncol = K)
  for (s in 1:K) {
    for (t in 1:K) {
      cfMat[s,t] = sum(pp_it[, t] * w_is[, s])
    }
  }

  if (scale) {
    cfMat = cfMat / rowSums(cfMat)
  }

  clusNames = clusterNames(object)
  rownames(cfMat) = clusNames
  colnames(cfMat) = clusNames

  cfMat
}


# . converged ####
#' @export
#' @name converged
#' @aliases converged,lcModel-method
#' @title Check model convergence
#' @description Check convergence of the fitted `lcModel` object.
#' The default implementation returns `NA`.
#' @param object The `lcModel` to check for convergence.
#' @param ... Additional arguments.
#' @return Either `logical` indicating convergence, or a `numeric` status code.
#' @section Implementation:
#' Classes extending `lcModel` can override this method to return a convergence status or code.
#' \preformatted{
#' setMethod("converged", "lcModelExt", function(object, ...) {
#'   # return convergence code
#' })
#' }
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ 1,
#'   id = "Id", time = "Time", nClusters = 3)
#' gbtm <- latrend(method, data = latrendData)
#' converged(gbtm)
setMethod('converged', signature('lcModel'), function(object, ...) {
  NA
})


#' @export
#' @importFrom stats deviance
#' @title lcModel deviance
#' @description Get the deviance of the fitted `lcModel` object.
#' @details The default implementation checks for the existence of the `deviance()` function for the internal model, and returns the output, if available.
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `numeric` with the deviance value. If unavailable, `NA` is returned.
#' @seealso [stats::deviance] [metric]
#' @family model-specific methods
deviance.lcModel = function(object, ...) {
  if (is.null(object@model) ||
      is.null(getS3method('deviance', class = class(object@model), optional = TRUE))) {
    as.numeric(NA)
  } else {
    deviance(object@model)
  }
}


#' @export
#' @importFrom stats df.residual
#' @title Extract the residual degrees of freedom from a lcModel
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `numeric` with the residual degrees of freedom. If unavailable, `NA` is returned.
#' @seealso [stats::df.residual] [nobs] [residuals]
#' @family model-specific methods
df.residual.lcModel = function(object, ...) {
  if (is.null(object@model) ||
      is.null(getS3method('df.residual', class = class(object@model), optional = TRUE))) {
    df = attr(logLik(object), 'df')
    if (!is.null(df) && is.finite(df)) {
      nobs(object) - df
    } else {
      as.numeric(NA)
    }
  } else {
    df.residual(object@model)
  }
}


#. externalMetric ####
#' @export
#' @rdname externalMetric
#' @aliases externalMetric,lcModel,lcModel-method
#' @examples
#' data(latrendData)
#' model1 <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' model2 <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), latrendData)
#' ari <- externalMetric(model1, model2, 'adjustedRand')
#' @return For `externalMetric(lcModel, lcModel)`: A `numeric` vector of the computed metrics.
#' @family metric functions
setMethod('externalMetric', signature('lcModel', 'lcModel'), function(object, object2, name, ...) {
  assert_that(length(name) > 0, msg = 'no external metric names provided')
  assert_that(is.character(name))

  funMask = name %in% getExternalMetricNames()
  if (!all(funMask)) {
    warning(
      'External metric(s) ',
      paste0('"', name[!funMask], '"', collapse = ', '),
      ' are not defined. Returning NA.'
    )
  }
  metricFuns = lapply(name[funMask], getExternalMetricDefinition)
  metricValues = mapply(function(fun, name) {
    value = fun(object, object2)
    assert_that(
      is.scalar(value) && (is.numeric(value) || is.logical(value)),
      msg = sprintf('invalid output for metric "%s"; expected scalar number or logical value', name)
    )
    return(value)
  }, metricFuns, name[funMask])

  allMetrics = rep(NA * 0, length(name))
  allMetrics[funMask] = unlist(metricValues)
  names(allMetrics) = name
  return(allMetrics)
})


#' @export
#' @importFrom stats fitted
#' @title Extract lcModel fitted values
#' @description Returns the cluster-specific fitted values for the given `lcModel` object.
#' The default implementation calls [predict()] with `newdata = NULL`.
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @param clusters Optional cluster assignments per id. If unspecified, a `matrix` is returned containing the cluster-specific predictions per column.
#' @return A `numeric` vector of the fitted values for the respective class, or a `matrix` of fitted values for each cluster.
#' @section Implementation:
#' Classes extending `lcModel` can override this method to adapt the computation of the predicted values for the training data.
#' Note that the implementation of this function is only needed when [predict()] and [predictForCluster()] are not defined for the `lcModel` subclass.
#' \preformatted{
#' fitted.lcModelExt <- function(object, ..., clusters = trajectoryAssignments(object)) {
#'   pred = predict(object, newdata = NULL)
#'   transformFitted(pred = pred, model = object, clusters = clusters)
#' }
#' }
#' The [transformFitted()] function takes care of transforming the prediction input to the right output format.
#' @seealso [fittedTrajectories] [plotFittedTrajectories] [stats::fitted] [predict.lcModel] [trajectoryAssignments] [transformFitted]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' fitted(model)
fitted.lcModel = function(object, ..., clusters = trajectoryAssignments(object)) {
  pred = predict(object, newdata = NULL)
  transformFitted(pred = pred, model = object, clusters = clusters)
}


# . fittedTrajectories ####
#' @export
#' @name fittedTrajectories
#' @rdname fittedTrajectories
#' @aliases fittedTrajectories,lcModel-method
#' @title Extract the fitted trajectories for all strata
#' @param object The model.
#' @param at The time points at which to compute the id-specific trajectories.
#' @param what The distributional parameter to compute the response for.
#' @param clusters The cluster assignments for the strata to base the trajectories on.
#' @param ... Additional arguments.
#' @return A `data.frame` representing the fitted response per trajectory per moment in time.
#' @examples
#' data(latrendData)
#' m <- lcMethodKML("Y", id = "Id", time = "Time")
#' model <- latrend(method = m, data = latrendData)
#' fittedTrajectories(model)
#'
#' fittedTrajectories(model, at = c(0, .5, 1))
#' @family model-specific methods
setMethod('fittedTrajectories', signature('lcModel'), function(object, at, what, clusters, ...) {
  ids = ids(object)
  assert_that(length(clusters) == nIds(object))

  if (is.numeric(at)) {
    newdata = data.table(
      Id = rep(ids, each = length(at)),
      Cluster = rep(clusters, each = length(at)),
      Time = at
    ) %>%
      setnames('Id', idVariable(object)) %>%
      setnames('Time', timeVariable(object))
  } else if (is.list(at)) {
    assert_that(has_name(at, timeVariable(object)), msg = 'Named list at must contain the time covariate')
    assert_that(!has_name(at, c(idVariable(object), 'Cluster')))

    at = as.data.table(at)
    idx = seq_len(nrow(at)) %>% rep(length(ids))
    newdata = data.table(Id = rep(ids, each = nrow(at)),
      Cluster = rep(clusters, each = nrow(at)),
      at[idx,]) %>%
      setnames('Id', idVariable(object))
  } else {
    stop('unsupported input')
  }

  preds = predict(object, newdata = newdata, what = what)

  if (is.null(preds)) {
    warning('fitted predictions not supported by the model: got NULL output')
    return (NULL)
  }

  assert_that(is.data.frame(preds))
  assert_that(nrow(preds) == nrow(newdata), msg = 'invalid output from predict function of lcModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what = what)) := preds$Fit]
  return(newdata[])
})


#' @export
#' @importFrom stats formula
#' @title Extract the formula of a lcModel
#' @description Get the formula associated with the fitted `lcModel` object.
#' This is determined by the `formula` argument of the `lcMethod` specification that was used to fit the model.
#' @param x The `lcModel` object.
#' @param what The distributional parameter.
#' @param ... Additional arguments.
#' @return Returns the associated `formula`, or `response ~ 0` if not specified.
#' @seealso [stats::formula]
#' @examples
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
#' lmkm <- latrend(method, data = latrendData)
#' formula(lmkm) # Y ~ Time
#'
#' kml <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' formula(kml) # Y ~ 0
formula.lcModel = function(x, what = 'mu', ...) {
  method = getLcMethod(x)
  if (what == 'mu') {
    if (has_name(method, 'formula')) {
      method$formula
    } else {
      as.formula(paste(x@response, '~ 0'))
    }
  } else {
    formulaName = paste('formula', what, sep = '.')
    if (has_name(method, formulaName)) {
      formula(method, what = what)
    } else {
      ~ 0
    }
  }
}


#' @export
#' @importFrom stats getCall
#' @title Get the model call
#' @description Extract the `call` that was used to fit the given `lcModel` object.
#' @param x The `lcModel` object.
#' @param ... Not used.
#' @return A `call` to [latrend()] with the necessary arguments and data.
#' @seealso [stats::getCall] [getLcMethod]
#' @keywords internal
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' getCall(model)
getCall.lcModel = function(x, ...) {
  x@call
}

#. getLabel ####
#' @export
#' @rdname getLabel
#' @aliases getLabel,lcModel-method
setMethod('getLabel', signature('lcModel'), function(object, ...) {
  object@label
})


#' @export
#' @title Get the method specification of a lcModel
#' @description Get the `lcMethod` specification object that was used for fitting the given `lcModel` object.
#' @param object The `lcModel` object.
#' @return An `lcMethod` object.
#' @seealso [getCall.lcModel]
#' @examples
#' model = latrend(method=lcMethodKML("Y", id = "Id", time = "Time"), data=latrendData)
#' getLcMethod(model)
getLcMethod = function(object) {
  assert_that(is.lcModel(object))
  object@method
}


# . getName ####
#' @export
#' @rdname getName
#' @aliases getName,lcModel-method
setMethod('getName', signature('lcModel'), function(object) {
  basename = getLcMethod(object) %>% getName()
  lbl = getLabel(object)
  if (length(lbl) > 0 && nchar(lbl) > 0) {
    paste(basename, lbl, sep = '-')
  } else {
    basename
  }
})

# . getShortName ####
#' @export
#' @rdname getName
#' @aliases getShortName,lcModel-method
setMethod('getShortName',  signature('lcModel'),
  function(object) getLcMethod(object) %>% getShortName())


# . ids ####
#' @export
#' @title Get the trajectory ids on which the model was fitted
#' @details The order returned by `ids(object)` determines the id order for any output involving id-specific values, such as in [trajectoryAssignments()] or [postprob()].
#' @param object The `lcModel` object.
#' @return A `character vector` or `integer vector` of the identifier for every fitted trajectory.
#' @examples
#' data(latrendData)
#' model = latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' ids(model) # 1, 2, ..., 200
ids = function(object) {
  if (length(object@ids) == 0) {
    iddata = model.data(object)[[idVariable(object)]]
    if (is.factor(iddata)) {
      levels(iddata)[levels(iddata) %in% iddata]
    } else {
      unique(iddata) %>% sort()
    }
  } else {
    object@ids
  }
}


#. idVariable ####
#' @export
#' @name idVariable
#' @rdname idVariable
#' @aliases idVariable,lcModel-method
#' @examples
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' idVariable(model) # "Id"
#'
#' @family lcModel variables
setMethod('idVariable', signature('lcModel'), function(object) object@id)


#' @export
#' @rdname is
is.lcModel = function(x) {
  isS4(x) && is(x, 'lcModel')
}


#' @export
#' @importFrom stats logLik
#' @title Extract the log-likelihood of a lcModel
#' @details The default implementation checks for the existence of the `logLik()` function for the internal model, and returns the output, if available.
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `numeric` with the computed log-likelihood. If unavailable, `NA` is returned.
#' @seealso [stats::logLik] [metric]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ 1,
#'   id = "Id", time = "Time", nClusters = 3)
#' gbtm <- latrend(method, data = latrendData)
#' logLik(gbtm)
logLik.lcModel = function(object, ...) {
  if (is.null(object@model) ||
      is.null(getS3method('logLik', class = class(object@model), optional = TRUE))) {
    N = nIds(object)
    df = length(coef(object))
    ll = as.numeric(NA)
    attr(ll, 'nobs') = N
    attr(ll, 'df') = df
    class(ll) = 'logLik'
    ll
  } else {
    logLik(object@model)
  }
}


#. metric ####
#' @export
#' @name metric
#' @rdname metric
#' @aliases metric,lcModel-method
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), latrendData)
#' bic <- metric(model, "BIC")
#'
#' ic <- metric(model, c("AIC", "BIC"))
#'
#'
#' @family metric functions
setMethod('metric', signature('lcModel'), function(object, name, ...) {
  assert_that(length(name) > 0, msg = 'no metric names provided')
  assert_that(is.lcModel(object),
              is.character(name))

  funMask = name %in% getInternalMetricNames()
  if (!all(funMask)) {
    warning(
      'Internal metric(s) ',
      paste0('"', name[!funMask], '"', collapse = ', '),
      ' are not defined. Returning NA.'
    )
  }
  metricFuns = lapply(name[funMask], getInternalMetricDefinition)
  metricValues = mapply(function(fun, name) {
    value = fun(object)
    assert_that(
      is.scalar(value) && (is.numeric(value) || is.logical(value)),
      msg = sprintf(
        'invalid output for metric "%s"; expected scalar number or logical value',
        name
      )
    )
    return(value)
  }, metricFuns, name[funMask])

  allMetrics = rep(NA * 0, length(name))
  allMetrics[funMask] = unlist(metricValues)
  names(allMetrics) = name
  return(allMetrics)
})


#' @export
#' @importFrom stats model.frame
#' @title Extract model training data
#' @description See [stats::model.frame()] for more details.
#' @param formula The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `data.frame` containing the variables used by the model.
#' @seealso [stats::model.frame] [model.data.lcModel]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
#' lmkm <- latrend(method, data = latrendData)
#' model.frame(lmkm)
model.frame.lcModel = function(formula, ...) {
  if (is.null(formula@model) ||
      is.null(getS3method('model.frame', class = class(formula@model), optional = TRUE))) {
    labs = stats::formula(formula) %>%
      terms() %>%
      labels()

    if (length(labs) > 0) {
      model.data(formula) %>% subset(select = labs)
    } else {
      stop(sprintf('cannot determine model.frame for the given model of class %s', class(formula)))
    }
  } else {
    model.frame(formula@model)
  }
}


#' @export
#' @title Extract the model training data
#' @param object The object.
#' @param ... Additional arguments.
#' @keywords internal
model.data = function(object, ...) {
  UseMethod('model.data')
}

#' @export
#' @title Extract the model data that was used for fitting
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @description Evaluates the data call in the environment that the model was trained in.
#' @return The full `data.frame` that was used for fitting the `lcModel`.
#' @seealso [model.frame.lcModel] [time.lcModel]
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
#' kml <- latrend(method, latrendData)
#' model.data(kml)
model.data.lcModel = function(object, ...) {
  if (!is.null(object@data)) {
    object@data
    assert_that(is.data.frame(object@data), msg = 'expected data reference to be a data.frame')
    return(object@data)
  } else {
    assert_that(has_name(getCall(object), 'data'), msg = 'Cannot determine data used to train this lcModel. Data not part of model call, and not assigned to the @data slot')
    data = eval(getCall(object)$data, envir = environment(object))
    assert_that(!is.null(data),
                msg = sprintf('could not find "%s" in the model environment', deparse(data)))
    assert_that(!is.function(data), msg = sprintf('The data object was not found in the model environment. The data object currently evaluates to a function, indicating the original training data is not loaded.'))

    modelData = trajectories(
      data,
      id = idVariable(object),
      time = timeVariable(object),
      response = responseVariable(object),
      envir = environment(object)
    )

    assert_that(is.data.frame(modelData), msg = 'expected data reference to be a data.frame')
    return(modelData)
  }
}


#' @export
#' @title Number of trajectories
#' @description Get the number of trajectories (strata) that were used for fitting the given `lcModel` object.
#' The number of trajectories is determined from the number of unique identifiers in the training data. In case the trajectory ids were supplied using a `factor` column, the number of trajectories is determined by the number of levels instead.
#' @param object The `lcModel` object.
#' @return An `integer` with the number of trajectories on which the `lcModel` was fitted.
#' @seealso [nobs] [nClusters]
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
#' kml <- latrend(method, latrendData)
#' nIds(kml)
nIds = function(object) {
  iddata = model.data(object)[[idVariable(object)]]
  if (is.factor(iddata)) {
    nlevels(iddata)
  } else {
    uniqueN(iddata)
  }
}

#' @export
#' @title Number of clusters
#' @description Get the number of clusters estimated by the given `lcModel` object.
#' @param object The `lcModel` object.
#' @return An `integer` with the number of clusters identified by the `lcModel`.
#' @seealso [nIds] [nobs]
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
#' kml <- latrend(method, latrendData)
#' nClusters(kml)
nClusters = function(object) {
  assert_that(is.lcModel(object))
  length(object@clusterNames)
}


#' @export
#' @importFrom stats nobs
#' @title Extract the number of observations from a lcModel
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @family model-specific methods
#' @seealso [nIds] [nClusters]
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
#' kml <- latrend(method, latrendData)
#' nobs(kml)
nobs.lcModel = function(object, ...) {
  nrow(model.data(object))
}


#' @export
#' @rdname predict.lcModel
#' @importFrom stats predict
#' @title lcModel predictions
#' @description Predicts the expected trajectory observations at the given time for each cluster.
#' @section Implementation:
#' Note: Subclasses of `lcModel` should preferably implement [predictForCluster()] instead of overriding `predict.lcModel` as that function is designed to be easier to implement because it is single-purpose.
#'
#' The `predict.lcModelExt` function should be able to handle the case where `newdata = NULL` by returning the fitted values.
#' After post-processing the non-NULL newdata input, the observation- and cluster-specific predictions can be computed.
#' Lastly, the output logic is handled by the [transformPredict()] function. It converts the computed predictions (e.g., `matrix` or `data.frame`) to the appropriate output format.
#' \preformatted{
#' predict.lcModelExt <- function(object, newdata = NULL, what = "mu", ...) {
#'   if (is.null(newdata)) {
#'     newdata = model.data(object)
#'     if (hasName(newdata, 'Cluster')) {
#'       # allowing the Cluster column to remain would break the fitted() output.
#'       newdata[['Cluster']] = NULL
#'     }
#'   }
#'
#'   # compute cluster-specific predictions for the given newdata
#'   pred <- NEWDATA_COMPUTATIONS_HERE
#'   transformPredict(pred = pred, model = object, newdata = newdata)
#' })
#' }
#' @param object The `lcModel` object.
#' @param newdata Optional `data.frame` for which to compute the model predictions. If omitted, the model training data is used.
#' Cluster trajectory predictions are made when ids are not specified.
#' @param what The distributional parameter to predict. By default, the mean response 'mu' is predicted. The cluster membership predictions can be obtained by specifying `what = 'mb'`.
#' @return If `newdata` specifies the cluster membership; a `data.frame` of cluster-specific predictions. Otherwise, a `list` of `data.frame` of cluster-specific predictions is returned.
#' @param ... Additional arguments.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(
#'    fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), latrendData)
#' predFitted <- predict(model) # same result as fitted(model)
#'
#' # Cluster trajectory of cluster A
#' predCluster <- predict(model, newdata = data.frame(Cluster = "A", Time = time(model)))
#'
#' # Prediction for id S1 given cluster A membership
#' predId <- predict(model, newdata = data.frame(Cluster = "A", Id = "S1", Time = time(model)))
#'
#' # Prediction matrix for id S1 for all clusters
#' predIdAll <- predict(model, newdata = data.frame(Id = "S1", Time = time(model)))
#' @seealso [predictForCluster] [stats::predict] [fitted.lcModel] [clusterTrajectories] [trajectories] [predictPostprob] [predictAssignments]
#' @family model-specific methods
predict.lcModel = function(object, newdata = NULL, what = 'mu', ...) {
  assert_that(is.newdata(newdata))

  predMethod = selectMethod('predictForCluster', class(object), optional = TRUE)
  if (is.null(predMethod) || predMethod@defined@.Data == 'lcModel') {
    stop(sprintf('Cannot compute predictions for model of class %1$s because neither predict.%1$s nor predictForCluster(%1$s) are implemented for this model', class(object)[1]))
  }

  # special case for when no newdata is provided
  if (is.null(newdata)) {
    newdata = model.data(object)
    if (hasName(newdata, 'Cluster')) {
      newdata[['Cluster']] = NULL # allowing the Cluster column to remain would break the fitted() output.
    }
  }
  else {
    if (nrow(newdata) == 0) {
      warning('called predict() with empty newdata data.frame (nrow = 0)')
    }
  }

  newdata = as.data.table(newdata)

  if (hasName(newdata, 'Cluster')) {
    # enforce cluster ordering
    newdata[, Cluster := factor(Cluster, levels = clusterNames(object))]

    assert_that(noNA(newdata$Cluster) &
        all(unique(newdata$Cluster) %in% clusterNames(object)),
      msg = paste0('The provided newdata "Cluster" column must be complete and only contain cluster names associated with the model (',
        paste0(shQuote(clusterNames(object)), collapse = ', '), ').'))

    # predictForCluster with newdata subsets
    clusdataList = split(newdata, by = 'Cluster', sorted = TRUE, drop = TRUE) %>%
      lapply(function(cdata) cdata[, Cluster := NULL])
  }
  else {
    # predictForCluster with newdata for each cluster
    clusdataList = replicate(nClusters(object), newdata, simplify = FALSE)
    names(clusdataList) = clusterNames(object)
  }

  predList = mapply(function(cname, cdata) {
    predictForCluster(object, cluster = cname, newdata = cdata, what = what, ...)
  }, names(clusdataList), clusdataList, SIMPLIFY = FALSE)

  assert_that(
    length(predList) == length(clusdataList),
    msg = 'unexpected internal state. please report')
  assert_that(
    all(vapply(predList, function(x) is(x, class(predList[[1]])), FUN.VALUE = TRUE)),
    msg = 'output from predictForCluster() must be same class for all clusters. Check the model implementation.')


  if (is.data.frame(predList[[1]])) {
    pred = rbindlist(predList, idcol = 'Cluster')
    pred[, Cluster := factor(Cluster, levels = seq_len(nClusters(object)), labels = clusterNames(object))]
  }
  else if (is.numeric(predList[[1]])) {
    clusDataRows = vapply(clusdataList, nrow, FUN.VALUE = 0)
    clusPredRows = vapply(predList, length, FUN.VALUE = 0)
    assert_that(all(clusDataRows == clusPredRows),
      msg = 'Numeric output length from predictForCluster() does not match the number of input newdata rows for one or more clusters')

    pred = data.table(
      Cluster = rep(factor(names(clusDataRows), levels = clusterNames(object)), clusDataRows),
      Fit = do.call(c, predList)
    )
  }
  else {
    stop(
      'unsupported output from predictForCluster(): must be data.frame or numeric. Check the model implementation.'
    )
  }

  preddata = cbind(rbindlist(clusdataList), pred)

  transformPredict(pred = preddata, model = object, newdata = newdata)
}


# . predictForCluster ####
#' @export
#' @name predictForCluster
#' @rdname predictForCluster
#' @aliases predictForCluster,lcModel-method
#' @title lcModel prediction conditional on a cluster
#' @description Predicts the expected trajectory observations at the given time under the assumption that the trajectory belongs to the specified cluster.
#'
#' The same result can be obtained by calling [`predict()`][predict.lcModel()] with the `newdata` `data.frame` having a `"Cluster"` assignment column.
#' The main purpose of this function is to make it easier to implement the prediction computations for custom `lcModel` classes.
#'
#' @details The default `predictForCluster()` method makes use of [predict.lcModel()], and vice versa. For this to work, any extending `lcModel` classes, e.g., `lcModelExample`, should implement either `predictForCluster(lcModelExample)` or `predict.lcModelExample()`. When implementing new models, it is advisable to implement `predictForCluster` as the cluster-specific computation generally results in shorter and simpler code.
#' @inheritParams predict.lcModel
#' @param cluster The cluster name (as `character`) to predict for.
#' @param ... Additional arguments.
#' @return A `vector` with the predictions per `newdata` observation, or a `data.frame` with the predictions and newdata alongside.
#' @section Implementation:
#' Classes extending `lcModel` should override this method, unless [predict.lcModel()] is preferred.
#' \preformatted{
#' setMethod("predictForCluster", "lcModelExt",
#'  function(object, newdata = NULL, cluster, ..., what = "mu") {
#'   # return model predictions for the given data under the
#'   # assumption of the data belonging to the given cluster
#' })
#' }
#' @seealso [predict.lcModel]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method, latrendData)
#'
#' predictForCluster(model,
#'   newdata = data.frame(Time = c(0, 1)),
#'   cluster = "B")
#'
#' # all fitted values under cluster B
#' predictForCluster(model, cluster = "B")
setMethod('predictForCluster', signature('lcModel'),
  function(object, newdata = NULL, cluster, ..., what = 'mu') {
  # check whether predict.lcModelType exists
  cls = class(object)
  classes = extends(class(object)) %>% setdiff('lcModel')
  methodsAvailable = vapply(classes, function(x) !is.null(getS3method('predict', x, optional = TRUE)), FUN.VALUE = FALSE)

  assert_that(any(methodsAvailable),
    msg = sprintf('Cannot compute cluster-specific predictions for model of class %1$s because neither predict.%1$s nor predictForCluster(%1$s) are implemented for this model', cls))

  newdata = cbind(newdata, Cluster = cluster)
  pred = predict(object, newdata = newdata, ..., what = what)
  pred$Fit
})



# . predictPostprob ####
#' @export
#' @name predictPostprob
#' @rdname predictPostprob
#' @aliases predictPostprob,lcModel-method
#' @title lcModel posterior probability prediction
#' @description Returns the observation-specific posterior probabilities for the given data.
#' The default implementation returns a uniform probability matrix.
#' @param object The `lcModel` to predict the posterior probabilities with.
#' @param newdata Optional data frame for which to compute the posterior probability. If omitted, the model training data is used.
#' @param ... Additional arguments.
#' @return A N-by-K `matrix` indicating the posterior probability per trajectory per measurement on each row, for each cluster (the columns).
#' Here, `N = nrow(newdata)` and `K = nClusters(object)`.
#' @section Implementation:
#' Classes extending `lcModel` should override this method to enable posterior probability predictions for new data.
#' \preformatted{
#' setMethod("predictPostprob", "lcModelExt", function(object, newdata = NULL, ...) {
#'   # return observation-specific posterior probability matrix
#' })
#' }
#' @family model-specific methods
setMethod('predictPostprob', signature('lcModel'), function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    N = nrow(model.data(object))
    pp = postprob(object, ...)
    rownames(pp) = NULL
    pp[make.idRowIndices(object), ]
  }
  else {
    warning(
      'predictPostprob() not implemented for ',
      class(object)[1],
      '. Returning uniform probability matrix.'
    )

    matrix(1 / nClusters(object), nrow = nrow(newdata), ncol = nClusters(object))
  }
})


#. predictAssignments ####
#' @export
#' @name predictAssignments
#' @rdname predictAssignments
#' @aliases predictAssignments,lcModel-method
#' @title Predict the cluster assignments for new trajectories
#' @description Computes the posterior probability based on the provided (observed) data.
#' @inheritParams predict.lcModel
#' @param strategy A function returning the cluster index based on the given `vector` of membership probabilities.
#' By default (`strategy = which.max`), trajectories are assigned to the most likely cluster.
#' @details The default implementation uses [predictPostprob] to determine the cluster membership.
#' @return A `factor` of length `nrow(newdata)` that indicates the assigned cluster per trajectory per observation.
#' @seealso [predictPostprob] [predict.lcModel]
#' @family model-specific methods
#' @examples
#' \dontrun{
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' predictAssignments(model, newdata = data.frame(Id = 999, Y = 0, Time = 0))
#' }
setMethod('predictAssignments', signature('lcModel'), function(
  object,
  newdata = NULL,
  strategy = which.max,
  ...
  ) {

  pp = predictPostprob(object, newdata = newdata)

  if (is.null(newdata)) {
    newdata = model.data(object)
  }

  assert_that(
    is_valid_postprob(pp, object),
    nrow(pp) == nrow(newdata)
  )

  apply(pp, 1, strategy, ...) %>%
    factor(levels = 1:nClusters(object),
           labels = clusterNames(object))
})


#. plot ####
#' @export
#' @name plot
#' @aliases plot,lcModel,ANY-method
#' @title Plot a lcModel
#' @description Plot a `lcModel` object. By default, this plots the cluster trajectories of the model, along with the training data.
#' @param x The `lcModel` object.
#' @param y Not used.
#' @inheritDotParams plotClusterTrajectories
#' @return A `ggplot` object.
#' @seealso [plotClusterTrajectories] [plotFittedTrajectories] [plotTrajectories] [ggplot2::ggplot]
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' plot(model)
setMethod('plot', signature('lcModel'), function(x, y, ...) {
  args = list(...)

  if(!has_name(args, 'trajectories')) {
    args$trajectories = TRUE
  }

  do.call(plotClusterTrajectories, c(x, args))
})


#. plotFittedTrajectories ####
#' @export
#' @name plotFittedTrajectories
#' @rdname plotFittedTrajectories
#' @aliases plotFittedTrajectories,lcModel-method
#' @title Plot fitted trajectories of a lcModel
#' @param object The `lcModel` object.
#' @param ... Arguments passed to [fittedTrajectories()].
#' @inheritDotParams trajectories
#' @seealso [fittedTrajectories] [plotClusterTrajectories] [plotTrajectories] [plot]
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' plotFittedTrajectories(model)
setMethod('plotFittedTrajectories', signature('lcModel'), function(object, ...) {
  data = fittedTrajectories(object, ...)

  plotTrajectories(
    data,
    response = responseVariable(object),
    time = timeVariable(object),
    id = idVariable(object),
    cluster = 'Cluster'
  )
})

#. plotClusterTrajectories ####
# NOTE: Cannot include @inheritDotParams clusterTrajectories-method because the name is not supported by Roxygen
#' @export
#' @name plotClusterTrajectories
#' @rdname plotClusterTrajectories
#' @aliases plotClusterTrajectories,lcModel-method
#' @title Plot the cluster trajectories of a lcModel
#' @inheritParams clusterTrajectories
#' @param clusterLabels Cluster display names. By default it's the cluster name with its proportion enclosed in parentheses.
#' @param trajAssignments The cluster assignments for the fitted trajectories. Only used when `trajectories = TRUE` and `facet = TRUE`. See [trajectoryAssignments].
#' @param ... Arguments passed to [clusterTrajectories()], or [ggplot2::geom_line()] for plotting the cluster trajectory lines.
#' @return A `ggplot` object.
#' @seealso [clusterTrajectories] [plotFittedTrajectories] [plotTrajectories] [plot]
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' plotClusterTrajectories(model)
setMethod('plotClusterTrajectories', signature('lcModel'),
  function(object,
    what = 'mu',
    at = time(object),
    clusterLabels = sprintf('%s (%s)',
      clusterNames(object),
      percent(clusterProportions(object))),
    trajectories = FALSE,
    facet = isTRUE(trajectories),
    trajAssignments = trajectoryAssignments(object),
    ...
  ) {
  assert_that(length(clusterLabels) == nClusters(object))

  clusdata = clusterTrajectories(object, at = at, what = what, ...) %>%
    as.data.table() %>%
    .[, Cluster := factor(Cluster, levels = levels(Cluster), labels = clusterLabels)]

  rawdata = model.data(object) %>% as.data.table()
  if (!is.null(trajAssignments)) {
    assert_that(
      length(trajAssignments) == nIds(object),
      all(trajAssignments %in% clusterNames(object))
    )
    trajAssignments = factor(trajAssignments, levels = clusterNames(object), labels = clusterLabels)
    rawdata[, Cluster := trajAssignments[make.idRowIndices(object)]]
  }

  .plotClusterTrajs(clusdata,
    response = responseVariable(object, what = what),
    time = timeVariable(object),
    id = idVariable(object),
    trajectories = trajectories,
    facet = facet,
    rawdata = rawdata,
    ...)
})


#. plotTrajectories ####
#' @export
#' @rdname plotTrajectories
#' @aliases plotTrajectories,lcModel-method
#' @inheritDotParams trajectories
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' plotTrajectories(model)
setMethod('plotTrajectories', signature('lcModel'), function(object, ...) {
  data = trajectories(object, ...)
  plotTrajectories(data,
    id = idVariable(object),
    time = timeVariable(object),
    response = responseVariable(object)
  )
})


#. postprob ####
#' @export
#' @name postprob
#' @rdname postprob
#' @aliases postprob,lcModel-method
#' @title Posterior probability per fitted trajectory
#' @description Get the posterior probability matrix with element \eqn{(i,j)} indicating the probability of trajectory \eqn{i} belonging to cluster \eqn{j}.
#' @details This method should be extended by `lcModel` implementations. The default implementation returns uniform probabilities for all observations.
#' @param object The `lcModel`.
#' @param ... Additional arguments.
#' @return A I-by-K `matrix` with `I = nIds(object)` and `K = nClusters(object)`.
#' @section Implementation:
#' Classes extending `lcModel` should override this method.
#' \preformatted{
#' setMethod("postprob", "lcModelExt", function(object, ...) {
#'   # return trajectory-specific posterior probability matrix
#' })
#' }
#' @seealso [trajectoryAssignments] [predictPostprob] [predictAssignments]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), data = latrendData)
#' postprob(model)
setMethod('postprob', signature('lcModel'), function(object, ...) {
  warning('postprob() not implemented for ', class(object)[1],
    '. Returning uniform posterior probability matrix.')

  matrix(1 / nClusters(object), nrow = nIds(object), ncol = nClusters(object))
})


# . QQ plot ####
#' @export
#' @name qqPlot
#' @rdname qqPlot
#' @aliases qqPlot,lcModel-method
#' @title Quantile-quantile plot
#' @description Plot the quantile-quantile (Q-Q) plot for the fitted `lcModel` object. This function is based on the \pkg{qqplotr} package.
#' @param object The `lcModel` object.
#' @param byCluster Whether to plot the Q-Q line per cluster
#' @param ... Additional arguments passed to [qqplotr::geom_qq_band()], [qqplotr::stat_qq_line()], and [qqplotr::stat_qq_point()].
#' @return A `ggplot` object.
#' @seealso [residuals.lcModel] [metric] [plotClusterTrajectories]
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), data = latrendData)
#' qqPlot(model)
setMethod('qqPlot', signature('lcModel'), function(object, byCluster = FALSE, ...) {
  assert_that(is.lcModel(object))
  idIndexColumn = factor(model.data(object)[[idVariable(object)]], levels = ids(object)) %>% as.integer()
  rowClusters = trajectoryAssignments(object)[idIndexColumn]

  res = residuals(object)
  requireNamespace('qqplotr')
  p = ggplot(data = data.frame(Cluster = rowClusters, res = res), aes(sample = res)) +
    qqplotr::geom_qq_band(...) +
    qqplotr::stat_qq_line(...) +
    qqplotr::stat_qq_point(...) +
    labs(x = 'Theoretical quantiles', y = 'Sample quantiles', title = 'Quantile-quantile plot')

  if (byCluster) {
    p = p + facet_wrap(~ Cluster)
  }

  return(p)
})


#' @export
#' @importFrom stats residuals
#' @title Extract lcModel residuals
#' @description Extract the residuals for a fitted `lcModel` object.
#' By default, residuals are computed under the most likely cluster assignment for each trajectory.
#' @inheritParams fitted.lcModel
#' @return A `numeric vector` of residuals for the cluster assignments specified by clusters.
#' If the `clusters` argument is unspecified, a `matrix` of cluster-specific residuals per observations is returned.
#' @family model-specific methods
#' @seealso [fitted.lcModel] [trajectories]
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), data = latrendData)
#' summary(residuals(model))
residuals.lcModel = function(object, ..., clusters = trajectoryAssignments(object)) {
  ypred = fitted(object, clusters = clusters, ...)
  yref = model.data(object)[[responseVariable(object)]]

  if (is.matrix(ypred)) {
    assert_that(length(yref) == nrow(ypred))
    resMat = matrix(yref, nrow = nrow(ypred), ncol = ncol(ypred)) - ypred
    colnames(resMat) = colnames(ypred)
    resMat
  } else if (is.numeric(ypred)) {
    assert_that(length(yref) == length(ypred))
    yref - ypred
  } else {
    return(NULL)
  }
}

#. responseVariable ####
#' @export
#' @name responseVariable
#' @rdname responseVariable
#' @aliases responseVariable,lcModel-method
#' @title Get the response variable
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' responseVariable(model) # "Value"
#' @family lcModel variables
setMethod('responseVariable', signature('lcModel'), function(object, ...) object@response)

#' @export
#' @title Get the model estimation time
#' @description Get the estimation time of the model, determined by the time taken for the associated [fit()] function to finish.
#' @param object The `lcModel` object.
#' @return A `numeric` representing the model estimation time, in seconds.
estimationTime = function(object) {
  assert_that(is.lcModel(object))
  object@estimationTime
}


# . show ####
setMethod('show', 'lcModel', function(object) {
  summary(object) %>% show()
})


#' @export
#' @importFrom stats sigma
#' @title Extract residual standard deviation from a lcModel
#' @description Extracts or estimates the residual standard deviation. If [sigma()] is not defined for a model, it is estimated from the residual error vector.
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `numeric` indicating the residual standard deviation.
#' @seealso [coef.lcModel] [metric]
#' @family model-specific methods
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), data = latrendData)
#' sigma(model)
sigma.lcModel = function(object, ...) {
  if (is.null(object@model) ||
      is.null(getS3method('sigma', class = class(object@model), optional = TRUE))) {
    residuals(object) %>% sd()
  } else {
    sigma(object@model)
  }
}


#. strip ####
#' @export
#' @name strip
#' @rdname strip
#' @aliases strip,lcModel-method
#' @title Reduce the lcModel memory footprint for serialization
#' @description Strip a lcModel of non-essential variables and environments in order to reduce the model size for serialization.
#' @param object The `lcModel` object.
#' @param classes The object classes for which to remove their assigned environment. By default, only environments from `formula` are removed.
#' @param ... Additional arguments.
#' @return An `lcModel` object of the same type as the `object` argument.
#' @section Implementation:
#' Classes extending `lcModel` can override this method to remove additional non-essentials.
#' \preformatted{
#' setMethod("strip", "lcModelExt", function(object, ..., classes = "formula") {
#'   object <- callNextMethod()
#'   # further process the object
#'   return (object)
#' })
#' }
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), data = latrendData)
#' strip(model)
setMethod('strip', signature('lcModel'), function(object, ..., classes = 'formula') {
  newObject = object

  environment(newObject) = NULL
  newObject@method = strip(object@method, ..., classes = classes)
  newObject@call = strip(object@call, ..., classes = classes)

  return(newObject)
})


#. timeVariable ####
#' @export
#' @name timeVariable
#' @rdname timeVariable
#' @aliases timeVariable,lcModel-method
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' idVariable(model) # "Id"
#' @family lcModel variables
setMethod('timeVariable', signature('lcModel'), function(object) object@time)




#' @export
#' @importFrom stats time
#' @title Sampling times of a lcModel
#' @description Extract the sampling times on which the `lcModel` was fitted.
#' @param x The `lcModel` object.
#' @param ... Not used.
#' @return A `numeric vector` of the unique times at which observations occur, in increasing order.
#' @family model-specific methods
#' @seealso [timeVariable] [model.data]
time.lcModel = function(x, ...) {
  if (length(x@times) == 0) {
    assert_that(
      has_name(model.data(x), timeVariable(x)),
      msg = sprintf(
        'cannot identify model times: model.data() is missing time variable column "%s"',
        timeVariable(x)
    ))
    times = model.data(x)[[timeVariable(x)]] %>% unique() %>% sort()
  } else {
    times = x@times
  }

  assert_that(
    length(times) > 0,
    is.vector(times),
    is.numeric(times)
  )

  times
}


# . trajectories ####
#' @rdname trajectories
#' @aliases trajectories,lcModel-method
setMethod('trajectories', signature('lcModel'), function(object, ...) {
  data = model.data(object)

  assert_that(!is.null(data), msg = 'no associated training data for this model')

  id = idVariable(object)
  time = timeVariable(object)
  res = responseVariable(object)

  trajdata = subset(data, select = c(id, time, res))

  trajectories(trajdata, id = id, time = time, response = res)
})


#. trajectoryAssignments ####
#' @export
#' @name trajectoryAssignments
#' @aliases trajectoryAssignments,lcModel-method
#' @title Get the cluster membership of each trajectory
#' @description Classify the fitted trajectories based on the posterior probabilities computed by [postprob()], according to a given classification strategy.
#'
#' By default, trajectories are assigned based on the highest posterior probability using [which.max()].
#' In cases where identical probabilities are expected between clusters, it is preferable to use \link[nnet]{which.is.max} instead, as this function breaks ties at random.
#' Another strategy to consider is the function [which.weight()], which enables weighted sampling of cluster assignments based on the trajectory-specific probabilities.
#' @param object The object to obtain the cluster assignments from.
#' @param strategy A function returning the cluster index based on the given vector of membership probabilities. By default, ids are assigned to the cluster with the highest probability.
#' @param ... Any additional arguments passed to the strategy function.
#' @return A `factor` indicating the cluster membership for each trajectory.
#' @seealso [postprob] [clusterSizes] [predictAssignments]
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' trajectoryAssignments(model)
#'
#' # assign trajectories at random using weighted sampling
#' trajectoryAssignments(model, strategy = which.weight)
setMethod('trajectoryAssignments', signature('lcModel'), function(object, strategy = which.max, ...) {
  pp = postprob(object)

  result = apply(pp, 1, strategy, ...)

  assert_that(
    is.numeric(result),
    length(result) == nIds(object),
    all(
      vapply(result, is.count, FUN.VALUE = TRUE) |
        vapply(result, is.na, FUN.VALUE = TRUE)),
    min(result, na.rm = TRUE) >= 1,
    max(result, na.rm = TRUE) <= nClusters(object)
  )

  assignments = factor(result, levels = 1:nClusters(object), labels = clusterNames(object))

  make.trajectoryAssignments(object, assignments)
})


#' @export
#' @importFrom stats update
#' @title Update a lcModel
#' @description Fit a new model with modified arguments from the current model.
#' @param object The `lcModel` object.
#' @param ... Any new method arguments to refit the model with.
#' @return The refitted `lcModel` object, of the same type as the `object` argument.
#' @inheritDotParams latrend
#' @seealso [latrend] [getCall]
#' @examples
#' data(latrendData)
#' m <- lcMethodKML("Y", id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method = m, data = latrendData)
#' # fit for a different number of clusters
#' update(model, nClusters = 2)
update.lcModel = function(object, ...) {
  assert_that(is.lcModel(object))
  modelCall = getCall(object)

  assert_that(as.character(modelCall[[1]]) != '<undef>', msg = 'cannot update lcModel because lcMethod call is undefined')

  updateCall = match.call() %>% tail(-2)
  updateNames = names(updateCall)

  clCall = replace(modelCall, updateNames, updateCall[updateNames])

  eval(clCall, envir = parent.frame())
}
