#' @include method.R plot.R
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
#' @slot ids The possible trajectory identifier values the model was fitted on.
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

  assert_that(nchar(object@id) > 0,
              nchar(object@time) > 0,
              nchar(object@response) > 0)

  data = model.data(object)
  assert_that(!is.null(data), msg = 'invalid data object for new lcModel. Either specify the data slot or ensure that the model call contains a data argument which correctly evaluates.')
  assert_that(has_name(data, c(
    object@id, object@time, object@response
  )))
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
#' @param at An optional vector, list or data frame of covariates at which to compute the cluster trajectory predictions.
#' If a vector is specified, this is assumed to be the time covariate. Otherwise, a named list or data frame must be provided.
#' @return A data.frame of the estimated values at the given times
#' @examples
#' model <- latrend(method=lcMethodLcmmGMM(Y ~ Time + (1 | Id)), data = latrendData)
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
#' model <- latrend(lcMethodKML("Y"), latrendData)
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
#' model <- latrend(lcMethodKML("Y"), latrendData)
#' clusterNames(model) <- c("Group 1", "Group 2")
`clusterNames<-` = function(object, value) {
  assert_that(is.lcModel(object),
    is.character(value),
    length(value) == nClusters(object))
  object@clusterNames = value
  return(object)
}

#' @export
#' @title Number of strata per cluster
#' @param object The `lcModel` object.
#' @examples
#' model <- latrend(lcMethodKML("Y"), latrendData)
#' clusterSizes(model)
clusterSizes = function(object) {
  assert_that(is.lcModel(object))
  clusterAssignments(object) %>%
    table() %>%
    as.numeric() %>%
    setNames(clusterNames(object))
}

#. clusterProportions ####
#' @export
#' @name clusterProportions
#' @rdname clusterProportions
#' @aliases clusterProportions,lcModel-method
#' @title Proportional size of each cluster
#' @param object The `lcModel` to obtain the proportions from.
#' @param ... Not used.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y"), latrendData)
#' clusterProportions(model)
setMethod('clusterProportions', signature('lcModel'), function(object, ...) {
  pp = postprob(object)
  assert_that(!is.null(pp), msg = 'cannot determine cluster assignments because postprob() returned NULL')
  assert_that(is.matrix(pp))
  colMeans(pp)
})

#. clusterAssignments ####
#' @export
#' @name clusterAssignments
#' @rdname clusterAssignments
#' @aliases clusterAssignments,lcModel-method
#' @title Get the cluster membership of each trajectory
#' @details While the default strategy is [which.max], it is recommended to use \link[nnet]{which.is.max} instead, as this function breaks ties randomly.
#' Another strategy to consider is the function [which.weight], which enables weighted sampling of cluster assignments.
#' @param object The `lcModel` to obtain the cluster assignments from.
#' @param strategy A function returning the cluster index based on the given vector of membership probabilities. By default, ids are assigned to the cluster with the highest probability.
#' @param ... Any additional arguments passed to the strategy function.
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y"), latrendData)
#' clusterAssignments(model)
#'
#' # only assign ids with a probability over 0.9
#' clusterAssignments(model, strategy = function(x) which(x > .9))
setMethod('clusterAssignments', signature('lcModel'), function(object, strategy = which.max, ...) {
  pp = postprob(object)
  assert_that(is_valid_postprob(pp, object))

  apply(pp, 1, strategy, ...) %>%
    factor(levels = 1:nClusters(object),
           labels = clusterNames(object))
})



#' @export
#' @importFrom stats coef
#' @title Coefficients of a lcModel
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @return A `named numeric vector` with all coefficients, or a `matrix` with each column containing the cluster-specific coefficients.
#' @family model-specific methods
coef.lcModel = function(object, ...) {
  if (is.null(getS3method(
    'coef',
    class = class(object@model),
    optional = TRUE
  ))) {
    numeric()
  } else {
    coef(object@model)
  }
}



#' @export
#' @importFrom caret confusionMatrix
#' @title Posterior confusion matrix
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @examples
#' model = latrend(method=lcMethodLcmmGMM(Y ~ Time + (1 | Id)), data=latrendData)
#' confusionMatrix(model)
confusionMatrix.lcModel = function(object, ...) {
  assert_that(is.lcModel(object))
  IMIFA::post_conf_mat(postprob(object)) %>%
    set_colnames(clusterNames(object)) %>%
    set_rownames(clusterNames(object))
}


# . converged ####
#' @export
#' @name converged
#' @rdname converged
#' @aliases converged,lcModel-method
#' @title Check model convergence
#' @description Check convergence of the fitted model.
#' @param object The `lcModel` to check for convergence.
#' @param ... Additional arguments.
#' @return Either `logical` indicating convergence, or a `numeric` status code.
#' @family model-specific methods
setMethod('converged', signature('lcModel'), function(object, ...) {
  TRUE
})


#' @export
#' @importFrom stats deviance
#' @title lcModel deviance
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @family model-specific methods
deviance.lcModel = function(object, ...) {
  if (is.null(getS3method('deviance', class = class(object@model), optional = TRUE))) {
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
#' @family model-specific methods
df.residual.lcModel = function(object, ...) {
  if (is.null(getS3method(
    'df.residual',
    class = class(object@model),
    optional = TRUE
  ))) {
    nobs(object) - attr(logLik(object), 'df')
  } else {
    df.residual(object@model)
  }
}


#. externalMetric ####
#' @export
#' @rdname externalMetric
#' @examples
#' data(latrendData)
#' model1 <- latrend(lcMethodKML("Y"), latrendData)
#' model2 <- latrend(lcMethodLcmmGMM(Y ~ Time + (1 | Id)), latrendData)
#' ari <- externalMetric(model1, model2, 'adjustedRand')
#' @return For `externalMetric(lcModel, lcModel)`: A `numeric` vector of the computed metrics.
#' @family metric functions
setMethod('externalMetric', signature('lcModel', 'lcModel'), function(object, object2, name, ...) {
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
#' @importFrom stats fitted
#' @title Extract lcModel fitted values
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @param clusters Optional cluster assignments per id. If unspecified, a `matrix` is returned containing the cluster-specific predictions per column.
#' @return A `numeric` vector of the fitted values for the respective class, or a `matrix` of fitted values for each cluster.
#' @family model-specific methods
fitted.lcModel = function(object, ..., clusters = clusterAssignments(object)) {
  pred = predict(object, newdata = NULL)
  transformFitted(pred = pred, model = object, clusters = clusters)
}


#' @export
#' @importFrom stats formula
#' @title Extract the formula of a lcModel
#' @param x The `lcModel` object.
#' @param what The distributional parameter
#' @param ... Additional arguments.
#' @return Returns the associated `formula`, or ` ~ 0` if not specified.
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
#' @param x The `lcModel` object.
#' @param ... Not used.
#' @keywords internal
getCall.lcModel = function(x, ...) {
  x@call
}

#. getLabel ####
#' @export
#' @rdname lcModel-class
#' @aliases getLabel,lcModel-method
setMethod('getLabel', signature('lcModel'), function(object, ...) {
  object@label
})


#' @export
#' @title Get the method specification of a lcModel
#' @param object The `lcModel` object.
#' @examples
#' model = latrend(method=lcMethodKML("Y"), data=latrendData)
#' getLcMethod(model)
getLcMethod = function(object) {
  assert_that(is.lcModel(object))
  object@method
}


# . getName ####
#' @export
#' @rdname lcModel-class
#' @aliases getName,lcModel-method
#' @description Extracts the name of the `lcModel` object.
#' The name is comprised of the underlying `lcMethod` name, and the assigned label (if any).
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
#' @rdname lcModel-class
#' @aliases getShortName,lcModel-method
setMethod('getShortName',
  signature('lcModel'), function(object) getLcMethod(object) %>% getShortName())


#' @noRd
#' @title Generate a vector indicating the id-number (between 1 and numIds()) per row
#' @details The id order is determined by the output of ids()
#' @param object The `lcModel` object.
#' @keywords internal
genIdRowIndices = function(object) {
  model.data(object)[[idVariable(object)]] %>%
    factor(levels = ids(object)) %>%
    as.integer()
}


# . ids ####
#' @export
#' @title Get the unique ids included in this model
#' @details The order returned by ids(lcModel) determines the id order for any output involving id-specific values, such as in clusterAssignments() or postprob()
#' @param object The `lcModel` object.
#' @examples
#' model = latrend(lcMethodKML("Y"), latrendData)
#' ids(model) # S1, S2, ..., S500
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
#' model <- latrend(lcMethodKML("Y"), latrendData)
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
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @family model-specific methods
logLik.lcModel = function(object, ...) {
  logLik(object@model)
}


#. metric ####
#' @export
#' @rdname metric
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(Y ~ Time + (1 | Id)), latrendData)
#' bic <- metric(model, "BIC")
#'
#' ic <- metric(model, c("AIC", "BIC"))
#' @family metric functions
setMethod('metric', signature('lcModel'), function(object, name = c('AIC', 'BIC', 'WRSS', 'APPA'), ...) {
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
#' @name lcModel-make
#' @rdname lcModel-make
#' @title Cluster-handling functions for lcModel implementations.
#' @description Ensures a proper cluster assignments factor vector
#' @param object The `lcModel` object.
#' @param clusters The unprocessed trajectory cluster assignment vector.
#' @param finite Whether to check for missing or non-finite values.
#' @return Factor cluster assignments.
#' @keywords internal
make.clusterAssignments = function(object, clusters, finite = TRUE) {
  clusNames = clusterNames(object)
  nClusters = nClusters(object)

  assert_that(!finite ||
                !anyNA(clusters), msg = 'cluster assignments should be finite values')

  if (is.null(clusters)) {
    NULL
  } else if (is.factor(clusters)) {
    # factor
    assert_that(nlevels(clusters) == nClusters)
    if (all(levels(clusters) == clusNames)) {
      clusters
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels = clusNames)
    }
  } else if (is.integer(clusters)) {
    # integer
    assert_that(min(clusters) >= 1,
                max(clusters) <= nClusters)

    factor(clusters,
           levels = seq_len(nClusters),
           labels = clusNames)
  } else if (is.numeric(clusters)) {
    # numeric
    assert_that(all(vapply(clusters, is.count, FUN.VALUE = FALSE)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1,
                max(clusters) <= nClusters)

    factor(clusters,
           levels = seq_len(nClusters),
           labels = clusNames)
  } else if (is.character(clusters)) {
    # character
    assert_that(uniqueN(clusters) == nClusters,
                all(clusters %in% clusNames))

    factor(clusters, levels = clusNames)
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}

#' @export
#' @rdname lcModel-make
#' @title Ensures a proper cluster index vector
#' @return A cluster assignments index vector of type `integer`.
make.clusterIndices = function(object, clusters, finite = TRUE) {
  clusNames = clusterNames(object)
  nClusters = nClusters(object)

  assert_that(!finite ||
                !anyNA(clusters), msg = 'cluster assignments should be finite values')

  if (is.null(clusters)) {
    NULL
  } else if (is.integer(clusters)) {
    # integer
    assert_that(min(clusters) >= 1,
                max(clusters) <= nClusters)
    clusters
  } else if (is.factor(clusters)) {
    # factor
    if (all(levels(clusters) == clusNames)) {
      as.integer(clusters)
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels = clusNames) %>%
        as.integer()
    }
  } else if (is.numeric(clusters)) {
    # numeric
    assert_that(all(vapply(clusters, is.count, FUN.VALUE = FALSE)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1,
                max(clusters) <= nClusters)
    clusters
  } else if (is.character(clusters)) {
    # character
    assert_that(all(clusters %in% clusNames))
    factor(clusters, levels = clusNames) %>%
      as.integer()
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}

#' @export
#' @rdname lcModel-make
#' @title Generate cluster names
#' @param n The number of clusters.
#' @return A `character` vector length `n` with the cluster names.
make.clusterNames = function(n) {
  assert_that(is.count(n))

  opt = getOption('latrend.clusterNames', LETTERS)

  if (length(opt) == 0) {
    warning('latrend.clusterNames is NULL or empty. Using LETTERS for names')
    clusNames = LETTERS
  } else if (is.function(opt)) {
    clusNames = opt(n) %>% as.character()
  } else {
    clusNames = as.character(opt)
  }

  assert_that(length(clusNames) > 0, anyDuplicated(clusNames) == 0)

  if (n > length(LETTERS)) {
    warning('not enough cluster names provided by latrend.clusterNames')
    clusNames = c(clusNames, paste0('C', seq(length(clusNames) + 1, n)))
  } else {
    clusNames[seq_len(n)]
  }
}


#' @export
#' @importFrom stats model.frame
#' @title Extract model training data
#' @param formula The `lcModel` object.
#' @param ... Additional arguments.
#' @family model-specific methods
model.frame.lcModel = function(formula, ...) {
  if (is.null(getS3method(
    'model.frame',
    class = class(formula@model),
    optional = TRUE
  ))) {
    labs = getLcMethod(formula) %>% stats::formula %>% terms %>% labels
    model.data(formula)[, labs]
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
#' @return The `data.frame` that was used for fitting the `lcModel`.
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

    modelData = transformLatrendData(
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
#' @title Number of strata
#' @param object The `lcModel` object.
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
#' @param object The `lcModel` object.
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
nobs.lcModel = function(object, ...) {
  nrow(model.data(object))
}


#' @export
#' @rdname predict.lcModel
#' @importFrom stats predict
#' @title lcModel predictions
#' @description Predicts the expected trajectory observations at the given time for each cluster, unless specified.
#' @details The default `predict.lcModel` implementation.
#' @param object The `lcModel` object.
#' @param newdata Optional data frame for which to compute the model predictions. If omitted, the model training data is used.
#' Cluster trajectory predictions are made when ids are not specified. If the clusters are specified under the Cluster column, output is given only for the specified cluster. Otherwise, a matrix is returned with predictions for all clusters.
#' @param what The distributional parameter to predict. By default, the mean response 'mu' is predicted. The cluster membership predictions can be obtained by specifying what='mb'.
#' @return If newdata specifies the cluster membership; a vector of cluster-specific predictions. Otherwise, a matrix of predictions is returned corresponding to each cluster.
#' @param ... Additional arguments.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(Y ~ Time + (1 | Id)), latrendData)
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
#' @family model-specific methods
predict.lcModel = function(object, ...,
                           newdata = NULL,
                           what = 'mu') {
  # special case for when no newdata is provided
  if (is.null(newdata)) {
    newdata = model.data(object)
    if (hasName(newdata, 'Cluster')) {
      newdata[['Cluster']] = NULL # allowing the Cluster column to remain would break the fitted() output.
    }
  }

  if (hasName(newdata, 'Cluster')) {
    # predictForCluster with newdata subsets
    clusdataList = as.data.table(newdata) %>%
      split(by = 'Cluster', sorted = TRUE) %>%
      lapply(function(cdata) cdata[, Cluster := NULL])
  }
  else {
    # predictForCluster with newdata for each cluster
    clusdataList = replicate(nClusters(object), newdata, simplify = FALSE)
  }

  predList = mapply(function(cname, cdata) {
    predictForCluster(object,
                      cluster = cname,
                      newdata = cdata,
                      what = what,
                      ...)
  }, clusterNames(object), clusdataList, SIMPLIFY = FALSE)

  assert_that(uniqueN(vapply(predList, class, FUN.VALUE = '')) == 1, msg =
                'output from predictForCluster() must be same class for all clusters. Check the model implementation.')

  if (is.data.frame(predList[[1]])) {
    pred = rbindlist(predList, idcol = 'Cluster')
  }
  else if (is.numeric(predList[[1]])) {
    clusDataRows = vapply(clusdataList, nrow, FUN.VALUE=0)
    clusPredRows = vapply(predList, length, FUN.VALUE=0)
    assert_that(all(clusDataRows == clusPredRows), msg='Numeric output length from predictForCluster() does not match the number of input newdata rows for one or more clusters')
    pred = data.table(Cluster = rep(seq_len(nClusters(object)), clusDataRows),
                      Fit = do.call(c, predList))
  }
  else {
    stop(
      'unsupported output from predictForCluster(): must be data.frame or numeric. Check the model implementation.'
    )
  }

  pred[, Cluster := factor(Cluster,
                           levels = seq_len(nClusters(object)),
                           labels = clusterNames(object))]

  transformPredict(pred = pred,
                   model = object,
                   newdata = newdata)
}


# . predictForCluster ####
#' @export
#' @name predictForCluster
#' @rdname predictForCluster
#' @aliases predictForCluster,lcModel-method
#' @title lcModel prediction for a specific cluster
#' @description Predicts the expected trajectory observations at the given time under the assumption that the trajectory belongs to the specified cluster.
#' @inheritParams predict.lcModel
#' @param cluster The cluster name (as `character`) to predict for.
#' @param ... Additional arguments.
#' @return A `vector` with the predictions per `newdata` observation, or a `data.frame` with the predictions and newdata alongside.
#' @seealso [predict.lcModel]
#' @family model-specific methods
setMethod('predictForCluster', signature('lcModel'), function(object, newdata = NULL, cluster, ..., what = 'mu') {
  assert_that(is.newdata(newdata), !is.null(newdata))
  warning(
    'predictForCluster() not implemented for ',
    class(object)[1],
    '. Returning NA predictions.'
  )

  rep(as.numeric(NA), nrow(newdata))
})



# . predictPostprob ####
#' @export
#' @name predictPostprob
#' @rdname predictPostprob
#' @aliases predictPostprob,lcModel-method
#' @title lcModel posterior probability prediction
#' @details The default implementation returns a uniform probability matrix.
#' @param object The `lcModel` to predict the posterior probabilities with.
#' @param newdata Optional data frame for which to compute the posterior probability. If omitted, the model training data is used.
#' @param ... Additional arguments.
#' @return A `matrix` indicating the posterior probability per trajectory per measurement on each row, for each cluster (the columns).
#' @family model-specific methods
setMethod('predictPostprob', signature('lcModel'), function(object, newdata, ...) {
  warning(
    'predictPostprob() not implemented for ',
    class(object)[1],
    '. Returning uniform probability matrix.'
  )

  if (is.null(newdata)) {
    N = nrow(model.data(object))
  }
  else {
    N = nrow(newdata)
  }

  pp = matrix(1 / nClusters(object),
              nrow = N,
              ncol = nClusters(object))
  colnames(pp) = clusterNames(object)
  pp
})


#. predictAssignments ####
#' @export
#' @name predictAssignments
#' @rdname predictAssignments
#' @aliases predictAssignments,lcModel-method
#' @title Predict the cluster assignments for new trajectories
#' @description Computes the posterior probability based on the provided (observed) data.
#' @inheritParams predict.lcModel
#' @param strategy A function returning the cluster index based on the given vector of membership probabilities. By default, ids are assigned to the cluster with the highest probability.
#' @details The default implementation uses [predictPostprob] to determine the cluster membership.
#' @return A `factor` with length `nrow(newdata)` that indicates the posterior probability per trajectory per observation.
#' @seealso [predictPostprob]
#' @family model-specific methods
setMethod('predictAssignments', signature('lcModel'), function(object, newdata, strategy = which.max, ...) {
  pp = predictPostprob(object, newdata = newdata)
  assert_that(is_valid_postprob(pp, object),
              nrow(pp) == nrow(newdata))

  apply(pp, 1, strategy, ...) %>%
    factor(levels = 1:nClusters(object),
           labels = clusterNames(object))
})


#. plot ####
#' @export
#' @title Plot a lcModel
#' @description Plot a lcModel. By default, the cluster trajectories of the model solution are plotted.
#' @param x The `lcModel` object.
#' @param y Not used.
#' @param ... Arguments passed to [plotClusterTrajectories].
#' @return A `ggplot` object.
setMethod('plot', signature('lcModel'), function(x, y, ...) {
  plotClusterTrajectories(x, ...)
})


#. plotTrajectories ####
#' @export
#' @name plotTrajectories
#' @rdname plotTrajectories
#' @aliases plotTrajectories,lcModel-method
#' @title Plot fitted trajectories of a lcModel
#' @param ... Arguments passed to [trajectories].
#' @inheritDotParams trajectories
setMethod('plotTrajectories', signature('lcModel'), function(object, ...) {
  data = trajectories(object, ...)
  .plotTrajs(
    data,
    response = responseVariable(object),
    time = timeVariable(object),
    id = idVariable(object),
    cluster = 'Cluster'
  )
})

#. plotClusterTrajectories
#' @export
#' @name plotClusterTrajectories
#' @rdname plotClusterTrajectories
#' @aliases plotClusterTrajectories,lcModel-method
#' @title Plot the cluster trajectories of a lcModel
#' @inheritParams clusterTrajectories
#' @inheritDotParams clusterTrajectories
#' @param clusterLabels Cluster display names. By default it's the cluster name with its proportion enclosed in parentheses.
#' @param ... Arguments passed to [clusterTrajectories].
#' @return A `ggplot` object.
setMethod('plotClusterTrajectories', signature('lcModel'),
  function(object,
    what = 'mu',
    at = time(object),
    clusterLabels = sprintf('%s (%g%%)',
      clusterNames(object),
      round(clusterProportions(object) * 100)),
    showTrajs = FALSE,
    ...
  ) {
  assert_that(length(clusterLabels) == nClusters(object))

  cdata = clusterTrajectories(object, at = at, what = what, ...) %>%
    as.data.table() %>%
    .[, Cluster := factor(Cluster, levels = levels(Cluster), labels = clusterLabels)]

  .plotClusterTrajs(cdata,
    response = responseVariable(object, what = what),
    time = timeVariable(object),
    id = idVariable(object),
    showTrajs = showTrajs,
    rawdata = model.data(object))
})


#. postprob ####
#' @export
#' @name postprob
#' @rdname postprob
#' @aliases postprob,lcModel-method
#' @title Posterior probability per fitted id
#' @param object The `lcModel`.
#' @param ... Additional arguments.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodLcmmGMM(Y ~ Time + (1 | Id)), data = latrendData)
#' postprob(model)
#' @family model-specific methods
setMethod('postprob', signature('lcModel'), function(object, ...) {
  warning('postprob() not implemented for ', class(object)[1],
    '. Returning uniform posterior probability matrix.')

  pp = matrix(1 / nClusters(object),
              nrow = nIds(object),
              ncol = nClusters(object))
  colnames(pp) = clusterNames(object)
  pp
})


# . QQ plot ####
#' @export
#' @rdname plotQQ
#' @title Quantile-quantile plot
#' @param object The model.
#' @param byCluster Whether to plot the Q-Q line per cluster
#' @param ... Other arguments passed to qqplotr::geom_qq_band, qqplotr::stat_qq_line, and qqplotr::stat_qq_point.
setMethod('plotQQ', signature('lcModel'), function(object, byCluster = FALSE, ...) {
  assert_that(is.lcModel(object))
  idIndexColumn = factor(model.data(object)[[idVariable(object)]], levels = ids(object)) %>% as.integer()
  rowClusters = clusterAssignments(object)[idIndexColumn]

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
#' @inheritParams fitted.lcModel
#' @return A vector of residuals for the cluster assignments specified by clusters. If clusters is unspecified, a matrix of cluster-specific residuals per observations is returned.
#' @family model-specific methods
residuals.lcModel = function(object, ..., clusters = clusterAssignments(object)) {
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
#' @aliases responseVariable,lcModel-method
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y"), latrendData)
#' responseVariable(model) # "Value"
#' @family lcModel variables
setMethod('responseVariable', signature('lcModel'), function(object, ...) object@response)

#' @export
#' @title Get the model estimation time
#' @param object The `lcModel` object.
#' @return The model estimation time in seconds.
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
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
#' @family model-specific methods
sigma.lcModel = function(object, ...) {
  if (is.null(getS3method('sigma', class = class(object@model), optional = TRUE))) {
    residuals(object) %>% sd
  } else {
    sigma(object@model)
  }
}

#. strip ####
#' @export
#' @name strip
#' @aliases strip,lcModel-method
#' @title Strip a lcModel for serialization
#' @description Removes associated environments from any of the slots.
#' @param object The `lcModel`.
#' @param ... Additional arguments.
#' @aliases strip,lcModel-method
setMethod('strip', signature('lcModel'), function(object, ...) {
  newObject = object

  environment(newObject) = NULL
  newObject@method = strip(object@method)

  # recursively strip elements (for calls in calls)
  rstrip = function(x) {
    if (is.list(x) || is(x, 'call')) { # is.call is TRUE for formulas
      replace(x, seq_along(x), lapply(x, rstrip))
    } else {
      environment(x) = NULL
      x
    }
  }

  newObject@call = rstrip(object@call)

  return(newObject)
})

#' @export
#' @title Summarize a lcModel
#' @description Extracts all relevant information from the underlying model into a list
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
summary.lcModel = function(object, ...) {
  res = residuals(object)
  if (is.null(res)) {
    res = as.numeric(NA)
  }

  new(
    'clSummary',
    method = getLcMethod(object),
    name = getName(object),
    nClusters = nClusters(object),
    nObs = nobs(object),
    id = idVariable(object),
    coefficients = coef(object),
    residuals = res,
    clusterNames = clusterNames(object),
    clusterAssignments = clusterAssignments(object),
    clusterSizes = clusterSizes(object),
    clusterProportions = clusterProportions(object)
  )
}


#. timeVariable ####
#' @export
#' @name timeVariable
#' @rdname timeVariable
#' @aliases timeVariable,lcModel-method
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y"), latrendData)
#' idVariable(model) # "Id"
#' @family lcModel variables
setMethod('timeVariable', signature('lcModel'), function(object) object@time)


# . trajectories ####
#' @export
#' @name trajectories
#' @rdname trajectories
#' @aliases trajectories,lcModel-method
#' @title Extract the fitted trajectories for all strata
#' @param object The model.
#' @param at The time points at which to compute the id-specific trajectories.
#' @param what The distributional parameter to compute the response for.
#' @param clusters The cluster assignments for the strata to base the trajectories on.
#' @param ... Additional arguments.
#' @examples
#' data(latrendData)
#' model <- latrend(method = lcMethodKML("Y"), data = latrendData)
#' trajectories(model)
#'
#' trajectories(model, at = c(0, .5, 1))
#' @family model-specific methods
setGeneric('trajectories', function(object,
                                    at = time(object),
                                    what = 'mu',
                                    clusters = clusterAssignments(object),
                                    ...) standardGeneric('trajectories'))

#' @rdname trajectories
setMethod('trajectories', signature('lcModel'), function(object, at, what, clusters, ...) {
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

  assert_that(is.data.frame(preds))
  assert_that(nrow(preds) == nrow(newdata), msg = 'invalid output from predict function of lcModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what = what)) := preds$Fit]
  return(newdata[])
})


#' @export
#' @importFrom stats time
#' @title Sampling times of a lcModel
#' @param x The `lcModel` object.
#' @param ... Not used.
#' @return The unique times at which observations occur.
#' @family model-specific methods
time.lcModel = function(x, ...) {
  model.data(x)[[timeVariable(x)]] %>% unique() %>% sort()
}


#' @export
#' @importFrom stats update
#' @title Update a lcModel
#' @description Fit a new model with modified arguments from the current model.
#' @param object The `lcModel` object.
#' @param ... Any new method arguments to refit the model with.
#' @inheritDotParams latrend
update.lcModel = function(object, ...) {
  assert_that(is.lcModel(object))
  modelCall = getCall(object)

  assert_that(as.character(modelCall[[1]]) != '<undef>', msg = 'cannot update lcModel because lcMethod call is undefined')

  updateCall = match.call() %>% tail(-2)
  updateNames = names(updateCall)

  clCall = replace(modelCall, updateNames, updateCall[updateNames])

  eval(clCall)
}


# Model summary ####
setClass(
  'clSummary',
  representation(
    method = 'lcMethod',
    name = 'character',
    nClusters = 'integer',
    nObs = 'numeric',
    id = 'character',
    coefficients = 'ANY',
    #TODO
    residuals = 'numeric',
    clusterNames = 'character',
    clusterAssignments = 'factor',
    clusterSizes = 'numeric',
    clusterProportions = 'numeric',
    metrics = 'numeric'
  )
)

# . show ####
setMethod('show', 'clSummary',
          function(object) {
            cat('Longitudinal cluster model using ', object@name, '\n', sep = '')
            print(object@method)
            cat('\n')
            sprintf('Cluster sizes (K=%d):\n', object@nClusters) %>% cat
            sprintf('%g (%g%%)',
                    object@clusterSizes,
                    round(object@clusterProportions * 100, 1)) %>%
              setNames(object@clusterNames) %>%
              noquote %>%
              print
            cat('\n')
            sprintf(
              'Number of obs: %d, strata (%s): %d\n',
              object@nObs,
              object@id,
              length(object@clusterAssignments)
            ) %>% cat
            cat('\n')
            cat('Scaled residuals:\n')
            object@residuals %>% scale %>% as.vector %>% summary %>% print
            cat('\n')
          })
