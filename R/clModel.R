#' @include clMethod.R plot.R
# Model ####
#' @export
#' @name clModel-class
#' @title clModel class
#' @description Abstract class for defining estimated longitudinal cluster models.
#' @details An extending class must implement the following methods to ensure basic functionality:
#' * predict.clModelExt: Used to obtain the fitted cluster trajectories and trajectories.
#' * postprob(clModelExt): The posterior probability matrix is used to determine the cluster assignments of the trajectories.
#'
#' @slot method The \link{clMethod-class} object specifying the arguments under which the model was fitted.
#' @slot call The `call` that was used to create this `clModel` object. Typically, this is the call to `cluslong()` or any of the other fitting functions.
#' @slot model An arbitrary underlying model representation.
#' @slot id The name of the trajectory identifier column.
#' @slot time The name of the time variable.
#' @slot response The name of the response variable.
#' @slot ids The possible trajectory identifier values the model was fitted on.
#' @slot clusterNames The names of the clusters.
#' @slot estimationTime The time, in seconds, that it took to fit the model.
#' @family model-specific methods
setClass('clModel',
         representation(model='ANY',
                        method='clMethod',
                        call='call',
                        data='ANY',
                        id='character',
                        time='character',
                        response='character',
                        ids='vector',
                        clusterNames='character',
                        estimationTime='numeric'))

# . initialize ####
setMethod('initialize', 'clModel', function(.Object, ...) {
  .Object = callNextMethod(.Object, ...)
  method = .Object@method

  assert_that(length(.Object@id) > 0 || has_name(method, 'id'), msg='@id not specified, nor defined in clMethod')
  if(length(.Object@id) == 0) {
    .Object@id = method$id
  }
  assert_that(length(.Object@time) > 0 || has_name(method, 'time'), msg='@time not specified, nor defined in clMethod')
  if(length(.Object@time) == 0) {
    .Object@time = method$time
  }
  assert_that(length(.Object@response) > 0 || has_name(method, 'response') || has_name(method, 'formula'), msg='@response not specified, nor defined in clMethod$response or clMethod$formula')
  if(length(.Object@response) == 0) {
    if(has_name(method, 'response')) {
      .Object@response = method$response
    } else {
      .Object@response = method$formula %>% getResponse
    }
  }
  .Object
})


setValidity('clModel', function(object) {
  return(TRUE)

  if(as.character(object@call[[1]]) == "<undef>") {
    # nothing to validate as clModel is incomplete (yet)
    return(TRUE)
  }

  assert_that(nchar(object@id) > 0,
              nchar(object@time) > 0,
              nchar(object@response) > 0)

  data = model.data(object)
  assert_that(!is.null(data), msg='invalid data object for new clModel. Either specify the data slot or ensure that the model call contains a data argument which correctly evaluates.')
  assert_that(has_name(data, c(object@id, object@time, object@response)))
  return(TRUE)
})


# . clusterTrajectories ####
#' @export
#' @rdname clusterTrajectories
#' @title Extract the cluster trajectories
#' @description Extracts a data frame of all cluster trajectories.
#' @inheritParams predict.clModel
#' @param at An optional vector, list or data frame of covariates at which to compute the cluster trajectory predictions.
#' If a vector is specified, this is assumed to be the time covariate. Otherwise, a named list or data frame must be provided.
#' @return A data.frame of the estimated values at the given times
#' @examples
#' model = cluslong(method=clMethodLcmmGMM(), data=testLongData)
#' clusterTrajectories(model)
#'
#' clusterTrajectories(model, at=c(0, .5, 1))
#' @family model-specific methods
setGeneric('clusterTrajectories', function(object, at=time(object), what='mu', ...) standardGeneric('clusterTrajectories'))
setMethod('clusterTrajectories', signature('clModel'), function(object, at, what, ...) {
  if(is.numeric(at)) {
    newdata = data.table(Cluster=rep(clusterNames(object, factor=TRUE), each=length(at)), Time=at) %>%
      setnames('Time', timeVariable(object))
  } else if(is.list(at)) {
    at = as.data.table(at)
    idx = seq_len(nrow(at)) %>% rep(nClusters(object))
    newdata = data.table(Cluster=rep(clusterNames(object, factor=TRUE), each=nrow(at)), at[idx,])
  } else {
    stop('unsupported input')
  }

  dfPred = predict(object, newdata=newdata, what=what, ...)
  assert_that(is.data.frame(dfPred), msg='invalid output from predict()')
  assert_that(nrow(dfPred) == nrow(newdata), msg='invalid output from predict function of clModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what=what)) := dfPred$Fit]
  return(newdata[])
})


#' @export
#' @title Get the cluster names
#' @param factor Whether to return the cluster names as a factor.
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterNames(model) # A, B
clusterNames = function(object, factor=FALSE) {
  assert_that(is.clModel(object))
  if(factor[1]) {
    object@clusterNames %>% factor(levels=object@clusterNames)
  } else {
    object@clusterNames
  }
}

#' @export
#' @title Update the cluster names
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterNames(model) = c('Group 1', 'Group 2')
`clusterNames<-` = function(object, value) {
  assert_that(is.clModel(object))
  assert_that(is.character(value))
  assert_that(length(value) == nClusters(object))
  object@clusterNames = value
  return(object)
}

#' @export
#' @title Number of strata per cluster
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterSizes(model)
clusterSizes = function(object) {
  assert_that(is.clModel(object))
  clusterAssignments(object) %>% table %>% as.numeric %>% setNames(clusterNames(object))
}

#' @export
#' @title Proportional size of each cluster
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterProportions(model)
clusterProportions = function(object) {
  assert_that(is.clModel(object))
  pp = postprob(object)
  assert_that(!is.null(pp), msg='cannot determine cluster assignments because postprob() returned NULL')
  assert_that(is.matrix(pp))
  colMeans(pp)
}


#' @export
#' @title Get the cluster membership for each strata
#' @param strategy A function returning the cluster index based on the given vector of membership probabilities. By default, ids are assigned to the cluster with the highest probability.
#' @param ... Any additional arguments passed to the strategy function.
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterAssignments(model)
#'
#' clusterAssignments(model, strategy=function(x) which(x > .9)) # only assign ids with a probability over 0.9
clusterAssignments = function(object, strategy=which.max, ...) {
  assert_that(is.clModel(object))
  pp = postprob(object)
  assert_that(!is.null(pp), msg='cannot determine cluster assignments because postprob() returned NULL')
  assert_that(is.matrix(pp))

  apply(pp, 1, strategy, ...) %>%
    factor(levels=1:nClusters(object), labels=clusterNames(object))
}



#' @export
#' @title Coefficients of a clModel
#' @return A `named numeric vector` with all coefficients, or a `matrix` with each column containing the cluster-specific coefficients.
#' @family model-specific methods
coef.clModel = function(object, ...) {
  if (is.null(getS3method('coef', class=class(object@model), optional=TRUE))) {
    numeric()
  } else {
    coef(object@model)
  }
}



#' @export
#' @importFrom IMIFA post_conf_mat
#' @title Posterior confusion matrix
#' @examples
#' model = cluslong(method=clMethodLcmmGMM(), data=testLongData)
#' confusionMatrix(model)
confusionMatrix = function(object) {
  assert_that(is.clModel(object))
  post_conf_mat(postprob(object)) %>%
    set_colnames(clusterNames(object)) %>%
    set_rownames(clusterNames(object))
}


# . converged ####
#' @export
#' @title Whether the model converged
#' @family model-specific methods
setGeneric('converged', function(object, ...) standardGeneric('converged'))
setMethod('converged', signature('clModel'), function(object) {
  TRUE
})


#' @export
#' @title clModel deviance
#' @family model-specific methods
deviance.clModel = function(object, ...) {
  if (is.null(getS3method('deviance', class=class(object@model), optional=TRUE))) {
    as.numeric(NA)
  } else {
    deviance(object@model)
  }
}


#' @export
#' @title Extract the residual degrees of freedom from a clModel
#' @family model-specific methods
df.residual.clModel = function(object, ...) {
  if (is.null(getS3method('df.residual', class=class(object@model), optional=TRUE))) {
    nobs(object) - attr(logLik(object), 'df')
  } else {
    df.residual(object@model)
  }
}


#' @export
#' @title Extract clModel fitted values
#' @param clusters Optional cluster assignments per id. If unspecified, a matrix is returned containing the cluster-specific predictions per column.
#' @return A vector of the fitted values for the respective class, or a matrix of fitted values for each cluster.
#' @family model-specific methods
fitted.clModel = function(object, clusters=clusterAssignments(object)) {
  predict(object, newdata=NULL) %>%
    transformFitted(object, ., clusters=clusters)
}


#' @export
#' @title Extract the formula of a clModel
#' @param what The distributional parameter
#' @return Returns the associated `formula`, or ` ~ 0` if not specified.
formula.clModel = function(object, what='mu') {
  method = getClMethod(object)
  if(what == 'mu') {
    if(has_name(method, 'formula')) {
      method$formula
    } else {
      as.formula(paste(object@response, '~ 0'))
    }
  } else {
    formulaName = paste('formula', what, sep='.')
    if(has_name(method, formulaName)) {
      formula(method, what=what)
    } else {
      ~ 0
    }
  }
}


#' @export
getCall.clModel = function(object) {
  object@call
}


#' @export
#' @title Extract the underlying model
getModel = function(object) {
  assert_that(is.clModel(object))
  object@model
}


#' @export
#' @title Get the method specification of a clModel
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' getClMethod(model)
getClMethod = function(object) {
  assert_that(is.clModel(object))
  object@method
}


# . getName ####
setMethod('getName', signature('clModel'), function(object) getClMethod(object) %>% getName)

# . getShortName ####
setMethod('getShortName', signature('clModel'), function(object) getClMethod(object) %>% getShortName)


#' @title Generate a vector indicating the id-number (between 1 and numIds()) per row
#' @details The id order is determined by the output of ids()
#' @keywords internal
genIdRowIndices = function(object) {
  model.data(object)[[idVariable(object)]] %>%
    factor(levels=ids(object)) %>%
    as.integer()
}


# . ids ####
#' @export
#' @title Get the unique ids included in this model
#' @details The order returned by ids(clModel) determines the id order for any output involving id-specific values, such as in clusterAssignments() or postprob()
#' @examples
#' model = cluslong(clMethodKML(), testLongData)
#' ids(model) # S1, S2, ..., S500
ids = function(object) {
  if(length(object@ids) == 0) {
    iddata = model.data(object)[[idVariable(object)]]
    if(is.factor(iddata)) {
      levels(iddata)[levels(iddata) %in% iddata]
    } else {
      unique(iddata) %>% sort()
    }
  } else {
    object@ids
  }
}


#' @export
#' @title Get the id variable name
#' @examples
#' model = cluslong(clMethodKML(), testLongData)
#' idVariable(model) # "Id"
#' @family clModel variables
idVariable = function(object) {
  object@id
}


#' @export
is.clModel = function(object) {
  isS4(object) && is(object, 'clModel')
}


#' @export
#' @title Extract the log-likelihood of a clModel
#' @family model-specific methods
logLik.clModel = function(object, ...) {
  logLik(object@model)
}


#' @export
#' @rdname metric
#' @title Compute internal model metric(s)
#' @description Internal metric.
#' @param object The `clModel`, `clModels`, or `list` of `clModel` objects to compute the metrics for.
#' @param name The name(s) of the metric(s) to compute. All defined metrics are computed by default.
#' @examples
#' data(testLongData)
#' model = cluslong(clMethodLcmmGMM(), testLongData)
#' bic = metric(model, 'BIC')
#'
#' ic = metric(model, c('AIC', 'BIC'))
#' @family metric functions
setGeneric('metric', function(object, name=getInternalMetricNames(), ...) standardGeneric('metric'))

#' @export
#' @rdname metric
#' @return A named `numeric` vector containing the computed model metrics.
#' @examples
#' clModel metric example here
setMethod('metric', signature('clModel'), function(object, name) {
  assert_that(is.clModel(object))
  assert_that(is.character(name))

  funMask = name %in% getInternalMetricNames()
  metricFuns = lapply(name[funMask], getInternalMetricDefinition)
  metricValues = mapply(function(fun, name) {
    value = fun(object)
    assert_that(is.scalar(value) && (is.numeric(value) || is.logical(value)),
                msg=sprintf('invalid output for metric "%s"; expected scalar number or logical value', name))
    return(value)
  }, metricFuns, name[funMask])

  allMetrics = rep(NA*0, length(name))
  allMetrics[funMask] = unlist(metricValues)
  names(allMetrics) = name
  return(allMetrics)
})


#' @export
#' @title Compute external comparison metric(s) based on a reference clModel.
#' @inheritParams metric
#' @param object2 The other clModel to compare with.
#' @examples
#' data(testLongData)
#' model1 = cluslong(clMethodKML(), testLongData)
#' model2 = cluslong(clMethodLcmmGMM(), testLongData)
#' bic = externalMetric(model1, model2, 'Rand')
#' @family metric functions
setGeneric('externalMetric', function(object, object2, name=getExternalMetricNames(), ...) standardGeneric('externalMetric'))

#' @export
#' @rdname metric
#' @return A named `numeric` vector containing the computed model metrics.
#' @examples
#' clModel metric example here
setMethod('externalMetric', signature('clModel', 'clModel'), function(object, object2, name) {
  assert_that(is.clModel(object))
  assert_that(is.clModel(object2))
  assert_that(is.character(name))

  funMask = name %in% getExternalMetricNames()
  metricFuns = lapply(name[funMask], getExternalMetricDefinition)
  metricValues = mapply(function(fun, name) {
    value = fun(object, object2)
    assert_that(is.scalar(value) && (is.numeric(value) || is.logical(value)),
                msg=sprintf('invalid output for metric "%s"; expected scalar number or logical value', name))
    return(value)
  }, metricFuns, name[funMask])

  allMetrics = rep(NA*0, length(name))
  allMetrics[funMask] = unlist(metricValues)
  names(allMetrics) = name
  return(allMetrics)
})


#' @title Ensures a proper cluster assignments factor vector
#' @param finite Whether to check for missing or non-finite values.
#' @return Factor cluster assignments.
#' @keywords internal
make.clusterAssignments = function(object, clusters, finite=TRUE) {
  clusNames = clusterNames(object)
  nClusters = nClusters(object)

  assert_that(!finite || !anyNA(clusters), msg='cluster assignments should be finite values')

  if(is.null(clusters)) {
    NULL
  } else if(is.factor(clusters)) {
    # factor
    assert_that(nlevels(clusters) == nClusters)
    if(all(levels(clusters) == clusNames)) {
      clusters
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels=clusNames)
    }
  } else if(is.integer(clusters)) {
    # integer
    assert_that(min(clusters) >= 1)
    assert_that(max(clusters) <= nClusters)

    factor(clusters, levels=seq_len(nClusters), labels=clusNames)
  } else if(is.numeric(clusters)) {
    # numeric
    assert_that(all(vapply(clusters, is.count, FUN.VALUE=FALSE)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1)
    assert_that(max(clusters) <= nClusters)

    factor(clusters, levels=seq_len(nClusters), labels=clusNames)
  } else if(is.character(clusters)) {
    # character
    assert_that(uniqueN(clusters) == nClusters)
    assert_that(all(clusters %in% clusNames))

    factor(clusters, levels=clusNames)
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}


#' @title Ensure a proper cluster index vector
#' @inheritParams make.clusterAssignments
#' @seealso make.clusterAssignments
#' @keywords internal
make.clusterIndices = function(object, clusters, finite=TRUE) {
  clusNames = clusterNames(object)
  nClusters = nClusters(object)

  assert_that(!finite || !anyNA(clusters), msg='cluster assignments should be finite values')

  if(is.null(clusters)) {
    NULL
  } else if(is.integer(clusters)) {
    # integer
    assert_that(min(clusters) >= 1)
    assert_that(max(clusters) <= nClusters)
    clusters
  } else if(is.factor(clusters)) {
    # factor
    if(all(levels(clusters) == clusNames)) {
      as.integer(clusters)
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels=clusNames) %>%
        as.integer
    }
  } else if(is.numeric(clusters)) {
    # numeric
    assert_that(all(vapply(clusters, is.count, FUN.VALUE=FALSE)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1)
    assert_that(max(clusters) <= nClusters)
    clusters
  } else if(is.character(clusters)) {
    # character
    assert_that(all(clusters %in% clusNames))
    factor(clusters, levels=clusNames) %>%
      as.integer
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}

#' @export
make.clusterNames = function(n) {
  assert_that(is.count(n))

  opt = getOption('cluslong.clusterNames', LETTERS)

  if(length(opt) == 0) {
    warning('cluslong.clusterNames is NULL or empty. Using LETTERS for names')
    clusNames = LETTERS
  } else if(is.function(opt)) {
    clusNames = opt(n) %>% as.character()
  } else {
    clusNames = as.character(opt)
  }

  assert_that(length(clusNames) > 0, anyDuplicated(clusNames) == 0)

  if(n > length(LETTERS)) {
    warning('not enough cluster names provided by cluslong.clusterNames')
    clusNames = c(clusNames, paste0('C', seq(length(clusNames) + 1, n)))
  } else {
    clusNames[seq_len(n)]
  }
}


#' @export
#' @title Extract model training data
#' @family model-specific methods
model.frame.clModel = function(object) {
  if (is.null(getS3method('model.frame', class=class(object@model), optional=TRUE))) {
    labs = getClMethod(object) %>% formula %>% terms %>% labels
    model.data(object)[, labs]
  } else {
    model.frame(object@model)
  }
}


#' @export
model.data = function(object) {
  UseMethod('model.data')
}

#' @export
#' @title Extract the model data that was used for fitting
#' @description Evaluates the data call in the environment that the model was trained in.
#' @return The `data.frame` that was used for fitting the `clModel`.
model.data.clModel = function(object) {
  if(!is.null(object@data)) {
    object@data
    assert_that(is.data.frame(object@data), msg='expected data reference to be a data.frame')
    return(object@data)
  } else {
    assert_that(has_name(getCall(object), 'data'), msg='Cannot determine data used to train this clModel. Data not part of model call, and not assigned to the @data slot')
    data = eval(getCall(object)$data, envir=environment(object))
    assert_that(!is.null(data), msg=sprintf('could not find "%s" in the model environment', deparse(data)))
    assert_that(is.data.frame(data), msg='expected data reference to be a data.frame')
    return(data)
  }
}


# . model.response ####
#' @title  Extract model response data
#' @details Model response that was used for training
#' @keywords internal
model.response = function(object) {
  model.data(object)[[responseVariable(object)]]
}


#' @export
#' @title Number of strata
nIds = function(object) {
  iddata = model.data(object)[[idVariable(object)]]
  if(is.factor(iddata)) {
    nlevels(iddata)
  } else {
    uniqueN(iddata)
  }
}

#' @export
#' @title Number of clusters
nClusters = function(object) {
  assert_that(is.clModel(object))
  length(object@clusterNames)
}


#' @export
#' @title Extract the number of observations from a clModel
#' @family model-specific methods
nobs.clModel = function(object, ...) {
  length(model.response(object))
}


#' @export
#' @rdname predict.clModel
#' @title clModel prediction
#' @param newdata Optional data frame for which to compute the model predictions. If omitted, the model training data is used.
#' Cluster trajectory predictions are made when ids are not specified. If the clusters are specified under the Cluster column, output is given only for the specified cluster. Otherwise, a matrix is returned with predictions for all clusters.
#' @param what The distributional parameter to predict. By default, the mean response 'mu' is predicted. The cluster membership predictions can be obtained by specifying what='mb'.
#' @return If newdata specifies the cluster membership; a vector of cluster-specific predictions. Otherwise, a matrix of predictions is returned corresponding to each cluster.
#' @examples
#' model = cluslong(clMethodLcmmGMM(), testLongData)
#' predFitted = predict(model) # same result as fitted(model)
#'
#' predCluster = predict(model, newdata=data.frame(Cluster='A', Time=time(model))) # Cluster trajectory of cluster A
#'
#' predId = predict(model, newdata=data.frame(Cluster='A', Id='S1', Time=time(model))) # Prediction for id S1 given cluster A membership
#'
#' predIdAll = predict(model, newdata=data.frame(Id='S1', Time=time(model))) # Prediction matrix for id S1 for all clusters
#' @family model-specific methods
predict.clModel = function(object, newdata=NULL, what='mu', ...) {
  stop('not implemented')
}

# . predictPostprob ####
#' @export
#' @title clModel posterior probability prediction
#' @param newdata Optional data frame for which to compute the posterior probability. If omitted, the model training data is used.
#' @family model-specific methods
setGeneric('predictPostprob', function(object, newdata=NULL, ...) standardGeneric('predictPostprob'))
setMethod('predictPostprob', signature('clModel'), function(object, newdata, ...) {
  stop('not implemented')
})



#' @export
#' @title Plot a clModel
#' @inheritParams clusterTrajectories
#' @param points The indices of the `at` argument at which to draw points, or the number of points to draw. By default, 10 evenly spread points are drawn.
#' @param clusterLabels Cluster display names. By default it's the cluster name with its proportion enclosed in parentheses.
#' @param ... Any other arguments passed to [clusterTrajectories].
#' @return A `ggplot` object.
plot.clModel = function(object, what='mu', at=time(object),
                        points=10,
                        clusterLabels=sprintf('%s (%g%%)', clusterNames(object), round(clusterProportions(object) * 100)),
                        ...) {
  assert_that(is.numeric(points), min(points) >= 1)
  assert_that(length(clusterLabels) == nClusters(object))

  if(length(points) == 1) {
    points = seq(1, length(at), length.out=max(length(at), points))
  }

  dt_ctraj = clusterTrajectories(object, at=at, what=what) %>%
    as.data.table %>%
    .[, Cluster := factor(Cluster, levels=levels(Cluster), labels=clusterLabels)]

    ggplot(data=dt_ctraj,
         mapping=aes_string(x=timeVariable(object),
                            y=responseVariable(object, what=what),
                            color='Cluster',
                            shape='Cluster')) +
    scale_shape_manual(values=seq_len(nClusters(object))) +
    geom_line() +
    geom_point(data=dt_ctraj[get(timeVariable(object)) %in% at[points]]) +
    labs(title='Cluster trajectories')
}

#' @export
#' @rdname plotTrajectories
#' @title Plot fitted trajectories of a clModel
#' @inheritDotParams trajectories
setMethod('plotTrajectories', signature('clModel'), function(object, ...) {
  data = trajectories(object, ...)
  plotTrajs(data,
            response=responseVariable(object),
            time=timeVariable(object),
            id=idVariable(object),
            cluster='Cluster')
})


#' @export
#' @title Posterior probability per fitted id
#' @examples
#' model = cluslong(clMethodLcmmGMM(), data=testLongData)
#' postprob(model)
#' @family model-specific methods
setGeneric('postprob', function(object, ...) standardGeneric('postprob'))
setMethod('postprob', signature('clModel'), function(object) {
  predictPostprob(object, newdata=NULL)
})


# . QQ plot ####
#' @export
#' @title Quantile-quantile plot
#' @inheritParams qqplotr::geom_qq_band
#' @param byCluster Whether to plot the Q-Q line per cluster
#' @param detrend Whether to detrend the Q-Q line.
#' @param ... Other arguments passed to qqplotr::geom_qq_band, qqplotr::stat_qq_line, and qqplotr::stat_qq_point.
setGeneric('plotQQ', function(object, byCluster=FALSE, ...) standardGeneric('plotQQ'))
setMethod('plotQQ', signature('clModel'), function(object, byCluster, ...) {
  assert_that(is.clModel(object))
  rowClusters = clusterAssignments(object)[model.data(object)[[idVariable(object)]]]

  p = ggplot(data=data.frame(Cluster=rowClusters, res=residuals(object)), aes(sample=res)) +
    qqplotr::geom_qq_band(...) +
    qqplotr::stat_qq_line(...) +
    qqplotr::stat_qq_point(...) +
    labs(x='Theoretical quantiles', y='Sample quantiles', title='Quantile-quantile plot')

  if (byCluster) {
    p = p + facet_wrap(~Cluster)
  }

  return(p)
})


#' @export
#' @title Extract clModel residuals
#' @inheritParams fitted.clModel
#' @return A vector of residuals for the cluster assignments specified by clusters. If clusters is unspecified, a matrix of cluster-specific residuals per observations is returned.
#' @family model-specific methods
residuals.clModel = function(object, clusters=clusterAssignments(object), ...) {
  ypred = fitted(object, clusters=clusters, ...)
  yref = model.response(object)

  if(is.matrix(ypred)) {
    assert_that(length(yref) == nrow(ypred))
    resMat = matrix(yref, nrow=nrow(ypred), ncol=ncol(ypred)) - ypred
    colnames(resMat) = colnames(ypred)
    resMat
  } else if(is.numeric(ypred)) {
    assert_that(length(yref) == length(ypred))
    yref - ypred
  } else {
    return(NULL)
  }
}


#' @export
#' @title Get the response variable name
#' @inheritParams formula.clModel
#' @examples
#' model = cluslong(clMethodKML(), testLongData)
#' responseVariable(model) # "Value"
#' @family clModel variables
responseVariable = function(object, what='mu') {
  if(what == 'mu') {
    object@response
  } else {
    paste(object@response, what, sep='.')
  }
}


#' @export
#' @title Get the model estimation time
#' @return The model estimation time in seconds.
estimationTime = function(object) {
  object@estimationTime
}


# . show ####
setMethod('show', 'clModel', function(object) {
  summary(object) %>% show
})


#' @export
#' @title Extract residual standard deviation from a clModel
#' @family model-specific methods
sigma.clModel = function(object, ...) {
  if (is.null(getS3method('sigma', class=class(object@model), optional=TRUE))) {
    residuals(object) %>% sd
  } else {
    sigma(object@model)
  }
}


#' @export
#' @title Summarize a clModel
#' @description Extracts all relevant information from the underlying model into a list
summary.clModel = function(object, ...) {
  res = residuals(object)
  if(is.null(res)) {
    res = as.numeric(NA)
  }

  new('clSummary',
      method=getClMethod(object),
      name=getName(object),
      nClusters=nClusters(object),
      nObs=nobs(object),
      id=idVariable(object),
      coefficients=coef(object),
      residuals=res,
      clusterNames=clusterNames(object),
      clusterAssignments=clusterAssignments(object),
      clusterSizes=clusterSizes(object),
      clusterProportions=clusterProportions(object))
}


#' @export
#' @title Get the time variable name
#' @family clModel variables
timeVariable = function(object) {
  object@time
}


# . trajectories ####
#' @export
#' @rdname trajectories
#' @title Extract the fitted trajectories for all strata
#' @param at The time points at which to compute the id-specific trajectories.
#' @param what The distributional parameter to compute the response for.
#' @param clusters The cluster assignments for the strata to base the trajectories on.
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' trajectories(model)
#'
#' trajectories(model, at=c(0, .5, 1))
#' @family model-specific methods
setGeneric('trajectories', function(object, at=time(object), what='mu', clusters=clusterAssignments(object), ...) standardGeneric('trajectories'))
setMethod('trajectories', signature('clModel'), function(object, at, what, clusters) {
  ids = ids(object)
  assert_that(length(clusters) == nIds(object))

  if(is.numeric(at)) {
    newdata = data.table(Id=rep(ids, each=length(at)),
                         Cluster=rep(clusters, each=length(at)),
                         Time=at) %>%
      setnames('Id', idVariable(object)) %>%
      setnames('Time', timeVariable(object))
  } else if(is.list(at)) {
    assert_that(has_name(at, timeVariable(object)), msg='Named list at must contain the time covariate')
    assert_that(!has_name(at, c(idVariable(object), 'Cluster')))

    at = as.data.table(at)
    idx = seq_len(nrow(at)) %>% rep(length(ids))
    newdata = data.table(Id=rep(ids, each=nrow(at)),
                         Cluster=rep(clusters, each=nrow(at)),
                         at[idx,]) %>%
      setnames('Id', idVariable(object))
  } else {
    stop('unsupported input')
  }

  preds = predict(object, newdata=newdata, what=what)

  assert_that(is.data.frame(preds))
  assert_that(nrow(preds) == nrow(newdata), msg='invalid output from predict function of clModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what=what)) := preds$Fit]
  return(newdata[])
})

#' @export
#' @title Helper function for ensuring the right fitted() output
#' @details Includes additional checks
#' @return A vector or matrix
#' @keywords internal
transformFitted = function(object, pred, clusters) {
  if(is.null(pred)) {
    return(NULL)
  }
  if(!is.data.frame(pred) && is.list(pred)) {
    # convert to matrix
    assert_that(length(pred) == nClusters(object))
    pred = lapply(pred, '[[', 'Fit') %>%
      do.call(cbind, .)
  } else if(is.data.frame(pred)) {
    browser()
    assert_that(has_name(pred, c('Fit', 'Cluster')))
    pred = matrix(pred$Fit, ncol=nClusters(object))
  }

  assert_that(is.matrix(pred), ncol(pred) == nClusters(object), nrow(pred) == nobs(object))
  colnames(pred) = clusterNames(object)

  if(is.null(clusters)) {
    pred
  } else {
    clusters = make.clusterIndices(object, clusters)
    rowClusters = clusters[genIdRowIndices(object)]
    rowColumns(pred, rowClusters)
  }
}

#' @export
#' @title Helper function that matches the output to the specified newdata
#' @description If Cluster is not provided, the prediction is outputted in long format per cluster,
#' resulting in a longer data.frame than the newdata input
#' @details Includes additional checks
#' @return A data.frame with the predictions, or a list of cluster-specific prediction frames
#' @keywords internal
transformPredict = function(object, pred, newdata) {
  if(is.null(pred)) {
    assert_that(nrow(newdata) == 0)
    browser()
    if(hasName(newdata, 'Cluster')) {
      data.frame(Cluster=factor(levels=seq_len(nClusters(object)),
                                labels=clusterNames(object)), Fit=numeric(0))
    } else {
      data.table(Fit=numeric(0))
    }
  }
  else if(is.vector(pred)) {
    assert_that(is.null(newdata) || length(pred) == nrow(newdata))
    data.frame(Fit=pred)
  }
  else if(is.matrix(pred)) {
    # format where multiple cluster-specific predictions are given per newdata entry (per row)
    assert_that(is.matrix(pred))
    assert_that(ncol(pred) == nClusters(object))
    assert_that(is.null(newdata) || nrow(pred) == nrow(newdata))

    if(hasName(newdata, 'Cluster')) {
      rowClusters = make.clusterIndices(object, newdata$Cluster)
      data.frame(Fit=rowColumns(pred, rowClusters))
    } else {
      data.frame(Fit=as.vector(pred)) %>%
        split(clusterNames(object, factor=TRUE) %>% rep(each=nrow(pred)))
    }
  }
  else if(is.data.frame(pred)) {
    assert_that(!is.null(newdata))
    # generic form, possibly containing more predictions than newdata. These are filtered
    # if the pred object contains the newdata variables. Else, newdata is replicated.
    pred = as.data.table(pred)
    newdata = as.data.table(newdata)

    if(nrow(newdata) == 0) {
      return(as.data.table(pred)[0,])
    }

    assert_that(hasName(pred, 'Fit'), nrow(pred) > 0)

    mergevars = intersect(names(pred), names(newdata))
    predvars = setdiff(names(pred), names(newdata))

    if(length(mergevars) == 0) {
      if(nrow(pred) == nrow(newdata)) {
        # newdata may have Cluster column, but we assume results are correct since rows match
        if(hasName(newdata, 'Cluster')) {
          newpred = cbind(pred, Cluster=newdata$Cluster)
        } else {
          newpred = pred
        }
      }
      else if(hasName(pred, 'Cluster')) {
        assert_that(nrow(pred) == nrow(newdata) * uniqueN(pred$Cluster), msg='cannot merge pred and newdata (no shared columns), and nrow(pred) is not a multiple of nrow(newdata)')
        newpred = pred
      }
      else {
        stop('non-matching rows for pred and newdata, and no shared columns to merge on')
      }
    }
    else if(length(mergevars) == 1 && mergevars == 'Cluster') {
      # can only merge on cluster. order cannot be validated
      # number of observations per cluster must match the number of predictions per cluster
      obsCounts = pred[, .N, keyby=Cluster] %>%
        .[newdata[, .N, keyby=Cluster]]
      if(!all(obsCounts[is.finite(N) & is.finite(i.N), N == i.N]))
        browser()
      assert_that(all(obsCounts[is.finite(N) & is.finite(i.N), N == i.N]), msg='number of observations per cluster must match the number of predictions per cluster')

      newpred = pred[Cluster %in% unique(newdata$Cluster)]
    }
    else {
      # attempt to merge pred and newdata to ensure correct filtering of predictions
      newpred = merge(newdata, pred, by=mergevars, sort=FALSE, allow.cartesian=TRUE) %>%
        subset(select=predvars)
    }


    # only split when newdata does not specify Cluster
    if(hasName(newdata, 'Cluster') || !hasName(newpred, 'Cluster')) {
      newpred
    } else {
      split(newpred, newpred$Cluster)
    }

  }
  else {
    stop('unsupported input for pred')
  }
}


#' @export
#' @title Sampling times of a clModel
#' @return The unique times at which observations occur.
#' @family model-specific methods
time.clModel = function(object) {
  model.data(object)[[timeVariable(object)]] %>% unique %>% sort
}


#' @export
#' @title Update a clModel
#' @description Fit a new model with modified arguments from the current model.
#' @inheritDotParams cluslong
update.clModel = function(object, ...) {
  assert_that(is.clModel(object))
  modelCall = getCall(object)

  assert_that(as.character(modelCall@call[[1]]) != '<undef>', msg='cannot update clModel because clMethod call is undefined')

  updateCall = match.call() %>% tail(-2)
  updateNames = names(updateCall)

  clCall = replace(modelCall, updateNames, updateCall[updateNames])

  eval(clCall)
}


# Model summary ####
setClass('clSummary',
         representation(method='clMethod',
                        name='character',
                        nClusters='integer',
                        nObs='numeric',
                        id='character',
                        coefficients='ANY', #TODO
                        residuals='numeric',
                        clusterNames='character',
                        clusterAssignments='factor',
                        clusterSizes='numeric',
                        clusterProportions='numeric',
                        metrics='numeric'))

# . show ####
setMethod('show', 'clSummary',
          function(object) {
            cat('Longitudinal cluster model using ', object@name, '\n', sep='')
            print(object@method)
            cat('\n')
            sprintf('Cluster sizes (K=%d):\n', object@nClusters) %>% cat
            sprintf('%g (%g%%)', object@clusterSizes, round(object@clusterProportions * 100, 1)) %>%
              setNames(object@clusterNames) %>%
              noquote %>%
              print
            cat('\n')
            sprintf('Number of obs: %d, strata (%s): %d\n',
                        object@nObs, object@id, length(object@clusterAssignments)) %>% cat
            cat('\n')
            cat('Scaled residuals:\n')
            object@residuals %>% scale %>% as.vector %>% summary %>% print
            cat('\n')
          })
