#' @include plot.R
# Model ####
setClass('clModel',
         representation(model='ANY',
                        method='clMethod',
                        call='call',
                        data='ANY',
                        id='character',
                        time='character',
                        response='character',
                        ids='vector',
                        clusterNames='character'))

# NOTE: do not specify a validity method for clModel!
# Referencing the clModel object within setValidity breaks clModelCustom intialization resulting in unspecified slots for some mysterious reason.

# . initialize ####
setMethod('initialize', 'clModel', function(.Object, ...) {
  .Object = callNextMethod()

  method = .Object@method
  if(length(.Object@id) == 0) {
    assert_that(has_name(method, 'id'))
    .Object@id = method$id
  }
  if(length(.Object@time) == 0) {
    assert_that(has_name(method, 'time'))
    .Object@time = method$time
  }
  if(length(.Object@response) == 0) {
    assert_that(has_name(method, 'formula'))
    .Object@response = method$formula %>% getResponse
  }

  assert_that(has_name(.Object@data, c(.Object@id, .Object@time, .Object@response)))
  assert_that(length(.Object@data) > 0, msg='invalid data object for new clModel. Did you forget to specify the data slot?')
  .Object@ids = ids(.Object)
  .Object
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
#' model = cluslong(method=clMethodGMM(), data=testLongData)
#' clusterTrajectories(model)
#'
#' clusterTrajectories(model, at=c(0, .5, 1))
setGeneric('clusterTrajectories', function(object, what='mu', at=time(object), ...) standardGeneric('clusterTrajectories'))
setMethod('clusterTrajectories', signature('clModel'), function(object, what, at, ...) {
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

  predMat = predict(object, newdata=newdata, what=what, ...)

  assert_that(is.vector(predMat), msg='invalid output from predict function of clModel; expected vector')
  assert_that(length(predMat) == nrow(newdata), msg='invalid output from predict function of clModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what=what)) := as.numeric(predMat)]
  return(newdata[])
})


#' @export
#' @title Get the cluster names
#' @param factor Whether to return the cluster names as a factor.
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterNames(model) # A, B
clusterNames = function(object, factor=FALSE) {
  assert_that(is(object, 'clModel'))
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
  assert_that(is(object, 'clModel'))
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
  assert_that(is(object, 'clModel'))
  clusterAssignments(object) %>% table %>% as.numeric %>% setNames(clusterNames(object))
}

#' @export
#' @title Proportional size of each cluster
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' clusterProportions(model)
clusterProportions = function(object) {
  assert_that(is(object, 'clModel'))
  postprob(object) %>% colMeans
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
  assert_that(is(object, 'clModel'))
  postprob(object) %>%
    apply(1, strategy, ...) %>%
    factor(labels=clusterNames(object))
}



#' @export
#' @title Coefficients of a clModel
#' @return A matrix of the coefficients per class.
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
#' model = cluslong(method=clMethodGMM(), data=testLongData)
#' confusionMatrix(model)
confusionMatrix = function(object) {
  assert_that(is(object, 'clModel'))
  post_conf_mat(postprob(object)) %>%
    set_colnames(clusterNames(object)) %>%
    set_rownames(clusterNames(object))
}


# . converged ####
#' @export
#' @title Whether the model converged
setGeneric('converged', function(object, ...) standardGeneric('converged'))



#' @export
#' @title clModel deviance
deviance.clModel = function(object, ...) {
  if (is.null(getS3method('deviance', class=class(object@model), optional=TRUE))) {
    warning('deviance is not implemented for the given model')
    as.numeric(NA)
  } else {
    deviance(object@model)
  }
}


#' @export
#' @title Extract the residual degrees of freedom from a clModel
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
fitted.clModel = function(object, clusters=clusterAssignments(object)) {
  if (is.null(getS3method('fitted', class=class(object@model), optional=TRUE))) {
    trajectories(object)[[responseVariable(object)]]
  } else {
    warning('clusters argument is ignored in direct call to fitted(object@model)')
    fitted(object@model)
  }
}


#' @export
#' @title Extract the formula of a clModel
#' @param what The distributional parameter
formula.clModel = function(object, what='mu') {
  getMethod(object) %>% formula(what=what)
}


#' @export
getCall.clModel = function(object) {
  object@call
}


#' @export
#' @title Extract the underlying model
getModel = function(object) {
  assert_that(is(object, 'clModel'))
  object@model
}


#' @export
#' @title Get the method specification of a clModel
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' getMethod(model)
getMethod = function(object) {
  assert_that(is(object, 'clModel'))
  object@method
}


# . getName ####
setMethod('getName', signature('clModel'), function(object) getMethod(object) %>% getName)

# . getName0 ####
setMethod('getName0', signature('clModel'), function(object) getMethod(object) %>% getName0)


#' @title Generate a vector indicating the id-number (between 1 and numIds()) per row
#' @details The id order is determined by the output of ids()
#' @keywords internal
genIdRowIndices = function(object) {
  model.data(object)[[idVariable(object)]] %>%
    factor(levels=ids(object)) %>%
    as.integer
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
      levels(iddata)
    } else {
      unique(iddata) %>% sort
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
logLik.clModel = function(object, ...) {
  logLik(object@model)
}


#' @export
#' @title Compute internal model metric(s)
#' @description Internal metric.
#' @param name The name(s) of the metric(s) to compute. Computes all metrics if NULL.
#' @examples
#' data(testLongData)
#' model = cluslong(...)
#' bic = metric(model, 'BIC')
#'
#' ic = metric(model, c('AIC', 'BIC'))
#' @family metric functions
metric = function(object, name) {
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
}


#' @export
#' @title Compute external comparison metric(s) with another clModel.
#' @inheritParams metric
#' @param object2 The other clModel to compare with.
#' @examples
#' data(testLongData)
#' model1 = cluslong(...)
#' model2 = cluslong(...)
#' bic = externalMetric(model1, model2, 'Rand')
#' @family metric functions
externalMetric = function(object, object2, name) {
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
}


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
    assert_that(all(sapply(clusters, is.count)))
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
    assert_that(nlevels(clusters) == nClusters)
    if(all(levels(clusters) == clusNames)) {
      as.integer(clusters)
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels=clusNames) %>%
        as.integer
    }
  } else if(is.numeric(clusters)) {
    # numeric
    assert_that(all(sapply(clusters, is.count)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1)
    assert_that(max(clusters) <= nClusters)

    clusters
  } else if(is.character(clusters)) {
    # character
    assert_that(uniqueN(clusters) == nClusters)
    assert_that(all(clusters %in% clusNames))

    factor(clusters, levels=clusNames) %>%
      as.integer
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}


make.clusterNames = function(n) {
  assert_that(is.count(n))
  if(n > length(LETTERS)) {
    paste0('C', seq_len(n))
  } else {
    LETTERS[seq_len(n)]
  }
}


#' @export
#' @title Extract model training data
model.frame.clModel = function(object) {
  if (is.null(getS3method('model.frame', class=class(object@model), optional=TRUE))) {
    labs = getMethod(object) %>% formula %>% terms %>% labels
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
#' @description Evaluates the data call in the environment that the model was trained from.
#' @return The resulting dataset that was used for fitting, as a data.frame.
model.data.clModel = function(object) {
  if(length(object@data) > 0) {
    object@data
  } else {
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
  assert_that(is(object, 'clModel'))
  length(object@clusterNames)
}


#' @export
#' @title Extract the number of observations from a clModel
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
#' model = cluslong(clMethodGMM(), testLongData)
#' predFitted = predict(model) # same result as fitted(model)
#'
#' predCluster = predict(model, newdata=data.frame(Cluster='A', Time=time(model))) # Cluster trajectory of cluster A
#'
#' predId = predict(model, newdata=data.frame(Cluster='A', Id='S1', Time=time(model))) # Prediction for id S1 given cluster A membership
#'
#' predIdAll = predict(model, newdata=data.frame(Id='S1', Time=time(model))) # Prediction matrix for id S1 for all clusters
predict.clModel = function(object, newdata=NULL, what='mu', ...) {
  stop('not implemented')
}

# . predictPostprob ####
#' @export
#' @title clModel posterior probability prediction
#' @param newdata Optional data frame for which to compute the posterior probability. If omitted, the model training data is used.
setGeneric('predictPostprob', function(object, newdata=NULL, ...) standardGeneric('predictPostprob'))
setMethod('predictPostprob', signature('clModel'), function(object, newdata, ...) {
  stop('not implemented')
})



#' @export
#' @title Plot a clModel
#' @inheritParams clusterTrajectories
#' @param clusterLabels Cluster display names. By default it's the cluster name with its proportion enclosed in parentheses.
plot.clModel = function(object, what='mu', at=time(object),
                        clusterLabels=sprintf('%s (%g%%)', clusterNames(object), round(clusterProportions(object) * 100))) {
  dt_ctraj = clusterTrajectories(object, what=what, at=at) %>%
    as.data.table %>%
    .[, Cluster := factor(Cluster, levels=levels(Cluster), labels=clusterLabels)]
  ggplot(data=dt_ctraj,
         mapping=aes_string(x=timeVariable(object),
                            y=responseVariable(object, what=what),
                            color='Cluster',
                            shape='Cluster')) +
    theme(legend.position='top') +
    geom_line(size=1) +
    geom_point(size=2) +
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
#' model = cluslong(clMethodGMM(), data=testLongData)
#' postprob(model)
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
  assert_that(is(object, 'clModel'))
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


# . show ####
setMethod('show', 'clModel', function(object) {
  summary(object) %>% show
})


#' @export
#' @title Extract residual standard deviation from a clModel
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
  new('clSummary',
      call=getCall(object),
      name=getName(object),
      nClusters=nClusters(object),
      nObs=nobs(object),
      formula=formula(object),
      id=idVariable(object),
      coefficients=coef(object),
      residuals=residuals(object),
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
#' @param what The distributional parameter to compute the response for.
#' @param at The time points at which to compute the id-specific trajectories.
#' @param clusters The cluster assignments for the strata to base the trajectories on.
#' @examples
#' model = cluslong(method=clMethodKML(), data=testLongData)
#' trajectories(model)
#'
#' trajectories(model, at=c(0, .5, 1))
setGeneric('trajectories', function(object, what='mu', at=time(object), clusters=clusterAssignments(object), ...) standardGeneric('trajectories'))
setMethod('trajectories', signature('clModel'), function(object, what, at, clusters) {
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

  assert_that(is.vector(preds), msg='invalid output from predict function of clModel; expected vector')
  assert_that(length(preds) == nrow(newdata), msg='invalid output from predict function of clModel; expected a prediction per newdata row')
  newdata[, c(responseVariable(object, what=what)) := preds]
  return(newdata[])
})


#' @title Helper function for ensuring the right fitted() output
#' @details Includes additional checks
#' @keywords internal
transformFitted = function(object, predMat, clusters) {
  assert_that(is.matrix(predMat))
  assert_that(ncol(predMat) == nClusters(object))
  assert_that(nrow(predMat) == nobs(object))

  if(is.null(clusters)) {
    predMat
  } else {
    clusters = make.clusterIndices(object, clusters)
    rowClusters = clusters[genIdRowIndices(object)]
    rowColumns(predMat, rowClusters)
  }
}

#' @title Helper function for ensuring the right fitted() output
#' @details Includes additional checks
#' @keywords internal
transformPredict = function(object, predMat, newdata) {
  assert_that(is.matrix(predMat))
  assert_that(ncol(predMat) == nClusters(object))
  assert_that(nrow(predMat) == nrow(newdata))

  if(has_name(newdata, 'Cluster')) {
    rowClusters = make.clusterIndices(object, newdata$Cluster)
    rowColumns(predMat, rowClusters)
  } else {
    predMat
  }
}


#' @export
#' @title Sampling times of a clModel
#' @return The unique times at which observations occur.
time.clModel = function(object) {
  model.data(object)[[getMethod(object)$time]] %>% unique %>% sort
}


#' @export
#' @title Update a clModel
#' @description Fit a new model with modified arguments from the current model.
#' @inheritDotParams cluslong
update.clModel = function(object, ...) {
  updateCall = match.call() %>% tail(-2)
  updateNames = names(updateCall)

  clCall = getCall(object) %>%
    replace(updateNames, updateCall[updateNames])

  eval(clCall)
}


# Model summary ####
setClass('clSummary',
         representation(call='call',
                        name='character',
                        nClusters='integer',
                        nObs='numeric',
                        id='character',
                        formula='formula',
                        coefficients='numeric',
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
            cat('Formula: ')
            print(object@formula)
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
