# Model ####
setClass('clModel',
         representation(model='ANY',
                        method='clMethod',
                        call='call',
                        control='list',
                        clusterNames='character'))

setValidity('clModel', function(object) {
  assert_that(is(object@method, 'clMethod'))
  assert_that(!is.null(object@model))
  assert_that(is.character(object@clusterNames))
})

setMethod('initialize', 'clModel', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

setMethod('show', 'clModel',
          function(object) {
            summary(object) %>% show
          })

setMethod('getName', signature('clModel'), function(object) getMethod(object) %>% getName)

setMethod('getName0', signature('clModel'), function(object) getMethod(object) %>% getName0)


#' @export
getCall.clModel = function(object) {
  object@call
}

#' @export
#' @title Plot a clModel
#' @inheritParams clusterTrajectories
#' @param clusterLabels Cluster display names. By default it's the cluster name with its proportion enclosed in parentheses.
plot.clModel = function(object, what='mu', at=NULL,
                        clusterLabels=sprintf('%s (%g%%)', clusterNames(object), round(clusterProportions(object) * 100))) {
  dt_ctraj = clusterTrajectories(object, what=what, at=at) %>%
    .[, Cluster := factor(Cluster, levels=levels(Cluster), labels=clusterLabels)]
  ggplot(data=dt_ctraj,
         mapping=aes_string(x=getTimeName(object),
                        y=getResponseName(object, what=what),
                        color='Cluster',
                        shape='Cluster')) +
    theme(legend.position='top') +
    geom_line(size=1) +
    geom_point(size=2) +
    labs(title='Cluster trajectories')
}

#' @export
#' @title Update a clModel
update.clModel = function(object, ...) {

}

#' @export
#' @title Summarize a clModel
#' @description Extracts all relevant information from the underlying model into a list
summary.clModel = function(object, ...) {
  new('clSummary',
      call=getCall(object),
      name=getName(object),
      nClus=nClus(object),
      nObs=nobs(object),
      formula=formula(object),
      id=getIdName(object),
      coefficients=coef(object),
      residuals=residuals(object),
      clusterNames=clusterNames(object),
      clusterAssignments=clusterAssignments(object),
      clusterSizes=clusterSizes(object),
      clusterProportions=clusterProportions(object))
}

#' @export
#' @title Extract model training data
model.frame.clModel = function(object) {
  if (is.null(getS3method('model.frame', class=class(object@model), optional=TRUE))) {
    labs = getMethod(object) %>% formula %>% terms %>% labels
    modelData(object)[, labs]
  } else {
    model.frame(object@model)
  }
}

#' @export
#' @title Extract the formula of a clModel
#' @param what The distributional parameter
formula.clModel = function(object, what='mu') {
  getMethod(object) %>% formula(what=what)
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
#' @title Extract clModel fitted values
#' @param all Whether to return a matrix with the fitted value of each class
#' @return a vector of the fitted values for the respective class, or a matrix of fitted values for each class.
fitted.clModel = function(object, all=FALSE) {
  fitted(object@model)
}

#' @export
#' @title Extract clModel residuals
residuals.clModel = function(object, ...) {
  if (is.null(getS3method('residuals', class=class(object@model), optional=TRUE))) {
    modelResponses(object) - fitted(object)
  } else {
    residuals(object@model)
  }
}

#' @export
#' @title clModel deviance
deviance.clModel = function(object, ...) {
  deviance(object@model)
}

#' @export
#' @title Extract the number of observations from a clModel
nobs.clModel = function(object, ...) {
  # check if nobs is defined for the model
  if (is.null(getS3method('nobs', class=class(object@model), optional=TRUE))) {
    length(modelResponses(object))
  } else {
    nobs(object@model)
  }
}

#' @export
#' @title Extract the residual degrees of freedom from a clModel
df.residual.clModel = function(object, ...) {
  df.residual(object@model)
}


#' @export
#' @title Extract the log-likelihood of a clModel
logLik.clModel = function(object, ...) {
  logLik(object@model)
}

# Extra methods ####
#' @export
#' @title Number of strata
nIds = function(object) {
  modelIds(object) %>% length
}

#' @export
#' @title Number of clusters
nClus = function(object) {
  assert_that(is(object, 'clModel'))
  length(object@clusterNames)
}

#' @export
#' @title Get the cluster names
clusterNames = function(object) {
  assert_that(is(object, 'clModel'))
  object@clusterNames
}

#' @export
#' @title Update the cluster names
`clusterNames<-` = function(object, value) {
  assert_that(is(object, 'clModel'))
  assert_that(is.character(value))
  assert_that(length(value) == nClus(object))
  object@clusterNames = value
  return(object)
}

#' @export
#' @title Number of strata per cluster
clusterSizes = function(object) {
  assert_that(is(object, 'clModel'))
  clusterAssignments(object) %>% table %>% as.numeric %>% setNames(clusterNames(object))
}

#' @export
#' @title Proportional size of each cluster
clusterProportions = function(object) {
  assert_that(is(object, 'clModel'))
  pp(object) %>% colMeans
}

#' @export
#' @importFrom IMIFA post_conf_mat
#' @title Posterior confusion matrix
confusionMatrix = function(object) {
  assert_that(is(object, 'clModel'))
  post_conf_mat(pp(object)) %>%
    set_colnames(clusterNames(object)) %>%
    set_rownames(clusterNames(object))
}

#' @export
#' @title Get the cluster membership for each strata
clusterAssignments = function(object) {
  assert_that(is(object, 'clModel'))
  pp(object) %>% apply(1, which.max) %>% factor(labels=clusterNames(object))
}

#' @export
#' @title Extract the cluster trajectories
#' @return A data.frame of the estimated values at the given times
setGeneric('clusterTrajectories', function(object, what='mu', at=NULL, ...) standardGeneric('clusterTrajectories'))

#' @export
#' @title Posterior probability per strata
setGeneric('pp', function(object, newdata=NULL, ...) standardGeneric('pp'))


#' @export
#' @title Extract the fitted trajectories for all strata
setGeneric('trajectories', function(object, what='mu', at=NULL, ...) standardGeneric('trajectories'))

#' @export
#' @title Extract a model criterion
#' @param what The criterion or criteria names to extract
#' @examples
#' data(testLongData)
#' model = cluslong(...)
#' bic = criterion(model, 'BIC')
#'
#' ic = criterion(model, c('AIC', 'BIC'))
criterion = function(object, what) {

}

#' @export
#' @title Get the available criteria for this clModel
criterionNames = function(object) {

}

#' @export
#' @title Get the method specification of a clModel
getMethod = function(object) {
  assert_that(is(object, 'clModel'))
  object@method
}

#' @export
getResponseName = function(object, what='mu') {
  assert_that(is(object, 'clModel'))
  if(what == 'mu') {
    formula(object) %>% getResponse
  } else {
    formula(object) %>% getResponse %>% paste(what, sep='.')
  }
}

#' @export
getIdName = function(object) {
  assert_that(is(object, 'clModel'))
  getMethod(object)$id
}

#' @export
getTimeName = function(object) {
  assert_that(is(object, 'clModel'))
  getMethod(object)$time
}

#' @export
#' @title Extract model training data
setGeneric('modelData', function(object) standardGeneric('modelData'))

#' @export
#' @title  Extract model response
#' @details Model response that was used for training
setGeneric('modelResponses', function(object) standardGeneric('modelResponses'))
setMethod('modelResponses', signature('clModel'), function(object) {
  data = modelData(object)
  data[[getResponseName(object)]]
})

#' @export
setGeneric('modelIds', function(object) standardGeneric('modelIds'))
setMethod('modelIds', signature('clModel'), function(object) {
  data = modelData(object)
  data[[getIdName(object)]] %>% unique
})

#' @export
#' @title Extract the unique time points
setGeneric('modelTimes', function(object) standardGeneric('modelTimes'))
setMethod('modelTimes', signature('clModel'), function(object) {
  data = modelData(object)
  sort(unique(data[[getMethod(object)$time]]))
})

# Model summary ####
setClass('clSummary',
         representation(call='call',
                        name='character',
                        nClus='integer',
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

setMethod('show', 'clSummary',
          function(object) {
            cat('Longitudinal cluster model using ', object@name, '\n', sep='')
            cat('Formula: ')
            print(object@formula)
            cat('\n')
            sprintf('Cluster sizes (K=%d):\n', object@nClus) %>% cat
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


genClusNames = function(n) {
  LETTERS[seq_len(n)]
}