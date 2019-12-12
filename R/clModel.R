# Model ####
setClass('clModel',
         representation(model='ANY',
                        method='clMethod',
                        call='call',
                        control='list',
                        clusterNames='character'))

setMethod('show', 'clModel',
          function(object) {
            cat('clModel')
          })

#' @export
getCall.clModel = function(object) {
  object@call
}

#' @export
#' @title Plot a clModel
#' @param what Response to plot
plot.clModel = function(object, what='mu') {
  dt_grouptraj = groupTrajectories(object, what=what)
  ggplot(data=dt_grouptraj,
         aes=aes_string(x=getTimeName(object),
                        y=getResponseName(object, what=what),
                        color=Cluster)) +
    geom_line()
}

#' @export
#' @title Update a clModel
update.clModel = function(object, ...) {

}

#' @export
#' @title Summarize a clModel
summary.clModel = function(object, ...) {
  summary(object@model)
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
  coef(object@model)
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
  residuals(object@model)
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
    length(fitted(object))
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
#' @title Number of clusters
nClus = function(object) {
  length(object@clusterNames)
}

#' @export
#' @title Get the cluster names
clusterNames = function(object) {
  object@clusterNames
}

#' @export
#' @title Update the cluster names
`clusterNames<-` = function(object, value) {
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
#' @title Posterior probability per strata
setGeneric('pp', function(object, newdata=NULL) standardGeneric('pp'))

#' @export
#' @title Extract the group trajectories
#' @return A data.frame of the estimated values at the given times
setGeneric('groupTrajectories', function(object, what='mu', at=NULL) standardGeneric('groupTrajectories'))

#' @export
#' @title Extract the fitted trajectories for all strata
setGeneric('trajectories', function(object, what='mu', at=NULL) standardGeneric('trajectories'))

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
#' @title Extract the unique time points
setGeneric('modelTime', function(object) standardGeneric('modelTime'))
setMethod('modelTime', signature('clModel'), function(object) {
  data = modelData(object)
  sort(unique(data[[getMethod(object)$time]]))
})

# Model summary ####
setClass('clSummary',
         representation(call='call',
                        metrics='numeric'))

setMethod('show', 'clSummary',
          function(object) {
            cat('summary!')
          })


genClusNames = function(n) {
  LETTERS[seq_len(n)]
}