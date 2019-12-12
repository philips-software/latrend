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
plot.clModel = function(object) {

}

#' @export
#' @title Update a clModel
update.clModel = function(object, ...) {

}

#' @export
#' @title Summarize a clModel
summary.clModel = function(object, ...) {

}

#' @export
#' @title Coefficients of a clModel
coef.clModel = function(object, ...) {

}

#' @export
#' @title Extract clModel fitted values
fitted.values.clModel = function(object, ...) {

}

#' @export
#' @title Extract clModel residuals
residuals.clModel = function(object, ...) {

}

#' @export
#' @title clModel deviance
deviance.clModel = function(object, ...) {

}

#' @export
#' @title Extract the number of observations from a clModel
nobs.clModel = function(object, ...) {

}

#' @export
#' @title Extract the log-likelihood of a clModel
logLik.clModel = function(object, ...) {
  attr(ll, 'nall') <- N0
  attr(ll, 'nobs') <- N
  attr(ll, 'df') <- p + 1
  class(ll) = 'logLik'
  ll
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
#' @title Get the cluster membership for each strata
clusterAssignments = function(object) {
  object@clusterAssignments
}

#' @export
#' @title Posterior probability per strata
pp = function(object) {

}

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

# Model summary ####
setClass('clSummary',
         representation(call='call',
                        metrics='numeric'))

setMethod('show', 'clSummary',
          function(object) {
            cat('summary!')
          })