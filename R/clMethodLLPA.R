#' @include clMethod.R
setClass('clMethodLLPA', contains='clMethod')

setValidity('clMethodLLPA', function(object) {
  if(isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))
    assert_that(!hasCovariates(object$formula), msg='covariates are not supported')
  }

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.count(object$nClusters))
  }
})

#' @export
#' @importFrom mclust Mclust mclustBIC emControl mclust.options
#' @title Longitudinal latent profile analysis
#' @description Latent profile analysis or finite Gaussian mixture modeling.
#' @param formula Formula. Covariates are not supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @inheritDotParams mclust::Mclust
#' @examples
#' method = clMethodLLPA(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod classes
clMethodLLPA = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       ...
) {
  clMethod('clMethodLLPA', call=match.call.defaults(),
           defaults=mclust::Mclust,
           excludeArgs=c('data', 'G', 'verbose'))
}

setMethod('getName', signature('clMethodLLPA'), function(object) 'longitudinal latent profile analysis')

setMethod('getName0', signature('clMethodLLPA'), function(object) 'llpa')

setMethod('prepare', signature('clMethodLLPA'), function(method, data, verbose, ...) {
  e = new.env()

  valueColumn = formula(method) %>% getResponse
  assert_that(!anyNA(data[[valueColumn]]), msg='data contains missing values')

  # Data
  wideFrame = dcast(data, get(method$id) ~ get(method$time), value.var=valueColumn)
  e$data = as.matrix(wideFrame[, -'method']) %>%
    set_rownames(wideFrame$method)

  return(e)
})

setMethod('fit', signature('clMethodLLPA'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  args = as.list(method)
  args$data = envir$data
  args$G = method$nClusters

  model = do.call(Mclust, args)
  model$time = unique(data[[method$time]]) %>% sort
  e$model = model
  return(e)
})

setMethod('finalize', signature('clMethodLLPA'), function(method, data, envir, verbose, ...) {
  model = new('clModelLLPA',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
