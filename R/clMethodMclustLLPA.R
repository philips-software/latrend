#' @include clMethod.R
setClass('clMethodMclustLLPA', contains = 'clMethod')

setValidity('clMethodMclustLLPA', function(object) {
  if (isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))
    assert_that(!hasCovariates(object$formula), msg = 'covariates are not supported')
  }

  if (isArgDefined(object, 'nClusters')) {
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
#' method = clMethodMclustLLPA(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod implementations
clMethodMclustLLPA = function(response = getOption('cluslong.response'),
                              time = getOption('cluslong.time'),
                              id = getOption('cluslong.id'),
                              nClusters = 2,
                              ...) {
  clMethod.call(
    'clMethodMclustLLPA',
    call = match.call.defaults(),
    defaults = mclust::Mclust,
    excludeArgs = c('data', 'G', 'verbose')
  )
}

setMethod('getName', signature('clMethodMclustLLPA'), function(object) 'longitudinal latent profile analysis')

setMethod('getShortName', signature('clMethodMclustLLPA'), function(object) 'llpa')

setMethod('prepareData', signature('clMethodMclustLLPA'), function(method, data, verbose, ...) {
  e = new.env()

  valueColumn = responseVariable(method)
  assert_that(!anyNA(data[[valueColumn]]), msg = 'data contains missing values')

  # Data
  wideFrame = dcast(data, get(idVariable(method)) ~ get(timeVariable(method)), value.var =
                      valueColumn)
  e$data = as.matrix(wideFrame[, -'method']) %>%
    set_rownames(wideFrame$method)

  return(e)
})

setMethod('fit', signature('clMethodMclustLLPA'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = mclust::Mclust)
  args$data = envir$data
  args$G = method$nClusters

  model = do.call(mclust::Mclust, args)
  model$time = unique(data[[timeVariable(method)]]) %>% sort

  new(
    'clModelMclustLLPA',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
