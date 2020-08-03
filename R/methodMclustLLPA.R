#' @include method.R
setClass('lcMethodMclustLLPA', contains = 'lcMethod')

setValidity('lcMethodMclustLLPA', function(object) {
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
#' @title Longitudinal latent profile analysis
#' @description Latent profile analysis or finite Gaussian mixture modeling.
#' @param formula Formula. Covariates are not supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @inheritDotParams mclust::Mclust
#' @examples
#' method = lcMethodMclustLLPA(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family lcMethod implementations
lcMethodMclustLLPA = function(response,
                              time = getOption('latrend.time'),
                              id = getOption('latrend.id'),
                              nClusters = 2,
                              ...) {
  .loadPackage('mclust')

  lcMethod.call(
    'lcMethodMclustLLPA',
    call = match.call.defaults(),
    defaults = mclust::Mclust,
    excludeArgs = c('data', 'G', 'verbose')
  )
}

setMethod('getName', signature('lcMethodMclustLLPA'), function(object) 'longitudinal latent profile analysis')

setMethod('getShortName', signature('lcMethodMclustLLPA'), function(object) 'llpa')

setMethod('prepareData', signature('lcMethodMclustLLPA'), function(method, data, verbose, ...) {
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

setMethod('compose', signature('lcMethodMclustLLPA'), function(method, envir = NULL) {
  evaluate.lcMethod(method, try = TRUE, envir = envir)
})

setMethod('fit', signature('lcMethodMclustLLPA'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = mclust::Mclust)
  args$data = envir$data
  args$G = method$nClusters

  model = do.call(mclust::Mclust, args)
  model$time = unique(data[[timeVariable(method)]]) %>% sort

  new(
    'lcModelMclustLLPA',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
