#' @include method.R

#' @name interface-mclust
#' @rdname interface-mclust
#' @title mclust interface
#' @seealso [lcMethodMclustLLPA] \link[mclust]{mclust-package}
#' @keywords internal
NULL

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
#' @inheritParams lcMethodKML
#' @param ... Arguments passed to [mclust::Mclust].
#' The following external arguments are ignored: data, G, verbose.
#' @examples
#' library(mclust)
#' data(latrendData)
#' method <- lcMethodMclustLLPA("Y", id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method, latrendData)
#' @family lcMethod implementations
#' @references
#' \insertRef{scrucca2016mclust}{latrend}
lcMethodMclustLLPA = function(response,
                              time = getOption('latrend.time'),
                              id = getOption('latrend.id'),
                              nClusters = 2,
                              ...) {
  .loadOptionalPackage('mclust')

  lcMethod.call(
    'lcMethodMclustLLPA',
    call = match.call.defaults(),
    defaults = mclust::Mclust,
    excludeArgs = c('data', 'G', 'verbose')
  )
}

#' @rdname interface-mclust
#' @inheritParams getName
setMethod('getName', signature('lcMethodMclustLLPA'), function(object) 'longitudinal latent profile analysis')

#' @rdname interface-mclust
setMethod('getShortName', signature('lcMethodMclustLLPA'), function(object) 'llpa')

#' @rdname interface-mclust
setMethod('prepareData', signature('lcMethodMclustLLPA'), function(method, data, verbose, ...) {
  e = new.env()

  valueColumn = responseVariable(method)
  assert_that(noNA(data[[valueColumn]]), msg = 'data contains missing values')

  # Data
  wideFrame = as.data.table(data) %>%
    data.table::dcast(get(idVariable(method)) ~ get(timeVariable(method)), value.var = valueColumn)
  e$data = as.matrix(wideFrame[, -'method']) %>%
    set_rownames(wideFrame$method)

  return(e)
})

#' @rdname interface-mclust
setMethod('compose', signature('lcMethodMclustLLPA'), function(method, envir = NULL) {
  evaluate.lcMethod(method, try = TRUE, envir = envir)
})

#' @rdname interface-mclust
#' @inheritParams fit
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
