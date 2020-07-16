#' @include method.R
setClass('lcMethodCustom', contains = 'lcMethod')

#' @export
#' @title Specify a custom method based on a model function
#' @param fun The cluster `function` with signature `(method, data)`.
#' @param center Optional `function` for computing the longitudinal cluster centers, with signature `(x)`.
#' @param response The response variable name. Only a single response is supported.
#' @param time The time variable name.
#' @param id The trajectory identification variable name.
#' @examples
#' # Stratification based on the mean response level
#' clusfun = function(method, data) {
#'    clusters = data[, mean(Value) > 2, by=Id] %>%
#'        factor(levels=c(F,T), labels=c('Low', 'High'))
#'    list(clusters=clusters)
#'    lcModelCustom(clusters=clusters)
#' }
#' method = lcMethodCustom(fun=clusfun)
#' model = latrend(method, testLongData)
#' summary(model)
#' @family lcMethod implementations
lcMethodCustom = function(fun,
                          center = meanNA,
                          response = getOption('latrend.response'),
                          time = getOption('latrend.time'),
                          id = getOption('latrend.id'),
                          name = 'custom') {
  lcMethod.call('lcMethodCustom', call = match.call.defaults())
}

setValidity('lcMethodCustom', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodCustom)))

  if (isArgDefined(object, 'fun')) {
    assert_that(is.function(object$fun))
  }

  if (isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})

setMethod('getName', signature('lcMethodCustom'), function(object) {
  if (isArgDefined(object, 'name') && !is.null(object$name)) {
    return(object$name)
  }

  if (isArgDefined(object, 'fun')) {
    fun = object[['fun', eval = FALSE]]
    if (is.name(fun)) {
      return(paste('custom function ', fun))
    }
  }

  return('custom function')
})

setMethod('getShortName', signature('lcMethodCustom'), function(object) 'custom')


setMethod('prepareData', signature('lcMethodCustom'), function(method, data, verbose) {
  assert_that(has_name(data, responseVariable(method)))
  return(NULL)
})

setMethod('fit', signature('lcMethodCustom'), function(method, data, envir, verbose) {
  args = as.list(method)
  args$data = data

  model = do.call(method$fun, args)
  model@method = method
  assert_that(is.lcModel(model))
  return(model)
})
