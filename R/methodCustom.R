#' @include method.R

#' @name interface-custom
#' @rdname interface-custom
#' @title custom interface
#' @seealso [lcMethodCustom] [lcModelCustom] [lcMethodRandom] [lcMethodStratify] [lcModelPartition] [lcModelWeightedPartition]
NULL

setClass('lcMethodCustom', contains = 'lcMethod')

#' @export
#' @title Specify a custom method based on a model function
#' @param fun The cluster `function` with signature `(method, data)`.
#' @param center Optional `function` for computing the longitudinal cluster centers, with signature `(x)`.
#' @param response The name of the response variable.
#' @param time The name of the time variable.
#' @param id The name of the trajectory identification variable.
#' @param name The name of the method.
#' @examples
#' data(latrendData)
#' # Stratification based on the mean response level
#' clusfun <- function(method, data) {
#'    clusters <- data[, mean(Y) > 2, by = Id] %>%
#'        factor(levels = c(F,T), labels = c("Low", "High"))
#'    lcModelCustom(clusters = clusters)
#' }
#' method <- lcMethodCustom(fun = clusfun)
#' model <- latrend(method, data = latrendData)
#' @family lcMethod implementations
lcMethodCustom = function(response,
                          fun,
                          center = meanNA,
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

#' @rdname interface-custom
#' @inheritParams getName
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

#' @rdname interface-custom
setMethod('getShortName', signature('lcMethodCustom'), function(object) 'custom')

#' @rdname interface-custom
setMethod('prepareData', signature('lcMethodCustom'), function(method, data, verbose) {
  assert_that(has_name(data, responseVariable(method)))
  return(NULL)
})

#' @rdname interface-custom
#' @inheritParams fit
setMethod('fit', signature('lcMethodCustom'), function(method, data, envir, verbose) {
  args = as.list(method)
  args$data = data

  model = do.call(method$fun, args)
  model@method = method
  assert_that(is.lcModel(model))
  return(model)
})
