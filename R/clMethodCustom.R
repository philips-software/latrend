#' @include clMethod.R
setClass('clMethodCustom', contains='clMethod')

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
#'    clModelCustom(clusters=clusters)
#' }
#' method = clMethodCustom(fun=clusfun)
#' model = cluslong(method, testLongData)
#' summary(model)
#' @family clMethod implementations
clMethodCustom = function(fun,
                       center=meanNA,
                       response=getOption('cluslong.response'),
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       name='custom') {
  clMethod('clMethodCustom', call=match.call.defaults())
}

setValidity('clMethodCustom', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodCustom)))

  if(isArgDefined(object, 'fun')) {
    assert_that(is.function(object$fun))
  }

  if(isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})

setMethod('getName', signature('clMethodCustom'), function(object) {
  if(isArgDefined(object, 'name') && !is.null(object$name)) {
    return(object$name)
  }

  if(isArgDefined(object, 'fun')) {
    fun = object[['fun', eval=FALSE]]
    if(is.name(fun)) {
      return(paste('custom function ', fun))
    }
  }

  return('custom function')
})

setMethod('getShortName', signature('clMethodCustom'), function(object) 'custom')


setMethod('prepare', signature('clMethodCustom'), function(method, data, verbose) {
  assert_that(has_name(data, method$response))
  assert_that(has_name(data, method$id))
  assert_that(has_name(data, method$time))
  return(NULL)
})

setMethod('fit', signature('clMethodCustom'), function(method, data, envir, verbose) {
  args = as.list(method)
  args$data = data

  model = do.call(method$fun, args)
  model@method = method
  assert_that(is.clModel(model))
  return(model)
})

