#' @export
#' @docType methods
#' @rdname cluslong-methods
#' @import data.table
#' @import assertthat
#' @import magrittr
#' @import foreach
#' @import logging
#' @import ggplot2
#' @importFrom stackoverflow match.call.defaults
#' @importFrom R.utils printf
#' @title Cluster longitudinal data
#' @param method The clMethod object specifying the longitudinal cluster method to apply.
#' @param data The data.frame or matrix to which to apply the method.
#' @param formula The model formula.
#' @param ... Any other arguments as part of the respective clMethod definition. The clMethod object is updated accordingly.
#' @param envir The environment in which to evaluate the method arguments.
cluslong = function(method=clMethodKML(), data, ..., envir=NULL) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')

  if(is.matrix(data)) {
    data = data.frame(data)
  }

  argList = list(...)
  envir = clMethod.env(method, parent.frame(), envir)
  argList$envir = envir
  method = do.call(update, c(object=method, argList))
  methodx = substitute.clMethod(method, envir=envir)

  if (getLogger()$level <= loglevels['INFO']) {
    cat('== Longitudinal clustering with "', getName(method), '" ==\n', sep='')
    clMethodPrintArgs(method)
  }

  assert_that(not('formula' %in% names(methodx)) || hasSingleResponse(methodx$formula))
  assert_that(has_name(data, methodx$id))
  assert_that(has_name(data, methodx$time))

  loginfo('Preparing data and method...')
  prepEnv = prepare(methodx, data)
  loginfo('Fitting model...')
  fitEnv = fit(methodx, data, prepEnv)
  loginfo('Finalizing...')
  model = finalize(methodx, data, fitEnv)
  assert_that(inherits(model, 'clModel'), msg='finalize(clMethod, ...) returned an unexpected object. Should be clModel.')

  clCall = match.call.defaults()
  model@method = methodx
  model@call = do.call(call,
                       c('cluslong',
                         method=quote(getCall(method)),
                         data=quote(clCall$data)))
  model@call['envir'] = list(clCall$envir)

  return(model)
}

canShowModelOutput = function(minLevel='INFO') {
  getLogger()$level <= loglevels[minLevel]
}