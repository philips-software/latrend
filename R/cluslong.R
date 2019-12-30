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
#' @param .control Named list of control options.
#' @param .init Model initialization strategy.
#' @param envir The environment in which to evaluate the method arguments.
cluslong = function(method=clMethodKML(), data, ..., .control=list(), .init='random', envir=NULL) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')
  assert_that(is.list(.control), msg='.control must be a named list of options')

  if(is.matrix(data)) {
    data = data.frame(data)
  }

  argList = list(...)
  argList$envir = clMethod.env(method, parent.frame(), envir)
  method = do.call(update, c(object=method, argList))

  if (getLogger()$level >= loglevels['INFO']) {
    cat('== Longitudinal clustering with "', getName(method), '" ==\n', sep='')
    clMethodPrintArgs(method)
  }

  assert_that(hasSingleResponse(method$formula))
  assert_that(has_name(data, method$id))
  assert_that(has_name(data, method$time))

  loginfo('Preparing data and method...')
  prepEnv = prepare(method, data, .control)
  loginfo('Fitting model...')
  fitEnv = fit(method, data, .control, prepEnv)
  loginfo('Finalizing...')
  model = finalize(method, data, .control, fitEnv)
  assert_that(inherits(model, 'clModel'), msg='finalize(clMethod, ...) returned an unexpected object. Should be clModel.')

  clCall = match.call.defaults()
  model@call = do.call(call,
                       c('cluslong',
                         method=quote(getCall(method)),
                         data=quote(clCall$data),
                         .control=quote(clCall$.control),
                         .init=quote(clCall$.init)))
  model@call['envir'] = list(clCall$envir)

  return(model)
}
