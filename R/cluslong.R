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
#' @param ... Any other arguments as part of the respective clMethod definition. This updates the clMethod object.
#' @param .control Named list of control options.
#' @param .init Model initialization strategy.
cluslong = function(method=clMethodKML(), data, formula=method$formula, ..., .control=list(), .init='random') {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')
  assert_that(is.list(.control), msg='.control must be a named list of options')

  if(is.matrix(data)) {
    data = data.frame(data)
  }

  clCall = match.call.defaults()
  argList = clCall %>% as.list %>% tail(-1)
  argList[c('method', 'data', '.control', '.init')] = NULL
  method = do.call(update, c(object=method, argList))

  if (getLogger()$level >= loglevels['INFO']) {
    cat('== Longitudinal clustering with "', getName(method), '" ==\n', sep='')
    clMethodPrintArgs(method)
  }

  assert_that(hasResponse(method$formula))
  assert_that(hasSingleResponse(method$formula))

  loginfo('Preparing data and method...')
  prepEnv = prepare(method, data, .control)
  loginfo('Fitting model...')
  fitEnv = fit(method, data, .control, prepEnv)
  loginfo('Finalizing...')
  model = finalize(method, data, .control, fitEnv)
  assert_that(inherits(model, 'clModel'), msg='finalize(clMethod, ...) returned an unexpected object. Should be clModel.')

  model@call = do.call(call,
                       c('cluslong',
                         method=quote(getCall(method)),
                         data=quote(clCall$data),
                         .control=quote(clCall$.control),
                         .init=quote(clCall$.init)))

  return(model)
}
