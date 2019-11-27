#' @export
#' @docType methods
#' @rdname cluslong-methods
#' @import data.table
#' @import assertthat
#' @import magrittr
#' @import foreach
#' @import logging
#' @importFrom stackoverflow match.call.defaults
#' @importFrom R.utils printf
#' @title Cluster longitudinal data
cluslong = function(formula=method$formula, method=clMethodKML(), data, control=list(), init='random', ...) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')
  assert_that(is.list(control), msg='control must be a named list of options')

  if(is.matrix(data)) {
    data = data.frame(data)
  }

  if (getLogger()$level >= loglevels['INFO']) {
    cat('== Longitudinal clustering with "', getName(method), '" ==\n', sep='')
    clMethodPrintArgs(method)
  }

  method = do.call(update, c(object=method, list(...)))

  assert_that(hasResponse(method$formula))
  assert_that(hasSingleResponse(method$formula))

  loginfo('Preparing data and method...')
  prepEnv = prepare(method, data, control)
  loginfo('Fitting model...')
  fitEnv = fit(method, data, control, prepEnv)
  loginfo('Finalizing...')
  model = finalize(method, data, control, fitEnv)
  assert_that(inherits(model, 'clModel'), msg='finalize(clMethod, ...) returned an unexpected object. Should be clModel.')

  return(model)
}
