#' @export
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
#' @details If a seed value is specified in the `clMethod` object or arguments to `cluslong`, this seed is set using `set.seed` prior to the cluster preparation step.
#' @return A `clModel` object representing the fitted model.
#' @examples
#' model = cluslong(clMethodKML(), data=testLongData)
#'
#' model = cluslong(clMethodKML(), data=testLongData, nClusters=3)
#'
#' model = cluslong(clMethodKML(), data=testLongData, nClusters=3, seed=1)
#' @family longitudinal cluster fit functions
cluslong = function(method, data, ..., envir=NULL) {
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
  environment(method) = envir
  validObject(method)

  if (getLogger()$level <= loglevels['INFO']) {
    cat('== Longitudinal clustering with "', getName(method), '" ==\n', sep='')
    clMethodPrintArgs(method)
  }

  assert_that(not('formula' %in% names(method)) || hasSingleResponse(method$formula))
  assert_that(isArgDefined(method, 'id'), isArgDefined(method, 'time'))
  assert_that(has_name(data, method$id))
  assert_that(has_name(data, method$time))

  if(isArgDefined(method, 'seed')) {
    seed = method$seed
    logfine('Setting seed %s', as.character(seed))
    set.seed(seed)
  }

  loginfo('Preparing data and method...')
  prepEnv = prepare(method, data)
  loginfo('Fitting model...')
  fitEnv = fit(method, data, prepEnv)
  loginfo('Finalizing...')
  model = finalize(method, data, fitEnv)
  assert_that(inherits(model, 'clModel'), msg='finalize(clMethod, ...) returned an unexpected object. Should be clModel.')

  clCall = match.call.defaults()
  model@method = method
  model@call = do.call(call,
                       c('cluslong',
                         method=quote(getCall(method)),
                         data=quote(clCall$data)))
  model@call['envir'] = list(clCall$envir)

  loginfo('Done.')
  return(model)
}

#' @export
#' @title Cluster longitudinal data repeatedly
#' @description Performs a repeated fit of the specified cluslong model on the given data.
#' @inheritParams cluslong
#' @param .rep The number of repeated fits.
#' @param .prepareAll Whether to prepare the data separately per repeated run.
#' @details This method is faster than repeatedly calling [cluslong]() as it only prepares the data once, unless `.prepareAll=TRUE`.
#' @return A `clModels` object containing the resulting models.
#' @examples
#' models = cluslongRep(clMethodKML(), data=testLongData, .rep=5) # 5 repeated runs
#'
#' models = cluslongRep(clMethodKML(), data=testLongData, seed=1, .rep=3)
#' @family longitudinal cluster fit functions
cluslongRep = function(method, data, .rep=1, .prepareAll=FALSE, ..., envir=NULL) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')

  assert_that(is.count(.rep))

  if(is.matrix(data)) {
    data = data.frame(data)
  }

  if(isArgDefined(method, 'seed')) {
    seed = method$seed
    logfine('Setting seed %s', as.character(seed))
    set.seed(seed)
  }

  argList = list(...)
  envir = clMethod.env(method, parent.frame(), envir)
  argList$envir = envir
  method = do.call(update, c(object=method, argList))
  environment(method) = envir
  validObject(method)

  if (getLogger()$level <= loglevels['INFO']) {
    cat(sprintf('== Repeated (%d) longitudinal clustering with "%s" ==\n', .rep, getName(method)))
    clMethodPrintArgs(method)
  }

  if(.prepareAll) {
    loginfo('Preparing data and method %d times...', .rep)
    prepEnvs = replicate(.rep, prepare(method, data))
  } else {
    loginfo('Preparing data and method...')
    prepEnv = prepare(method, data)
    prepEnvs = replicate(.rep, prepEnv)
  }

  fitEnvs = mapply(function(i, iPrepEnv) {
      loginfo('Fitting model %d/%d...', i, .rep)
      fitEnv = fit(method, data, iPrepEnv)
      return(fitEnv)
    }, seq_len(.rep), prepEnvs, SIMPLIFY=FALSE)

  loginfo('Finalizing models...')
  clCall = match.call.defaults()
  models = mapply(function(i, iFitEnv) {
      model = finalize(method, data, iFitEnv)
      model@method = method
      model@call = do.call(call,
                           c('cluslong',
                             method=quote(getCall(method)),
                             data=quote(clCall$data)))
      model@call['envir'] = list(clCall$envir)
      assert_that(inherits(model, 'clModel'), msg=sprintf('finalize(clMethod, ...) returned an unexpected object for run %d. Should be clModel.', i))
      return(model)
    }, seq_len(.rep), fitEnvs, SIMPLIFY=FALSE)

  loginfo('Done.')
  as.clModels(models)
}

canShowModelOutput = function(minLevel='INFO') {
  getLogger()$level <= loglevels[minLevel]
}