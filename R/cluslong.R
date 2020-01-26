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
#' @param method The `clMethod` object specifying the longitudinal cluster method to apply.
#' @param data The `data.frame` or `matrix` to which to apply the method.
#' @param ... Any other arguments to update the `clMethod` definition with.
#' @param envir The `environment` in which to evaluate the method arguments.
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
  if(is.matrix(data)) {
    data = meltRepeatedMeasures(data, id=method$id, time=method$time, response=getResponse(formula(method)))
  } else {
    assert_that(has_name(data, c(method$id, method$time)))
  }

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
    data = meltRepeatedMeasures(data, id=method$id, time=method$time, response=getResponse(formula(method)))
  } else {
    assert_that(has_name(data, c(method$id, method$time)))
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

#' @export
#' @title Cluster longitudinal data for a list of model specifications
#' @description Fit a list of longitudinal cluster methods.
#' @inheritParams cluslong
#' @param data A `data.frame`, `matrix`, or a `list` thereof to which to apply to the respective `clMethod`. Multiple datasets can be supplied by encapsulating the datasets using `data=.(df1, df2, ..., dfN)`.
#' @param envir The `environment` in which to evaluate the `clMethod` arguments.
#' @return A `clModels` object.
#' @examples
#' methods = clMethods(clMethodKML(), nClusters=1:3)
#' models = cluslongBatch(methods, data=testLongData)
#'
#' models = cluslongBatch(clMethods(clMethodKML(), nClusters=1:2),
#'    data=.(testLongData[Time > .5,], testLongData[Time < .5,])) # different data per method
#'
#' @seealso clMethods
#' @family longitudinal cluster fit functions
cluslongBatch = function(methods, data, envir=NULL) {
  if(!is.list(methods)) {methods = list(methods)}
  assert_that(is.list(methods), all(vapply(methods, inherits, 'clMethod', FUN.VALUE=FALSE)), msg='methods argument must be a list of clMethod objects')
  assert_that(!missing(data))
  envir = clMethod.env(methods[[1]], parent.frame(), envir)

  nModels = length(methods)
  mc = match.call()[-1]

  dataCall = mc$data
  if(is.name(dataCall)) {
    dataArg = replicate(nModels, dataCall)
  } else if(is.call(dataCall) && dataCall[[1]] == '.') {
    assert_that(nModels == length(dataCall) - 1, msg='either provide 1 data object, or a data object per method')
    dataArg = as.list(dataCall[-1])
  } else {
    dataArg = eval(dataCall, envir=parent.frame())
    if(is(dataArg, 'list')) {
      assert_that(length(dataArg) %in% c(1, nModels))
    } else {
      dataArg = list(dataArg)
    }
  }

  if (getLogger()$level <= loglevels['INFO']) {
    cat(sprintf('== Batch estimation (N=%d) for longitudinal clustering ==\n', nModels))
  }

  # cluslong
  loginfo('Calling cluslong for each method...')
  models = vector('list', nModels)
  for(i in seq_along(methods)) {
    cl = do.call(call, c('cluslong',
                  method=quote(methods[[i]]),
                  data=quote(dataArg[[min(i, length(dataArg))]]),
                  envir=quote(envir)))
    models[[i]] = eval(cl)
  }

  loginfo('Done fitting %d models', nModels)
  as.clModels(models)
}

#' @export
#' @title Cluster longitudinal data with bootstrapping
#' @description Performs bootstrapping, generating samples from the given data at the id level, fitting a clModel to each sample.
#' @inheritParams cluslong
#' @param data A `data.frame`.
#' @param .samples The number of bootstrap samples to evaluate.
#' @return A `clModels` object of length `.samples`.
#' @examples
#' model = cluslongBoot(clMethodKML(), testLongData, .samples=10)
#' @family longitudinal cluster fit functions
cluslongBoot = function(method, data, .samples=50, ..., envir=NULL) {
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data), msg='data must be data.frame')
  assert_that(is.count(.samples))

  mc = match.call()

  if(isArgDefined(method, 'seed')) {
    seed = method$seed
    logfine('Setting seed %s', as.character(seed))
    set.seed(seed)
  }

  assert_that(hasName(method, 'id'))
  id = method$id
  assert_that(hasName(data, id))

  # fit models
  methods = replicate(.samples, method)
  sampleSeeds = sample.int(.Machine$integer.max, size=.samples, replace=FALSE)
  dataCalls = lapply(sampleSeeds, function(s) enquote(substitute(bootSample(data, id, s),
                                                         env=list(data=mc$data, id=id, s=s))))
  dataCall = do.call(call, c(name='.', dataCalls))



  cl = do.call(call, list(name='cluslongBatch', methods=methods, data=enquote(dataCall), envir=quote(envir)))
  models = eval(cl)
  return(models)
}

#' @export
#' @title Generate a bootstrap sample from the data
#' @param data The `data.frame` to sample from.
#' @param seed The local seed to set.
bootSample = function(data, id, seed) {
  prevSeed = .Random.seed
  assert_that(is.data.frame(data), has_name(data, id))
  ids = unique(data[[id]])
  set.seed(seed)
  sampleIdx = sample.int(length(ids), replace=TRUE)
  newdata = data[data[[id]] %in% ids[sampleIdx],]
  .Random.seed = prevSeed
  return(newdata)
}

canShowModelOutput = function(minLevel='INFO') {
  getLogger()$level <= loglevels[minLevel]
}