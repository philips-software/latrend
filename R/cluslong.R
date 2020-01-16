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

#' @export
#' @title Cluster longitudinal data for a series of argument values
#' @description Fit a specified longitudinal cluster method for a series of argument values.
#' Non-`scalar` arguments must all be the same length.
#' @inheritParams cluslong
#' @param data The `data.frame` or `matrix` to which to apply the method. Multiple datasets can be supplied by encapsulating the datasets using `data=.(df1, df2, ..., dfN)`.
#' @param ... Any other arguments to update the `clMethod` definition with. Values must be `scalar`, `vector`, `list`, or encapsulated in a `.()` call.
#' Arguments wrapped in `.()` are passed as-is to the model call, ensuring a readable method.
#' Arguments comprising a single `symbol` (e.g. a variable name) are interpreted as a constant. To force evaluation, specify `arg=(var)` or `arg=force(var)`.
#' Arguments of type `vector` or `list` are split across a series of method fit calls.
#' Arguments of type `scalar` are constant across the method fits.
#' If a `list` is intended to be passed as a constant argument, then specifying `arg=.(listObject)` results in it being treated as such.
#' @param envir The `environment` in which to evaluate the method arguments.
#' @return A `clModels` object.
#' @examples
#' models = cluslongBatch(clMethodKML(), testLongData, nClusters=1:4)
#' models = cluslongBatch(clMethodKML(), testLongData, nClusters=1:4, seed=1) # same seed for all methods
#' models = cluslongBatch(clMethodKML(), testLongData, nClusters=1:4, seed=1:4)
#' nclus = 1:3
#' models = cluslongBatch(clMethodKML(), testLongData, nClusters=(nclus)) # passing a vector variable.
#'
#' models = cluslongBatch(clMethodKML(),
#'    data=.(testLongData[Time > .5,], testLongData[Time < .5,]),
#'    nClusters=1:2, center=meanNA) # different data per method
#'
#' models = cluslongBatch(clMethodKML(),
#'    data=testLongData,
#'    nClusters=1:4,
#'    center=.(meanNA, meanNA, median, median)) # preserve function name per call
#'
#' @family longitudinal cluster fit functions
cluslongBatch = function(method, data, ..., envir=NULL) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  envir = clMethod.env(method, parent.frame(), envir)

  mc = match.call()[-1]
  argNames = names(mc) %>% setdiff(c('', 'method', 'envir'))
  argCalls = mc[argNames]

  if (getLogger()$level <= loglevels['INFO']) {
    cat(sprintf('== Batch estimation for longitudinal clustering of "%s" ==\n', .rep, getName(method)))
    clMethodPrintArgs(mc)
  }

  nameMask = vapply(argCalls, is.name, FUN.VALUE=FALSE)
  dotMask = vapply(argCalls, function(x) is.call(x) && x[[1]] == '.', FUN.VALUE=FALSE)
  evalMask = !nameMask & !dotMask
  evalArgs = lapply(argCalls[evalMask], eval, envir=parent.frame())

  dotLengths = vapply(argCalls[dotMask], length, FUN.VALUE=0) - 1
  evalLengths = lengths(evalArgs)
  nModels = max(1, dotLengths, evalLengths)

  loginfo('Preparing %d method estimations...', nModels)
  assert_that(all(c(dotLengths, evalLengths) %in% c(1L, nModels)), msg=sprintf('arguments must be of length 1 or of equal length to all other arguments (%d)', nModels))

  # create method for each cluslong call
  nameArgs = lapply(which(nameMask), function(i) as.list(argCalls[[i]]))
  dotArgs = lapply(which(dotMask), function(i) as.list(argCalls[[i]][-1]))

  if(hasName(nameArgs, 'data')) {
    dataArg = nameArgs$data
    nameArgs$data = NULL
  } else if(hasName(dotArgs, 'data')) {
    dataArg = dotArgs$data
    dotArgs$data = NULL
  } else if(hasName(evalArgs, 'data')) {
    dataArg = evalArgs$data
    evalArgs$data = NULL
  } else {
    stop('data not specified')
  }

  firstOrN = function(x, i) x[[min(length(x), i)]]

  # using mapply results in dots[[1L]] errors
  methods = vector('list', nModels)
  for(i in seq_len(nModels)) {
    logfinest('\tMethod %d/%d', i, nModels)
    methods[[i]] = do.call(update,
                           c(object=method,
                             lapply(nameArgs, firstOrN, i),
                             lapply(dotArgs, firstOrN, i),
                             lapply(evalArgs, firstOrN, i),
                             envir=envir))
  }

  assert_that(all(vapply(nameArgs, is.list, FUN.VALUE=FALSE)),
              all(vapply(dotArgs, is.list, FUN.VALUE=FALSE)),
              all(vapply(evalArgs, is.vector, FUN.VALUE=FALSE)), msg='The processed argument lists are in an unexpected format. Please report this issue.')

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

canShowModelOutput = function(minLevel='INFO') {
  getLogger()$level <= loglevels[minLevel]
}