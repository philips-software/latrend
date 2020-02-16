#' @export
#' @import data.table
#' @import assertthat
#' @import magrittr
#' @import foreach
#' @import ggplot2
#' @importFrom stackoverflow match.call.defaults
#' @title Cluster longitudinal data
#' @param method The `clMethod` object specifying the longitudinal cluster method to apply.
#' @param data The `data.frame` or `matrix` to which to apply the method.
#' @param ... Any other arguments to update the `clMethod` definition with.
#' @param envir The `environment` in which to evaluate the method arguments.
#' @param verbose The level of verbosity. Either an object of class `Verbose` (see [R.utils::Verbose] for details),
#' a `logical` indicating whether to show basic computation information,
#' a `numeric` indicating the verbosity level (see [Verbose]),
#' or one of `c('info', 'fine', 'finest')`.
#' @details If a seed value is specified in the `clMethod` object or arguments to `cluslong`, this seed is set using `set.seed` prior to the cluster preparation step.
#' @return A `clModel` object representing the fitted model.
#' @examples
#' model = cluslong(clMethodKML(), data=testLongData)
#'
#' model = cluslong(clMethodKML(), data=testLongData, nClusters=3)
#'
#' model = cluslong(clMethodKML(), data=testLongData, nClusters=3, seed=1)
#' @family longitudinal cluster fit functions
cluslong = function(method, data, ..., envir=NULL, verbose=getOption('cluslong.verbose')) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')

  verbose = as.Verbose(verbose)
  envir = clMethod.env(method, parent.frame(), envir)
  argList = list(...)
  argList$envir = envir
  method = do.call(update, c(object=method, argList))
  environment(method) = envir

  header(verbose, sprintf('Longitudinal clustering with "%s"', getName(method)))
  cat(verbose, 'Method arguments:')
  print(verbose, method)
  ruler(verbose)

  cat(verbose, 'Validating method arguments...', level=verboseLevels$finest)
  validObject(method)

  assert_that(not('formula' %in% names(method)) || hasSingleResponse(method$formula))
  assert_that(isArgDefined(method, 'id'), isArgDefined(method, 'time'))
  if(is.matrix(data)) {
    cat(verbose, 'Transforming repeated measures matrix to long format...', level=verboseLevels$fine)
    data = meltRepeatedMeasures(data, id=method$id, time=method$time, response=getResponse(formula(method)))
  } else {
    assert_that(has_name(data, c(method$id, method$time)))
  }

  if(isArgDefined(method, 'seed')) {
    seed = method$seed
    cat(verbose, sprintf('Setting seed %s.', as.character(seed)))
    set.seed(seed)
  }

  cat(verbose, 'Preparing data and method')
  pushState(verbose)
  prepEnv = prepare(method=method, data=data, verbose=verbose)
  popState(verbose)

  cat(verbose, 'Fitting model')
  pushState(verbose)
  fitEnv = fit(method=method, data=data, envir=prepEnv, verbose=verbose)
  popState(verbose)

  cat(verbose, 'Finalizing...')
  pushState(verbose)
  model = finalize(method=method, data=data, envir=fitEnv, verbose=verbose)
  assert_that(inherits(model, 'clModel'), msg='finalize(clMethod, ...) returned an unexpected object. Should be clModel.')
  popState(verbose)

  clCall = match.call.defaults()
  model@method = method
  model@call = do.call(call,
                       c('cluslong',
                         method=quote(getCall(method)),
                         data=quote(clCall$data)))
  model@call['envir'] = list(clCall$envir)
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
cluslongRep = function(method, data, .rep=1, .prepareAll=FALSE, ..., envir=NULL, verbose=getOption('cluslong.verbose')) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data) || is.matrix(data), msg='data must be data.frame or matrix')

  assert_that(is.count(.rep))

  verbose = as.Verbose(verbose)
  header(verbose, sprintf('Repeated (%d) longitudinal clustering with "%s"', .rep, getName(method)))
  cat(verbose, 'Method arguments:')
  print(verbose, method)
  ruler(verbose)


  if(is.matrix(data)) {
    data = meltRepeatedMeasures(data, id=method$id, time=method$time, response=getResponse(formula(method)))
  } else {
    assert_that(has_name(data, c(method$id, method$time)))
  }

  if(isArgDefined(method, 'seed')) {
    seed = method$seed
    cat(verbose, sprintf('Setting seed %s.', as.character(seed)))
    set.seed(seed)
  }

  envir = clMethod.env(method, parent.frame(), envir)
  argList = list(...)
  argList$envir = envir
  method = do.call(update, c(object=method, argList))
  environment(method) = envir

  cat(verbose, 'Validating method arguments...', level=verboseLevels$finest)
  validObject(method)

  if(.prepareAll) {
    enter(verbose, 'Preparing data and method %d times', .rep)
    prepEnvs = replicate(.rep, prepare(method=method, data=data, verbose=verbose))
    exit(verbose)
  } else {
    enter(verbose, 'Preparing data and method')
    prepEnv = prepare(method=method, data=data, verbose=verbose)
    prepEnvs = replicate(.rep, prepEnv)
    exit(verbose)
  }

  fitEnvs = mapply(function(i, iPrepEnv) {
      cat(verbose, 'Fitting model %d/%d...', i, .rep)
      fitEnv = fit(method=method, data=data, envir=iPrepEnv, verbose=verbose)
      return(fitEnv)
    }, seq_len(.rep), prepEnvs, SIMPLIFY=FALSE)

  enter(verbose, 'Finalizing models')
  clCall = match.call.defaults()
  models = mapply(function(i, iFitEnv) {
      model = finalize(method=method, data=data, envir=iFitEnv, verbose=verbose)
      model@method = method
      model@call = do.call(call,
                           c('cluslong',
                             method=quote(getCall(method)),
                             data=quote(clCall$data)))
      model@call['envir'] = list(clCall$envir)
      assert_that(inherits(model, 'clModel'), msg=sprintf('finalize(clMethod, ...) returned an unexpected object for run %d. Should be clModel.', i))
      return(model)
    }, seq_len(.rep), fitEnvs, SIMPLIFY=FALSE)
  exit(verbose)

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
cluslongBatch = function(methods, data, envir=NULL, verbose=getOption('cluslong.verbose')) {
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

  cat(verbose, sprintf('== Batch estimation (N=%d) for longitudinal clustering ==\n', nModels))

  # cluslong
  cat(verbose, 'Calling cluslong for each method...')
  pushState(verbose)
  models = vector('list', nModels)
  for(i in seq_along(methods)) {
    cl = do.call(call, c('cluslong',
                  method=quote(methods[[i]]),
                  data=quote(dataArg[[min(i, length(dataArg))]]),
                  envir=quote(envir)))
    models[[i]] = eval(cl)
  }

  popState(verbose)
  cat(verbose, sprintf('Done fitting %d models.', nModels))
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
