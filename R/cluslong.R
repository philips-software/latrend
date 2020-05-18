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
  if(is.call(data)) {
    data = eval(data, envir=envir)
  }
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
  assert_that(is.null(prepEnv) || is.environment(prepEnv), msg='expected NULL or environment from prepare(clMethod, ...) call')
  popState(verbose)

  cat(verbose, 'Fitting model')
  pushState(verbose)
  start = Sys.time()
  fitEnv = fit(method=method, data=data, envir=prepEnv, verbose=verbose)
  runTime = Sys.time() - start
  assert_that(is.environment(fitEnv), msg='expected environment from fit(clMethod, ...) call')
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
  model@runTime = as.numeric(runTime, 'secs')
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
      start = Sys.time()
      fitEnv = fit(method=method, data=data, envir=iPrepEnv, verbose=verbose)
      runTime = Sys.time() - start
      assert_that(is.environment(fitEnv), msg='expected environment from fit(clMethod, ...) call')
      fitEnv$.runTime = runTime
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
      model@runTime = as.numeric(iFitEnv$.runTime, 'secs')
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

  verbose = as.Verbose(verbose)
  nModels = length(methods)
  mc = match.call()[-1]

  header(verbose, sprintf('Batch estimation (N=%d) for longitudinal clustering', nModels))

  dataCall = mc$data
  if(is.name(dataCall)) {
    dataEval = eval(dataCall, envir=parent.frame())
    assert_that(length(dataEval) >= 1)
    if(is(dataEval, 'list')) {
      dataList = lapply(as.numeric(seq_along(dataEval)), function(d) substitute(dataObj[[d]], list(dataObj=dataCall, d=d)))
    } else {
      dataList = list(dataCall)
    }
  } else if(is.call(dataCall) && dataCall[[1]] == '.') {
    dataList = as.list(dataCall[-1])
  } else {
    stop('unsupported data input')
  }
  nData = length(dataList)

  # cluslong
  cat(verbose, 'Calling cluslong for each method...')
  pushState(verbose)

  models = vector('list', nModels * nData)
  for(m in seq_along(methods)) {
    for(d in seq_along(dataList)) {
      cl = do.call(call, c('cluslong',
                           method=quote(methods[[m]]),
                           data=quote(dataList[[d]]),
                           envir=quote(envir),
                           verbose=quote(verbose)))
      models[[(m - 1) * nData + d]] = eval(cl)
    }
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
#' @param samples The number of bootstrap samples to evaluate.
#' @return A `clModels` object of length `samples`.
#' @examples
#' model = cluslongBoot(clMethodKML(), testLongData, samples=10)
#' @family longitudinal cluster fit functions
#' @family validation methods
cluslongBoot = function(method, data, samples=50, seed=NULL, envir=NULL, verbose=getOption('cluslong.verbose')) {
  assert_that(is(method, 'clMethod'), msg='method must be clMethod object (e.g., clMethodKML() )')
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data), msg='data must be data.frame')
  assert_that(is.count(samples))

  verbose = as.Verbose(verbose)
  header(verbose, sprintf('Longitudinal cluster estimation with %d bootstrap samples', samples))
  ruler(verbose)

  mc = match.call()

  # generate seeds
  prevSeed = .Random.seed
  set.seed(seed)
  sampleSeeds = sample.int(.Machine$integer.max, size=samples, replace=FALSE)
  if(!is.null(seed)) {
    .Random.seed = prevSeed
  }

  assert_that(hasName(method, 'id'))
  id = method$id
  assert_that(hasName(data, id))

  # fit models
  methods = replicate(samples, method)

  dataCalls = lapply(sampleSeeds, function(s) enquote(substitute(bootSample(data=data, id=id, seed=s),
                                                         env=list(data=mc$data, id=id, s=s))))
  dataCall = do.call(call, c(name='.', dataCalls))

  cl = do.call(call, list(name='cluslongBatch', methods=methods, data=enquote(dataCall), envir=quote(envir), verbose=verbose))
  models = eval(cl)

  return(models)
}


# Cross validation ####

#' @export
#' @title Cluster longitudinal data over k folds
#' @description Apply k-fold cross validation for internal cluster validation.
#' Creates k random subsets ("folds") from the data, estimating a model for each of the k-1 combined folds.
#' @inheritParams cluslong
#' @param data A `data.frame`.
#' @param folds The number of folds. Ten folds by default.
#' @return A `clModels` object of containing the `folds` training models.
#' @examples
#' model = cluslongCV(clMethodKML(), testLongData, folds=10)
#'
#' model = cluslongCV(clMethodKML(), testLongData[, Time < .5], folds=10, seed=1)
#' @family longitudinal cluster fit functions
#' @family validation methods
cluslongCV = function(method, data, folds=10, seed=NULL, envir=NULL, verbose=getOption('cluslong.verbose')) {
  assert_that(!missing(data), msg='data must be specified')
  assert_that(is.data.frame(data), msg='data must be data.frame')
  assert_that(is.count(folds))

  verbose = as.Verbose(verbose)
  header(verbose, sprintf('Longitudinal clustering with %d-fold cross validation', folds))
  ruler(verbose)

  if(is.null(seed)) {
    seed = sample.int(.Machine$integer.max, size=1)
  }

  assert_that(hasName(method, 'id'))
  id = method$id
  assert_that(hasName(data, id))

  mc = match.call()
  dataFoldCalls = lapply(as.numeric(1:folds), function(fold) {
    enquote(substitute(trainFold(data, fold=fold, id, folds, seed),
                       env=list(data=mc$data, id=id, fold=fold, folds=folds, seed=seed)))
  })
  dataCall = do.call(call, c('.', dataFoldCalls))

  models = do.call(cluslongBatch, list(method=method, data=dataCall, verbose=verbose))

  return(models)
}



#' @export
#' @importFrom caret createFolds
#' @title Create the training data for each of the k models in k-fold cross validation evaluation
#' @return A `list` of `data.frame` of the `folds` training datasets.
#' @family validation methods
#' @examples
#' createTrainDataFolds(testLongData, folds=10)
#'
#' createTrainDataFolds(testLongData, folds=10, seed=1)
createTrainDataFolds = function(data, folds=10, id=getOption('cluslong.id'), seed=NULL) {
  assert_that(is.count(folds), folds > 1)
  assert_that(is.data.frame(data), has_name(data, id))

  prevSeed = .Random.seed

  ids = unique(data[[id]])
  set.seed(seed)

  foldIdsList = caret::createFolds(seq_along(ids), k=folds, list=TRUE, returnTrain=TRUE) %>%
    lapply(function(i) ids[i])

  dataList = lapply(foldIdsList, function(foldIds) {
    data[data[[id]] %in% foldIds,]
  })

  if(!is.null(seed)) {
    .Random.seed = prevSeed
  }
  return(dataList)
}


#' @export
#' @title Create the test fold data for validation
#' @seealso createTrainDataFolds
#' @family validation methods
#' @examples
#' trainDataList = createTrainDataFolds(testLongData, folds=10)
#' testData1 = createTestDataFold(testLongData, trainDataList[[1]])
createTestDataFold = function(data, trainData, id=getOption('cluslong.id')) {
  assert_that(is.data.frame(trainData))
  trainIds = unique(trainData[[id]])
  allIds = unique(data[[id]])
  assert_that(all(trainIds %in% allIds))
  testIds = setdiff(allIds, trainIds)
  data[data[[id]] %in% testIds]
}


#' @export
#' @title Create all k test folds from the training data
#' @family validation methods
#' @examples
#' trainDataList = createTrainDataFolds(testLongData, folds=10)
#' testDataList = createTestDataFolds(testLongData, trainDataList)
createTestDataFolds = function(data, trainDataList, ...) {
  lapply(trainDataList, function(trainData) createTestDataFold(data=data, trainData=trainData, ...))
}



# Data helper functions ####

#' @export
#' @family validation methods
#' @family model data filters
bootSample = function(data, id=getOption('cluslong.id'), seed=NULL) {
  assert_that(is.data.frame(data), has_name(data, id))
  prevSeed = .Random.seed
  ids = unique(data[[id]])
  set.seed(seed)
  sampleIdx = sample.int(length(ids), replace=TRUE)
  newdata = data[data[[id]] %in% ids[sampleIdx],]
  if(!is.null(seed)) {
    .Random.seed = prevSeed
  }
  return(newdata)
}


#' @export
#' @importFrom caret createFolds
#' @family validation methods
#' @family model data filters
trainFold = function(data, fold, id, folds, seed) {
  assert_that(is.data.frame(data), has_name(data, id))
  assert_that(!is.null(seed))

  prevSeed = .Random.seed
  set.seed(seed)

  ids = unique(data[[id]])
  foldIdx = caret::createFolds(seq_along(ids), k=folds, list=TRUE, returnTrain=TRUE)[[fold]]
  foldIds = ids[foldIdx]

  .Random.seed = prevSeed
  return(data[data[[id]] %in% foldIds,])
}


#' @export
#' @family validation methods
#' @family model data filters
testFold = function(data, fold, id, folds, seed) {
  trainData = foldsTrainData(data, id=id, fold=fold, folds=folds, seed=seed)
  createTestDataFold(data, trainData=trainData, id=id)
}
