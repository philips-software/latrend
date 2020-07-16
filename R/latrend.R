#' @export
#' @importFrom stackoverflow match.call.defaults
#' @title Cluster longitudinal data
#' @param method The `lcMethod` object specifying the longitudinal cluster method to apply.
#' @param data The `data.frame` or `matrix` to which to apply the method.
#' @param ... Any other arguments to update the `lcMethod` definition with.
#' @param envir The `environment` in which to evaluate the method arguments. Note that this only applies to `data` when `data` is a `call`.
#' @param verbose The level of verbosity. Either an object of class `Verbose` (see [R.utils::Verbose] for details),
#' a `logical` indicating whether to show basic computation information,
#' a `numeric` indicating the verbosity level (see [Verbose]),
#' or one of `c('info', 'fine', 'finest')`.
#' @details If a seed value is specified in the `lcMethod` object or arguments to `latrend`, this seed is set using `set.seed` prior to the cluster preparation step.
#' @return A `lcModel` object representing the fitted model.
#' @examples
#' model = latrend(lcMethodKML(), data=testLongData)
#'
#' model = latrend(lcMethodKML(), data=testLongData, nClusters=3)
#'
#' model = latrend(lcMethodKML(), data=testLongData, nClusters=3, seed=1)
#' @family longitudinal cluster fit functions
latrend = function(method,
                    data,
                    ...,
                    envir = NULL,
                    verbose = getOption('latrend.verbose')) {
  envir = lcMethod.env(method, parent.frame(), envir)
  assert_that(is.lcMethod(method))

  verbose = as.Verbose(verbose)
  argList = list(...)
  argList$envir = envir
  newmethod = do.call(update, c(object = method, argList))
  environment(newmethod) = envir

  header(verbose,
         sprintf('Longitudinal clustering using "%s"', getName(newmethod)))
  cat(verbose, 'Method arguments:')
  print(verbose, newmethod)
  ruler(verbose)

  cat(verbose,
      'Composing & validating method arguments...',
      level = verboseLevels$finest)

  # compose
  cmethod = compose(newmethod, envir = envir)
  assert_that(is.lcMethod(cmethod), msg=paste0('invalid lcMethod output from compose(', class(newmethod), ')'))

  id = idVariable(cmethod)
  time = timeVariable(cmethod)
  response = responseVariable(cmethod)
  assert_that(
    is.character(idVariable(cmethod)),
    is.character(timeVariable(cmethod)),
    is.character(responseVariable(cmethod))
  )

  # transform data
  modelData = transformLatrendData(
    data,
    id = id,
    time = time,
    response = response,
    envir = envir
  )
  assert_that(is.data.frame(modelData))

  validationResult = validate(cmethod, modelData)
  if (!isTRUE(validationResult)) {
    stop('method validation failed: ', validationResult)
  }

  # prepare
  modelEnv = prepareData(method = cmethod,
                         data = modelData,
                         verbose = verbose)
  assert_that(is.null(modelEnv) ||
                is.environment(modelEnv), msg = 'prepareData(lcMethod, ...) returned an unexpected object. Should be environment or NULL')

  cat(verbose, 'Fitting model')
  pushState(verbose)
  mc = match.call.defaults()
  model = fitLatrendMethod(
    cmethod,
    modelData,
    envir = modelEnv,
    mc = mc,
    verbose = verbose
  )
  popState(verbose)

  # done
  ruler(verbose)
  return(model)
}




fitLatrendMethod = function(method, data, envir, mc, verbose) {
  assert_that(
    is.lcMethod(method),
    is.data.frame(data),
    is.call(mc),
    is.environment(envir) || is.null(envir)
  )

  if (hasName(method, 'seed')) {
    cat(verbose, sprintf('Setting seed %s.', as.character(method$seed)))
    set.seed(method$seed)
  }

  # preFit
  modelEnv = preFit(
    method = method,
    data = data,
    envir = envir,
    verbose = verbose
  )
  assert_that(is.null(modelEnv) ||
                is.environment(modelEnv), msg = 'preFit(lcMethod, ...) returned an unexpected object. Should be environment or NULL')

  # fit
  start = Sys.time()
  model = fit(
    method = method,
    data = data,
    envir = modelEnv,
    verbose = verbose
  )
  estimationTime = Sys.time() - start
  assert_that(is.lcModel(model), msg = 'fit(lcMethod, ...) returned an unexpected object. Should be of type lcModel.')

  model@method = method
  model@call = do.call(call,
                       c(
                         'latrend',
                         method = quote(getCall(method)),
                         data = quote(mc$data)
                       ))
  model@call['envir'] = list(mc$envir)
  model@id = idVariable(method)
  model@time = timeVariable(method)
  model@response = responseVariable(method)
  model@label = getLabel(method)
  model@estimationTime = as.numeric(estimationTime, 'secs')

  # postFit
  model = postFit(
    method = method,
    data = data,
    model = model,
    envir = modelEnv,
    verbose = verbose
  )
  assert_that(inherits(model, 'lcModel'), msg = 'postFit(lcMethod, ...) returned an unexpected object. Should be of type lcModel.')

  return(model)
}



#' @export
#' @title Cluster longitudinal data repeatedly
#' @description Performs a repeated fit of the specified latrend model on the given data.
#' @inheritParams latrend
#' @param .rep The number of repeated fits.
#' @param .errorhandling How to handle fits in which on error occurs.
#' If `"remove"`, errors are ignored and the respective repetition is exempt from the returned model list.
#' If `"stop"`, errors are not caught, ensuring that the function halts on the first error.
#' @param .seed Set the seed for generating the respective seed for each of the repeated fits.
#' @details This method is faster than repeatedly calling [latrend]() as it only prepares the data via `prepareData()` once.
#' @return A `lcModels` object containing the resulting models.
#' @examples
#' models = latrendRep(lcMethodKML(), data=testLongData, .rep=5) # 5 repeated runs
#'
#' models = latrendRep(lcMethodKML(), data=testLongData, seed=1, .rep=3)
#' @family longitudinal cluster fit functions
latrendRep = function(method,
                       data,
                       .rep = 10,
                       ...,
                       .errorhandling = 'remove',
                       .seed = NULL,
                       envir = NULL,
                       verbose = getOption('latrend.verbose')) {
  envir = lcMethod.env(method, parent.frame(), envir)
  assert_that(is.lcMethod(method),
              is.count(.rep))

  verbose = as.Verbose(verbose)
  errh = match.arg(.errorhandling, c('stop', 'remove'))
  argList = list(...)
  argList$envir = envir
  newmethod = do.call(update, c(object = method, argList))
  environment(newmethod) = envir

  header(verbose,
         sprintf(
           'Repeated (%d) longitudinal clustering using "%s"',
           .rep,
           getName(method)
         ))
  cat(verbose, 'Method arguments:')
  print(verbose, newmethod)
  ruler(verbose)


  mc = match.call.defaults()

  # compose
  cmethod = compose(newmethod, envir = envir)

  # seed
  if (hasName(cmethod, 'seed')) {
    warning(
      'The supplied lcMethod object defines a seed, which will result in repeated identical results. Use the .seed argument of latrendRep() to generate different seeds for the repetitions in a reproducible way.'
    )
  }

  cat(verbose,
      sprintf('Generating method seeds for seed = %s.', as.character(.seed)))
  localRNG(seed = .seed, {
    repSeeds = sample.int(.Machine$integer.max,
                          size = .rep,
                          replace = FALSE)
  })

  id = idVariable(cmethod)
  time = timeVariable(cmethod)
  response = responseVariable(cmethod)
  assert_that(
    is.character(idVariable(cmethod)),
    is.character(timeVariable(cmethod)),
    is.character(responseVariable(cmethod))
  )

  # transform data
  modelData = transformLatrendData(
    data,
    id = id,
    time = time,
    response = response,
    envir = envir
  )
  assert_that(is.data.frame(modelData))

  validationResult = validate(cmethod, modelData)
  if (!isTRUE(validationResult)) {
    stop('lcMethod validation failed: ', validationResult)
  }

  enter(verbose, 'Preparing...')
  prepEnv = prepareData(method = method,
                        data = modelData,
                        verbose = verbose)
  exit(verbose)

  models = foreach(
    i = seq_len(.rep),
    iseed = repSeeds,
    .combine = c,
    .errorhandling = errh
  ) %do% {
    cat(verbose,
        'Fitting model %d/%d for seed %s...',
        i,
        .rep,
        as.character(iseed))
    imethod = update(cmethod, seed = iseed, .eval = TRUE)
    fitLatrendMethod(
      imethod,
      data = modelData,
      envir = prepEnv,
      mc = mc,
      verbose = verbose
    )
  }

  as.lcModels(models)
}


# latrend-derived ####
#' @export
#' @title Cluster longitudinal data for a list of model specifications
#' @description Fit a list of longitudinal cluster methods.
#' @inheritParams latrend
#' @param data A `data.frame`, `matrix`, or a `list` thereof to which to apply to the respective `lcMethod`. Multiple datasets can be supplied by encapsulating the datasets using `data=.(df1, df2, ..., dfN)`.
#' @param cartesian Whether to fit the provided methods on each of the datasets. If `cartesian=FALSE`, only a single dataset may be provided or a list of data matching the length of `methods`.
#' @param envir The `environment` in which to evaluate the `lcMethod` arguments.
#' @return A `lcModels` object.
#' @examples
#' methods = lcMethods(lcMethodKML(), nClusters=1:3)
#' models = latrendBatch(methods, data=testLongData)
#'
#' models = latrendBatch(lcMethods(lcMethodKML(), nClusters=1:2),
#'    data=.(testLongData[Time > .5,], testLongData[Time < .5,])) # different data per method
#'
#' @seealso lcMethods
#' @family longitudinal cluster fit functions
latrendBatch = function(methods,
                         data,
                         cartesian = TRUE,
                         envir = NULL,
                         verbose = getOption('latrend.verbose')) {
  if (!is.list(methods)) {
    methods = list(methods)
  }
  assert_that(is.list(methods), all(vapply(
    methods, inherits, 'lcMethod', FUN.VALUE = FALSE
  )), msg = 'methods argument must be a list of lcMethod objects')
  assert_that(!missing(data))
  assert_that(is.logical(cartesian), is.scalar(cartesian))
  envir = lcMethod.env(methods[[1]], parent.frame(), envir)

  verbose = as.Verbose(verbose)
  nModels = length(methods)
  mc = match.call()[-1]

  header(verbose,
         sprintf('Batch estimation (N=%d) for longitudinal clustering', nModels))

  dataCall = mc$data
  if (is.name(dataCall)) {
    dataEval = eval(dataCall, envir = parent.frame())
    assert_that(length(dataEval) >= 1)
    if (is(dataEval, 'list')) {
      dataList = lapply(as.numeric(seq_along(dataEval)), function(d)
        substitute(dataObj[[d]], list(dataObj = dataCall, d = d)))
    } else {
      dataList = list(dataCall)
    }
  } else if (is.call(dataCall) && dataCall[[1]] == '.') {
    dataList = as.list(dataCall[-1])
  } else {
    stop('unsupported data input')
  }
  nData = length(dataList)
  assert_that(cartesian ||
                nData %in% c(1, nModels), msg = 'number of datasets must be 1 or match the number of specified methods')

  # latrend
  cat(verbose, 'Calling latrend for each method...')
  pushState(verbose)

  models = vector('list', nModels * ifelse(cartesian, nData, 1))

  for (m in seq_along(methods)) {
    if (cartesian) {
      dOpts = seq_along(dataList)
    } else {
      dOpts = min(m, nData, nModels)
    }
    for (d in dOpts) {
      cl = do.call(call,
                   c(
                     'latrend',
                     method = quote(methods[[m]]),
                     data = quote(dataList[[d]]),
                     envir = quote(envir),
                     verbose = quote(verbose)
                   ))
      models[[(m - 1) * length(dOpts) + ifelse(cartesian, d, 1)]] = eval(cl)
    }
  }

  popState(verbose)
  cat(verbose, sprintf('Done fitting %d models.', nModels))
  as.lcModels(models)
}


#' @export
#' @title Cluster longitudinal data using bootstrapping
#' @description Performs bootstrapping, generating samples from the given data at the id level, fitting a lcModel to each sample.
#' @inheritParams latrend
#' @param data A `data.frame`.
#' @param samples The number of bootstrap samples to evaluate.
#' @return A `lcModels` object of length `samples`.
#' @examples
#' model = latrendBoot(lcMethodKML(), testLongData, samples=10)
#' @family longitudinal cluster fit functions
#' @family validation methods
latrendBoot = function(method,
                        data,
                        samples = 50,
                        seed = NULL,
                        envir = NULL,
                        verbose = getOption('latrend.verbose')) {
  assert_that(is.lcMethod(method), msg = 'method must be lcMethod object (e.g., lcMethodKML() )')
  assert_that(!missing(data), msg = 'data must be specified')
  assert_that(is.data.frame(data), msg = 'data must be data.frame')
  assert_that(is.count(samples))

  verbose = as.Verbose(verbose)
  header(
    verbose,
    sprintf(
      'Longitudinal cluster estimation using %d bootstrap samples',
      samples
    )
  )
  ruler(verbose)

  mc = match.call()

  # generate seeds
  localRNG(seed = seed, {
    sampleSeeds = sample.int(.Machine$integer.max,
                             size = samples,
                             replace = FALSE)
  })

  id = idVariable(method)
  assert_that(has_name(data, id))

  # fit models
  methods = replicate(samples, method)

  dataCalls = lapply(sampleSeeds, function(s)
    enquote(substitute(
      bootSample(data, id, s),
      env = list(
        data = mc$data,
        id = id,
        s = s
      )
    )))
  dataCall = do.call(call, c(name = '.', dataCalls))

  cl = do.call(
    call,
    list(
      name = 'latrendBatch',
      methods = methods,
      data = enquote(dataCall),
      cartesian = FALSE,
      envir = quote(envir),
      verbose = verbose
    )
  )
  models = eval(cl)

  return(models)
}


# Cross validation ####

#' @export
#' @title Cluster longitudinal data over k folds
#' @description Apply k-fold cross validation for internal cluster validation.
#' Creates k random subsets ("folds") from the data, estimating a model for each of the k-1 combined folds.
#' @inheritParams latrend
#' @param data A `data.frame`.
#' @param folds The number of folds. Ten folds by default.
#' @return A `lcModels` object of containing the `folds` training models.
#' @examples
#' model = latrendCV(lcMethodKML(), testLongData, folds=10)
#'
#' model = latrendCV(lcMethodKML(), testLongData[, Time < .5], folds=10, seed=1)
#' @family longitudinal cluster fit functions
#' @family validation methods
latrendCV = function(method,
                      data,
                      folds = 10,
                      seed = NULL,
                      envir = NULL,
                      verbose = getOption('latrend.verbose')) {
  assert_that(!missing(data), msg = 'data must be specified')
  assert_that(is.data.frame(data), msg = 'data must be data.frame')
  assert_that(is.count(folds))

  verbose = as.Verbose(verbose)
  header(verbose,
         sprintf('Longitudinal clustering with %d-fold cross validation', folds))
  ruler(verbose)

  if (is.null(seed)) {
    seed = sample.int(.Machine$integer.max, size = 1)
  }

  id = idVariable(method)
  assert_that(has_name(data, id))

  mc = match.call()
  dataFoldCalls = lapply(as.numeric(1:folds), function(fold) {
    enquote(substitute(
      trainFold(data, fold = fold, id, folds, seed),
      env = list(
        data = mc$data,
        id = id,
        fold = fold,
        folds = folds,
        seed = seed
      )
    ))
  })
  dataCall = do.call(call, c('.', dataFoldCalls))

  models = do.call(latrendBatch,
                   list(
                     method = method,
                     data = dataCall,
                     verbose = verbose
                   ))

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
createTrainDataFolds = function(data,
                                folds = 10,
                                id = getOption('latrend.id'),
                                seed = NULL) {
  assert_that(is.count(folds), folds > 1)
  assert_that(is.data.frame(data), has_name(data, id))

  ids = unique(data[[id]])

  localRNG(seed = seed, {
    foldIdsList = caret::createFolds(
      seq_along(ids),
      k = folds,
      list = TRUE,
      returnTrain = TRUE
    ) %>%
      lapply(function(i)
        ids[i])
  })

  dataList = lapply(foldIdsList, function(foldIds) {
    data[data[[id]] %in% foldIds, ]
  })
  return(dataList)
}


#' @export
#' @title Create the test fold data for validation
#' @seealso createTrainDataFolds
#' @family validation methods
#' @examples
#' trainDataList = createTrainDataFolds(testLongData, folds=10)
#' testData1 = createTestDataFold(testLongData, trainDataList[[1]])
createTestDataFold = function(data, trainData, id = getOption('latrend.id')) {
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
  lapply(trainDataList, function(trainData)
    createTestDataFold(data = data, trainData = trainData, ...))
}



# Data helper functions ####
#. transformLatrendData ####
#' @export
#' @title Transform latrend input data into the right format
#' @description This function is also responsible for checking whether the input data is valid, such that the fitting process can fail early.
#' @return A `data.frame` with an id, time, and measurement columns.
setGeneric('transformLatrendData', function(object, id, time, response, envir)
  standardGeneric('transformLatrendData'))
setMethod('transformLatrendData', signature('data.frame'), function(object, id, time, response, envir) {
  assert_that(
    is.data.frame(object),
    has_name(object, id),
    has_name(object, time),
    is.numeric(object[[id]]) ||
      is.factor(object[[id]]) || is.character(object[[id]]),
    noNA(object[[id]]),
    noNA(object[[time]])
  )
  object
})

setMethod('transformLatrendData', signature('matrix'), function(object, id, time, response, envir) {
  data = meltRepeatedMeasures(object,
                              id = id,
                              time = time,
                              response = response)
  transformLatrendData(
    data,
    id = id,
    time = time,
    response = response,
    envir = envir
  )
})

setMethod('transformLatrendData', signature('call'), function(object, id, time, response, envir) {
  data = eval(object, envir = envir)
  transformLatrendData(
    data,
    id = id,
    time = time,
    response = response,
    envir = envir
  )
})

#' @export
#' @family validation methods
#' @family model data filters
bootSample = function(data,
                      id = getOption('latrend.id'),
                      seed = NULL) {
  assert_that(is.data.frame(data), has_name(data, id))
  ids = unique(data[[id]])

  localRNG(seed = seed, {
    sampleIdx = sample.int(length(ids), replace = TRUE)
  })

  newdata = data[data[[id]] %in% ids[sampleIdx], ]
  return(newdata)
}


#' @export
#' @importFrom caret createFolds
#' @family validation methods
#' @family model data filters
trainFold = function(data, fold, id, folds, seed) {
  assert_that(is.data.frame(data), has_name(data, id))
  assert_that(!is.null(seed))

  ids = unique(data[[id]])
  localRNG(seed = seed, {
    foldIdx = caret::createFolds(
      seq_along(ids),
      k = folds,
      list = TRUE,
      returnTrain = TRUE
    )[[fold]]
  })

  foldIds = ids[foldIdx]
  return(data[data[[id]] %in% foldIds, ])
}


#' @export
#' @family validation methods
#' @family model data filters
testFold = function(data, fold, id, folds, seed) {
  trainData = foldsTrainData(
    data,
    id = id,
    fold = fold,
    folds = folds,
    seed = seed
  )
  createTestDataFold(data, trainData = trainData, id = id)
}
