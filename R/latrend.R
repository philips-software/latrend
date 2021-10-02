#' @export
#' @title Cluster longitudinal data
#' @description Fit a longitudinal cluster method to the given training data, according to the specification provided by the `lcMethod` object.
#'
#' This function runs all steps as part of the [method fitting procedure][lcMethod-class].
#' @param method An `lcMethod` object specifying the longitudinal cluster method to apply, or the name (as `character`) of an `lcMethod` subclass. See [lcMethod-class][lcMethod-class] for details.
#' @param data The `data.frame` to which to apply the method. Inputs supported by [trajectories()] can also be used.
#' @param ... Any other arguments to update the `lcMethod` definition with.
#' @param envir The `environment` in which to evaluate the method arguments (by [compose()]). This environment is also used to evaluate the `data` argument if it is of type `call`.
#' @param verbose The level of verbosity. Either an object of class `Verbose` (see [R.utils::Verbose] for details),
#' a `logical` indicating whether to show basic computation information,
#' a `numeric` indicating the verbosity level (see [Verbose]),
#' or one of `c('info', 'fine', 'finest')`.
#' @details If a seed value is specified in the `lcMethod` object or arguments to `latrend`, this seed is set using `set.seed` prior to the cluster preparation step.
#' @return A `lcModel` object representing the fitted model.
#' @examples
#' data(latrendData)
#' model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), data = latrendData)
#'
#' model <- latrend("lcMethodKML", response = "Y", id = "Id", time = "Time", data = latrendData)
#'
#' method <- lcMethodKML("Y", id = "Id", time = "Time")
#' model <- latrend(method, data = latrendData, nClusters = 3)
#'
#' model <- latrend(method, data = latrendData, nClusters = 3, seed = 1)
#' @family longitudinal cluster fit functions
latrend = function(method, data, ..., envir = NULL, verbose = getOption('latrend.verbose')) {
  method = as.lcMethod(method)
  assert_that(!missing(data))
  envir = .selectEnvironment(method, parent.frame(), envir)

  verbose = as.Verbose(verbose)
  argList = list(...)
  argList$envir = envir
  newmethod = do.call(update, c(object = method, argList))
  environment(newmethod) = envir

  header(verbose, sprintf('Longitudinal clustering using "%s"', getName(newmethod)))
  cat(verbose, c('Method arguments:', as.character(newmethod)[-1]))
  ruler(verbose)

  # compose
  enter(verbose, 'Evaluating the method arguments.', level = verboseLevels$fine, suffix = '')
  cmethod = compose(newmethod, envir = envir)
  exit(verbose, level = verboseLevels$finest)

  id = idVariable(cmethod)
  time = timeVariable(cmethod)
  response = responseVariable(cmethod)

  # transform data
  enter(verbose, 'Checking and transforming the training data format.', suffix = '')
  modelData = trajectories(
    data,
    id = id,
    time = time,
    response = response,
    envir = envir
  )
  exit(verbose, level = verboseLevels$finest)

  enter(verbose, 'Validating method arguments.', level = verboseLevels$fine, suffix = '')
  validate(cmethod, modelData)
  exit(verbose, level = verboseLevels$finest)

  # prepare
  enter(verbose, 'Preparing the training data for fitting')
  modelEnv = prepareData(method = cmethod,
                         data = modelData,
                         verbose = verbose)
  exit(verbose, level = verboseLevels$finest)

  fitTiming = .enterTimed(verbose, 'Fitting the method')
  mc = match.call.all()
  model = fitLatrendMethod(
    cmethod,
    modelData,
    envir = modelEnv,
    mc = mc,
    verbose = verbose
  )
  environment(model) = envir
  .exitTimed(fitTiming, msg = 'Done fitting the method (%s)')

  # done
  ruler(verbose)
  return (model)
}




fitLatrendMethod = function(method, data, envir, mc, verbose) {
  assert_that(
    is_class_defined(method),
    is.lcMethod(method),
    is.data.frame(data),
    is.call(mc),
    is.environment(envir) || is.null(envir)
  )

  if (hasName(method, 'seed')) {
    cat(verbose, sprintf('Setting seed %s.', as.character(method$seed)))
    set.seed(method$seed)
  }

  suppressFun = ifelse(as.logical(verbose), force, function(...) capture.output(suppressMessages(...)))

  suppressFun({
    # preFit
    modelEnv = preFit(
      method = method,
      data = data,
      envir = envir,
      verbose = verbose
    )

    # fit
    model = fit(
      method = method,
      data = data,
      envir = modelEnv,
      verbose = verbose
    )
  })

  assert_that(is_class_defined(model))

  model@call = do.call(call,
                       c(
                         'latrend',
                         method = quote(getCall(method)),
                         data = quote(mc$data)
                       ))
  model@call['envir'] = list(mc$envir)

  # postFit
  suppressFun({
    model = postFit(
      method = method,
      data = data,
      model = model,
      envir = modelEnv,
      verbose = verbose
    )
  })

  return(model)
}



#' @export
#' @title Cluster longitudinal data repeatedly
#' @description Performs a repeated fit of the specified latrend model on the given data.
#' @inheritParams latrend
#' @param .rep The number of repeated fits.
#' @param .errorHandling Whether to `"stop"` on an error, or to `"remove'` evaluations that raised an error.
#' @param .seed Set the seed for generating the respective seed for each of the repeated fits.
#' @param .parallel Whether to use parallel evaluation. See \link{latrend-parallel}.
#' @details This method is faster than repeatedly calling [latrend] as it only prepares the data via `prepareData()` once.
#' @return A `lcModels` object containing the resulting models.
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time")
#' models <- latrendRep(method, data = latrendData, .rep = 5) # 5 repeated runs
#'
#' models <- latrendRep(method, data = latrendData, .seed = 1, .rep = 3)
#' @family longitudinal cluster fit functions
latrendRep = function(method,
                       data,
                       .rep = 10,
                       ...,
                       .errorHandling = 'stop',
                       .seed = NULL,
                       .parallel = FALSE,
                       envir = NULL,
                       verbose = getOption('latrend.verbose')) {
  method = as.lcMethod(method)
  envir = .selectEnvironment(method, parent.frame(), envir)

  assert_that(
    !missing(data),
    is.count(.rep),
    is.flag(.parallel)
  )

  verbose = as.Verbose(verbose)
  argList = list(...)
  argList$envir = envir
  newmethod = do.call(update, c(object = method, argList))
  environment(newmethod) = envir
  header(verbose, sprintf('Repeated (%d) longitudinal clustering using "%s"', .rep, getName(method)))
  cat(verbose, c('Method arguments:', as.character(newmethod)[-1]))
  ruler(verbose)

  mc = match.call.all()

  # compose
  enter(verbose, 'Evaluating the method arguments.', suffix = '', level = verboseLevels$fine)
  cmethod = compose(newmethod, envir = envir)
  exit(verbose, level = verboseLevels$finest)

  # seed
  if (hasName(cmethod, 'seed')) {
    warning('The supplied lcMethod object defines a seed, which would result in repeated identical results. This seed will be ignored.
      Use the .seed argument of latrendRep() to generate different seeds for the repetitions in a reproducible way.')
  }

  cat(verbose, sprintf('Generating method seeds for seed = %s.', as.character(.seed)))
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
  enter(verbose, 'Checking the training data and ensuring the standard data format.', suffix = '')
  modelData = trajectories(
    data,
    id = id,
    time = time,
    response = response,
    envir = envir
  )
  exit(verbose, level = verboseLevels$finest)

  enter(verbose, 'Validating the method arguments.', suffix = '', level = verboseLevels$fine)
  validate(cmethod, modelData)
  exit(verbose, level = verboseLevels$finest)

  enter(verbose, 'Preparing the training data for fitting')
  prepEnv = prepareData(method = cmethod,
                        data = modelData,
                        verbose = verbose)
  exit(verbose, level = verboseLevels$finest)

  `%infix%` = ifelse(.parallel, `%dopar%`, `%do%`)

  fitTiming = .enterTimed(verbose, 'Fitting the methods')
  models = foreach(
    i = seq_len(.rep),
    iseed = repSeeds,
    .combine = c,
    .errorhandling = .errorHandling
  ) %infix% {
    enter(verbose, sprintf('Fitting model %d/%d (%d%%)', i, .rep, signif(i / .rep * 100, 2)))
    assert_that(is_class_defined(cmethod))
    imethod = update(cmethod, seed = iseed, .eval = TRUE)
    model = fitLatrendMethod(
      imethod,
      data = modelData,
      envir = prepEnv,
      mc = mc,
      verbose = verbose
    )
    environment(model) = envir
    exit(verbose, level = verboseLevels$finest)
    model
  }
  .exitTimed(fitTiming, msg = 'Done fitting the methods (%s)')
  ruler(verbose)

  as.lcModels(models)
}


# latrend-derived ####
#' @export
#' @title Cluster longitudinal data for a list of method specifications
#' @description Fit a list of longitudinal cluster methods on one or more datasets.
#' @details Methods and datasets are evaluated and validated prior to any fitting. This ensures that the batch estimation fails as early as possible in case of errors.
#' @inheritParams latrend
#' @param methods A `list` of `lcMethod` objects.
#' @param data The dataset(s) to which to fit the respective `lcMethod` on.
#' Either a `data.frame`, `matrix`, `list` or an expression evaluating to one of the supported types.
#' Multiple datasets can be supplied by encapsulating the datasets using `data = .(df1, df2, ..., dfN)`.
#' Doing this results in a more readable `call` associated with each fitted `lcModel` object.
#' @param cartesian Whether to fit the provided methods on each of the datasets. If `cartesian=FALSE`, only a single dataset may be provided or a list of data matching the length of `methods`.
#' @param parallel Whether to enable parallel evaluation. See \link{latrend-parallel}. Method evaluation and dataset transformation is done on the calling thread.
#' @param seed Sets the seed for generating the respective seed for each of the method fits. Seeds are only set for methods without a seed argument.
#' @param errorHandling Whether to `"stop"` on an error, or to `"remove'` evaluations that raised an error.
#' @param envir The `environment` in which to evaluate the `lcMethod` arguments.
#' @return A `lcModels` object.
#' In case of a model fit error under `errorHandling = pass`, a `list` is returned.
#' @examples
#' data(latrendData)
#' methods <- lcMethods(lcMethodKML("Y", id = "Id", time = "Time"), nClusters = 1:3)
#' models <- latrendBatch(methods, data = latrendData)
#'
#' models <- latrendBatch(lcMethods(lcMethodKML("Y", id = "Id", time = "Time"), nClusters = 1:2),
#'    data = .(subset(latrendData, Time > .5),
#'             subset(latrendData, Time < .5))) # different data per method
#'
#' @seealso lcMethods
#' @family longitudinal cluster fit functions
latrendBatch = function(methods,
                         data,
                         cartesian = TRUE,
                         seed = NULL,
                         parallel = FALSE,
                         errorHandling = 'stop',
                         envir = NULL,
                         verbose = getOption('latrend.verbose')) {
  if (!is.list(methods)) {
    methods = list(methods)
  }

  assert_that(
    is.list(methods),
    all(vapply(methods, inherits, 'lcMethod', FUN.VALUE = FALSE)),
    msg = 'methods argument must be a list of lcMethod objects'
  )
  assert_that(
    all(lengths(methods) > 0),
    msg = sprintf(
      'the lcMethod object(s) in the "methods" argument at index %s do not have any arguments.',
      paste0(
        which(lengths(methods) == 0),
        ' (',
        vapply(methods[lengths(methods) == 0], class, FUN.VALUE = ''),
        ')',
        collapse = ', '
      )
    )
  )
  assert_that(
    !missing(data),
    is.flag(cartesian),
    is.flag(parallel)
  )

  envir = .selectEnvironment(methods[[1]], parent.frame(), envir)

  verbose = as.Verbose(verbose)
  nMethods = length(methods)
  mc = match.call()[-1]

  dataCall = mc$data
  if (is.call(dataCall) && dataCall[[1]] == '.') {
    # data = .(d1, d2, dN)
    dataList = as.list(dataCall[-1])
  } else {
    # data = varName  OR data = expr
    dataEval = eval(dataCall, envir = parent.frame())
    assert_that(length(dataEval) >= 1)
    if (is(dataEval, 'list')) {
      dataList = lapply(as.numeric(seq_along(dataEval)), function(d)
        substitute(dataObj[[d]], list(dataObj = dataCall, d = d)))
    } else {
      dataList = list(dataCall)
    }
  }
  nData = length(dataList)
  assert_that(
    cartesian || nData %in% c(1, nMethods),
    msg = 'number of datasets must be 1 or match the number of specified methods'
  )

  header(verbose, sprintf('Longitudinal clustering of %d dataset(s) using %d method(s)', nData, nMethods))

  # compose methods
  enter(verbose, sprintf('Evaluating method arguments of %d methods', nMethods), level = verboseLevels$fine, suffix = '')
  methods = lapply(methods, compose, envir = envir)
  exit(verbose, level = verboseLevels$finest)

  # generate method and data lists
  if (cartesian) {
    allMethods = methods[rep(seq_len(nMethods), nData)]
    allDataOpts = dataList[rep(seq_len(nData), nMethods)]
  } else if (nMethods == nData) {
    allMethods = methods
    allDataOpts = dataList
  } else {
    # replicate methods and data such that they are equal length
    allMethods = methods[rep_len(seq_len(nMethods), length.out = max(nMethods, nData))]
    allDataOpts = dataList[rep_len(seq_len(nData), length.out = max(nMethods, nData))]
  }
  assert_that(length(allMethods) == length(allDataOpts))
  nModels = length(allMethods)

  # transform data
  enter(verbose, sprintf('Checking and transforming the data format for %d datasets.', nData), suffix = '', level = verboseLevels$fine)
  allData = lapply(allDataOpts, eval, envir = envir)
  allModelData = mapply(function(data, method) {
    trajectories(data, id = idVariable(method), time = timeVariable(method), response = responseVariable(method), envir = envir)
  }, allData, allMethods, SIMPLIFY = FALSE)
  assert_that(
    is.list(allModelData),
    length(allModelData) == nModels,
    all(vapply(allModelData, is.data.frame, FUN.VALUE = TRUE))
  )
  exit(verbose, level = verboseLevels$finest)

  # validate methods on data
  enter(verbose, sprintf('Validating methods against the datasets (%d checks).', nModels), level = verboseLevels$fine, suffix = '')
  mapply(validate, allMethods, allModelData, SIMPLIFY = FALSE)
  exit(verbose, level = verboseLevels$finest)

  # seeds
  cat(verbose, sprintf('Generating method seeds for seed = %s.', as.character(seed)))
  localRNG(seed = seed, {
    allSeeds = sample.int(.Machine$integer.max, size = nModels, replace = FALSE)
  })

  # update methods that don't have a seed argument
  seedMask = !vapply(allMethods, hasName, 'seed', FUN.VALUE = TRUE)
  allMethods[seedMask] = mapply(
    function(method, seed) update(method, seed = seed, .eval = TRUE),
    allMethods[seedMask],
    allSeeds[seedMask]
  )

  # generate calls
  allCalls = vector('list', length(allMethods))
  for (i in seq_along(allMethods)) {
    allCalls[[i]] = do.call(call,
      c(
        'latrend',
        method = allMethods[[i]],
        data = quote(allDataOpts[[i]]),
        seed = allSeeds[[i]],
        envir = quote(envir),
        verbose = quote(verbose)
      ))
  }

  `%infix%` = ifelse(parallel, `%dopar%`, `%do%`)
  penv = parent.frame()

  # latrend
  fitTiming = .enterTimed(verbose, sprintf('Fitting %d models', nModels))
  ruler(verbose)

  models = foreach(
    i = seq_along(allMethods),
    modelMethod = allMethods,
    modelData = allModelData,
    modelCall = allCalls,
    .packages = 'latrend',
    .errorhandling = errorHandling) %infix%
  {
    modelTiming = .enterTimed(verbose, sprintf('Fitting model %d/%d (%d%%)', i, nModels, round(i / nModels * 100), getName(modelMethod)))
    on.exit(expr = .exitTimed(modelTiming), add = TRUE)

    cat(verbose, as.character(modelMethod, prefix = '- '))
    prepEnv = local({
      enter(verbose, 'Preparing the training data for fitting', suffix = '', level = verboseLevels$fine)
      on.exit(expr = exit(verbose, level = verboseLevels$finest), add = TRUE)
      prepareData(method = modelMethod, data = modelData, verbose = verbose)
    })

    model = fitLatrendMethod(
      method = modelMethod,
      data = modelData,
      mc = modelCall,
      envir = prepEnv,
      verbose = verbose
    )

    return (model)
  }

  .exitTimed(fitTiming)

  # handle model results
  errorMask = !vapply(models, is.lcModel, FUN.VALUE = TRUE)

  if (any(errorMask)) {
    # some list entries are not lcModel
    nError = sum(errorMask)
    cat(verbose, sprintf('Done, but errors occurred in %d out of %d methods', nError, nModels))
    ruler(verbose)
    warning(sprintf(
      'Returning "list" object instead of "lcModels" object for latrendBatch()
      because %d method estimations produced an error',
      nError
    ))
    return (models)
  } else if (length(models) < nModels ) {
    # fewer models were obtained than expected
    nError = nModels - length(models)
    cat(verbose, sprintf('Done, but errors occurred in %d out of %d methods', nError, nModels))
    ruler(verbose)
    return (as.lcModels(models))
  } else {
    # no errors
    ruler(verbose)
    return (as.lcModels(models))
  }
}

#' @export
#' @title Cluster longitudinal data using bootstrapping
#' @description Performs bootstrapping, generating samples from the given data at the id level, fitting a lcModel to each sample.
#' @inheritParams latrend
#' @param data A `data.frame`.
#' @param samples The number of bootstrap samples to evaluate.
#' @param seed The seed to use. Optional.
#' @inheritParams latrendBatch
#' @return A `lcModels` object of length `samples`.
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time")
#' model <- latrendBoot(method, latrendData, samples = 10)
#' @family longitudinal cluster fit functions
#' @family validation methods
latrendBoot = function(method,
                        data,
                        samples = 50,
                        seed = NULL,
                        parallel = FALSE,
                        errorHandling = 'stop',
                        envir = NULL,
                        verbose = getOption('latrend.verbose')) {
  assert_that(is.lcMethod(method), msg = 'method must be lcMethod object (e.g., lcMethodKML("Y") )')
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
      parallel = parallel,
      errorHandling = errorHandling,
      envir = quote(envir),
      verbose = verbose
    )
  )
  models = eval(cl, envir = parent.frame())

  return (models)
}


# Cross validation ####
#' @export
#' @title Cluster longitudinal data over k folds
#' @description Apply k-fold cross validation for internal cluster validation.
#' Creates k random subsets ("folds") from the data, estimating a model for each of the k-1 combined folds.
#' @inheritParams latrend
#' @inheritParams latrendBoot
#' @param data A `data.frame`.
#' @param folds The number of folds. Ten folds by default.
#' @return A `lcModels` object of containing the `folds` training models.
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time")
#' model <- latrendCV(method, latrendData, folds = 5)
#'
#' model <- latrendCV(method, subset(latrendData, Time < .5), folds = 5, seed = 1)
#' @family longitudinal cluster fit functions
#' @family validation methods
latrendCV = function(method,
                      data,
                      folds = 10,
                      seed = NULL,
                      parallel = FALSE,
                      errorHandling = 'stop',
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
                     parallel = parallel,
                     errorHandling = errorHandling,
                     verbose = verbose
                   ), envir = parent.frame())

  return(models)
}



#' @export
#' @importFrom caret createFolds
#' @title Create the training data for each of the k models in k-fold cross validation evaluation
#' @param data A `data.frame` representing the complete dataset.
#' @param folds The number of folds. By default, a 10-fold scheme is used.
#' @param id The trajectory identifier variable.
#' @param seed The seed to use, in order to ensure reproducible fold generation at a later moment.
#' @return A `list` of `data.frame` of the `folds` training datasets.
#' @family validation methods
#' @examples
#' data(latrendData)
#' trainFolds <- createTrainDataFolds(latrendData, folds = 10, id = "Id")
#'
#' trainFolds <- createTrainDataFolds(latrendData, folds = 10, id = "Id", seed = 1)
createTrainDataFolds = function(data,
                                folds = 10,
                                id = getOption('latrend.id'),
                                seed = NULL) {
  assert_that(is.count(folds),
    folds > 1,
    is.data.frame(data),
    has_name(data, id))

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
#' @inheritParams createTrainDataFolds
#' @param trainData A `data.frame` representing the training data, which should be a subset of `data`.
#' @seealso createTrainDataFolds
#' @family validation methods
#' @examples
#' data(latrendData)
#' trainDataList <- createTrainDataFolds(latrendData, id = "Id", folds = 10)
#' testData1 <- createTestDataFold(latrendData, trainDataList[[1]], id = "Id")
createTestDataFold = function(data, trainData, id = getOption('latrend.id')) {
  assert_that(is.data.frame(trainData))
  trainIds = unique(trainData[[id]])
  allIds = unique(data[[id]])
  assert_that(all(trainIds %in% allIds))
  testIds = setdiff(allIds, trainIds)
  data[data[[id]] %in% testIds, ]
}


#' @export
#' @title Create all k test folds from the training data
#' @inheritParams createTestDataFold
#' @param trainDataList A `list` of `data.frame` representing each of the data training folds. These should be derived from `data`.
#' @param ... Arguments passed to [createTestDataFold].
#' @family validation methods
#' @examples
#' data(latrendData)
#' trainDataList <- createTrainDataFolds(latrendData, folds = 10, id = "Id")
#' testDataList <- createTestDataFolds(latrendData, trainDataList)
createTestDataFolds = function(data, trainDataList, ...) {
  lapply(trainDataList, function(trainData)
    createTestDataFold(data = data, trainData = trainData, ...))
}



# Data helper functions ####
#' @export
#' @name lcModel-data-filters
#' @rdname lcModel-data-filters
#' @title Data filters for lcModel
#' @description The data filters are applied by [latrend] prior to model estimation. These filters are used in [latrendBoot] and [latrendCV].
#' @inheritParams trajectories
#' @param data The `data.frame` representing the model dataset.
#' @param seed Optional seed for ensuring reproducibility.
#' @return A subset of `data` of type `data.frame`.
#' @family validation methods
bootSample = function(data, id, seed = NULL) {
  assert_that(is.data.frame(data), has_name(data, id))
  ids = unique(data[[id]])

  localRNG(seed = seed, {
    sampleIdx = sample.int(length(ids), replace = TRUE)
  })

  newdata = data[data[[id]] %in% ids[sampleIdx], ]
  return(newdata)
}


#' @export
#' @rdname lcModel-data-filters
#' @importFrom caret createFolds
#' @param fold The fold to select.
#' @param folds Total number of folds to create.
#' @family validation methods
#' @keywords internal
trainFold = function(data, fold, id, folds, seed) {
  assert_that(is.data.frame(data),
    has_name(data, id),
    !is.null(seed))

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
#' @rdname lcModel-data-filters
#' @family validation methods
testFold = function(data, fold, id, folds, seed) {
  trainData = trainFold(
    data,
    id = id,
    fold = fold,
    folds = folds,
    seed = seed
  )
  createTestDataFold(data, trainData = trainData, id = id)
}
