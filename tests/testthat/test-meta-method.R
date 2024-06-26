method = lcMethodLMKM(Value ~ time, id = 'id', time = 'time', nClusters = 2)

setClass('lcMethodConv', contains = 'lcMethod')

lcMethodConv = function(
  response = 'Value',
  time = 'time',
  id = 'id',
  nClusters = 1,
  nAttempts = 1,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodConv'
  do.call(new, as.list(mc))
}

setMethod('preFit', 'lcMethodConv', function(method, data, envir, verbose) {
  convAttempts <<- 0
  callNextMethod()
})

setMethod('fit', 'lcMethodConv', function(method, data, envir, verbose) {
  convAttempts <<- convAttempts + 1
  lcModelPartition(
    data = data,
    response = method$response,
    trajectoryAssignments = rep(1, uniqueN(data[[method$id]])),
    converged = convAttempts >= method$nAttempts
  )
})


# tests ####
test_that('specify converged', {
  metaMethod = lcFitConverged(method)
  expect_s4_class(metaMethod, 'lcFitConverged')
  expect_true(has_lcMethod_args(metaMethod, 'maxRep'))
})


test_that('specify converged with maxRep', {
  metaMethod = lcFitConverged(method, maxRep = 13)
  expect_true(has_lcMethod_args(metaMethod, 'maxRep'))
  expect_equal(metaMethod$maxRep, 13)
})


test_that('meta method class', {
  metaMethod = lcFitConverged(method)

  expect_output(print(metaMethod), 'encapsulating')
  expect_output(show(metaMethod), 'encapsulating')

  expect_equal(getName(metaMethod), getName(method))
  expect_equal(getShortName(metaMethod), getShortName(method))
  expect_equal(getLabel(metaMethod), getLabel(method))
  expect_equal(getLcMethod(metaMethod), method)

  expect_equal(idVariable(metaMethod), idVariable(method))
  expect_equal(timeVariable(metaMethod), timeVariable(method))
  expect_equal(responseVariable(metaMethod), responseVariable(method))

  expect_equal(
    getCall(metaMethod),
    call('lcFitConverged', method = getCall(method), maxRep = Inf)
  )
})


test_that('meta converged fit', {
  metaMethod = lcFitConverged(mRandom)

  expect_no_warning({
    model = latrend(metaMethod, testLongData)
  })
})


test_that('meta converged fit until converged', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 2), maxRep = 3)

  # workaround because testthat::expect_message() is failing to capture the output...
  out = capture.output({
    model = latrend(metaMethod, testLongData, verbose = TRUE)
  }, type = 'message')
  expect_match(paste0(out, collapse = '\n'), regexp = 'attempt 2')
  expect_true(converged(model))
})


test_that('meta converged fit always fails', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 3), maxRep = 2)
  expect_warning({
    model = latrend(metaMethod, testLongData)
  }, regexp = 'Failed to obtain converged')

  expect_false(converged(model))
})


test_that('meta converged fit with seed on first attempt', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 1, seed = 13))
  model = latrend(metaMethod, testLongData)

  expect_equal(getLcMethod(model)$seed, 13)
  expect_equal(
    getCall(model),
    call(
      'latrend',
      method = call('lcFitConverged', method = getCall(getLcMethod(model)), maxRep = Inf),
      data = quote(testLongData),
      envir = NULL
    )
  )
})


test_that('meta converged fit different seed on second attempt', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 2, seed = 13))
  model = latrend(metaMethod, testLongData)

  expect_true(getLcMethod(model)$seed != 13)
  expect_equal(
    getCall(model),
    call(
      'latrend',
      method = getCall(metaMethod),
      data = quote(testLongData),
      envir = NULL
    )
  )
})


test_that('meta update passthrough', {
  metaMethod = lcFitConverged(lcMethodLMKM(Value ~ time, nClusters = 2), maxRep = 2)
  model = latrend(metaMethod, data = testLongData, nClusters = 3)

  expect_equal(nClusters(model), 3L)
})
