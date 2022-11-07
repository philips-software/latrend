method = lcMethodLMKM(Value ~ Assessment, id = 'Traj', time = 'Assessment', nClusters = 2)

setClass('lcMethodConv', contains = 'lcMethod')

lcMethodConv = function(
  response = 'Value',
  time = 'Assessment',
  id = 'Traj',
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


test_that('specify default', {
  metaMethod = lcFitConverged(method)
  expect_s4_class(metaMethod, 'lcFitConverged')
  expect_true(has_lcMethod_args(metaMethod, 'maxRep'))
})

test_that('specify with maxRep', {
  metaMethod = lcFitConverged(method, maxRep = 13)
  expect_true(has_lcMethod_args(metaMethod, 'maxRep'))
  expect_equal(metaMethod$maxRep, 13)
})

test_that('fit', {
  metaMethod = lcFitConverged(mRandom)

  expect_no_warning({
    model = latrend(metaMethod, testLongData)
  })
})

test_that('fit until converged', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 2), maxRep = 3)

  # workaround because testthat::expect_message() is failing to capture the output...
  out = capture.output({
    model = latrend(metaMethod, testLongData, verbose = TRUE)
  }, type = 'message')
  expect_match(paste0(out, collapse = '\n'), regexp = 'attempt 2')
  expect_true(converged(model))
})

test_that('fit always fails', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 3), maxRep = 2)
  expect_warning({
    model = latrend(metaMethod, testLongData)
  }, regexp = 'Failed to obtain converged')

  expect_false(converged(model))
})

test_that('fit with seed on first attempt', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 1, seed = 13))
  model = latrend(metaMethod, testLongData)

  expect_equal(getLcMethod(model)$method$seed, 13)
})

test_that('fit different seed on second attempt', {
  metaMethod = lcFitConverged(lcMethodConv(nAttempts = 2, seed = 13))
  model = latrend(metaMethod, testLongData)

  expect_true(getLcMethod(model)$method$seed != 13)
})
