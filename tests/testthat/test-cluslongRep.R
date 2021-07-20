context('latrendRep')
rngReset()

m = lcMethodKML(response = 'Value', nbRedrawing = 1, maxIt = 10)
test_that('default', {
  models = latrendRep(lcMethodKML(nbRedrawing = 1, maxIt = 10, response = 'Value'), data=testLongData, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)

  expect_equal(deparse(getCall(models[[1]])$data), 'testLongData')
  expect_equal(deparse(getCall(models[[2]])$data), 'testLongData')
})

test_that('method var', {
  models = latrendRep(m, data = testLongData, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('single rep', {
  models = latrendRep(m, data = testLongData, .rep = 1)

  expect_is(models, 'lcModels')
  expect_length(models, 1)
})

test_that('matrix input', {
  mat = dcastRepeatedMeasures(testLongData, response = 'Value')
  models = latrendRep(m, data = mat, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('envir', {
  kml = lcMethodKML(nClusters = a, response = 'Value', nbRedrawing = 1, maxIt = 10)
  e = list2env(list(a = 1))
  models = latrendRep(kml, data = testLongData, envir = e, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
  expect_equal(nClusters(models[[1]]), 1)
})
