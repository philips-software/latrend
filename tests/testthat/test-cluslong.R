context('cluslong')

test_that('cluslong', {
  model = cluslong(clMethodTestKML(), data=testLongData) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'testLongData')
  expect_equal(deparse(getCall(model)$envir), 'NULL')
})

test_that('cluslong with matrix input', {
  mat = dcastRepeatedMeasures(testLongData)
  model = cluslong(clMethodTestKML(), data=mat) %>%
    expect_is('clModel')
})

test_that('cluslong with overwritten argument', {
  model = cluslong(clMethodTestKML(), data=testLongData, nClusters=1)

  expect_equal(nClusters(model), 1)
  expect_equal(getMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('cluslong with new arguments', {
  model = cluslong(clMethodTestKML(), data=testLongData, test=2)

  expect_equal(getMethod(model)$test, 2)
})


