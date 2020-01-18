context('cluslong')

test_that('cluslong', {
  model = cluslong(clMethodTestKML(), data=testLongData) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'testLongData')
  expect_equal(deparse(getCall(model)$envir), 'NULL')
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

test_that('cluslongRep', {
  cluslongRep(clMethodTestKML(), data=testLongData, .rep=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('cluslongBatch', {
  cluslongBatch(clMethodTestKML(), testLongData) %>%
    expect_is('clModels') %>%
    expect_length(1)
})

test_that('cluslongBatch with multiple datasets', {
  methods = clMethods(clMethodTestKML(), nClusters=1:3)
  models = cluslongBatch(methods, data=.(testLongData, testLongData[Time < .5], testLongData[Time >= .5])) %>%
    expect_is('clModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time < 0.5]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time >= 0.5]')}
})

test_that('cluslongBoot', {
  set.seed(1)
  models = cluslongBoot(clMethodTestKML(), data=testLongData, .samples=3) %>%
    expect_is('clModels') %>%
    expect_length(3)

  # test if data bootstrap sample calls are correct
  expect_equal(deparse(getCall(models[[1]])$data), 'bootSample(testLongData, "Id", 1140350788L)')
  expect_equal(deparse(getCall(models[[2]])$data), 'bootSample(testLongData, "Id", 312928385L)')
  expect_equal(deparse(getCall(models[[3]])$data), 'bootSample(testLongData, "Id", 866248189L)')
})