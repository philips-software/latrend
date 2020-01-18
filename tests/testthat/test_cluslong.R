context('cluslong')

test_that('cluslong', {
  cluslong(clMethodTestKML(), data=testLongData) %>%
    expect_is('clModel')
})

test_that('cluslong with overwritten argument', {
  cluslong(clMethodTestKML(), data=testLongData, nClusters=1) %T>%
    {expect_equal(nClusters(.), 1)}
})

test_that('cluslong with new arguments', {
  cluslong(clMethodTestKML(), data=testLongData, test=2) %T>%
    {expect_equal(getMethod(.)$test, 2)}
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