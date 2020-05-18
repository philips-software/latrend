context('cluslongBatch')

test_that('cluslongBatch', {
  cluslongBatch(clMethodTestKML(), testLongData) %>%
    expect_is('clModels') %>%
    expect_length(1)
})

test_that('cluslongBatch with multiple datasets', {
  methods = clMethods(clMethodTestKML(), nClusters=1:2)

  models = cluslongBatch(methods, data=.(testLongData, testLongData[Time < .5], testLongData[Time >= .5])) %>%
    expect_is('clModels') %>%
    expect_length(2*3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time < 0.5]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time >= 0.5]')}
})

test_that('cluslongBatch with datasets list', {
  methods = clMethods(clMethodTestKML(), nClusters=1:2)
  dataList = list(testLongData, testLongData[Time < .5], testLongData[Time >= .5])

  models = cluslongBatch(methods, data=dataList) %>%
    expect_is('clModels') %>%
    expect_length(2*3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})

