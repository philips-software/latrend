context('cluslongBatch')

test_that('single method and data, cartesian=TRUE', {
  models = cluslongBatch(clMethodTestKML(), testLongData, cartesian=TRUE) %>%
    expect_is('clModels') %>%
    expect_length(1)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
})

test_that('single method and data, cartesian=FALSE', {
  models = cluslongBatch(clMethodTestKML(), testLongData, cartesian=FALSE) %>%
    expect_is('clModels') %>%
    expect_length(1)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
})

test_that('multiple datasets', {
  methods = clMethods(clMethodTestKML(), nClusters=1:2)

  models = cluslongBatch(methods, data=.(testLongData, testLongData[Time < .5], testLongData[Time >= .5]), cartesian=TRUE) %>%
    expect_is('clModels') %>%
    expect_length(2*3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time < 0.5]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time >= 0.5]')}
})

test_that('datasets list', {
  methods = clMethods(clMethodTestKML(), nClusters=1:2)
  dataList = list(testLongData, testLongData[Time < .5], testLongData[Time >= .5])

  models = cluslongBatch(methods, data=dataList, cartesian=TRUE) %>%
    expect_is('clModels') %>%
    expect_length(2*3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})

test_that('datasets list, cartesian=FALSE', {
  methods = clMethods(clMethodTestKML(), nClusters=1:3)
  dataList = list(testLongData, testLongData[Time < .5], testLongData[Time >= .5])
  models = cluslongBatch(methods, data=dataList, cartesian=FALSE) %>%
    expect_is('clModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})

test_that('single method, multiple datasets', {
  dataList = list(testLongData, testLongData[Time < .5], testLongData[Time >= .5])

  models = cluslongBatch(clMethodTestKML(), data=dataList, cartesian=TRUE) %>%
    expect_is('clModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[1]]')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[2]]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'dataList[[3]]')}
})