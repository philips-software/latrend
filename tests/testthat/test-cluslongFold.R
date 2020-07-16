context('latrendFold')

test_that('2 folds', {
  latrendCV(lcMethodTestKML(), testLongData, folds=2, seed=1) %>%
    expect_is('lcModels') %>%
    expect_length(2)
})

test_that('3 folds', {
  models = latrendCV(lcMethodTestKML(), testLongData, folds=3, seed=1) %>%
    expect_is('lcModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData, fold = 1, "Id", 3, 1)')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData, fold = 2, "Id", 3, 1)')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData, fold = 3, "Id", 3, 1)')}
})

test_that('without seed', {
  latrendCV(lcMethodTestKML(), testLongData, folds=2) %>%
    expect_is('lcModels') %>%
    expect_length(2)
})

test_that('data subset', {
  models = latrendCV(lcMethodTestKML(), testLongData[Time < .5], folds=2, seed=1) %>%
    expect_is('lcModels') %>%
    expect_length(2)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData[Time < 0.5], fold = 1, "Id", 2, 1)')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData[Time < 0.5], fold = 2, "Id", 2, 1)')}
})

test_that('method var', {
  kml = lcMethodTestKML()
  latrendCV(kml, testLongData, folds=2, seed=1) %>%
    expect_is('lcModels') %>%
    expect_length(2)
})

test_that('createTrainDataFolds', {
  trainDataList = createTrainDataFolds(testLongData, folds=2, seed=1)
  expect_is(trainDataList, 'list')
  expect_length(trainDataList, 2)
  expect_is(trainDataList[[1]], 'data.frame')
  expect_is(trainDataList[[2]], 'data.frame')
  expect_length(union(unique(trainDataList[[1]]$Id), unique(trainDataList[[2]]$Id)), uniqueN(testLongData$Id))
})

test_that('createTestDataFold', {
  trainDataList = createTrainDataFolds(testLongData, folds=2, seed=1)
  testData = createTestDataFold(testLongData, trainDataList[[1]])
  expect_length(intersect(unique(testData$Id), unique(trainDataList[[1]]$Id)), 0)
})

test_that('createTestDataFolds', {
  trainDataList = createTrainDataFolds(testLongData, folds=2, seed=1)
  testDataList = createTestDataFolds(testLongData, trainDataList)
  expect_is(testDataList, 'list')
  expect_length(testDataList, length(trainDataList))

  for(i in seq_along(testDataList)) {
    expect_is(testDataList[[i]], 'data.frame')
    expect_length(intersect(unique(testDataList[[i]]$Id), unique(trainDataList[[i]]$Id)), 0)
  }
})
