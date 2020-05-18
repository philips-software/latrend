context('cluslongFold')

test_that('cluslongFold with 2 folds', {
  cluslongCV(clMethodTestKML(), testLongData, folds=2, seed=1) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('cluslongFold with 3 folds', {
  models = cluslongCV(clMethodTestKML(), testLongData, folds=3, seed=1) %>%
    expect_is('clModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData, fold = 1, "Id", 3, 1)')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData, fold = 2, "Id", 3, 1)')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData, fold = 3, "Id", 3, 1)')}
})

test_that('cluslongFold without seed', {
  cluslongCV(clMethodTestKML(), testLongData, folds=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('cluslongFold with data subset', {
  models = cluslongCV(clMethodTestKML(), testLongData[Time < .5], folds=2, seed=1) %>%
    expect_is('clModels') %>%
    expect_length(2)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData[Time < 0.5], fold = 1, "Id", 2, 1)')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'trainFold(testLongData[Time < 0.5], fold = 2, "Id", 2, 1)')}
})

test_that('cluslongFold with method var', {
  kml = clMethodTestKML()
  cluslongCV(kml, testLongData, folds=2, seed=1) %>%
    expect_is('clModels') %>%
    expect_length(2)
})