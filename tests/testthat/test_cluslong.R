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

test_that('cluslongBatch with scalar argument', {
  cluslongBatch(clMethodTestKML(), testLongData, nClusters=2) %>%
    expect_is('clModels') %>%
    expect_length(1)
})

test_that('cluslongBatch with vector argument', {
  cluslongBatch(clMethodTestKML(), testLongData, nClusters=1:3) %>%
    expect_is('clModels') %>%
    expect_length(3)
})

test_that('cluslongBatch with .() argument', {
  a = 1
  b = 2
  models = cluslongBatch(clMethodTestKML(), testLongData, nClusters=.(a,b)) %>%
    expect_is('clModels') %>%
    expect_length(2)

  getMethod(models[[1]]) %T>%
    {expect_equal(deparse(.[['nClusters', eval=FALSE]]), 'a')}
  getMethod(models[[2]]) %T>%
    {expect_equal(deparse(.[['nClusters', eval=FALSE]]), 'b')}
})

test_that('cluslongBatch with multiple datasets', {
  models = cluslongBatch(clMethodTestKML(),
                data=.(testLongData, testLongData[Time < .5], testLongData[Time >= .5]),
                nClusters=1:3,
                testfun=mean,
                testfuns=.(mean, median, sd)) %>%
    expect_is('clModels') %>%
    expect_length(3)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time < 0.5]')}
  getCall(models[[3]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData[Time >= 0.5]')}
})