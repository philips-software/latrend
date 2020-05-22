context('cluslongRep')

test_that('default', {
  models = cluslongRep(clMethodTestKML(), data=testLongData, .rep=2) %>%
    expect_is('clModels') %>%
    expect_length(2)

  getCall(models[[1]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}

  getCall(models[[2]]) %T>%
    {expect_equal(deparse(.$data), 'testLongData')}
})

test_that('method var', {
  kml = clMethodTestKML()
  cluslongRep(kml, data=testLongData, .rep=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('single rep', {
  cluslongRep(clMethodTestKML(), data=testLongData, .rep=1) %>%
    expect_is('clModels') %>%
    expect_length(1)
})

test_that('matrix input', {
  mat = dcastRepeatedMeasures(testLongData)
  cluslongRep(clMethodTestKML(), data=mat, .rep=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('envir', {
  kml = clMethodKML(nClusters=a, nbRedrawing=1, maxIt=10)
  e = list2env(list(a = 1))

  models = cluslongRep(kml, data=testLongData, envir=e, .rep=2) %>%
    expect_is('clModels') %>%
    expect_length(2)

  expect_equal(nClusters(models[[1]]), 1)
})
