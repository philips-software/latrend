context('externalMetrics')
skip_if_not_installed('mclust')
skip_if_not_installed('psych')
skip_if_not_installed('igraph')
rngReset()

externalMetrics = getExternalMetricNames() %>% setdiff('McNemar')

kml1 = latrend(lcMethodTestKML(), testLongData, nClusters=1)
kml2 = latrend(lcMethodTestKML(), testLongData, nClusters=2)
lmkm1 = latrend(lcMethodTestLMKM(), testLongData, nClusters=1)
lmkm2 = latrend(lcMethodTestLMKM(), testLongData, nClusters=2)
lmkm3 = latrend(lcMethodTestLMKM(), testLongData, nClusters=3)

test_that('two clusters', {
  for(name in externalMetrics) {
    value = externalMetric(kml2, lmkm2, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
    expect_true(is.finite(value), info=name)
  }
})

test_that('single cluster', {
  for(name in externalMetrics) {
    suppressWarnings({
      value = externalMetric(kml1, lmkm1, name=name)
    })
    expect_is(value, 'numeric')
    expect_length(value, 1)
  }
})

test_that('different clusters', {
  for(name in externalMetrics) {
    value = externalMetric(kml2, lmkm3, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
    expect_true(is.finite(value), info=name)
  }
})

test_that('error on empty name', {
  expect_error(externalMetric(kml1, kml2, name = NULL))
})

test_that('default names', {
  # if this test fails then the documentation needs to be updated
  expect_error(externalMetric(kml1, kml2))
})

test_that('missing metric', {
  expect_warning(met <- externalMetric(kml1, kml2, '.MISSING'))
  expect_true(is.na(met))
})

test_that('metric definition', {
  expect_true(is.function(getExternalMetricDefinition('adjustedRand')))
})

test_that('missing metric definition', {
  expect_error(getExternalMetricDefinition('.MISSING'))
})

test_that('define metric', {
  fun = function(m1, m2) { }

  defineExternalMetric('.NEW', fun)
  expect_warning(defineExternalMetric('.NEW', fun))

  expect_equal(getExternalMetricDefinition('.NEW'), fun)
})
