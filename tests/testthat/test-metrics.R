context('metrics')
skip_if_not_installed('mclust')
skip_if_not_installed('psych')
skip_if_not_installed('igraph')
rngReset()

internalMetrics = getInternalMetricNames() %>% setdiff('deviance')

kml1 = latrend(lcMethodTestKML(), testLongData, nClusters=1)
kml2 = latrend(lcMethodTestKML(), testLongData, nClusters=2)

test_that('two clusters', {


  for(name in internalMetrics) {
    value = metric(kml2, name=name)
    expect_is(value, 'numeric')
    expect_true(is.finite(value), info=name)
  }
})

test_that('single cluster', {
  for(name in internalMetrics) {
    value = metric(kml1, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
  }
})

test_that('error on empty name', {
  expect_error(metric(kml1, name = NULL))
})

test_that('default names', {
  # if this test fails then the documentation needs to be updated
  out = metric(kml2)
  expect_named(out, c('WRSS', 'APPA', 'AIC', 'BIC'))
})
