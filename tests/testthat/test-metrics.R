context('metrics')

internalMetrics = getInternalMetricNames() %>% setdiff('deviance')

externalMetrics = getExternalMetricNames() %>% setdiff('McNemar')

test_that('internal, two clusters', {
  model = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=2)

  for(name in internalMetrics) {
    value = metric(model, name=name)
    expect_is(value, 'numeric')
    expect_true(is.finite(value), info=name)
  }
})

test_that('internal, single cluster', {
  model = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=1)

  for(name in internalMetrics) {
    value = metric(model, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
  }
})

test_that('external, two clusters', {
  model1 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=2)
  model2 = latrend(lcMethodTestLcmmGBTM(), testLongData, nClusters=2)

  for(name in externalMetrics) {
    value = externalMetric(model1, model2, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
    expect_true(is.finite(value), info=name)
  }
})

test_that('external, different clusters', {
  model1 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=2)
  model2 = latrend(lcMethodTestLcmmGBTM(), testLongData, nClusters=3)

  for(name in externalMetrics) {
    value = externalMetric(model1, model2, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
    expect_true(is.finite(value), info=name)
  }
})
