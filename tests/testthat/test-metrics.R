context('metrics')

internalMetrics = getInternalMetricNames() %>%
  setdiff('deviance')

externalMetrics = getExternalMetricNames() %>% setdiff('McNemar')

test_that('internal, two clusters', {
  model = cluslong(clMethodTestLcmmGMM(), testLongData, nClusters=2)

  for(metric in internalMetrics) {
    metric(model, name=metric) %>%
      expect_is('numeric') %T>%
      {expect_true(is.finite(.), info=metric)}
  }
})

test_that('internal, single cluster', {
  model = cluslong(clMethodTestLcmmGMM(), testLongData, nClusters=1)

  for(metric in internalMetrics) {
    metric(model, name=metric) %>%
      expect_is('numeric') %>%
      expect_length(1)
  }
})

test_that('external, two clusters', {
  model1 = cluslong(clMethodTestLcmmGMM(), testLongData, nClusters=2)
  model2 = cluslong(clMethodTestLcmmGBTM(), testLongData, nClusters=2)

  for(metric in externalMetrics) {
    externalMetric(model1, model2, name=metric) %>%
      expect_is('numeric') %>%
      expect_length(1) %T>%
      {expect_true(is.finite(.), info=metric)}
  }
})

test_that('external, different clusters', {
  model1 = cluslong(clMethodTestLcmmGMM(), testLongData, nClusters=2)
  model2 = cluslong(clMethodTestLcmmGBTM(), testLongData, nClusters=3)

  for(metric in externalMetrics) {
    externalMetric(model1, model2, name=metric) %>%
      expect_is('numeric') %>%
      expect_length(1) %T>%
      {expect_true(is.finite(.), info=metric)}
  }
})