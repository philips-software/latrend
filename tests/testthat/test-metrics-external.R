context('externalMetrics')
skip_if_not_installed('mclustcomp')
skip_if_not_installed('clusterCrit')
skip_if_not_installed('psych')
skip_if_not_installed('igraph')
rngReset()

externalMetrics = getExternalMetricNames() %>% setdiff('McNemar')

test_that('two clusters', {
  for(name in externalMetrics) {
    value = externalMetric(testModel2, testModel2, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
    expect_true(is.finite(value), info=name)
  }
})

test_that('single cluster', {
  for(name in externalMetrics) {
    suppressWarnings({
      value = externalMetric(testModel1, testModel1, name=name)
    })
    expect_is(value, 'numeric')
    expect_length(value, 1)
  }
})

test_that('different clusters', {
  for(name in externalMetrics) {
    value = externalMetric(testModel2, testModel3, name=name)
    expect_is(value, 'numeric')
    expect_length(value, 1)
    expect_true(is.finite(value), info=name)
  }
})

test_that('error on empty name', {
  expect_error(externalMetric(testModel1, testModel2, name = NULL))
})

test_that('default names', {
  # if this test fails then the documentation needs to be updated
  expect_error(externalMetric(testModel1, testModel2))
})

test_that('missing metric', {
  expect_warning(met <- externalMetric(testModel1, testModel2, '.MISSING'))
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
