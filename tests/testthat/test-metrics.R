context('metrics')
skip_if_not_installed('mclust')
skip_if_not_installed('psych')
skip_if_not_installed('igraph')
rngReset()

internalMetrics = getInternalMetricNames() %>% setdiff('deviance')

test_that('two clusters', {
  for (name in internalMetrics) {
    value = metric(gmm2, name = name)
    expect_is(value, 'numeric')
    expect_true(is.finite(value), info = name)
  }
})

test_that('single cluster', {
  for (name in internalMetrics) {
    value = metric(gmm1, name = name)
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
  expect_named(out, c('WRSS', 'APPA.mean'))
})

test_that('MAE', {
  out = metric(kml2, 'MAE')

  expect_gt(out, 0)
  expect_equivalent(out, mean(abs(residuals(kml2, clusters = trajectoryAssignments(kml2)))))
})

test_that('MSE', {
  out = metric(kml2, 'MSE')

  expect_gt(out, 0)
  expect_equivalent(out, mean(residuals(kml2, clusters = trajectoryAssignments(kml2))^2))
})

test_that('WMAE', {
  wmae = metric(kml2, 'WMAE')
  mae = metric(kml2, 'MAE')

  # for kml, WMAE and MAE should yield same result due to modal assignment
  expect_equivalent(wmae, mae)

  wmaeFuzzy = metric(gmm, 'WMAE')
  maeFuzzy = metric(gmm, 'MAE')

  expect_gte(wmaeFuzzy, maeFuzzy)
})

test_that('Mahalanobis', {
  expect_true('Mahalanobis' %in% getInternalMetricNames())
})

test_that('missing metric', {
  expect_warning(met <- metric(kml2, '.MISSING'))
  expect_true(is.na(met))
})

test_that('metric definition', {
  expect_true(is.function(getInternalMetricDefinition('MAE')))
})

test_that('missing metric definition', {
  expect_error(getInternalMetricDefinition('.MISSING'))
})

test_that('define metric', {
  defineInternalMetric('.NEW', force)
  expect_warning(defineInternalMetric('.NEW', force))

  expect_equal(getInternalMetricDefinition('.NEW'), force)
})
