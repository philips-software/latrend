context('metrics')
skip_if_not_installed('mclust')
skip_if_not_installed('psych')
skip_if_not_installed('igraph')
rngReset()

internalMetrics = getInternalMetricNames() %>% setdiff('deviance')

kml1 = latrend(lcMethodTestKML(), testLongData, nClusters = 1)
kml2 = latrend(lcMethodTestKML(), testLongData, nClusters = 2)
gmm = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 3)

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
