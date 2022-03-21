context('longclust')
skip_if_not_installed('longclust')
rngReset()
# longclust does not support K=1
tests = setdiff(DEFAULT_LATREND_TESTS, c('cluster-single', 'data-na', 'data-varlen'))

test_that('vva', {
  expect_true({
    test.latrend(
      'lcMethodLongclust',
      tests = tests,
      clusterRecovery = 'skip',
      args = list(modelSubset = 'VVA', gaussian = TRUE, seed = 1)
    )
  })
})

test_that('vei', {
  expect_true({
    test.latrend(
      'lcMethodLongclust',
      tests = tests,
      clusterRecovery = 'skip',
      args = list(modelSubset = 'VEI', gaussian = FALSE, seed = 1)
    )
  })
})
