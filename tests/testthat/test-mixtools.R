context('mixtools')
skip_if_not_installed('mixtools')
rngReset()

lcMethodTestMixtoolsNPRM = function(...) {
  lcMethodMixtoolsNPRM(response = 'Value', maxiter = 10, eps = 1e-04, seed = 1)
}

lcMethodTestMixtoolsGMM = function(...) {
  lcMethodMixtoolsGMM(
    formula = Value ~ Assessment + (Assessment | Traj),
    epsilon = 1e-02,
    ...,
    seed = 1
  )
}

test_that('default np', {
  model = latrend(lcMethodTestMixtoolsNPRM(), testLongData)
  expect_valid_lcModel(model)
})

test_that('default np single cluster', {
  model = latrend(lcMethodTestMixtoolsNPRM(nClusters=1), testLongData)
  expect_valid_lcModel(model)
})


test_that('default np many clusters', {
  model = latrend(lcMethodTestMixtoolsNPRM(nClusters=3), testLongData)
  expect_valid_lcModel(model)
})

test_that('default gmm', {
  skip_on_cran()
  model = latrend(lcMethodTestMixtoolsGMM(), testLongData)
  expect_valid_lcModel(model)
})
