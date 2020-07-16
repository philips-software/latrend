context('mixtools')

test_that('default np', {
  model = latrend(lcMethodTestMixtoolsNPRM(), testLongData) %>%
    expect_valid_lcModel()
})

test_that('default np single cluster', {
  model = latrend(lcMethodTestMixtoolsNPRM(nClusters=1), testLongData) %>%
    expect_valid_lcModel()
})


test_that('default np many clusters', {
  skip('disabled')
  model = latrend(lcMethodTestMixtoolsNPRM(nClusters=5), testLongData) %>%
    expect_valid_lcModel()
})

test_that('default gmm', {
  model = latrend(lcMethodTestMixtoolsGMM(), testLongData) %>%
    expect_valid_lcModel()
})
