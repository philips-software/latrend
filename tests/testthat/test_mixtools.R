context('mixtools')

test_that('default np', {
  model = cluslong(clMethodTestMixtoolsNPRM(), testLongData) %>%
    expect_valid_clModel()
})

test_that('default np single cluster', {
  model = cluslong(clMethodTestMixtoolsNPRM(nClusters=1), testLongData) %>%
    expect_valid_clModel()
})


test_that('default np many clusters', {
  model = cluslong(clMethodTestMixtoolsNPRM(nClusters=5), testLongData) %>%
    expect_valid_clModel()
})
