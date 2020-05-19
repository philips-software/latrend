context('longclust')

test_that('default', {
  model = cluslong(clMethodTestLongclust(), testLongData) %>%
    expect_valid_clModel()
})

test_that('t', {
  model = cluslong(clMethodTestLongclustT(), testLongData) %>%
    expect_valid_clModel()
})

test_that('many clusters', {
  model = cluslong(clMethodTestLongclust(nClusters=5), testLongData) %>%
    expect_valid_clModel()
})