context('LCMM models')

test_that('default gmm', {
  m = clMethodTestLcmmGMM()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})

test_that('gmm with single cluster', {
  cluslong(clMethodTestLcmmGMM(), testLongData, nClusters=1) %>%
    expect_valid_clModel
})

test_that('gmm with empty cluster', {
  cluslong(clMethodTestLcmmGMM(), testLongData, nClusters=5) %>%
    expect_valid_clModel
})

test_that('default gbtm', {
  m = clMethodTestLcmmGBTM()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})

test_that('gbtm with nclusters', {
  cluslong(clMethodTestLcmmGBTM(), testLongData, nClusters=1) %>%
    expect_valid_clModel
})