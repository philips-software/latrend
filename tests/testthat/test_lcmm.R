context('LCMM models')

test_that('default gmm', {
  m = clMethodTestGMM()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})

test_that('gmm with single cluster', {
  cluslong(clMethodTestGMM(), testLongData, nClusters=1) %>%
    expect_valid_clModel
})

test_that('gmm with empty cluster', {
  cluslong(clMethodTestGMM(), testLongData, nClusters=5) %>%
    expect_valid_clModel
})

test_that('default gbtm', {
  m = clMethodTestGBTM()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})

test_that('gbtm with nclusters', {
  cluslong(clMethodTestGBTM(), testLongData, nClusters=1) %>%
    expect_valid_clModel
})