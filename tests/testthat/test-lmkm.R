context('LM-KM')

test_that('default', {
  set.seed(1)
  m = clMethodLMKM()
  model = cluslong(m, testLongData)
  expect_valid_clModel(model)
})

test_that('single cluster', {
  m = clMethodLMKM(nClusters=1)
  model = cluslong(m, testLongData)
  expect_valid_clModel(model)
})
