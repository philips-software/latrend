context('LM-KM')

test_that('default', {
  set.seed(1)
  m = lcMethodLMKM()
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})

test_that('single cluster', {
  m = lcMethodLMKM(nClusters=1)
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})
