context('LCMM models')
skip_if_not_installed('lcmm')
rngReset()

test_that('default gmm', {
  m = lcMethodTestLcmmGMM()
  expect_silent({
    model = latrend(m, testLongData)
  })
  expect_valid_lcModel(model)
})

test_that('gmm with single cluster', {
  model = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=1)
  expect_valid_lcModel(model)
})

test_that('gmm with empty cluster', {
  model = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters=5)
  expect_valid_lcModel(model)
})

test_that('default gbtm', {
  m = lcMethodTestLcmmGBTM()
  expect_silent({
    model = latrend(m, testLongData)
  })
  expect_valid_lcModel(model)
})

test_that('gbtm with nclusters', {
  model = latrend(lcMethodTestLcmmGBTM(), testLongData, nClusters=1)
  expect_valid_lcModel(model)
})

test_that('gmm with init=lme', {
  method = lcMethodTestLcmmGMM(init = 'lme')
  model = latrend(method, testLongData)
  expect_valid_lcModel(model)
})

test_that('gmm with init=lme.random', {
  method = lcMethodTestLcmmGMM(init = 'lme.random')
  model = latrend(method, testLongData)
  expect_valid_lcModel(model)
})
