context('MixTVEM')

test_that('default', {
  skip('disabled')
  skip_if(!exists('TVEMMixNormal'))

  method = lcMethodMixTVEM(convergenceCriterion = 1, maxIterations=3)
  model = latrend(method, testLongData)
  expect_valid_lcModel(model)
})
