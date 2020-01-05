context('MixTVEM')

test_that('default', {
  skip('disabled')
  skip_if(!exists('TVEMMixNormal'))

  method = clMethodMixTVEM(convergenceCriterion = 1, maxIterations=3)
  model = expect_silent(cluslong(method, testLongData))
  expect_valid_clModel(model)
})