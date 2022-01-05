context('MixTVEM')
skip_if(!exists('TVEMMixNormal'), message = 'skipping MixTVEM tests because the TVEMMixNormal() function is not loaded')
rngReset()

lcMethodTestMixTVEM = function(...) {
  lcMethodMixTVEM(
    formula = Value ~ time(1) - 1,
    ...,
    convergenceCriterion = 1e-6,
    numStarts = 10,
    maxIterations = 1e3,
    numInteriorKnots = 6,
    seed = 2L
  )
}

test_that('default', {
  method = lcMethodTestMixTVEM()
  model = latrend(method, testLongData)
  expect_valid_lcModel(model)
})
