context('mclust models')

test_that('default llpa', {
  m = lcMethodMclustLLPA()
  model = latrend(m, testLongData) %>%
    expect_silent
  expect_valid_lcModel(model)
})
