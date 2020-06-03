context('mclust models')

test_that('default llpa', {
  m = clMethodMclustLLPA()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})