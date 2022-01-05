context('mclust models')
skip_if_not_installed('mclust')
rngReset()

lcMethodTestMclustLLPA = function(...) {
  lcMethodMclustLLPA(response = 'Value', ...)
}

test_that('default llpa', {
  m = lcMethodTestMclustLLPA()
  expect_silent({
    model = latrend(m, testLongData)
  })
  expect_valid_lcModel(model)
})
