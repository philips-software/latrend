context('mixAK')
rngReset()

test_that('default', {
  m = lcMethodTestMixAK_GLMM(PED = FALSE)
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})

test_that('multichain', {
  m = lcMethodTestMixAK_GLMM(PED = TRUE)
  model = latrend(m, testLongData)
  expect_valid_lcModel(model)
})
