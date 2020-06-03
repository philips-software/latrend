context('LM-KM')

test_that('default', {
  m = clMethodLMKM()
  model = cluslong(m, testLongData)
  expect_valid_clModel(model)
})
