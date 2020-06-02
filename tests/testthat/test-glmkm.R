context('GLM-KM')

test_that('default', {
  model = cluslong(clMethodGLMKM(), testLongData)
  expect_valid_clModel(model)
})
