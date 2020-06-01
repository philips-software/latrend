context('GLM-KM')

test_that('default', {
  browser()
  cluslong(clMethodGLMKM(), testLongData)
})