context('KML model')

test_that('method', {
  kml = lcMethodTestKML()
  expect_output(print(kml))
})

test_that('default', {
  m = lcMethodTestKML()
  model = latrend(m, testLongData) %>%
    expect_silent()
  expect_valid_lcModel(model)
})

test_that('nclusters', {
  methods = lcMethods(lcMethodTestKML(), nClusters=c(1, 5))
  models = latrendBatch(methods, testLongData)

  expect_valid_lcModel(models[[1]])
  expect_valid_lcModel(models[[2]])
})
