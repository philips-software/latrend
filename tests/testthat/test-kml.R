context('KML model')

test_that('default', {
  m = clMethodTestKML()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})

test_that('nclusters', {
  methods = clMethods(clMethodTestKML(), nClusters=c(1, 5))
  models = cluslongBatch(methods, testLongData)

  expect_valid_clModel(models[[1]])
  expect_valid_clModel(models[[2]])
})