context('longclust')
skip_if_not_installed('longclust')
rngReset()

lcMethodTestLongclust = function(...) {
  lcMethodLongclust(
    response = 'Value',
    modelSubset = 'VVA',
    gaussian = TRUE,
    ...,
    seed = 1
  )
}

lcMethodTestLongclustT = function(...) {
  lcMethodLongclust(
    response = 'Value',
    modelSubset = 'VEI',
    gaussian = FALSE,
    ...,
    seed = 1
  )
}


test_that('default', {
  model = latrend(lcMethodTestLongclust(), testLongData)
  expect_valid_lcModel(model)
})

test_that('t', {
  model = latrend(lcMethodTestLongclustT(), testLongData)
  expect_valid_lcModel(model)
})

test_that('many clusters', {
  model = latrend(lcMethodTestLongclust(nClusters=3), testLongData)
  expect_valid_lcModel(model)
})
