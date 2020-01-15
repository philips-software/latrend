context('KML model')

test_that('default', {
  m = clMethodTestKML()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})