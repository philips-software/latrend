context('KML model')

test_that('default', {
  m = clMethodKML()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})