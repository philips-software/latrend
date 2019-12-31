context('LCMM models')

test_that('default gmm', {
  m = clMethodGMM()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})

test_that('default gbtm', {
  m = clMethodGBTM()
  model = cluslong(m, testLongData) %>%
    expect_silent
  expect_valid_clModel(model)
})