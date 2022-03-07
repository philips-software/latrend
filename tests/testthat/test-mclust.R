context('mclust models')
skip_if_not_installed('mclust')
rngReset()
tests = c(DEFAULT_LATREND_TESTS)

test_that('default', {
  expect_true({
    test.latrend('lcMethodMclustLLPA', tests = tests)
  })
})
