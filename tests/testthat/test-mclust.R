context('mclust models')
skip_if_not_installed('mclust')
rngReset()
tests = setdiff(DEFAULT_LATREND_TESTS, 'data-na')

test_that('default', {
  expect_true({
    test.latrend('lcMethodMclustLLPA', tests = tests)
  })
})
