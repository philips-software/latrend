context('mclust models')
skip_if_not_installed('mclust')
rngReset()
tests = c(DEFAULT_LATREND_TESTS)

test.latrend('lcMethodMclustLLPA', tests = tests)
