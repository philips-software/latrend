context('mixAK')
skip_if_not_installed('mixAK')
skip_on_cran()
rngReset()
tests = c(DEFAULT_LATREND_TESTS) %>% setdiff('cluster-three')

make.mixak = function(id, time, response, ...) {
  form = as.formula(sprintf('%s ~ 1', response))
  rand = as.formula(sprintf('~ %s', time))
  lcMethodMixAK_GLMM(fixed = form, time = time, id = id, random = rand, ..., seed = 1) %>% evaluate()
}

test_that('default', {
  expect_true({
    test.latrend('lcMethodMixAK_GLMM', instantiator = make.mixak, tests = tests, args = list(PED = FALSE))
  })
})

test_that('multichain', {
  expect_true({
    test.latrend('lcMethodMixAK_GLMM', instantiator = make.mixak, tests = tests, args = list(PED = TRUE))
  })
})
