context('LCMM models')
skip_if_not_installed('lcmm')
rngReset()
tests = c(DEFAULT_LATREND_TESTS)

# GMM ####
make.gmm = function(id, time, response, ..., init = NULL) {
  mc = match.call.all()
  mc$fixed = as.formula(sprintf('%s ~ 1', response))
  mc$id = id
  mc$time = time
  mc$maxiter = 10
  mc$seed = 1

  do.call(lcMethodLcmmGMM, as.list(mc)[-1]) %>% evaluate()
}

test.latrend('lcMethodLcmmGMM', instantiator = make.gmm, tests = tests)

test_that('gmm with init=lme', {
  skip_on_cran()
  method = make.gmm(id = 'Traj', time = 'Assessment', response = 'Value', init = 'lme')
  model = latrend(method, testLongData)
  expect_true(is.lcModel(model))
})

test_that('gmm with init=lme.random', {
  skip_on_cran()
  method = make.gmm(id = 'Traj', time = 'Assessment', response = 'Value', init = 'lme.random')
  model = latrend(method, testLongData)
  expect_true(is.lcModel(model))
})

# GBTM ####
make.gbtm = function(id, time, response, ..., init = NULL) {
  mc = match.call.all()
  mc$fixed = as.formula(sprintf('%s ~ 1', response))
  mc$id = id
  mc$time = time
  mc$maxiter = 10
  mc$seed = 1

  do.call(lcMethodLcmmGBTM, as.list(mc)[-1]) %>% evaluate()
}

test.latrend('lcMethodLcmmGBTM', instantiator = make.gbtm, tests = tests)
