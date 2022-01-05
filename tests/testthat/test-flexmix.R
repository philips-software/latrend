context('flexmix')
skip_if_not_installed('flexmix')
rngReset()

lcMethodTestFlexmixGBTM = function(...) {
  lcMethodFlexmixGBTM(
    formula = Value ~ Assessment,
    ...,
    control = list(iter.max = 1, tolerance = 1e-3),
    seed = 1
  )
}

lcMethodTestFlexmix = function(...) {
  lcMethodFlexmix(formula = Value ~ 0, ...)
}

test_that('default', {
  model = latrend(lcMethodTestFlexmix(), data=testLongData)
  expect_valid_lcModel(model)
})

test_that('one cluster', {
  model = latrend(lcMethodTestFlexmix(), data=testLongData, nClusters=1)
  expect_valid_lcModel(model)
})

test_that('empty cluster', {
  suppressWarnings({
    model = latrend(lcMethodTestFlexmix(), data=testLongData, nClusters=5)
  })
  expect_valid_lcModel(model)
})

test_that('model spec', {
  com = flexmix::FLXMRglm(formula=~Assessment)
  model = latrend(lcMethodTestFlexmix(), data=testLongData, model=com)
  expect_valid_lcModel(model)
})

test_that('gbtm', {
  model = latrend(lcMethodTestFlexmixGBTM(), data=testLongData)
  model@model@converged = TRUE
  expect_valid_lcModel(model)
})
