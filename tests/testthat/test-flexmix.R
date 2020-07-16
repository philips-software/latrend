context('flexmix')

test_that('default', {
  latrend(lcMethodTestFlexmix(), data=testLongData) %>%
    expect_valid_lcModel()
})

test_that('one cluster', {
  latrend(lcMethodTestFlexmix(), data=testLongData, nClusters=1) %>%
    expect_valid_lcModel()
})

test_that('empty cluster', {
  suppressWarnings({
    latrend(lcMethodTestFlexmix(), data=testLongData, nClusters=5) %>%
      expect_valid_lcModel()
  })
})

test_that('model spec', {
  model = flexmix::FLXMRglm(formula=~Time)
  latrend(lcMethodTestFlexmix(), data=testLongData, model=model) %>%
    expect_valid_lcModel()
})

test_that('gbtm', {
  latrend(lcMethodTestFlexmixGBTM(), data=testLongData) %>%
    expect_valid_lcModel()
})
