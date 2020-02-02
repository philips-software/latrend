context('flexmix')

test_that('default', {
  cluslong(clMethodFlexmix(), data=testLongData) %>%
    expect_valid_clModel()
})

test_that('one cluster', {
  cluslong(clMethodFlexmix(), data=testLongData, nClusters=1) %>%
    expect_valid_clModel()
})

test_that('empty cluster', {
  suppressWarnings({
    cluslong(clMethodFlexmix(), data=testLongData, nClusters=5) %>%
      expect_valid_clModel()
  })
})

test_that('model spec', {
  model = flexmix::FLXMRglm(formula=~Time)
  cluslong(clMethodFlexmix(), data=testLongData, model=model) %>%
    expect_valid_clModel()
})

test_that('gbtm', {
  cluslong(clMethodTestFlexmixGBTM(), data=testLongData) %>%
    expect_valid_clModel()
})
