context('flexmix')

test_that('default', {
  cluslong(clFlexmixMethod(), data=testLongData) %>%
    expect_valid_clModel()
})

test_that('one cluster', {
  cluslong(clFlexmixMethod(), data=testLongData, nClusters=1) %>%
    expect_valid_clModel()
})

test_that('empty cluster', {
  suppressWarnings({
    cluslong(clFlexmixMethod(), data=testLongData, nClusters=5) %>%
      expect_valid_clModel()
  })
})

test_that('model spec', {
  model = flexmix::FLXMRglm(formula=~Time)
  cluslong(clFlexmixMethod(), data=testLongData, model=model) %>%
    expect_valid_clModel()
})

test_that('gbtm', {
  cluslong(clFlexmixMethodTestGBTM(), data=testLongData) %>%
    expect_valid_clModel()
})
