context('stratify')

test_that('expression', {
  m = clMethodStratify(mean(Value) > 1)
  model = cluslong(m, data=testLongData) %T>%
    expect_valid_clModel()

  expect_equal(nClusters(model), 2)
})

test_that('function', {
  stratfun = function(data) mean(data$Value) > 1
  m = clMethodStratify(stratfun)

  model = cluslong(m, data=testLongData) %T>%
    expect_valid_clModel()

  expect_equal(nClusters(model), 2)
})

test_that('in-line function', {
  m = clMethodStratify(function(data) mean(data$Value) > 1)

  model = cluslong(m, data=testLongData) %T>%
    expect_valid_clModel()

  expect_equal(nClusters(model), 2)
})