context('stratify')

test_that('expression', {
  m = clMethodStratify(mean(Value) > 1)
  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)

  expect_equal(nClusters(model), 2)
  clusTrajs = clusterTrajectories(model)
  expect_lt(clusTrajs[Cluster == 'A', mean(Value)], 1)
  expect_gt(clusTrajs[Cluster == 'B', mean(Value)], 1)
})

test_that('function', {
  stratfun = function(data) mean(data$Value) < 1
  m = clMethodStratify(stratfun)

  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 2)
  clusTrajs = clusterTrajectories(model)
  expect_gt(clusTrajs[Cluster == 'A', mean(Value)], 1)
  expect_lt(clusTrajs[Cluster == 'B', mean(Value)], 1)
})

test_that('in-line function', {
  m = clMethodStratify(function(data) mean(data$Value) > 1)

  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 2)
})

test_that('single logical cluster assignment', {
  stratfun = function(data) FALSE
  m = clMethodStratify(stratfun)

  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 1)
})

test_that('single numeric cluster assignment', {
  stratfun = function(data) 1
  m = clMethodStratify(stratfun)

  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 1)
})

test_that('logical cluster assignment with empty cluster', {
  stratfun = function(data) TRUE
  m = clMethodStratify(stratfun)

  expect_warning({model = cluslong(m, data=testLongData)})
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 2)
})

test_that('multiple cluster factor expression', {
  m = clMethodStratify(cut(mean(Value), c(-Inf, -.5, 0, Inf), labels=c('C', 'B', 'A')))
  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterNames(model), c('C', 'B', 'A'))
})

test_that('multiple cluster numeric expression', {
  m = clMethodStratify(as.numeric(cut(mean(Value), c(-Inf, -.5, 0, Inf))))
  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterNames(model), c('A', 'B', 'C'))
})

test_that('multiple cluster expression with cluster names', {
  m = clMethodStratify(as.numeric(cut(mean(Value), c(-Inf, -.5, 0, Inf))),
                       clusterNames=LETTERS[3:1])
  model = cluslong(m, data=testLongData)
  expect_valid_clModel(model)
  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterNames(model), c('C', 'B', 'A'))
})