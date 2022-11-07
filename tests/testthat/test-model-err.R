context('lcModel implementation error')

setClass('lcModelErr', contains = 'lcModel')
model = testModel
class(model) = 'lcModelErr'

test_that('clusterProportions', {
  setMethod('clusterProportions', 'lcModelErr', function(object, ...) NULL)

  expect_error(clusterProportions(model), 'lcModelErr')
})

test_that('clusterTrajectories', {
  setMethod('clusterTrajectories', 'lcModelErr', function(object, ...) NULL)

  expect_error(clusterTrajectories(model), 'lcModelErr')
})

test_that('postprob', {
  setMethod('postprob', 'lcModelErr', function(object, ...) NULL)

  expect_error(postprob(model), 'lcModelErr')
})

test_that('predictForCluster', {
  setMethod('predictForCluster', 'lcModelErr', function(object, ...) NULL)

  expect_error(predictForCluster(model, cluster = 'A'), 'lcModelErr')
})
