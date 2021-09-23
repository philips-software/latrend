context('lcModel implementation error')

setClass('lcModelErr', contains = 'lcModel')
model = modelTest
class(model) = 'lcModelErr'

test_that('clusterProportions', {
  setMethod('clusterProportions', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(clusterProportions(model), 'lcModelErr')
})

test_that('clusterTrajectories', {
  setMethod('clusterTrajectories', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(clusterTrajectories(model), 'lcModelErr')
})

test_that('postprob', {
  setMethod('postprob', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(postprob(model), 'lcModelErr')
})

test_that('predictForCluster', {
  setMethod('predictForCluster', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(predictForCluster(model, cluster = 'A'), 'lcModelErr')
})
