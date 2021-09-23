context('lcModel implementation error')

setClass('lcModelErr', contains = 'lcModel')
testModel = latrend(lcMethodTestKML(), data = testLongData)
class(testModel) = 'lcModelErr'

test_that('clusterProportions', {
  setMethod('clusterProportions', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(clusterProportions(testModel), 'lcModelErr')
})

test_that('clusterTrajectories', {
  setMethod('clusterTrajectories', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(clusterTrajectories(testModel), 'lcModelErr')
})

test_that('postprob', {
  setMethod('postprob', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(postprob(testModel), 'lcModelErr')
})

test_that('predictForCluster', {
  setMethod('predictForCluster', signature('lcModelErr'), function(object, ...) NULL)

  expect_error(predictForCluster(testModel, cluster = 'A'), 'lcModelErr')
})
